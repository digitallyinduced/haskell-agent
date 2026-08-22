-- | Adapt an interactive Claude Code subscription session to the
-- provider-neutral agent loop.
module Agent.ClaudeCode.LoopBackend
    ( withClaudeCodeBackend
    , claudeCodeOneShotBackend
    ) where

import Agent.ClaudeCode.Session
    ( ClaudeCodeOptions
    , ClaudeCodeSession
    , ClaudeCodeTurn
    , claudeCodeTurnDiagnostic
    , claudeCodeTurnIsNewSession
    , claudeCodeTurnProcessExit
    , claudeCodeTurnSessionId
    , claudeCodeTurnTranscriptOffset
    , claudeCodeTurnTranscriptPath
    , sendClaudeCodePrompt
    , withClaudeCodeSession
    , withClaudeCodeSessionWithoutTools
    , withClaudeCodeTurn
    )
import Agent.ClaudeCode.Transcript
    ( CompletedTurn(..)
    , TurnAccumulator
    , consumeTranscriptLine
    , emptyTurnAccumulator
    , finishTranscriptOnExit
    , turnAccumulatorError
    )
import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    )
import Agent.InterAgentMessage (renderInterAgentMessage)
import Agent.Loop
    ( Backend(..)
    , LoopEvent(..)
    , TurnInput(..)
    , TurnOutput(..)
    )
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Responses.Types
    ( CustomToolCall(..)
    , CustomToolCallOutput(..)
    , FunctionCall(..)
    , FunctionCallOutput(..)
    , ItemStatus(..)
    , MessageContent(..)
    , ReasoningConfig(..)
    , ResponseContentPart(..)
    , ResponseCreateParams(..)
    , ResponseItem(..)
    , ResponseMessage(..)
    , ResponseRole(..)
    , TaggedObject
    )
import qualified Agent.ToolDispatch as ToolDispatch
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception.Safe
    ( SomeException
    , displayException
    , tryAny
    )
import Control.Monad (foldM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , readIORef
    )
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word8, Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (doesFileExist)
import System.Exit (ExitCode)
import System.IO
    ( IOMode(ReadMode)
    , SeekMode(AbsoluteSeek)
    , hFileSize
    , hSeek
    , withBinaryFile
    )

-- | Keep one interactive Claude process alive for the callback's complete
-- lifetime. The initial previous-response ID is consumed only by the first
-- submission when the caller does not provide an explicit ID.
withClaudeCodeBackend
    :: ClaudeCodeOptions
    -> Maybe Text
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> (Backend -> IO a)
    -> IO a
withClaudeCodeBackend options initialPrevious getParams transcript callback =
    withClaudeCodeSession options initialPrevious \session ->
        callback (backendForSession session getParams transcript)

-- | A backend for isolated side requests. Every submission owns and cleans up
-- its own interactive Claude process, while still using subscription auth.
claudeCodeOneShotBackend
    :: ClaudeCodeOptions
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
claudeCodeOneShotBackend options getParams transcript =
    Backend \previous inputs onEvent ->
        withClaudeCodeSessionWithoutTools options previous \session ->
            submitClaudeCodeTurn
                session
                Nothing
                getParams
                transcript
                inputs
                onEvent

backendForSession
    :: ClaudeCodeSession
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
backendForSession session getParams transcript =
    Backend \previous inputs onEvent ->
        submitClaudeCodeTurn
            session
            previous
            getParams
            transcript
            inputs
            onEvent

submitClaudeCodeTurn
    :: ClaudeCodeSession
    -> Maybe Text
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> [TurnInput]
    -> (LoopEvent -> IO ())
    -> IO (Either ApiError TurnOutput)
submitClaudeCodeTurn session previous getParams transcript inputs onEvent =
    case flattenTurnInputs inputs of
        Left err -> pure (Left err)
        Right inputText -> do
            params <- getParams
            history <- readIORef transcript
            withClaudeCodeTurn
                session
                previous
                params.model
                (params.reasoning >>= (.effort))
                \turn -> do
                    let prompt =
                            buildClaudePrompt
                                params
                                (claudeCodeTurnIsNewSession turn)
                                history
                                inputText
                    sendClaudeCodePrompt turn prompt
                    awaitResult <- awaitClaudeTurn turn onEvent
                    case awaitResult of
                        Left err ->
                            pure (Left err)
                        Right completed -> do
                            let output = TurnOutput
                                    { responseId =
                                        claudeCodeTurnSessionId turn
                                    -- Claude Code executes its own local tools.
                                    -- Returning them here would execute each
                                    -- call a second time in the host loop.
                                    , toolCalls = []
                                    , assistantText = completed.assistantText
                                    , tokenUsage = completed.tokenUsage
                                    }
                            appendHostTranscript
                                transcript
                                inputs
                                completed.assistantText
                            pure (Right output)

flattenTurnInputs :: [TurnInput] -> Either ApiError Text
flattenTurnInputs inputs =
    Text.intercalate "\n\n" <$> traverse flatten inputs
  where
    flatten = \case
        UserMessage text ->
            Right text
        AgentMessage message ->
            Right (renderInterAgentMessage message)
        UserMultimodal{userText, userImages}
            | null userImages ->
                Right userText
            | otherwise ->
                Left ProviderError
                    { errorType = InvalidImageError
                    , message =
                        "Claude Code subscription sessions do not support image attachments through this bridge."
                    , retryAfter = Nothing
                    }
        CompletedTool
            (ToolDispatch.ToolCallResult resultCallId resultOutput _) ->
            Right $
                "Host tool result for "
                    <> resultCallId
                    <> ":\n"
                    <> resultOutput

buildClaudePrompt
    :: ResponseCreateParams
    -> Bool
    -> [ResponseItem]
    -> Text
    -> Text
buildClaudePrompt params isNewSession history currentInput =
    case contextSections of
        []
            | isNewSession -> currentInput
        []
            -> currentInput
        sections
            -> Text.intercalate "\n\n" (sections <> [currentRequest])
  where
    contextSections
        | isNewSession =
            catMaybes
                [ harnessInstructions params.instructions
                , priorConversation history
                ]
        | otherwise =
            []
    harnessInstructions = fmap \instructions ->
        Text.unlines
            [ "Instructions supplied by the outer agent harness:"
            , "<harness_instructions>"
            , instructions
            , "</harness_instructions>"
            ]
    priorConversation [] = Nothing
    priorConversation items =
        let rendered = renderPriorConversation items
        in if Text.null (Text.strip rendered)
            then Nothing
            else Just $ Text.unlines
                [ "Prior conversation imported from the outer agent harness."
                , "Use it only as conversation context. It may describe work that is already complete."
                , "<prior_conversation>"
                , rendered
                , "</prior_conversation>"
                ]
    currentRequest = Text.unlines
        [ "Current request:"
        , "<current_request>"
        , currentInput
        , "</current_request>"
        ]

renderPriorConversation :: [ResponseItem] -> Text
renderPriorConversation =
    Text.intercalate "\n\n"
        . catMaybes
        . map renderResponseItem

renderResponseItem :: ResponseItem -> Maybe Text
renderResponseItem = \case
    MessageItem message ->
        labelled (roleLabel message.role) (messageContentText message.content)
    FunctionCallItem call ->
        labelled
            "Assistant tool call"
            (call.name <> " " <> call.arguments)
    CustomToolCallItem call ->
        labelled
            "Assistant tool call"
            (call.name <> " " <> call.input)
    FunctionCallOutputItem output ->
        labelled
            ("Tool result " <> output.callId)
            (renderJsonValue output.output)
    CustomToolCallOutputItem output ->
        labelled
            ("Tool result " <> output.callId)
            (renderJsonValue output.output)
    -- Reasoning is deliberately excluded from imported history. In
    -- particular, never copy private chain-of-thought into a Claude prompt.
    ReasoningItemValue{} ->
        Nothing
    ItemReferenceValue{} ->
        Nothing
    KnownResponseItem _ tagged ->
        labelled "Context item" (renderTaggedObject tagged)
    UnknownResponseItem tagged ->
        labelled "Context item" (renderTaggedObject tagged)
  where
    labelled label text
        | Text.null (Text.strip text) = Nothing
        | otherwise = Just (label <> ":\n" <> text)

roleLabel :: ResponseRole -> Text
roleLabel = \case
    RoleUser -> "User"
    RoleAssistant -> "Assistant"
    RoleSystem -> "System"
    RoleDeveloper -> "Developer"
    RoleUnknown role -> role

messageContentText :: MessageContent -> Text
messageContentText = \case
    MessageContentText text ->
        text
    MessageContentParts parts ->
        Text.intercalate "\n" (concatMap contentPartText parts)

contentPartText :: ResponseContentPart -> [Text]
contentPartText = \case
    InputTextPart{text} -> [text]
    OutputTextPart{text} -> [text]
    RefusalPart{refusal} -> [refusal]
    SummaryTextPart{text} -> [text]
    InputImagePart{} -> ["[image omitted]"]
    InputFilePart{filename} ->
        ["[file" <> maybe "" (" " <>) filename <> " omitted]"]
    InputAudioPart{} -> ["[audio omitted]"]
    -- Do not render reasoning text into another model's prompt.
    ReasoningTextPart{} -> []
    UnknownContentPart{} -> []

renderTaggedObject :: TaggedObject -> Text
renderTaggedObject =
    TextEncoding.decodeUtf8With lenientDecode
        . LazyByteString.toStrict
        . Aeson.encode

renderJsonValue :: Aeson.Value -> Text
renderJsonValue = \case
    Aeson.String text -> text
    value ->
        TextEncoding.decodeUtf8With lenientDecode
            (LazyByteString.toStrict (Aeson.encode value))

appendHostTranscript
    :: IORef [ResponseItem]
    -> [TurnInput]
    -> Maybe Text
    -> IO ()
appendHostTranscript transcript inputs assistantText =
    atomicModifyIORef' transcript \history ->
        ( history
            <> turnInputsToItems inputs
            <> [assistantMessageItem assistantText]
        , ()
        )

assistantMessageItem :: Maybe Text -> ResponseItem
assistantMessageItem assistantText =
    MessageItem ResponseMessage
        { messageId = Nothing
        , content = MessageContentParts
            [ OutputTextPart
                { text = fromMaybe "" assistantText
                , annotations = Nothing
                , logprobs = Nothing
                , extraFields = KeyMap.empty
                }
            ]
        , role = RoleAssistant
        , status = Just ItemCompleted
        , phase = Nothing
        , extraFields = KeyMap.empty
        }

data TranscriptTail = TranscriptTail
    { tailOffset :: !Integer
    , tailPending :: !ByteString.ByteString
    }

awaitClaudeTurn
    :: ClaudeCodeTurn
    -> (LoopEvent -> IO ())
    -> IO (Either ApiError CompletedTurn)
awaitClaudeTurn turn onEvent = do
    startedAt <- getMonotonicTimeNSec
    go startedAt emptyTurnAccumulator TranscriptTail
        { tailOffset = claudeCodeTurnTranscriptOffset turn
        , tailPending = ByteString.empty
        }
        Nothing
        startedAt
        False
  where
    go
        startedAt
        accumulator
        tailState
        endTurnObservedAt
        lastProgressAt
        sawTranscriptProgress = do
        readResult <-
            readTranscriptAppend
                (claudeCodeTurnTranscriptPath turn)
                tailState
        case readResult of
            Left err ->
                pure (Left err)
            Right (nextTail, completeLines) -> do
                now <- getMonotonicTimeNSec
                let madeProgress =
                        nextTail.tailOffset /= tailState.tailOffset
                            || nextTail.tailPending /= tailState.tailPending
                    nextLastProgressAt =
                        if madeProgress then now else lastProgressAt
                    nextSawTranscriptProgress =
                        sawTranscriptProgress || madeProgress
                consumed <-
                    consumeLines accumulator completeLines onEvent
                case consumed of
                    Left err ->
                        pure (Left err)
                    Right (_nextAccumulator, Just completed) ->
                        pure (Right completed)
                    Right (nextAccumulator, Nothing) -> do
                        let endTurnFallback =
                                finishTranscriptOnExit nextAccumulator
                            nextEndTurnObservedAt =
                                case endTurnFallback of
                                    Nothing -> Nothing
                                    Just _ ->
                                        endTurnObservedAt <|> Just now
                        case (endTurnFallback, nextEndTurnObservedAt) of
                            (Just completed, Just observedAt)
                                | elapsedAtLeast
                                    endTurnGraceNanoseconds
                                    observedAt
                                    now ->
                                    pure (Right completed)
                            _ -> do
                                processExit <-
                                    claudeCodeTurnProcessExit turn
                                waitingForConfirmation <-
                                    if nextSawTranscriptProgress
                                        then pure False
                                        else
                                            looksLikeInteractivePrompt
                                                <$> claudeCodeTurnDiagnostic turn
                                case processExit of
                                    Just exitCode ->
                                        finishAfterExit
                                            exitCode
                                            nextAccumulator
                                            nextTail
                                    Nothing
                                        | waitingForConfirmation ->
                                            Left <$> timeoutError
                                                "is waiting for an interactive confirmation"
                                        | not nextSawTranscriptProgress
                                        , elapsedAtLeast
                                            transcriptStartupTimeoutNanoseconds
                                            startedAt
                                            now ->
                                            Left <$> timeoutError
                                                "did not create or update its transcript"
                                        | nextSawTranscriptProgress
                                        , elapsedAtLeast
                                            transcriptInactivityTimeoutNanoseconds
                                            nextLastProgressAt
                                            now ->
                                            Left <$> timeoutError
                                                "stopped making transcript progress"
                                        | elapsedAtLeast
                                            turnTimeoutNanoseconds
                                            startedAt
                                            now ->
                                            Left <$> timeoutError
                                                "did not complete within the turn timeout"
                                        | otherwise -> do
                                            threadDelay transcriptPollMicros
                                            go
                                                startedAt
                                                nextAccumulator
                                                nextTail
                                                nextEndTurnObservedAt
                                                nextLastProgressAt
                                                nextSawTranscriptProgress

    finishAfterExit exitCode accumulator tailState = do
        -- Re-read once after observing process exit. The child may have
        -- flushed its final transcript records between the preceding file
        -- read and 'getProcessExitCode'.
        finalRead <-
            readTranscriptAppend
                (claudeCodeTurnTranscriptPath turn)
                tailState
        case finalRead of
            Left err ->
                pure (finishOrError accumulator err)
            Right (finalTail, completeLines) -> do
                completeConsumed <-
                    consumeLines accumulator completeLines onEvent
                case completeConsumed of
                    Left err ->
                        pure (Left err)
                    Right (_, Just completed) ->
                        pure (Right completed)
                    Right (afterLines, Nothing) -> do
                        let pending =
                                stripCarriageReturn finalTail.tailPending
                        if ByteString.null pending
                            then pure $
                                finishOrError
                                    afterLines
                                    (processExitError exitCode)
                            else do
                                pendingConsumed <-
                                    consumeLines afterLines [pending] onEvent
                                pure case pendingConsumed of
                                    Left err@JsonDecodeError{} ->
                                        finishOrError afterLines err
                                    Left err ->
                                        Left err
                                    Right (_, Just completed) ->
                                        Right completed
                                    Right (finalAccumulator, Nothing) ->
                                        finishOrError
                                            finalAccumulator
                                            (processExitError exitCode)

    finishOrError accumulator err =
        case finishTranscriptOnExit accumulator of
            Just completed -> Right completed
            Nothing -> Left err

    timeoutError reason = do
        diagnostic <- claudeCodeTurnDiagnostic turn
        pure $ ConnectionError
            ( "Claude Code "
                <> if looksLikeInteractivePrompt diagnostic
                    then
                        "is waiting at an interactive confirmation prompt that this bridge cannot answer."
                    else reason <> "."
            )

transcriptPollMicros :: Int
transcriptPollMicros = 50_000

endTurnGraceNanoseconds :: Word64
endTurnGraceNanoseconds = 1_000_000_000

transcriptStartupTimeoutNanoseconds :: Word64
transcriptStartupTimeoutNanoseconds = 60 * 1_000_000_000

transcriptInactivityTimeoutNanoseconds :: Word64
transcriptInactivityTimeoutNanoseconds = 15 * 60 * 1_000_000_000

turnTimeoutNanoseconds :: Word64
turnTimeoutNanoseconds = 2 * 60 * 60 * 1_000_000_000

elapsedAtLeast :: Word64 -> Word64 -> Word64 -> Bool
elapsedAtLeast duration startedAt now =
    now - startedAt >= duration

looksLikeInteractivePrompt :: Text -> Bool
looksLikeInteractivePrompt raw =
    any (`Text.isInfixOf` Text.toCaseFold raw)
        [ "trust this folder"
        , "do you trust"
        , "press enter to confirm"
        , "waiting for confirmation"
        ]

consumeLines
    :: TurnAccumulator
    -> [ByteString.ByteString]
    -> (LoopEvent -> IO ())
    -> IO (Either ApiError (TurnAccumulator, Maybe CompletedTurn))
consumeLines initial lines_ onEvent =
    foldM step (Right (initial, Nothing)) lines_
  where
    step result _line
        | Right (_, Just _) <- result =
            pure result
    step (Left err) _ =
        pure (Left err)
    step (Right (accumulator, Nothing)) line
        | ByteString.null (trimAsciiWhitespace line) =
            pure (Right (accumulator, Nothing))
        | otherwise =
            case consumeTranscriptLine accumulator line of
                Left decodeError ->
                    pure $ Left JsonDecodeError
                        { decodeError
                        , rawBody =
                            Text.take 2_000 $
                                TextEncoding.decodeUtf8With lenientDecode line
                        }
                Right (nextAccumulator, events, completed) -> do
                    mapM_ onEvent events
                    pure case turnAccumulatorError nextAccumulator of
                        Just message ->
                            Left ProviderError
                                { errorType = ApiErrorType
                                , message
                                , retryAfter = Nothing
                                }
                        Nothing ->
                            Right (nextAccumulator, completed)

readTranscriptAppend
    :: FilePath
    -> TranscriptTail
    -> IO
        (Either
            ApiError
            (TranscriptTail, [ByteString.ByteString]))
readTranscriptAppend path tailState = do
    result <- tryAny (readAvailable path tailState)
    pure case result of
        Left exception ->
            Left (transcriptReadError path exception)
        Right value ->
            Right value

readAvailable
    :: FilePath
    -> TranscriptTail
    -> IO (TranscriptTail, [ByteString.ByteString])
readAvailable path tailState = do
    exists <- doesFileExist path
    if not exists
        then pure (tailState, [])
        else withBinaryFile path ReadMode \handle -> do
            size <- hFileSize handle
            let (startOffset, oldPending)
                    | size < tailState.tailOffset =
                        (0, ByteString.empty)
                    | otherwise =
                        (tailState.tailOffset, tailState.tailPending)
            hSeek handle AbsoluteSeek startOffset
            appended <- ByteString.hGet
                handle
                (fromIntegral (size - startOffset))
            let newOffset = size
                combined = oldPending <> appended
            let (completeLines, pending) = splitCompleteLines combined
            pure
                ( TranscriptTail
                    { tailOffset = newOffset
                    , tailPending = pending
                    }
                , completeLines
                )

splitCompleteLines
    :: ByteString.ByteString
    -> ([ByteString.ByteString], ByteString.ByteString)
splitCompleteLines bytes
    | ByteString.null bytes =
        ([], ByteString.empty)
    | otherwise =
        let pieces = ByteString.split newlineByte bytes
            terminated =
                ByteString.last bytes == newlineByte
            (lines_, pending)
                | terminated =
                    (init pieces, ByteString.empty)
                | otherwise =
                    (init pieces, last pieces)
        in (map stripCarriageReturn lines_, pending)

newlineByte :: Word8
newlineByte = 10

stripCarriageReturn :: ByteString.ByteString -> ByteString.ByteString
stripCarriageReturn line
    | not (ByteString.null line)
    , ByteString.last line == 13 =
        ByteString.init line
    | otherwise =
        line

trimAsciiWhitespace :: ByteString.ByteString -> ByteString.ByteString
trimAsciiWhitespace =
    ByteString.dropWhileEnd isAsciiWhitespace
        . ByteString.dropWhile isAsciiWhitespace
  where
    isAsciiWhitespace byte =
        byte `elem` [9, 10, 13, 32]

transcriptReadError :: FilePath -> SomeException -> ApiError
transcriptReadError path exception =
    ConnectionError
        ( "Unable to read Claude Code transcript "
            <> Text.pack path
            <> ": "
            <> Text.pack (displayException exception)
        )

processExitError :: ExitCode -> ApiError
processExitError exitCode =
    ConnectionError
        ( "Claude Code exited before completing its response ("
            <> Text.pack (show exitCode)
            <> ")."
        )
