-- | Adapt a structured Claude Code subscription session to the
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
    , readClaudeCodeOutputLine
    , resolveClaudeCodeTurnUsage
    , sendClaudeCodePrompt
    , withClaudeCodeSession
    , withClaudeCodeSessionWithoutTools
    , withClaudeCodeTurn
    )
import Agent.ClaudeCode.Stream
    ( CompletedTurn(..)
    , StreamAccumulator
    , consumeStreamLine
    , emptyStreamAccumulator
    , streamAccumulatorError
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
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.UUID.Types as UUID
import System.Exit (ExitCode)
import System.Mem.StableName
    ( StableName
    , eqStableName
    , makeStableName
    )
import System.Timeout (timeout)

data HostTranscriptCheckpoint = HostTranscriptCheckpoint
    { checkpointTranscript :: !(StableName [ResponseItem])
    , checkpointSessionId :: !Text
    }

-- | Keep one structured Claude process alive for the callback's complete
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
    withClaudeCodeSession options initialPrevious \session -> do
        checkpoint <- newIORef Nothing
        callback (backendForSession session checkpoint getParams transcript)

-- | A backend for isolated side requests. Every submission owns and cleans up
-- its own structured Claude process, while still using subscription auth.
claudeCodeOneShotBackend
    :: ClaudeCodeOptions
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
claudeCodeOneShotBackend options getParams transcript =
    Backend \previous inputs onEvent -> do
        checkpoint <- newIORef Nothing
        withClaudeCodeSessionWithoutTools options previous \session ->
            submitClaudeCodeTurn
                session
                checkpoint
                Nothing
                getParams
                transcript
                inputs
                onEvent

backendForSession
    :: ClaudeCodeSession
    -> IORef (Maybe HostTranscriptCheckpoint)
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
backendForSession session checkpoint getParams transcript =
    Backend \previous inputs onEvent ->
        submitClaudeCodeTurn
            session
            checkpoint
            previous
            getParams
            transcript
            inputs
            onEvent

submitClaudeCodeTurn
    :: ClaudeCodeSession
    -> IORef (Maybe HostTranscriptCheckpoint)
    -> Maybe Text
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> [TurnInput]
    -> (LoopEvent -> IO ())
    -> IO (Either ApiError TurnOutput)
submitClaudeCodeTurn
    session
    checkpoint
    previous
    getParams
    transcript
    inputs
    onEvent =
    case flattenTurnInputs inputs of
        Left err -> pure (Left err)
        Right inputText -> do
            params <- getParams
            withClaudeCodeTurn
                session
                (hostTranscriptMatches checkpoint transcript previous)
                previous
                params.model
                (params.reasoning >>= (.effort))
                \turn -> do
                    history <- readIORef transcript
                    let prompt =
                            buildClaudePrompt
                                params
                                (claudeCodeTurnIsNewSession turn)
                                history
                                inputText
                    awaitResult <- awaitClaudeTurn turn prompt onEvent
                    case awaitResult of
                        Left err ->
                            pure (Left err)
                        Right completed -> do
                            usage <- resolveClaudeCodeTurnUsage
                                turn
                                completed.tokenUsage
                                completed.cumulativeModelUsage
                            let output = TurnOutput
                                    { responseId = completed.sessionId
                                    -- Claude Code executes its own local tools.
                                    -- Returning them here would execute each
                                    -- call a second time in the host loop.
                                    , toolCalls = []
                                    , assistantText = completed.assistantText
                                    , tokenUsage = usage
                                    }
                                commit =
                                    commitHostTranscript
                                        checkpoint
                                        transcript
                                        completed.sessionId
                                        inputs
                                        completed.assistantText
                            pure (Right (output, commit))

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
                        "Claude Code subscription sessions do not support image attachments through this provider."
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

hostTranscriptMatches
    :: IORef (Maybe HostTranscriptCheckpoint)
    -> IORef [ResponseItem]
    -> Maybe Text
    -> IO Bool
hostTranscriptMatches checkpoint transcript previous = do
    expected <- readIORef checkpoint
    case expected of
        Nothing ->
            pure True
        Just expectedCheckpoint -> do
            current <- readIORef transcript
            currentName <- makeStableName current
            pure $
                eqStableName
                    expectedCheckpoint.checkpointTranscript
                    currentName
                    || requestedDifferentSession expectedCheckpoint
  where
    requestedDifferentSession :: HostTranscriptCheckpoint -> Bool
    requestedDifferentSession expectedCheckpoint =
        case previous >>= canonicalSessionId of
            Just requested ->
                requested
                    /= expectedCheckpoint.checkpointSessionId
            Nothing ->
                False

    canonicalSessionId value =
        UUID.toText <$> UUID.fromText (Text.strip value)

commitHostTranscript
    :: IORef (Maybe HostTranscriptCheckpoint)
    -> IORef [ResponseItem]
    -> Text
    -> [TurnInput]
    -> Maybe Text
    -> IO ()
commitHostTranscript
    checkpoint
    transcript
    sessionId
    inputs
    assistantText = do
    appendHostTranscript transcript inputs assistantText
    -- Read the exact object installed in the IORef. GHC is free to rebuild an
    -- equivalent result returned from atomicModifyIORef', which would give it
    -- a different StableName despite no host-side transcript change.
    committed <- readIORef transcript
    committedName <- makeStableName committed
    writeIORef checkpoint $
        Just HostTranscriptCheckpoint
            { checkpointTranscript = committedName
            , checkpointSessionId = sessionId
            }

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

awaitClaudeTurn
    :: ClaudeCodeTurn
    -> Text
    -> (LoopEvent -> IO ())
    -> IO (Either ApiError CompletedTurn)
awaitClaudeTurn turn prompt onEvent = do
    completed <-
        timeout turnTimeoutMicros
            do
                accepted <- sendClaudeCodePrompt turn prompt
                if accepted
                    then go emptyStreamAccumulator False
                    else
                        Left <$> timeoutError
                            "did not accept the prompt within the prompt-write timeout"
    case completed of
        Nothing ->
            Left <$> timeoutError
                "did not complete within the turn timeout"
        Just result ->
            pure result
  where
    go :: StreamAccumulator -> Bool -> IO (Either ApiError CompletedTurn)
    go accumulator sawOutput = do
        maybeLine <-
            timeout
                (if sawOutput
                    then streamInactivityTimeoutMicros
                    else streamStartupTimeoutMicros)
                (readClaudeCodeOutputLine turn)
        case maybeLine of
            Nothing ->
                Left <$> timeoutError
                    (if sawOutput
                        then "stopped producing structured output"
                        else "did not produce structured output")
            Just Nothing ->
                Left <$> prematureExitError turn
            Just (Just line)
                | ByteString.null (trimAsciiWhitespace line) ->
                    go accumulator sawOutput
                | otherwise ->
                    case consumeStreamLine accumulator line of
                        Left decodeError ->
                            pure $ Left JsonDecodeError
                                { decodeError
                                , rawBody =
                                    Text.take 2_000 $
                                        TextEncoding.decodeUtf8With
                                            lenientDecode
                                            line
                                }
                        Right (nextAccumulator, events, completed) -> do
                            case streamAccumulatorError nextAccumulator of
                                Just message ->
                                    pure $ Left ProviderError
                                        { errorType = ApiErrorType
                                        , message
                                        , retryAfter = Nothing
                                        }
                                Nothing ->
                                    case completed of
                                        Just value
                                            | value.sessionId
                                                == claudeCodeTurnSessionId turn ->
                                                mapM_ onEvent events
                                                    >> pure (Right value)
                                            | otherwise ->
                                                pure $ Left ProviderError
                                                    { errorType = ApiErrorType
                                                    , message =
                                                        "Claude Code returned session "
                                                            <> value.sessionId
                                                            <> " while "
                                                            <> claudeCodeTurnSessionId turn
                                                            <> " was active."
                                                    , retryAfter = Nothing
                                                    }
                                        Nothing -> do
                                            mapM_ onEvent events
                                            go nextAccumulator True

    timeoutError reason = do
        diagnostic <- claudeCodeTurnDiagnostic turn
        pure $ ConnectionError
            ( "Claude Code "
                <> reason
                <> "."
                <> diagnosticSuffix diagnostic
            )

streamStartupTimeoutMicros :: Int
streamStartupTimeoutMicros = 60 * 1_000_000

streamInactivityTimeoutMicros :: Int
streamInactivityTimeoutMicros = 15 * 60 * 1_000_000

turnTimeoutMicros :: Int
turnTimeoutMicros = 2 * 60 * 60 * 1_000_000

trimAsciiWhitespace :: ByteString.ByteString -> ByteString.ByteString
trimAsciiWhitespace =
    ByteString.dropWhileEnd isAsciiWhitespace
        . ByteString.dropWhile isAsciiWhitespace
  where
    isAsciiWhitespace byte =
        byte `elem` [9, 10, 13, 32]

prematureExitError :: ClaudeCodeTurn -> IO ApiError
prematureExitError turn = do
    processExit <- claudeCodeTurnProcessExit turn
    diagnostic <- claudeCodeTurnDiagnostic turn
    pure $ ConnectionError
        ( "Claude Code closed its structured output before completing the turn"
            <> exitSuffix processExit
            <> "."
            <> diagnosticSuffix diagnostic
        )

exitSuffix :: Maybe ExitCode -> Text
exitSuffix = \case
    Nothing -> ""
    Just exitCode ->
        " (" <> Text.pack (show exitCode) <> ")"

diagnosticSuffix :: Text -> Text
diagnosticSuffix diagnostic
    | Text.null (Text.strip diagnostic) =
        ""
    | otherwise =
        "\nClaude Code stderr:\n" <> Text.takeEnd 2_000 diagnostic
