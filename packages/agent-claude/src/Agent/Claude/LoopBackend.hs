-- | Adapt a structured Claude Code subscription session to the
-- provider-neutral agent loop.
module Agent.Claude.LoopBackend
    ( withClaudeCodeBackend
    , claudeCodeOneShotBackend
    , appendHostTranscript
    ) where

import Agent.Claude.Options
    ( ClaudeCodeOptions
    , ClaudeCodeToolMode(..)
    , toClaudeAgentOptions
    )
import Agent.Claude.Internal.Messages
    ( ClaudeEventState
    , CompletedClaudeTurn(..)
    , emptyClaudeEventState
    , interpretClaudeTurn
    , remainingClaudeEvents
    , streamClaudeMessage
    )
import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    )
import Agent.InterAgentMessage (renderInterAgentMessage)
import Agent.Json (RawJson, emptyExtensions, rawJsonBytes)
import qualified Agent.Json.Encoder as JsonEncoder
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , FileAttachment(..)
    , ImageAttachment(..)
    , LoopEvent(..)
    , TokenUsage(..)
    , TurnCompletion(..)
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
    , responseItemEncoder
    )
import qualified Agent.ToolDispatch as ToolDispatch
import Claude.Agent.SDK.Client
    ( ClaudeSDKClient
    , ClaudeSDKTurn
    , resolveTurnUsage
    , turnIsNewSession
    , withClaudeSDKClient
    , withClaudeSDKTurn
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Char as Char
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , modifyIORef'
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
import qualified System.Directory as Directory
import System.FilePath (takeExtension)
import System.IO (hClose, openBinaryTempFile)
import System.Mem.StableName
    ( StableName
    , eqStableName
    , makeStableName
    )
import Claude.Agent.SDK.Errors
    ( ClaudeSDKError(..)
    , renderClaudeSDKError
    )
import Claude.Agent.SDK.Query
    ( queryTurnContentWithMessageValidator
    )
import Claude.Agent.SDK.Types
    ( ClaudeAgentOptions(..)
    , Message(..)
    , ResultMessage(..)
    , SystemMessage(..)
    , UserContentBlock(..)
    , Usage(..)
    , messageHasParentToolUseId
    )
import Control.Exception.Safe (bracket, tryAny)
import Control.Monad (void)

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
    toClaudeAgentOptions ClaudeCodeDefaultTools options >>= \sdkOptions ->
    withClaudeSDKClient
        sdkOptions
            { resume = initialPrevious >>= canonicalClaudeSessionId }
        \session -> do
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
    Backend \_state previous inputs onEvent -> do
        checkpoint <- newIORef Nothing
        sdkOptions <-
            toClaudeAgentOptions ClaudeCodeNoTools options
        result <- withClaudeSDKClient sdkOptions \session ->
            submitClaudeCodeTurn
                session
                checkpoint
                previous
                getParams
                transcript
                inputs
                onEvent
        attachBackendState transcript result

backendForSession
    :: ClaudeSDKClient
    -> IORef (Maybe HostTranscriptCheckpoint)
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
backendForSession session checkpoint getParams transcript =
    Backend \_state previous inputs onEvent -> do
        result <- submitClaudeCodeTurn
            session
            checkpoint
            previous
            getParams
            transcript
            inputs
            onEvent
        attachBackendState transcript result

attachBackendState
    :: IORef [ResponseItem]
    -> Either ApiError TurnOutput
    -> IO (Either ApiError BackendResult)
attachBackendState _ (Left err) =
    pure (Left err)
attachBackendState transcript (Right output) = do
    state <- readIORef transcript
    pure $ Right BackendResult
        { backendOutput = output
        , backendState = state
        }

submitClaudeCodeTurn
    :: ClaudeSDKClient
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
    do
        bracket
            (collectTurnInputs inputs)
            (cleanupCollectedTurnInputs . snd3)
            \(inputText, inputImages, _inputFiles) -> do
                params <- getParams
                let previousSession =
                        previous >>= canonicalClaudeSessionId
                result <- withClaudeSDKTurn
                    session
                    (hostTranscriptMatches
                        checkpoint
                        transcript
                        previousSession)
                    previousSession
                    params.model
                    (params.reasoning >>= (.effort))
                    \turn -> do
                        history <- readIORef transcript
                        messages <- newIORef []
                        eventState <- newIORef emptyClaudeEventState
                        let prompt =
                                buildClaudePrompt
                                    params
                                    (turnIsNewSession turn)
                                    history
                                    inputText
                            content =
                                claudeUserContent inputImages prompt
                        awaitResult <-
                            queryTurnContentWithMessageValidator
                                turn
                                content
                                (\message -> do
                                    validated <-
                                        validateSubscriptionMessage message
                                    case validated of
                                        Left err -> pure (Left err)
                                        Right () -> do
                                            state <- readIORef eventState
                                            let (nextState, events) =
                                                    streamClaudeMessage
                                                        state
                                                        message
                                            writeIORef eventState nextState
                                            mapM_ onEvent events
                                            pure (Right ()))
                                (\message ->
                                    modifyIORef' messages (message :))
                        case awaitResult of
                            Left sdkError ->
                                pure (Left sdkError)
                            Right result -> do
                                turnMessages <- reverse <$> readIORef messages
                                case interpretClaudeTurn turnMessages result of
                                    Left message ->
                                        pure $
                                            Left ResultError
                                                { subtype = "authentication_error"
                                                , apiErrorStatus = Nothing
                                                , errors = [message]
                                                , result = Nothing
                                                }
                                    Right completed -> do
                                        finalEventState <-
                                            readIORef eventState
                                        completeTurn
                                            turn
                                            completed
                                            result
                                            inputs
                                            finalEventState
                                            onEvent
                pure (either (Left . sdkErrorToApiError) Right result)
  where
    snd3 (_, _, c) = c

    cleanupCollectedTurnInputs :: [FilePath] -> IO ()
    cleanupCollectedTurnInputs = mapM_ \path ->
        void (tryAny (Directory.removeFile path))

    completeTurn
        :: ClaudeSDKTurn
        -> CompletedClaudeTurn
        -> ResultMessage
        -> [TurnInput]
        -> ClaudeEventState
        -> (LoopEvent -> IO ())
        -> IO (Either ClaudeSDKError (TurnOutput, IO ()))
    completeTurn turn completed result inputs eventState onEvent = do
        usage <- resolveTurnUsage
            turn
            completed.tokenUsage
            result.modelUsage
        let output = TurnOutput
                { responseId = completed.sessionId
                -- Claude Code executes its own local tools. Returning them
                -- here would execute each call a second time in the host loop.
                , toolCalls = []
                , assistantText = completed.assistantText
                , tokenUsage = sdkUsageToTokenUsage usage
                , completion = TurnCompleted
                }
            commit =
                commitHostTranscript
                    checkpoint
                    transcript
                    completed.sessionId
                    inputs
                    completed.assistantText
        mapM_ onEvent (remainingClaudeEvents eventState completed)
        pure (Right (output, commit))

collectTurnInputs :: [TurnInput] -> IO (Text, [ImageAttachment], [FilePath])
collectTurnInputs inputs = do
    fileInputs <- fmap concat (mapM inputFiles inputs)
    pure
        ( Text.intercalate "\n\n"
            ( concatMap inputText inputs
                <> map fst fileInputs
            )
        , concatMap inputImages inputs
        , map snd fileInputs
        )
  where
    inputText = \case
        UserMessage text ->
            [text]
        AgentMessage message ->
            [renderInterAgentMessage message]
        UserMultimodal{userText} ->
            [userText]
        UserMultimodalFiles{userText} ->
            [userText]
        CompletedTool
            (ToolDispatch.ToolCallResult resultCallId resultOutput _) ->
            pure $
                "Host tool result for "
                    <> resultCallId
                    <> ":\n"
                    <> resultOutput
    inputImages = \case
        UserMultimodal{userImages} -> userImages
        UserMultimodalFiles{userImages} -> userImages
        _ -> []
    inputFiles = \case
        UserMultimodalFiles{userFiles} -> mapM writeFallbackFile userFiles
        _ -> pure []

    writeFallbackFile :: FileAttachment -> IO (Text, FilePath)
    writeFallbackFile FileAttachment{fileName, fileMime, fileBytes} = do
        tmpDir <- Directory.getTemporaryDirectory
        let prefix =
                maybe "attachment" sanitizePrefix fileName
                <> fromMaybe "" (fileName >>= nonEmptyExtension)
        (path, handle) <- openBinaryTempFile tmpDir prefix
        hClose handle
        LazyByteString.writeFile path (LazyByteString.fromStrict fileBytes)
        pure
            ( Text.unlines
                [ "[Attached file]"
                , "name: " <> fromMaybe "unknown" fileName
                , "mime: " <> fileMime
                , "path: " <> Text.pack path
                ]
            , path
            )

    sanitizePrefix = Text.unpack . Text.filter validPrefixChar
    nonEmptyExtension name =
        case takeExtension (Text.unpack name) of
            "" -> Nothing
            extension -> Just extension
    validPrefixChar char =
        Char.isAlphaNum char || char == '-' || char == '_'

claudeUserContent
    :: [ImageAttachment]
    -> Text
    -> [UserContentBlock]
claudeUserContent images prompt =
    map imageBlock images <> [UserTextBlock prompt]
  where
    imageBlock ImageAttachment{imageMime, imageBytes} =
        UserImageBlock
            { mediaType = imageMime
            , imageBytes
            }

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
            (renderRawJson output.output)
    CustomToolCallOutputItem output ->
        labelled
            ("Tool result " <> output.callId)
            (renderRawJson output.output)
    -- Reasoning is deliberately excluded from imported history. In
    -- particular, never copy private chain-of-thought into a Claude prompt.
    ReasoningItemValue{} ->
        Nothing
    ItemReferenceValue{} ->
        Nothing
    item@AgentMessageItem{} ->
        labelled "Context item" (renderResponseItemJson item)
    item@AdditionalToolsItemValue{} ->
        labelled "Context item" (renderResponseItemJson item)
    item@LocalShellCallItem{} ->
        labelled "Context item" (renderResponseItemJson item)
    item@ToolSearchCallItem{} ->
        labelled "Context item" (renderResponseItemJson item)
    item@ToolSearchOutputItem{} ->
        labelled "Context item" (renderResponseItemJson item)
    item@WebSearchCallItem{} ->
        labelled "Context item" (renderResponseItemJson item)
    item@ImageGenerationCallItem{} ->
        labelled "Context item" (renderResponseItemJson item)
    item@CompactionItemValue{} ->
        labelled "Context item" (renderResponseItemJson item)
    item@CompactionTriggerItemValue{} ->
        labelled "Context item" (renderResponseItemJson item)
    item@ContextCompactionItemValue{} ->
        labelled "Context item" (renderResponseItemJson item)
    item@KnownResponseItem{} ->
        labelled "Context item" (renderResponseItemJson item)
    item@UnknownResponseItem{} ->
        labelled "Context item" (renderResponseItemJson item)
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
    EncryptedContentPart{} -> []
    PlainTextPart{text} -> [text]
    UnknownContentPart{} -> []

renderRawJson :: RawJson -> Text
renderRawJson =
    TextEncoding.decodeUtf8With lenientDecode
        . rawJsonBytes

renderResponseItemJson :: ResponseItem -> Text
renderResponseItemJson =
    TextEncoding.decodeUtf8With lenientDecode
        . JsonEncoder.encode responseItemEncoder

renderAesonValue :: Aeson.Value -> Text
renderAesonValue = \case
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
            case previous of
                Nothing ->
                    -- In Agent.Loop, dropping the previous response ID is an
                    -- explicit conversation reset even if the host transcript
                    -- object has not changed.
                    pure False
                Just _ -> do
                    current <- readIORef transcript
                    currentName <- current `seq` makeStableName current
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
    appendHostTranscriptRef transcript inputs assistantText
    -- Read and enter the exact object installed in the IORef before taking its
    -- StableName. Otherwise the lazy append thunk can later be entered by the
    -- CLI, changing the StableName despite no host-side transcript change.
    committed <- readIORef transcript
    committedName <- committed `seq` makeStableName committed
    writeIORef checkpoint $
        Just HostTranscriptCheckpoint
            { checkpointTranscript = committedName
            , checkpointSessionId =
                fromMaybe
                    (Text.strip sessionId)
                    (canonicalClaudeSessionId sessionId)
            }

appendHostTranscript
    :: [ResponseItem]
    -> [TurnInput]
    -> Maybe Text
    -> [ResponseItem]
appendHostTranscript history inputs assistantText =
    history
        <> turnInputsToItems inputs
        <> [assistantMessageItem assistantText]

appendHostTranscriptRef
    :: IORef [ResponseItem]
    -> [TurnInput]
    -> Maybe Text
    -> IO ()
appendHostTranscriptRef transcript inputs assistantText =
    atomicModifyIORef' transcript \history ->
        (appendHostTranscript history inputs assistantText, ())

assistantMessageItem :: Maybe Text -> ResponseItem
assistantMessageItem assistantText =
    MessageItem ResponseMessage
        { messageId = Nothing
        , content = MessageContentParts
            [ OutputTextPart
                { text = fromMaybe "" assistantText
                , annotations = Nothing
                , logprobs = Nothing
                , extraFields = emptyExtensions
                }
            ]
        , role = RoleAssistant
        , status = Just ItemCompleted
        , phase = Nothing
        , passthrough = Nothing
        , extraFields = emptyExtensions
        }

sdkErrorToApiError :: ClaudeSDKError -> ApiError
sdkErrorToApiError = \case
    CLIJSONDecodeError{decodeError, rawBody} ->
        JsonDecodeError
            { decodeError
            , rawBody
            }
    MessageParseError{parseError, rawMessage} ->
        JsonDecodeError
            { decodeError = parseError
            , rawBody =
                maybe
                    ""
                    (Text.take 2_000 . renderAesonValue)
                    rawMessage
            }
    sdkError@ResultError{} ->
        ProviderError
            { errorType = ApiErrorType
            , message = renderClaudeSDKError sdkError
            , retryAfter = Nothing
            }
    sdkError@CLIProtocolError{} ->
        ProviderError
            { errorType = ApiErrorType
            , message = renderClaudeSDKError sdkError
            , retryAfter = Nothing
            }
    sdkError ->
        ConnectionError (renderClaudeSDKError sdkError)

sdkUsageToTokenUsage :: Usage -> TokenUsage
sdkUsageToTokenUsage usage =
    TokenUsage
        { inputTokens = usage.inputTokens
        , outputTokens = usage.outputTokens
        , cachedTokens = usage.cachedTokens
        }

validateSubscriptionMessage
    :: Message
    -> IO (Either ClaudeSDKError ())
validateSubscriptionMessage message
    | messageHasParentToolUseId message =
        pure (Right ())
    | otherwise =
        validateTopLevelSubscriptionMessage message

validateTopLevelSubscriptionMessage
    :: Message
    -> IO (Either ClaudeSDKError ())
validateTopLevelSubscriptionMessage = \case
    MessageSystem SystemMessage
        { subtype = "init"
        , apiKeySource = Just "none"
        } ->
            pure (Right ())
    MessageSystem SystemMessage
        { subtype = "init"
        , apiKeySource = Just source
        } ->
            pure $
                Left ResultError
                    { subtype = "authentication_error"
                    , apiErrorStatus = Nothing
                    , errors =
                        [ "Claude Code selected non-subscription credential source "
                            <> source
                            <> "."
                        ]
                    , result = Nothing
                    }
    MessageSystem SystemMessage
        { subtype = "init"
        , apiKeySource = Nothing
        } ->
            pure $
                Left ResultError
                    { subtype = "authentication_error"
                    , apiErrorStatus = Nothing
                    , errors =
                        [ "Claude Code did not identify its credential source."
                        ]
                    , result = Nothing
                    }
    _ ->
        pure (Right ())

canonicalClaudeSessionId :: Text -> Maybe Text
canonicalClaudeSessionId value =
    UUID.toText <$> UUID.fromText (Text.strip value)
