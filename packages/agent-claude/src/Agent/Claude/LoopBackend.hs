-- | Adapt a structured Claude Code subscription session to the
-- provider-neutral agent loop.
module Agent.Claude.LoopBackend
    ( withClaudeCodeBackend
    , withClaudeCodeBackendPermissions
    , claudeCodeOneShotBackend
    , appendHostTranscript
    , ClaudeToolPermissionDecision(..)
    , ClaudeToolPermissionRequest(..)
    , sdkErrorToApiError
    ) where

import Agent.Claude.Options
    ( ClaudeCodeOptions
    , ClaudeCodeToolMode(..)
    , toClaudeAgentOptions
    )
import Agent.Claude.Internal.Messages
    ( CompletedClaudeTurn(..)
    , interpretClaudeTurn
    )
import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    , errorTypeFromText
    )
import Agent.InterAgentMessage (renderInterAgentMessage)
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , FileAttachment(..)
    , ImageAttachment(..)
    , LoopEvent(..)
    , TokenUsage(..)
    , TurnInput(..)
    , TurnOutput(..)
    )
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Responses.Types
    ( CustomToolCall(..)
    , CustomToolCallOutput(..)
    , ComputerCallOutput(..)
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
import Claude.Agent.SDK.Client
    ( ClaudeSDKClient
    , ClaudeSDKTurn
    , resolveTurnUsage
    , sendControlResponse
    , turnIsNewSession
    , withClaudeSDKClient
    , withClaudeSDKTurn
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
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
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import qualified Data.Map.Strict as Map
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
    ( queryTurnContentWithControlHandler
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
import Data.Aeson ((.:))
import qualified Data.Aeson.Types as AesonTypes

data ClaudeToolPermissionRequest = ClaudeToolPermissionRequest
    { claudePermissionRequestId :: !Text
    , claudePermissionToolName :: !Text
    , claudePermissionInput :: !Aeson.Value
    }

data ClaudeToolPermissionDecision
    = ClaudeToolPermissionAllow
    | ClaudeToolPermissionDeny !Text

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
    withClaudeCodeBackendPermissions
        options
        Nothing
        initialPrevious
        getParams
        transcript
        callback

withClaudeCodeBackendPermissions
    :: ClaudeCodeOptions
    -> Maybe
        (ClaudeToolPermissionRequest
            -> IO ClaudeToolPermissionDecision)
    -> Maybe Text
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> (Backend -> IO a)
    -> IO a
withClaudeCodeBackendPermissions
    options permissionHandler initialPrevious getParams transcript callback =
    toClaudeAgentOptions ClaudeCodeDefaultTools options >>= \sdkOptions ->
    let effectiveOptions = case permissionHandler of
            Nothing -> sdkOptions
            Just _ -> sdkOptions
                { permissionMode = Nothing
                , extraArgs =
                    Map.insert
                        "permission-prompt-tool"
                        (Just "stdio")
                        sdkOptions.extraArgs
                }
    in
    withClaudeSDKClient
        effectiveOptions
            { resume = initialPrevious >>= canonicalClaudeSessionId }
        \session -> do
        checkpoint <- newIORef Nothing
        callback
            (backendForSession
                session
                checkpoint
                getParams
                transcript
                permissionHandler)

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
                Nothing
        attachBackendState transcript result

backendForSession
    :: ClaudeSDKClient
    -> IORef (Maybe HostTranscriptCheckpoint)
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Maybe
        (ClaudeToolPermissionRequest
            -> IO ClaudeToolPermissionDecision)
    -> Backend
backendForSession
    session checkpoint getParams transcript permissionHandler =
    Backend \_state previous inputs onEvent -> do
        result <- submitClaudeCodeTurn
            session
            checkpoint
            previous
            getParams
            transcript
            inputs
            onEvent
            permissionHandler
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
    -> Maybe
        (ClaudeToolPermissionRequest
            -> IO ClaudeToolPermissionDecision)
    -> IO (Either ApiError TurnOutput)
submitClaudeCodeTurn
    session
    checkpoint
    previous
    getParams
    transcript
    inputs
    onEvent
    permissionHandler =
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
                        let prompt =
                                buildClaudePrompt
                                    params
                                    (turnIsNewSession turn)
                                    history
                                    inputText
                            content =
                                claudeUserContent inputImages prompt
                        awaitResult <-
                            queryTurnContentWithControlHandler
                                turn
                                content
                                validateSubscriptionMessage
                                (handleControlRequest
                                    turn
                                    permissionHandler)
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
                                        completeTurn
                                            turn
                                            completed
                                            result
                                            inputs
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
        -> (LoopEvent -> IO ())
        -> IO (Either ClaudeSDKError (TurnOutput, IO ()))
    completeTurn turn completed result inputs onEvent = do
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
                }
            commit =
                commitHostTranscript
                    checkpoint
                    transcript
                    completed.sessionId
                    inputs
                    completed.assistantText
        mapM_ onEvent completed.events
        pure (Right (output, commit))

handleControlRequest
    :: ClaudeSDKTurn
    -> Maybe
        (ClaudeToolPermissionRequest
            -> IO ClaudeToolPermissionDecision)
    -> Aeson.Object
    -> IO (Either ClaudeSDKError ())
handleControlRequest _ Nothing _ =
    pure $ Left $ CLIProtocolError
        "Claude Code requested permission, but no permission handler is configured."
handleControlRequest turn (Just requestPermission) raw =
    case AesonTypes.parseEither
        parseClaudePermissionRequest
        (Aeson.Object raw) of
            Left err ->
                pure (Left (CLIProtocolError (Text.pack err)))
            Right request -> do
                decision <- requestPermission request
                sendControlResponse turn $
                    controlResponse request decision

parseClaudePermissionRequest
    :: Aeson.Value
    -> AesonTypes.Parser ClaudeToolPermissionRequest
parseClaudePermissionRequest =
    Aeson.withObject "control request" \outer -> do
        requestId <- outer .: "request_id"
        requestValue <- outer .: "request"
        Aeson.withObject "permission request" (\request -> do
            subtype <- request .: "subtype"
            if (subtype :: Text) /= "can_use_tool"
                then fail
                    ("unsupported control request subtype: "
                        <> Text.unpack subtype)
                else ClaudeToolPermissionRequest
                    requestId
                    <$> request .: "tool_name"
                    <*> request .: "input"
            ) requestValue

controlResponse
    :: ClaudeToolPermissionRequest
    -> ClaudeToolPermissionDecision
    -> Aeson.Value
controlResponse request decision =
    Aeson.object
        [ "type" Aeson..= ("control_response" :: Text)
        , "response" Aeson..= Aeson.object
            [ "subtype" Aeson..= ("success" :: Text)
            , "request_id" Aeson..= request.claudePermissionRequestId
            , "response" Aeson..= case decision of
                ClaudeToolPermissionAllow ->
                    Aeson.object
                        [ "behavior" Aeson..= ("allow" :: Text)
                        , "updatedInput" Aeson..=
                            request.claudePermissionInput
                        , "updatedPermissions" Aeson..=
                            ([] :: [Aeson.Value])
                        ]
                ClaudeToolPermissionDeny message ->
                    Aeson.object
                        [ "behavior" Aeson..= ("deny" :: Text)
                        , "message" Aeson..= message
                        , "interrupt" Aeson..= False
                        ]
            ]
        ]

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
            (renderJsonValue output.output)
    CustomToolCallOutputItem output ->
        labelled
            ("Tool result " <> output.callId)
            (renderJsonValue output.output)
    ComputerCallItem item ->
        labelled "Assistant computer call" (renderJsonValue (Aeson.toJSON item))
    ComputerCallOutputItem item ->
        labelled
            ("Computer result " <> item.computerOutputCallId)
            (renderJsonValue (Aeson.toJSON item))
    -- Reasoning is deliberately excluded from imported history. In
    -- particular, never copy private chain-of-thought into a Claude prompt.
    ReasoningItemValue{} ->
        Nothing
    ItemReferenceValue{} ->
        Nothing
    AgentMessageItem message ->
        labelled "Context item" (renderJsonValue (Aeson.toJSON message))
    AdditionalToolsItemValue item ->
        labelled "Context item" (renderJsonValue (Aeson.toJSON item))
    LocalShellCallItem item ->
        labelled "Context item" (renderJsonValue (Aeson.toJSON item))
    ToolSearchCallItem item ->
        labelled "Context item" (renderJsonValue (Aeson.toJSON item))
    ToolSearchOutputItem item ->
        labelled "Context item" (renderJsonValue (Aeson.toJSON item))
    WebSearchCallItem item ->
        labelled "Context item" (renderJsonValue (Aeson.toJSON item))
    ImageGenerationCallItem item ->
        labelled "Context item" (renderJsonValue (Aeson.toJSON item))
    CompactionItemValue item ->
        labelled "Context item" (renderJsonValue (Aeson.toJSON item))
    CompactionTriggerItemValue item ->
        labelled "Context item" (renderJsonValue (Aeson.toJSON item))
    ContextCompactionItemValue item ->
        labelled "Context item" (renderJsonValue (Aeson.toJSON item))
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
    EncryptedContentPart{} -> []
    PlainTextPart{text} -> [text]
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
                , extraFields = KeyMap.empty
                }
            ]
        , role = RoleAssistant
        , status = Just ItemCompleted
        , phase = Nothing
        , passthrough = Nothing
        , extraFields = KeyMap.empty
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
                    (Text.take 2_000 . renderJsonValue)
                    rawMessage
            }
    sdkError@ResultError{} ->
        ProviderError
            { errorType = classifyResultError sdkError
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

classifyResultError :: ClaudeSDKError -> ErrorType
classifyResultError ResultError{subtype, apiErrorStatus, errors, result} =
    case apiErrorStatus of
        Just 401 -> AuthenticationError
        Just 403 -> PermissionError
        Just 404 -> NotFoundError
        Just 413 -> PayloadTooLargeError
        Just 429 -> RateLimitError
        Just status
            | status >= 500 && status < 503 -> ServiceUnavailableError
            | status == 503 -> ServiceUnavailableError
            | status == 529 -> OverloadedError
        _ ->
            let bySubtype = errorTypeFromText (Text.toLower subtype)
            in case bySubtype of
                UnknownErrorType _ ->
                    classifyResultMessage
                        (Text.toLower (Text.intercalate " " (errors <> maybeToList result)))
                other -> other
classifyResultError _ = ApiErrorType

classifyResultMessage :: Text -> ErrorType
classifyResultMessage message
    | any (`Text.isInfixOf` message)
        [ "authentication"
        , "failed to authenticate"
        , "oauth session expired"
        , "unauthorized"
        , "invalid api key"
        ] =
        AuthenticationError
    | any (`Text.isInfixOf` message) ["permission", "forbidden", "not allowed"] =
        PermissionError
    | any (`Text.isInfixOf` message) ["context length", "context window", "too many tokens"] =
        ContextWindowExceeded
    | any (`Text.isInfixOf` message) ["rate limit", "rate_limit", "too many requests"] =
        RateLimitError
    | any (`Text.isInfixOf` message) ["overloaded", "overload"] =
        OverloadedError
    | any (`Text.isInfixOf` message) ["unavailable", "temporarily down"] =
        ServiceUnavailableError
    | any (`Text.isInfixOf` message) ["payload too large", "request too large"] =
        PayloadTooLargeError
    | otherwise = ApiErrorType

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
