-- | Adapt a structured Claude Code subscription session to the
-- provider-neutral agent loop.
module Agent.Claude.LoopBackend
    ( withClaudeCodeBackend
    , withClaudeCodeBackendWithHost
    , claudeCodeOneShotBackend
    , appendHostTranscript
    , sdkErrorToApiError
    ) where

import Agent.Claude.Options
    ( ClaudeCodeOptions(..)
    , ClaudeCodeToolMode(..)
    , toClaudeAgentOptions
    )
import Agent.Claude.Control
    ( ClaudeCodeBackendHandle(..)
    , ClaudeCodeHostHandlers
    , configureClaudeCodeHostTools
    , toClaudeAgentHandlers
    )
import Agent.Claude.Transport (ClaudeCodeTransport(..))
import Agent.Claude.Internal.Messages
    ( ClaudeEventState
    , ClaudeInterpretationError(..)
    , CompletedClaudeTurn(..)
    , assistantMessageItem
    , emptyClaudeEventState
    , interpretClaudeTurnWithCredentialValidation
    , remainingClaudeEvents
    , streamClaudeProgress
    )
import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    , errorTypeFromText
    )
import Agent.InterAgentMessage (renderInterAgentMessage)
import Agent.Loop
    ( Backend(..)
    , BackendContinuation(..)
    , BackendResult(..)
    , BackendSnapshot(..)
    , FileAttachment(..)
    , ImageAttachment(..)
    , LoopEvent(..)
    , TokenUsage(..)
    , TurnCompletion(..)
    , TurnInput(..)
    , TurnOutput(..)
    , advanceBackendSnapshot
    , backendContinuationToken
    , turnInputFiles
    , turnInputImages
    )
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Responses.Request
    ( filterCompactionCheckpointsByOrigin
    )
import Agent.Telemetry
    ( ModelTelemetry(..)
    , TurnTelemetry(..)
    )
import Agent.Responses.Types
    ( CustomToolCall(..)
    , CustomToolCallOutput(..)
    , FunctionCall(..)
    , FunctionCallOutput(..)
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
import Agent.Json (RawJson, rawJsonBytes)
import qualified Agent.Json.Decode as Json
import Claude.Agent.SDK.Client
    ( ClaudeSDKClient
    , ClaudeSDKTurn
    , resolveTurnUsage
    , turnIsNewSession
    , abort
    , withClaudeSDKClient
    , withClaudeSDKClientWithHandlers
    , withClaudeSDKTurn
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Char as Char
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Maybe (catMaybes, fromMaybe, isJust, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.UUID.Types as UUID
import qualified System.Directory as Directory
import System.FilePath (takeExtension)
import System.IO (hClose, openBinaryTempFile)
import Claude.Agent.SDK.Errors
    ( ClaudeSDKError(..)
    , renderClaudeSDKError
    )
import Claude.Agent.SDK.Query
    ( QueryResult(..)
    , queryTurnContentWithMessageValidatorAndProgress
    )
import Claude.Agent.SDK.Types
    ( ClaudeAgentOptions(..)
    , Message(..)
    , ModelUsage(..)
    , ResultMessage(..)
    , SystemMessage(..)
    , UserContentBlock(..)
    , Usage(..)
    , messageHasParentToolUseId
    )
import Control.Exception.Safe (bracket, mask, tryAny)
import Control.Monad (void)

claudeProviderNamespace :: Text
claudeProviderNamespace = "anthropic.claude-code"

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
    toClaudeAgentOptions ClaudeCodeDefaultTools options >>= \sdkOptions -> do
        processCheckpointRef <- newIORef Nothing
        withClaudeSDKClient
            sdkOptions
                { resume = Nothing }
            \session ->
                callback
                    (backendForSession
                        options.transport
                        session
                        getParams
                        transcript
                        (initialPrevious >>= canonicalClaudeSessionId)
                        processCheckpointRef)

-- | Handler-aware variant used by interactive hosts. The callback receives an
-- in-band interrupt action in addition to the provider-neutral backend.
withClaudeCodeBackendWithHost
    :: ClaudeCodeOptions
    -> ClaudeCodeHostHandlers
    -> Maybe Text
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> (ClaudeCodeBackendHandle -> IO a)
    -> IO a
withClaudeCodeBackendWithHost
        options host initialPrevious getParams transcript callback =
    toClaudeAgentOptions ClaudeCodeDefaultTools options >>= \baseOptions ->
    let sdkOptions =
            configureClaudeCodeHostTools host
                baseOptions
                    { resume = Nothing }
    in do
        processCheckpointRef <- newIORef Nothing
        withClaudeSDKClientWithHandlers
            sdkOptions
            (toClaudeAgentHandlers host)
            \session -> do
                callback ClaudeCodeBackendHandle
                    { loopBackend =
                        backendForSession
                            options.transport
                            session
                            getParams
                            transcript
                            (initialPrevious >>= canonicalClaudeSessionId)
                            processCheckpointRef
                    , interruptActiveTurn = abort session
                    }

-- | A backend for isolated side requests. Every submission owns and cleans up
-- its own structured Claude process, while still using subscription auth.
claudeCodeOneShotBackend
    :: ClaudeCodeOptions
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
claudeCodeOneShotBackend options getParams transcript =
    Backend \snapshot previous inputs onEvent -> do
        sdkOptions <-
            toClaudeAgentOptions ClaudeCodeNoTools options
        processCheckpointRef <- newIORef Nothing
        result <- withClaudeSDKClient sdkOptions \session ->
            submitClaudeCodeTurn
                options.transport
                session
                snapshot
                previous
                getParams
                transcript
                Nothing
                processCheckpointRef
                inputs
                onEvent
        attachBackendState snapshot result

backendForSession
    :: ClaudeCodeTransport
    -> ClaudeSDKClient
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Maybe Text
    -> IORef (Maybe BackendSnapshot)
    -> Backend
backendForSession
        transport session getParams transcript initialResume
        processCheckpointRef =
    Backend \snapshot previous inputs onEvent ->
        mask \restore -> do
            result <- restore $
                submitClaudeCodeTurn
                    transport
                    session
                    snapshot
                    previous
                    getParams
                    transcript
                    initialResume
                    processCheckpointRef
                    inputs
                    onEvent
            attached <- attachBackendState snapshot result
            case attached of
                Left _ -> pure ()
                Right backendResult ->
                    -- Record what the subprocess now contains before returning.
                    -- If the host discards this successful result, its next
                    -- older snapshot will force a fresh Claude process.
                    writeIORef processCheckpointRef
                        (Just backendResult.backendState)
            pure attached

attachBackendState
    :: BackendSnapshot
    -> Either ApiError (TurnOutput, [ResponseItem])
    -> IO (Either ApiError BackendResult)
attachBackendState _ (Left err) =
    pure (Left err)
attachBackendState snapshot (Right (output, items)) = do
    pure $ Right BackendResult
        { backendOutput = output
        , backendState = advanceBackendSnapshot snapshot items
            (Just BackendContinuation
                { continuationProvider = claudeProviderNamespace
                , continuationToken = output.responseId
                })
        }

submitClaudeCodeTurn
    :: ClaudeCodeTransport
    -> ClaudeSDKClient
    -> BackendSnapshot
    -> Maybe Text
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Maybe Text
    -> IORef (Maybe BackendSnapshot)
    -> [TurnInput]
    -> (LoopEvent -> IO ())
    -> IO (Either ApiError (TurnOutput, [ResponseItem]))
submitClaudeCodeTurn
    transport
    session
    snapshot
    previous
    getParams
    transcript
    initialResume
    processCheckpointRef
    inputs
    onEvent =
    do
        bracket
            (collectTurnInputs inputs)
            (cleanupCollectedTurnInputs . snd3)
            \(inputText, inputImages, _inputFiles) -> do
                params <- getParams
                expectedCheckpoint <- readIORef processCheckpointRef
                let checkpointMatches =
                        maybe True (== snapshot) expectedCheckpoint
                    checkpointSession =
                        backendContinuationToken
                            claudeProviderNamespace
                            snapshot
                    requestedSession =
                        previous >>= canonicalClaudeSessionId
                    runningSession =
                        expectedCheckpoint >>=
                            backendContinuationToken
                                claudeProviderNamespace
                    requestedSessionSwitch =
                        case (runningSession, requestedSession) of
                            (Just running, Just requested) ->
                                running /= requested
                            _ -> False
                    selectionsAgree =
                        case (checkpointSession, requestedSession) of
                            (Just checkpoint, Just requested) ->
                                checkpoint == requested
                            _ -> True
                    initialSession =
                        case initialResume of
                            Just initial
                                | expectedCheckpoint == Nothing
                                , maybe True (== initial) requestedSession
                                , maybe True (== initial) checkpointSession ->
                                    Just initial
                            _ -> Nothing
                    previousSession
                        | requestedSessionSwitch =
                            requestedSession
                        | not checkpointMatches || not selectionsAgree =
                            Nothing
                        -- A missing loop continuation is an explicit host
                        -- reset after this process has handled a turn. On the
                        -- first turn only, the constructor's initial resume is
                        -- still authoritative.
                        | previous == Nothing =
                            initialSession
                        | Just checkpoint <- checkpointSession =
                            Just checkpoint
                        | Just requested <- requestedSession =
                            Just requested
                        | otherwise = initialSession
                    processMatchesHost =
                        requestedSessionSwitch
                            || ( checkpointMatches
                                && selectionsAgree
                                && isJust previousSession
                               )
                    history = snapshot.backendItems
                result <- withClaudeSDKTurn
                    session
                    (pure processMatchesHost)
                    previousSession
                    params.model
                    (params.reasoning >>= (.effort))
                    \turn -> do
                        -- The snapshot is authoritative. Keep the compatibility
                        -- transcript reference aligned after compaction/reset
                        -- before its successful-turn callback appends.
                        writeIORef transcript history
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
                            queryTurnContentWithMessageValidatorAndProgress
                                turn
                                content
                                (\message -> do
                                    validated <-
                                        validateTransportMessage transport message
                                    pure validated)
                                (\progress -> do
                                    state <- readIORef eventState
                                    let (nextState, events) =
                                            streamClaudeProgress
                                                state
                                                progress
                                    writeIORef eventState nextState
                                    mapM_ onEvent events)
                                (const (pure ()))
                        case awaitResult of
                            Left sdkError ->
                                pure (Left sdkError)
                            Right QueryResult
                                { queryMessages = turnMessages
                                , queryResultMessage = result
                                } -> do
                                case
                                    interpretClaudeTurnWithCredentialValidation
                                        (transport == ClaudeCodeLocalSubscription)
                                        turnMessages
                                        result
                                  of
                                    Left (ClaudeAuthenticationFailure message) ->
                                        pure
                                            (Left ResultError
                                                { subtype = "authentication_error"
                                                , apiErrorStatus = Nothing
                                                , errors = [message]
                                                , result = Nothing
                                                })
                                    Left (ClaudeProtocolFailure message) ->
                                        pure (Left (CLIProtocolError message))
                                    Right completed -> do
                                        finalEventState <-
                                            readIORef eventState
                                        completeTurn
                                            turn
                                            completed
                                            result
                                            inputs
                                            history
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
        -> [ResponseItem]
        -> ClaudeEventState
        -> (LoopEvent -> IO ())
        -> IO (Either ClaudeSDKError ((TurnOutput, [ResponseItem]), IO ()))
    completeTurn turn completed result inputs history eventState onEvent = do
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
                , providerTelemetry = claudeResultTelemetry result
                , completion = TurnCompleted
                }
            commit =
                commitHostTranscript
                    transcript
                    completed.sessionId
                    inputs
                    completed.turnItems
        mapM_ onEvent (remainingClaudeEvents eventState completed)
        pure
            (Right
                ( ( output
                  , history <> turnInputsToItems inputs <> completed.turnItems
                  )
                , commit
                ))

claudeResultTelemetry :: ResultMessage -> Maybe TurnTelemetry
claudeResultTelemetry result
    | result.durationMs == Nothing
    , result.durationApiMs == Nothing
    , result.totalCostUsd == Nothing
    , result.stopReason == Nothing
    , result.numTurns == Nothing
    , null result.modelUsage
    , result.structuredOutput == Nothing =
        Nothing
    | otherwise =
        Just TurnTelemetry
            { telemetryDurationMs = result.durationMs
            , telemetryApiDurationMs = result.durationApiMs
            , telemetryCostUsd = result.totalCostUsd
            , telemetryStopReason = result.stopReason
            , telemetryProviderTurns = result.numTurns
            , telemetryModels =
                fmap claudeModelTelemetry result.modelUsage
            , telemetryStructuredOutput = result.structuredOutput
            }

claudeModelTelemetry :: ModelUsage -> ModelTelemetry
claudeModelTelemetry usage = ModelTelemetry
    { modelInputTokens = usage.inputTokens
    , modelOutputTokens = usage.outputTokens
    , modelCacheReadInputTokens = usage.cacheReadInputTokens
    , modelCacheCreationInputTokens = usage.cacheCreationInputTokens
    , modelWebSearchRequests = Just usage.webSearchRequests
    , modelCostUsd = usage.costUSD
    , modelContextWindow = usage.contextWindow
    , modelMaxOutputTokens = usage.maxOutputTokens
    , modelCanonicalName = usage.canonicalModel
    , modelProviderName = usage.provider
    }

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
        UserMessageWithAttachments text _ ->
            [text]
        CompletedTool result ->
            pure $
                "Host tool result for "
                    <> result.callId
                    <> ":\n"
                    <> result.output
    inputImages = turnInputImages
    inputFiles = mapM writeFallbackFile . turnInputFiles

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
        . filterCompactionCheckpointsByOrigin (const False)

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
    ComputerCallItem item ->
        labelled "Assistant computer call" (renderJsonValue (Aeson.toJSON item))
    ComputerCallOutputItem item ->
        labelled "Computer result" (renderJsonValue (Aeson.toJSON item))
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

renderRawJson :: RawJson -> Text
renderRawJson raw =
    case Json.decodeEither
        (Json.withType \case
            Json.VString -> Json.text
            _ -> pure rawText)
        bytes of
        Right value -> value
        Left _ -> rawText
  where
    bytes = rawJsonBytes raw
    rawText = TextEncoding.decodeUtf8With lenientDecode bytes

commitHostTranscript
    :: IORef [ResponseItem]
    -> Text
    -> [TurnInput]
    -> [ResponseItem]
    -> IO ()
commitHostTranscript
    transcript
    _sessionId
    inputs
    turnItems = do
    appendHostTranscriptRef transcript inputs turnItems

appendHostTranscript
    :: [ResponseItem]
    -> [TurnInput]
    -> Maybe Text
    -> [ResponseItem]
appendHostTranscript history inputs assistantText =
    history
        <> turnInputsToItems inputs
        <> [assistantMessageItem assistantText]

-- | Append one completed turn: its inputs followed by the turn's transcript
-- items, which already end in an assistant message.
appendHostTranscriptRef
    :: IORef [ResponseItem]
    -> [TurnInput]
    -> [ResponseItem]
    -> IO ()
appendHostTranscriptRef transcript inputs turnItems =
    atomicModifyIORef' transcript \history ->
        ( history
            <> turnInputsToItems inputs
            <> turnItems
        , ()
        )

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
                    ( Text.take 2_000
                        . TextEncoding.decodeUtf8With lenientDecode
                        . rawJsonBytes
                    )
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

validateTransportMessage
    :: ClaudeCodeTransport
    -> Message
    -> IO (Either ClaudeSDKError ())
validateTransportMessage ClaudeCodeGateway{} _ =
    pure (Right ())
validateTransportMessage ClaudeCodeLocalSubscription message =
    validateSubscriptionMessage message

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
