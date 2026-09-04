module Claude.Agent.SDK.Internal.ControlRuntime
    ( ControlRuntime
    , startControlRuntime
    , stopControlRuntime
    , runtimeReadMessage
    , runtimeWrite
    , runtimeSendRequest
    , runtimeInitializationResult
    ) where

import Claude.Agent.SDK.Control
import Claude.Agent.SDK.Errors (ClaudeSDKError(..))
import Claude.Agent.SDK.Internal.MessageParser (decodeMessageLine)
import Claude.Agent.SDK.Transport (Transport(..))
import Claude.Agent.SDK.Types (Message)
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , mapConcurrently_
    )
import Control.Concurrent.Chan
    ( Chan
    , newChan
    , readChan
    , writeChan
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar_
    , newMVar
    , readMVar
    , withMVar
    )
import qualified Control.Concurrent.MVar as MVar
import Control.Exception.Safe
    ( SomeException
    , catchAny
    , finally
    , mask
    , onException
    , throwIO
    )
import Control.Monad (forM_, unless, void)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    )
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Timeout (timeout)

data ControlRuntimeCore = ControlRuntimeCore
    { coreTransport :: !Transport
    , coreHandlers :: !ClaudeAgentHandlers
    , coreMessages
        :: !(Chan (Either ClaudeSDKError (Maybe Message)))
    , corePending
        :: !(MVar (Map.Map Text (MVar ControlResponse)))
    , coreWorkers :: !(MVar (Map.Map Text (Async ())))
    , coreWriteLock :: !(MVar ())
    , coreRequestCounter :: !(IORef Int)
    , coreStopped :: !(IORef Bool)
    }

data ControlRuntime = ControlRuntime
    { runtimeCore :: !ControlRuntimeCore
    , runtimeReader :: !(Async ())
    , runtimeInitialization :: !Aeson.Value
    }

startControlRuntime
    :: Transport
    -> ClaudeAgentHandlers
    -> IO ControlRuntime
startControlRuntime transport handlers =
    mask \restore -> do
        messages <- newChan
        pending <- newMVar Map.empty
        workers <- newMVar Map.empty
        writeLock <- newMVar ()
        requestCounter <- newIORef 0
        stopped <- newIORef False
        let core = ControlRuntimeCore
                { coreTransport = transport
                , coreHandlers = handlers
                , coreMessages = messages
                , corePending = pending
                , coreWorkers = workers
                , coreWriteLock = writeLock
                , coreRequestCounter = requestCounter
                , coreStopped = stopped
                }
        reader <- asyncWithUnmask \unmask ->
            unmask (readerLoop core)
        let cleanup = stopCore core reader
        initialization <-
            restore
                (runtimeSendRequestCore
                    core
                    handlers.initializeTimeoutMicros
                    (initializeRequest handlers.initializeOptions))
                `onException` cleanup
        pure ControlRuntime
            { runtimeCore = core
            , runtimeReader = reader
            , runtimeInitialization = initialization
            }

stopControlRuntime :: ControlRuntime -> IO ()
stopControlRuntime runtime =
    stopCore runtime.runtimeCore runtime.runtimeReader

runtimeReadMessage
    :: ControlRuntime
    -> IO (Either ClaudeSDKError (Maybe Message))
runtimeReadMessage runtime =
    readChan runtime.runtimeCore.coreMessages

runtimeWrite
    :: ControlRuntime
    -> ByteString.ByteString
    -> IO (Either ClaudeSDKError ())
runtimeWrite runtime =
    writeSerialized runtime.runtimeCore

runtimeSendRequest
    :: ControlRuntime
    -> Aeson.Value
    -> IO (Either ClaudeSDKError Aeson.Value)
runtimeSendRequest runtime request =
    catchAny
        ( Right
            <$> runtimeSendRequestCore
                runtime.runtimeCore
                runtime.runtimeCore.coreHandlers.controlRequestTimeoutMicros
                request
        )
        (pure . Left . controlException)

runtimeInitializationResult :: ControlRuntime -> Aeson.Value
runtimeInitializationResult = (.runtimeInitialization)

readerLoop :: ControlRuntimeCore -> IO ()
readerLoop core =
    (go `catchAny` finishWithException)
        `finally` finishPending
  where
    go =
        core.coreTransport.transportRead >>= \case
            Left err ->
                finishWithError err
            Right Nothing -> do
                writeChan core.coreMessages (Right Nothing)
            Right (Just bytes)
                | ByteString.null (trimAsciiWhitespace bytes) ->
                    go
                | otherwise ->
                    case Aeson.eitherDecodeStrict' bytes of
                        Right value
                            | Just response <- decodeControlResponse value -> do
                                deliverResponse core response
                                go
                            | Just request <- decodeControlEnvelope value -> do
                                spawnRequestWorker core request
                                go
                            | Just cancelled <- decodeControlCancel value -> do
                                cancelRequestWorker core cancelled
                                go
                        _ ->
                            case decodeMessageLine bytes of
                                Left CLIJSONDecodeError{}
                                    | not (looksLikeJsonObject bytes) ->
                                        go
                                Left err ->
                                    finishWithError err
                                Right message -> do
                                    writeChan core.coreMessages
                                        (Right (Just message))
                                    go

    finishWithException exception =
        finishWithError (controlException exception)

    finishWithError err = do
        writeChan core.coreMessages (Left err)

    finishPending = do
        pending <- readMVar core.corePending
        forM_ (Map.toList pending) \(requestId, responseVar) ->
            void $
                MVar.tryPutMVar
                    responseVar
                    (ControlError
                        requestId
                        "Claude Code control stream ended."
                        Aeson.Null)

data IncomingControl = IncomingControl
    { incomingRequestId :: !Text
    , incomingRequest :: !ControlRequest
    }

decodeControlEnvelope :: Aeson.Value -> Maybe IncomingControl
decodeControlEnvelope value@(Aeson.Object object)
    | lookupText "type" object == Just "control_request"
    , Just requestId <- lookupText "request_id" object
    , Just requestValue <- KeyMap.lookup "request" object =
        Just IncomingControl
            { incomingRequestId = requestId
            , incomingRequest = decodeControlRequest requestValue
            }
    | otherwise = Nothing
  where
    _preserveEnvelope = value
decodeControlEnvelope _ = Nothing

decodeControlRequest :: Aeson.Value -> ControlRequest
decodeControlRequest raw@(Aeson.Object object) =
    case lookupText "subtype" object of
        Just "can_use_tool" ->
            case (lookupText "tool_name" object, KeyMap.lookup "input" object) of
                (Just toolName, Just input) ->
                    ControlCanUseTool ToolPermissionRequest
                        { toolName
                        , input
                        , permissionSuggestions =
                            fromMaybe []
                                (lookupValues
                                    "permission_suggestions"
                                    object)
                        , blockedPath = lookupText "blocked_path" object
                        , decisionReason =
                            lookupText "decision_reason" object
                        , title = lookupText "title" object
                        , displayName = lookupText "display_name" object
                        , description = lookupText "description" object
                        , toolUseId = lookupText "tool_use_id" object
                        , agentId = lookupText "agent_id" object
                        , raw
                        }
                _ -> ControlUnknown (Just "can_use_tool") raw
        Just "mcp_message" ->
            case (lookupText "server_name" object, KeyMap.lookup "message" object) of
                (Just serverName, Just message) ->
                    ControlMcpMessage McpMessageRequest
                        { serverName
                        , message
                        , raw
                        }
                _ -> ControlUnknown (Just "mcp_message") raw
        Just "interrupt" -> ControlInterrupt raw
        Just "initialize" -> ControlInitialize raw
        Just "set_permission_mode" ->
            maybe
                (ControlUnknown (Just "set_permission_mode") raw)
                (\mode -> ControlSetPermissionMode mode raw)
                (lookupText "mode" object)
        Just "set_model" ->
            ControlSetModel (lookupNullableText "model" object) raw
        Just "get_context_usage" ->
            ControlGetContextUsage raw
        Just "stop_task" ->
            maybe
                (ControlUnknown (Just "stop_task") raw)
                (\taskId -> ControlStopTask taskId raw)
                (lookupText "task_id" object)
        subtype -> ControlUnknown subtype raw
decodeControlRequest raw =
    ControlUnknown Nothing raw

decodeControlResponse :: Aeson.Value -> Maybe ControlResponse
decodeControlResponse raw@(Aeson.Object object)
    | lookupText "type" object == Just "control_response"
    , Just (Aeson.Object response) <- KeyMap.lookup "response" object
    , Just requestId <- lookupText "request_id" response =
        case lookupText "subtype" response of
            Just "error" ->
                Just
                    (ControlError
                        requestId
                        (fromMaybe "Unknown control error"
                            (lookupText "error" response))
                        raw)
            Just "success" ->
                Just
                    (ControlSuccess
                        requestId
                        (fromMaybe (Aeson.Object KeyMap.empty)
                            (KeyMap.lookup "response" response))
                        raw)
            _ -> Nothing
    | otherwise = Nothing
decodeControlResponse _ = Nothing

decodeControlCancel :: Aeson.Value -> Maybe ControlCancelRequest
decodeControlCancel raw@(Aeson.Object object)
    | lookupText "type" object == Just "control_cancel_request"
    , Just requestId <- lookupText "request_id" object =
        Just ControlCancelRequest{requestId, raw}
    | otherwise = Nothing
decodeControlCancel _ = Nothing

deliverResponse :: ControlRuntimeCore -> ControlResponse -> IO ()
deliverResponse core response = do
    pending <- readMVar core.corePending
    forM_ (Map.lookup response.requestId pending) \responseVar ->
        void (MVar.tryPutMVar responseVar response)

spawnRequestWorker :: ControlRuntimeCore -> IncomingControl -> IO ()
spawnRequestWorker core incoming =
    mask \_ -> do
        gate <- MVar.newEmptyMVar
        worker <-
            asyncWithUnmask \unmask ->
                unmask do
                    MVar.takeMVar gate
                    handleIncoming core incoming
                        `finally`
                            modifyMVar_ core.coreWorkers
                                (pure . Map.delete incoming.incomingRequestId)
        modifyMVar_ core.coreWorkers \workers ->
            pure (Map.insert incoming.incomingRequestId worker workers)
        MVar.putMVar gate ()

cancelRequestWorker
    :: ControlRuntimeCore
    -> ControlCancelRequest
    -> IO ()
cancelRequestWorker core cancelled = do
    worker <-
        withMVar core.coreWorkers \workers ->
            pure (Map.lookup cancelled.requestId workers)
    forM_ worker \active ->
        void $
            timeout
                core.coreHandlers.shutdownTimeoutMicros
                (cancel active)

handleIncoming :: ControlRuntimeCore -> IncomingControl -> IO ()
handleIncoming core incoming =
    (do
        result <-
            timeout
                core.coreHandlers.controlRequestTimeoutMicros
                (dispatchIncoming
                    core.coreHandlers
                    incoming.incomingRequest)
        case result of
            Nothing ->
                throwIO $
                    CLIConnectionError
                        ("Control handler timed out: "
                            <> controlSubtype
                                incoming.incomingRequest)
            Just response ->
                writeSuccess response)
        `catchAny` (writeFailure . Text.pack . show)
  where
    writeSuccess response =
        writeControlValue core $
            Aeson.object
                [ "type" Aeson..= ("control_response" :: Text)
                , "response" Aeson..= Aeson.object
                    [ "subtype" Aeson..= ("success" :: Text)
                    , "request_id" Aeson..= incoming.incomingRequestId
                    , "response" Aeson..= response
                    ]
                ]
    writeFailure message =
        writeControlValue core $
            Aeson.object
                [ "type" Aeson..= ("control_response" :: Text)
                , "response" Aeson..= Aeson.object
                    [ "subtype" Aeson..= ("error" :: Text)
                    , "request_id" Aeson..= incoming.incomingRequestId
                    , "error" Aeson..= message
                    ]
                ]

dispatchIncoming
    :: ClaudeAgentHandlers
    -> ControlRequest
    -> IO Aeson.Value
dispatchIncoming handlers = \case
    ControlCanUseTool request ->
        case handlers.canUseTool of
            Nothing ->
                throwIO $
                    CLIProtocolError
                        "canUseTool callback is not configured."
            Just callback -> do
                result <- callback request
                pure (permissionResultValue request.input result)
    ControlMcpMessage request ->
        case handlers.handleMcpMessage of
            Nothing ->
                throwIO $
                    CLIProtocolError
                        "MCP control handler is not configured."
            Just callback -> do
                response <- callback request
                pure (Aeson.object ["mcp_response" Aeson..= response])
    request ->
        case handlers.handleUnknownControl of
            Nothing ->
                throwIO $
                    CLIProtocolError
                        ("Unsupported control request: "
                            <> controlSubtype request)
            Just callback ->
                callback request

permissionResultValue
    :: Aeson.Value
    -> ToolPermissionResult
    -> Aeson.Value
permissionResultValue originalInput = \case
    ToolPermissionAllow{updatedInput, updatedPermissions} ->
        Aeson.object $
            [ "behavior" Aeson..= ("allow" :: Text)
            , "updatedInput" Aeson..=
                fromMaybe originalInput updatedInput
            ]
                <> if null updatedPermissions
                    then []
                    else
                        [ "updatedPermissions"
                            Aeson..= updatedPermissions
                        ]
    ToolPermissionDeny{message, interrupt} ->
        Aeson.object $
            [ "behavior" Aeson..= ("deny" :: Text)
            , "message" Aeson..= message
            ]
                <> if interrupt
                    then ["interrupt" Aeson..= True]
                    else []

controlSubtype :: ControlRequest -> Text
controlSubtype = \case
    ControlCanUseTool{} -> "can_use_tool"
    ControlMcpMessage{} -> "mcp_message"
    ControlInterrupt{} -> "interrupt"
    ControlInitialize{} -> "initialize"
    ControlSetPermissionMode{} -> "set_permission_mode"
    ControlSetModel{} -> "set_model"
    ControlGetContextUsage{} -> "get_context_usage"
    ControlStopTask{} -> "stop_task"
    ControlUnknown subtype _ ->
        fromMaybe "<missing subtype>" subtype

runtimeSendRequestCore
    :: ControlRuntimeCore
    -> Int
    -> Aeson.Value
    -> IO Aeson.Value
runtimeSendRequestCore core timeoutMicros request =
    mask \restore -> do
        requestId <- nextRequestId core
        responseVar <- MVar.newEmptyMVar
        modifyMVar_ core.corePending \pending ->
            pure (Map.insert requestId responseVar pending)
        let unregister =
                modifyMVar_ core.corePending
                    (pure . Map.delete requestId)
            envelope =
                Aeson.object
                    [ "type" Aeson..= ("control_request" :: Text)
                    , "request_id" Aeson..= requestId
                    , "request" Aeson..= request
                    ]
        outcome <-
            restore
                (do
                    writeControlValue core envelope
                    timeout timeoutMicros
                        (MVar.takeMVar responseVar)
                )
                `finally` unregister
        case outcome of
            Nothing ->
                throwIO $
                    CLIConnectionError
                        ("Control request timed out: "
                            <> fromMaybe "<unknown>"
                                (valueSubtype request))
            Just ControlError{error} ->
                throwIO (CLIProtocolError error)
            Just ControlSuccess{response} ->
                pure response

writeControlValue :: ControlRuntimeCore -> Aeson.Value -> IO ()
writeControlValue core value = do
    outcome <-
        writeSerialized core
            (LazyByteString.toStrict (Aeson.encode value) <> "\n")
    either throwIO pure outcome

writeSerialized
    :: ControlRuntimeCore
    -> ByteString.ByteString
    -> IO (Either ClaudeSDKError ())
writeSerialized core bytes =
    withMVar core.coreWriteLock \_ ->
        core.coreTransport.transportWrite bytes

initializeRequest :: Maybe Aeson.Value -> Aeson.Value
initializeRequest extra =
    Aeson.Object $
        KeyMap.insert "subtype" (Aeson.String "initialize") $
            KeyMap.insert "hooks" Aeson.Null $
                case extra of
                    Just (Aeson.Object object) -> object
                    _ -> KeyMap.empty

nextRequestId :: ControlRuntimeCore -> IO Text
nextRequestId core =
    atomicModifyIORef' core.coreRequestCounter \counter ->
        let next = counter + 1
        in (next, "haskell_req_" <> Text.pack (show next))

stopCore :: ControlRuntimeCore -> Async () -> IO ()
stopCore core reader = do
    alreadyStopped <-
        atomicModifyIORef' core.coreStopped \stopped ->
            (True, stopped)
    unless alreadyStopped do
        workers <- readMVar core.coreWorkers
        let cancelWithinTimeout worker =
                void $
                    timeout
                        core.coreHandlers.shutdownTimeoutMicros
                        (cancel worker)
        mapConcurrently_ cancelWithinTimeout (Map.elems workers)
        void $
            timeout
                core.coreHandlers.shutdownTimeoutMicros
                (cancel reader)

controlException :: SomeException -> ClaudeSDKError
controlException exception =
    CLIConnectionError
        ("Claude control protocol failed: "
            <> Text.pack (show exception))

lookupText
    :: Key.Key
    -> KeyMap.KeyMap Aeson.Value
    -> Maybe Text
lookupText key object =
    case KeyMap.lookup key object of
        Just (Aeson.String value) -> Just value
        _ -> Nothing

lookupNullableText
    :: Key.Key
    -> KeyMap.KeyMap Aeson.Value
    -> Maybe Text
lookupNullableText = lookupText

lookupValues
    :: Key.Key
    -> KeyMap.KeyMap Aeson.Value
    -> Maybe [Aeson.Value]
lookupValues key object =
    case KeyMap.lookup key object of
        Just (Aeson.Array values) -> Just (foldr (:) [] values)
        _ -> Nothing

valueSubtype :: Aeson.Value -> Maybe Text
valueSubtype (Aeson.Object object) =
    lookupText "subtype" object
valueSubtype _ = Nothing

trimAsciiWhitespace :: ByteString.ByteString -> ByteString.ByteString
trimAsciiWhitespace =
    ByteString.dropWhileEnd isAsciiWhitespace
        . ByteString.dropWhile isAsciiWhitespace
  where
    isAsciiWhitespace byte =
        byte == 32 || (byte >= 9 && byte <= 13)

looksLikeJsonObject :: ByteString.ByteString -> Bool
looksLikeJsonObject bytes =
    case ByteString.uncons (trimAsciiWhitespace bytes) of
        Just (123, _) -> True
        _ -> False
