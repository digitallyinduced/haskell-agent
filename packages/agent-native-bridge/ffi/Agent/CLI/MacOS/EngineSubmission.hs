{-# LANGUAGE ForeignFunctionInterface #-}

-- | Synchronous validation and copying before commands enter the engine mailbox.
module Agent.CLI.MacOS.EngineSubmission
    ( integrationABISynchronousValidationSmoke
    ) where

import Agent.CLI.MacOS.EngineCallbacks
    ( IntegrationResultCallback
    , SearchCallback
    , SessionResultCallback
    , TaskSnapshotCallback
    )
import Agent.CLI.MacOS.EngineMailbox (acceptEngineCommand)
import Agent.CLI.MacOS.ConnectionBridge
import Agent.CLI.MacOS.NativeGatewayBoundary (loadNativeGatewayIdentity)
import Agent.Integration.Connection
import Agent.CLI.MacOS.EngineState (Engine(..), EngineCommand(..), SessionMutation(..))
import Agent.CLI.MacOS.Marshalling
    ( anyNonEmptyNull
    , decodeInput
    , decodeUtf8Input
    )
import Agent.CLI.MacOS.McpAdminBridge (McpResultCallback, maxMcpTextBytes, decodeMcpInput)
import Agent.CLI.MacOS.NativeRequest (BridgeRequest)
import Agent.Json (rawJsonDecoder)
import qualified Agent.Json.Decode as Json
import Control.Concurrent.STM (atomically, writeTVar)
import Control.Exception.Safe (tryAny)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8, Word64)
import Foreign (Ptr, FunPtr, StablePtr, castPtrToStablePtr, deRefStablePtr, castPtr, nullPtr, nullFunPtr)
import Foreign.C.Types (CInt(..), CSize(..))

foreign export ccall ha_engine_send_json
    :: Ptr () -> Ptr Word8 -> CSize -> IO CInt

foreign export ccall ha_engine_cancel_task
    :: Ptr () -> Ptr Word8 -> CSize -> IO CInt

foreign export ccall ha_engine_list_tasks
    :: Ptr () -> FunPtr TaskSnapshotCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_set_task_limit
    :: Ptr () -> CSize -> IO CInt

foreign export ccall ha_engine_session_rename
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr SessionResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_session_delete
    :: Ptr () -> Ptr Word8 -> CSize
    -> FunPtr SessionResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_session_archive
    :: Ptr () -> Ptr Word8 -> CSize -> CInt
    -> FunPtr SessionResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_search_conversations
    :: Ptr () -> Ptr Word8 -> CSize -> CSize
    -> FunPtr SearchCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_mcp_server_restart
    :: Ptr () -> Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_integration_admin_list
    :: Ptr () -> FunPtr IntegrationResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_connections_list
    :: Ptr () -> FunPtr ConnectionSecretCallback -> Ptr () -> FunPtr ConnectionCallback -> Ptr () -> IO CInt
foreign export ccall ha_engine_connections_search
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr ConnectionSecretCallback -> Ptr () -> FunPtr ConnectionCallback -> Ptr () -> IO CInt
foreign export ccall ha_engine_connection_begin
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr ConnectionSecretCallback -> Ptr () -> FunPtr ConnectionCallback -> Ptr () -> IO CInt
foreign export ccall ha_engine_connection_submit
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr () -> CSize
    -> FunPtr ConnectionSecretCallback -> Ptr () -> FunPtr ConnectionCallback -> Ptr () -> IO CInt
foreign export ccall ha_engine_connection_poll
    :: Ptr () -> Ptr Word8 -> CSize
    -> FunPtr ConnectionSecretCallback -> Ptr () -> FunPtr ConnectionCallback -> Ptr () -> IO CInt
foreign export ccall ha_engine_connection_cancel
    :: Ptr () -> Ptr Word8 -> CSize
    -> FunPtr ConnectionSecretCallback -> Ptr () -> FunPtr ConnectionCallback -> Ptr () -> IO CInt
foreign export ccall ha_engine_connection_disconnect
    :: Ptr () -> Ptr Word8 -> CSize
    -> FunPtr ConnectionSecretCallback -> Ptr () -> FunPtr ConnectionCallback -> Ptr () -> IO CInt

ha_engine_connections_list
    :: Ptr () -> FunPtr ConnectionSecretCallback -> Ptr ()
    -> FunPtr ConnectionCallback -> Ptr () -> IO CInt
ha_engine_connections_list pointer = submitConnectionCommand pointer (pure (Right ListConnections))
ha_engine_connections_search, ha_engine_connection_begin
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr ConnectionSecretCallback -> Ptr ()
    -> FunPtr ConnectionCallback -> Ptr () -> IO CInt
ha_engine_connections_search pointer a al b bl =
    submitConnectionCommand pointer do
        provider <- connectionText a al
        query <- connectionText b bl
        pure (SearchConnections <$> provider <*> query)
ha_engine_connection_begin pointer a al b bl =
    submitConnectionCommand pointer do
        provider <- connectionText a al
        identifier <- connectionText b bl
        pure (BeginConnection <$> provider <*> identifier)
ha_engine_connection_submit
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr () -> CSize
    -> FunPtr ConnectionSecretCallback -> Ptr ()
    -> FunPtr ConnectionCallback -> Ptr () -> IO CInt
ha_engine_connection_submit pointer session len answers count =
    submitConnectionCommand pointer do
        decodedSession <- connectionText session len
        decodedAnswers <- decodeConnectionAnswers answers count
        pure (SubmitConnection <$> decodedSession <*> decodedAnswers)
ha_engine_connection_poll, ha_engine_connection_cancel, ha_engine_connection_disconnect
    :: Ptr () -> Ptr Word8 -> CSize
    -> FunPtr ConnectionSecretCallback -> Ptr ()
    -> FunPtr ConnectionCallback -> Ptr () -> IO CInt
ha_engine_connection_poll pointer session len =
    submitConnectionCommand pointer (fmap PollConnection <$> connectionText session len)
ha_engine_connection_cancel pointer session len =
    submitConnectionCommand pointer (fmap CancelConnection <$> connectionText session len)
ha_engine_connection_disconnect pointer identifier len =
    submitConnectionCommand pointer (fmap DisconnectConnection <$> connectionText identifier len)

connectionText :: Ptr Word8 -> CSize -> IO (Either Text.Text Text.Text)
connectionText bytes len
    | len > 16384 || (len > 0 && bytes == nullPtr) = pure (Left "Invalid connection input.")
    | len == 0 = pure (Right "")
    | otherwise = either (const (Left "Invalid connection input.")) Right
        <$> decodeUtf8Input bytes (fromIntegral len)

submitConnectionCommand :: Ptr () -> IO (Either Text.Text ConnectionCommand)
    -> FunPtr ConnectionSecretCallback -> Ptr () -> FunPtr ConnectionCallback -> Ptr () -> IO CInt
submitConnectionCommand pointer decode secret secretContext callback context
    | pointer == nullPtr = pure 1
    | callback == nullFunPtr = pure 2
    | otherwise = do
        outcome <- tryAny do
            decode >>= \case
                Left _ -> pure 2
                Right command -> loadNativeGatewayIdentity >>= \case
                    Left _ -> pure 3
                    Right identity -> enqueueIntegrationCommand pointer
                        (EngineConnectionCommand identity command secret secretContext callback context)
        pure (either (const 3) id outcome)

foreign export ccall ha_engine_integration_admin_call
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr IntegrationResultCallback -> Ptr () -> IO CInt

ha_engine_send_json :: Ptr () -> Ptr Word8 -> CSize -> IO CInt
ha_engine_send_json pointer bytes (CSize length)
    | pointer == nullPtr = pure 1
    | bytes == nullPtr && length > 0 = pure 2
    | otherwise = do
        accepted <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            payload <- BS.packCStringLen
                (castPtr bytes, fromIntegral length)
            case (Aeson.eitherDecodeStrict' payload
                :: Either String BridgeRequest) of
                Left _ -> do
                    atomically do
                        writeTVar engine.engineStagedImages Map.empty
                        writeTVar engine.engineStagedTurnOptions Map.empty
                    pure Nothing
                Right request -> Just <$> atomically
                    (acceptEngineCommand
                        engine.engineCommands
                        (EngineRequest request))
        pure $ case accepted of
            Left _ -> 3
            Right Nothing -> 4
            Right (Just False) -> 3
            Right (Just True) -> 0

ha_engine_integration_admin_list
    :: Ptr ()
    -> FunPtr IntegrationResultCallback
    -> Ptr ()
    -> IO CInt
ha_engine_integration_admin_list pointer callback context
    | pointer == nullPtr = pure 1
    | callback == nullFunPtr = pure 2
    | otherwise =
        enqueueIntegrationCommand
            pointer
            (EngineIntegrationAdminList callback context)

ha_engine_integration_admin_call
    :: Ptr ()
    -> Ptr Word8
    -> CSize
    -> Ptr Word8
    -> CSize
    -> FunPtr IntegrationResultCallback
    -> Ptr ()
    -> IO CInt
ha_engine_integration_admin_call
    pointer
    nameBytes
    (CSize nameLength)
    argumentsBytes
    (CSize argumentsLength)
    callback
    context
    | pointer == nullPtr = pure 1
    | callback == nullFunPtr = pure 2
    | nameBytes == nullPtr
        || nameLength == 0
        || nameLength > maximumIntegrationAdminNameBytes = pure 2
    | argumentsBytes == nullPtr
        || argumentsLength == 0
        || argumentsLength > maximumIntegrationAdminArgumentsBytes = pure 2
    | otherwise = do
        decodedName <- decodeUtf8Input nameBytes nameLength
        arguments <- BS.packCStringLen
            (castPtr argumentsBytes, fromIntegral argumentsLength)
        case
            ( decodedName
            , Json.decodeEither rawJsonDecoder arguments
            ) of
            (Right name, Right rawArguments)
                | not (Text.null (Text.strip name)) ->
                    enqueueIntegrationCommand
                        pointer
                        (EngineIntegrationAdminCall
                            (Text.strip name)
                            rawArguments
                            callback
                            context)
            _ -> pure 2

enqueueIntegrationCommand
    :: Ptr ()
    -> EngineCommand
    -> IO CInt
enqueueIntegrationCommand pointer command = do
    accepted <- tryAny do
        let stable = castPtrToStablePtr pointer :: StablePtr Engine
        engine <- deRefStablePtr stable
        atomically $
            acceptEngineCommand engine.engineCommands command
    pure case accepted of
        Left _ -> 3
        Right False -> 3
        Right True -> 0

-- | Null handles and callbacks must fail synchronously without touching
-- integration state or launching work.
integrationABISynchronousValidationSmoke :: IO Bool
integrationABISynchronousValidationSmoke = do
    listStatus <-
        ha_engine_integration_admin_list nullPtr nullFunPtr nullPtr
    callStatus <-
        ha_engine_integration_admin_call
            nullPtr nullPtr 0 nullPtr 0 nullFunPtr nullPtr
    connectionList <- ha_engine_connections_list nullPtr nullFunPtr nullPtr nullFunPtr nullPtr
    connectionSubmit <- ha_engine_connection_submit
        nullPtr nullPtr 0 nullPtr 0 nullFunPtr nullPtr nullFunPtr nullPtr
    invalidAnswers <- decodeConnectionAnswers nullPtr 65
    pure (listStatus == 1 && callStatus == 1 && connectionList == 1 &&
        connectionSubmit == 1 && either (const True) (const False) invalidAnswers)

maximumIntegrationAdminNameBytes, maximumIntegrationAdminArgumentsBytes :: Word64
maximumIntegrationAdminNameBytes = 256
maximumIntegrationAdminArgumentsBytes = 1024 * 1024

ha_engine_search_conversations
    :: Ptr () -> Ptr Word8 -> CSize -> CSize
    -> FunPtr SearchCallback -> Ptr () -> IO CInt
ha_engine_search_conversations pointer bytes (CSize length) rawLimit callback context
    | pointer == nullPtr = pure 1
    | callback == nullFunPtr = pure 2
    | bytes == nullPtr || length == 0 = pure 2
    | otherwise = do
        accepted <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            payload <- BS.packCStringLen (castPtr bytes, fromIntegral length)
            case TextEncoding.decodeUtf8' payload of
                Left _ -> pure Nothing
                Right query
                    | Text.null (Text.strip query) -> pure Nothing
                    | otherwise -> do
                        let requested = fromIntegral rawLimit :: Integer
                            limit = fromInteger (max 1 (min 100 requested))
                        Just <$> atomically
                            (acceptEngineCommand engine.engineCommands
                                (EngineSearch
                                    query limit callback context))
        pure $ case accepted of
            Left _ -> 3
            Right Nothing -> 2
            Right (Just False) -> 3
            Right (Just True) -> 0

ha_engine_session_rename
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr SessionResultCallback -> Ptr () -> IO CInt
ha_engine_session_rename engine idBytes (CSize idLength)
    titleBytes (CSize titleLength) callback context
    | anyNonEmptyNull
        [ (idBytes, idLength), (titleBytes, titleLength) ] = pure 2
    | otherwise = do
        sessionId <- decodeInput idBytes idLength
        title <- decodeInput titleBytes titleLength
        enqueueSessionMutation engine (SessionRename sessionId title) callback context

ha_engine_session_delete
    :: Ptr () -> Ptr Word8 -> CSize
    -> FunPtr SessionResultCallback -> Ptr () -> IO CInt
ha_engine_session_delete engine idBytes (CSize idLength) callback context
    | anyNonEmptyNull [(idBytes, idLength)] = pure 2
    | otherwise = do
        sessionId <- decodeInput idBytes idLength
        enqueueSessionMutation engine (SessionDelete sessionId) callback context

ha_engine_session_archive
    :: Ptr () -> Ptr Word8 -> CSize -> CInt
    -> FunPtr SessionResultCallback -> Ptr () -> IO CInt
ha_engine_session_archive engine idBytes (CSize idLength)
    archived callback context
    | anyNonEmptyNull [(idBytes, idLength)] = pure 2
    | otherwise = do
        sessionId <- decodeInput idBytes idLength
        enqueueSessionMutation
            engine (SessionArchive sessionId (archived /= 0)) callback context

enqueueSessionMutation
    :: Ptr ()
    -> SessionMutation
    -> FunPtr SessionResultCallback
    -> Ptr ()
    -> IO CInt
enqueueSessionMutation pointer mutation callback context
    | pointer == nullPtr = pure 1
    | callback == nullFunPtr = pure 2
    | otherwise = do
        accepted <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            atomically $ acceptEngineCommand engine.engineCommands
                (EngineSessionMutation mutation callback context)
        pure $ case accepted of
            Left _ -> 3
            Right False -> 3
            Right True -> 0

ha_engine_cancel_task
    :: Ptr () -> Ptr Word8 -> CSize -> IO CInt
ha_engine_cancel_task pointer taskID (CSize taskIDLength)
    | pointer == nullPtr = pure 1
    | taskID == nullPtr || taskIDLength == 0 = pure 2
    | otherwise = do
        accepted <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            taskIDBytes <- BS.packCStringLen
                (castPtr taskID, fromIntegral taskIDLength)
            case TextEncoding.decodeUtf8' taskIDBytes of
                Left _ -> pure Nothing
                Right taskIDText
                    | Text.null taskIDText -> pure Nothing
                    | otherwise -> Just <$> atomically
                        (acceptEngineCommand
                            engine.engineCommands
                            (EngineCancelTask taskIDText))
        pure $ case accepted of
            Left _ -> 3
            Right Nothing -> 2
            Right (Just False) -> 3
            Right (Just True) -> 0

ha_engine_list_tasks
    :: Ptr () -> FunPtr TaskSnapshotCallback -> Ptr () -> IO CInt
ha_engine_list_tasks pointer callback context
    | pointer == nullPtr = pure 1
    | callback == nullFunPtr = pure 2
    | otherwise = do
        accepted <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            atomically $ acceptEngineCommand
                engine.engineCommands
                (EngineTaskSnapshot callback context)
        pure $ case accepted of
            Left _ -> 3
            Right False -> 3
            Right True -> 0

ha_engine_set_task_limit :: Ptr () -> CSize -> IO CInt
ha_engine_set_task_limit pointer rawLimit
    | pointer == nullPtr = pure 1
    | limit < 1 || limit > 32 = pure 2
    | otherwise = do
        accepted <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            atomically $ acceptEngineCommand
                engine.engineCommands
                (EngineSetTaskLimit limit)
        pure $ case accepted of
            Left _ -> 3
            Right False -> 3
            Right True -> 0
  where
    limit = fromIntegral rawLimit

ha_engine_mcp_server_restart
    :: Ptr () -> Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpResultCallback -> Ptr () -> IO CInt
ha_engine_mcp_server_restart pointer expected nameBytes (CSize nameLength)
        callback context
    | pointer == nullPtr = pure 1
    | callback == nullFunPtr = pure 2
    | nameBytes == nullPtr || nameLength == 0
        || nameLength > maxMcpTextBytes = pure 2
    | otherwise = do
        decodeMcpInput nameBytes nameLength >>= \case
            Left _ -> pure 2
            Right name -> do
                accepted <- tryAny do
                    let stable =
                            castPtrToStablePtr pointer :: StablePtr Engine
                    engine <- deRefStablePtr stable
                    atomically $ acceptEngineCommand engine.engineCommands
                        (EngineMcpRestart expected name callback context)
                pure case accepted of
                    Left _ -> 3
                    Right False -> 3
                    Right True -> 0
