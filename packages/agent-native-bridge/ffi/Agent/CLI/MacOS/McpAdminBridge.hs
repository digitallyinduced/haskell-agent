{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Independent MCP catalog endpoints and callback marshalling. Engine restart
-- admission and execution stay with the engine's mailbox and supervisor.
module Agent.CLI.MacOS.McpAdminBridge
    ( McpResultCallback
    , invokeMcpResultCallback
    , decodeMcpInput
    , maxMcpTextBytes
    , emitMcpResult
    , mcpAdminTry
    ) where

import Agent.CLI.MacOS.Marshalling
    ( nonEmptyText
    , withOptionalText
    , withText
    )
import Agent.CLI.McpAdmin
    ( McpAdminError(..)
    , McpAdminServer(..)
    , McpAdminServerInput(..)
    , McpAdminSnapshot(..)
    , addMcpAdminServer
    , editMcpAdminServer
    , listMcpAdminServers
    , readMcpAdminServer
    , removeMcpAdminServer
    , setMcpAdminServerEnabled
    )
import Control.Concurrent (forkIO)
import Control.Exception.Safe (tryAny)
import Control.Monad (forM_)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8, Word64)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Ptr
    ( FunPtr, Ptr, castPtr, nullFunPtr, nullPtr, plusPtr )
import Foreign.Storable (peekByteOff, sizeOf)
import System.Directory.OsPath (getHomeDirectory)
import System.OsPath (OsPath)

type McpServerCallback =
    Ptr () -> CInt -> Word64
    -> CString -> CSize -> CInt -> CString -> CSize -> CString -> CSize
    -> CInt -> CInt -> CSize -> CSize -> CString -> CSize -> IO ()

-- kind 0 is an argument and kind 1 is an environment key. Environment values
-- never cross the bridge.
type McpServerFieldCallback =
    Ptr () -> CString -> CSize -> CInt -> CSize
    -> CString -> CSize -> IO ()

type McpResultCallback =
    Ptr () -> CInt -> Word64 -> CString -> CSize -> IO ()

foreign import ccall "dynamic"
    invokeMcpServerCallback
        :: FunPtr McpServerCallback -> McpServerCallback

foreign import ccall "dynamic"
    invokeMcpServerFieldCallback
        :: FunPtr McpServerFieldCallback -> McpServerFieldCallback

foreign import ccall "dynamic"
    invokeMcpResultCallback
        :: FunPtr McpResultCallback -> McpResultCallback

foreign export ccall ha_mcp_servers_list
    :: FunPtr McpServerCallback -> FunPtr McpServerFieldCallback
    -> Ptr () -> IO CInt

foreign export ccall ha_mcp_server_read
    :: Ptr Word8 -> CSize -> FunPtr McpServerCallback
    -> FunPtr McpServerFieldCallback -> Ptr () -> IO CInt

foreign export ccall ha_mcp_server_status
    :: Ptr Word8 -> CSize -> FunPtr McpServerCallback
    -> FunPtr McpServerFieldCallback -> Ptr () -> IO CInt

foreign export ccall ha_mcp_server_add
    :: Word64 -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr () -> CSize -> Ptr Word8 -> CSize -> Ptr () -> CSize
    -> CInt -> CInt -> FunPtr McpResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_mcp_server_edit
    :: Word64 -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr () -> CSize -> Ptr Word8 -> CSize -> Ptr () -> CSize
    -> CInt -> CInt -> FunPtr McpResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_mcp_server_enable
    :: Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_mcp_server_disable
    :: Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_mcp_server_remove
    :: Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpResultCallback -> Ptr () -> IO CInt

ha_mcp_servers_list
    :: FunPtr McpServerCallback -> FunPtr McpServerFieldCallback
    -> Ptr () -> IO CInt
ha_mcp_servers_list callback fieldCallback context
    | callback == nullFunPtr || fieldCallback == nullFunPtr = pure 1
    | otherwise = do
        home <- getHomeDirectory
        _ <- forkIO $
            mcpAdminTry (listMcpAdminServers home) >>= emitMcpServers
                callback fieldCallback context
        pure 0

ha_mcp_server_status
    :: Ptr Word8 -> CSize -> FunPtr McpServerCallback
    -> FunPtr McpServerFieldCallback -> Ptr () -> IO CInt
ha_mcp_server_status = ha_mcp_server_read

ha_mcp_server_read
    :: Ptr Word8 -> CSize -> FunPtr McpServerCallback
    -> FunPtr McpServerFieldCallback -> Ptr () -> IO CInt
ha_mcp_server_read nameBytes (CSize nameLength) callback fieldCallback context
    | callback == nullFunPtr || fieldCallback == nullFunPtr = pure 1
    | nameBytes == nullPtr || nameLength == 0
        || nameLength > maxMcpTextBytes = pure 2
    | otherwise = do
        decodeMcpInput nameBytes nameLength >>= \case
            Left _ -> pure 2
            Right name -> do
                home <- getHomeDirectory
                _ <- forkIO $
                    mcpAdminTry (readMcpAdminServer home name) >>= emitMcpServer
                        callback fieldCallback context
                pure 0

ha_mcp_server_add
    :: Word64 -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr () -> CSize -> Ptr Word8 -> CSize -> Ptr () -> CSize
    -> CInt -> CInt -> FunPtr McpResultCallback -> Ptr () -> IO CInt
ha_mcp_server_add =
    mcpServerWrite addMcpAdminServer

ha_mcp_server_edit
    :: Word64 -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr () -> CSize -> Ptr Word8 -> CSize -> Ptr () -> CSize
    -> CInt -> CInt -> FunPtr McpResultCallback -> Ptr () -> IO CInt
ha_mcp_server_edit =
    mcpServerWrite editMcpAdminServer

ha_mcp_server_enable
    :: Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpResultCallback -> Ptr () -> IO CInt
ha_mcp_server_enable = mcpServerSetEnabled True

ha_mcp_server_disable
    :: Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpResultCallback -> Ptr () -> IO CInt
ha_mcp_server_disable = mcpServerSetEnabled False

ha_mcp_server_remove
    :: Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpResultCallback -> Ptr () -> IO CInt
ha_mcp_server_remove expected nameBytes (CSize nameLength) callback context
    | callback == nullFunPtr = pure 1
    | nameBytes == nullPtr || nameLength == 0
        || nameLength > maxMcpTextBytes = pure 2
    | otherwise = do
        decodeMcpInput nameBytes nameLength >>= \case
            Left _ -> pure 2
            Right name -> do
                home <- getHomeDirectory
                _ <- forkIO $
                    mcpAdminTry (removeMcpAdminServer home expected name)
                        >>= emitMcpResult
                        callback context
                pure 0

mcpServerSetEnabled
    :: Bool -> Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpResultCallback -> Ptr () -> IO CInt
mcpServerSetEnabled enabled expected nameBytes (CSize nameLength)
        callback context
    | callback == nullFunPtr = pure 1
    | nameBytes == nullPtr || nameLength == 0
        || nameLength > maxMcpTextBytes = pure 2
    | otherwise = do
        decodeMcpInput nameBytes nameLength >>= \case
            Left _ -> pure 2
            Right name -> do
                home <- getHomeDirectory
                _ <- forkIO $
                    mcpAdminTry
                        (setMcpAdminServerEnabled home expected name enabled)
                        >>= emitMcpResult callback context
                pure 0

mcpServerWrite
    :: (OsPath -> Word64 -> Text -> McpAdminServerInput
        -> IO (Either McpAdminError (McpAdminSnapshot McpAdminServer)))
    -> Word64 -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr () -> CSize -> Ptr Word8 -> CSize -> Ptr () -> CSize
    -> CInt -> CInt -> FunPtr McpResultCallback -> Ptr () -> IO CInt
mcpServerWrite write expected nameBytes (CSize nameLength)
        commandBytes (CSize commandLength) argsPointer argsCount
        cwdBytes (CSize cwdLength) envPointer envCount
        (CInt startupTimeout) (CInt requestTimeout) callback context
    | callback == nullFunPtr = pure 1
    | nameBytes == nullPtr || nameLength == 0
        || nameLength > maxMcpTextBytes
        || commandBytes == nullPtr || commandLength == 0
        || commandLength > maxMcpTextBytes
        || cwdLength > maxMcpTextBytes
        || argsCount > maxMcpFields || envCount > maxMcpFields = pure 2
    | argsPointer == nullPtr && argsCount > 0
        || envPointer == nullPtr && envCount > 0
        || cwdBytes == nullPtr && cwdLength > 0 = pure 2
    | otherwise = do
        decoded <- tryAny do
            name <- requireMcpInput nameBytes nameLength
            command <- requireMcpInput commandBytes commandLength
            cwd <- requireMcpInput cwdBytes cwdLength
            args <- mapM (peekUtf8Slice argsPointer)
                [0 .. fromIntegral argsCount - 1]
            env <- Map.fromList <$> mapM (peekEnvEntry envPointer)
                [0 .. fromIntegral envCount - 1]
            pure (name, McpAdminServerInput
                { mcpAdminInputCommand = command
                , mcpAdminInputArgs = args
                , mcpAdminInputCwd = nonEmptyText cwd
                , mcpAdminInputEnv = env
                , mcpAdminInputStartupTimeoutSeconds =
                    fromIntegral startupTimeout
                , mcpAdminInputRequestTimeoutSeconds =
                    fromIntegral requestTimeout
                })
        case decoded of
            Left _ -> pure 2
            Right (name, input) -> do
                home <- getHomeDirectory
                _ <- forkIO $
                    mcpAdminTry (write home expected name input)
                        >>= emitMcpResult callback context
                pure 0

peekUtf8Slice :: Ptr () -> Int -> IO Text
peekUtf8Slice pointer index = do
    let pointerSize = sizeOf (nullPtr :: Ptr ())
        sizeSize = sizeOf (undefined :: CSize)
        base = pointer `plusPtr` (index * (pointerSize + sizeSize))
    bytes <- peekByteOff base 0
    CSize length <- peekByteOff base pointerSize
    if (bytes == (nullPtr :: Ptr Word8) && length > 0)
            || length > maxMcpTextBytes
        then ioError (userError "null UTF-8 slice")
        else requireMcpInput bytes length

decodeMcpInput :: Ptr Word8 -> Word64 -> IO (Either Text Text)
decodeMcpInput pointer length
    | pointer == nullPtr || length == 0 = pure (Right "")
    | otherwise = do
        bytes <- BS.packCStringLen (castPtr pointer, fromIntegral length)
        pure (first (Text.pack . show) (TextEncoding.decodeUtf8' bytes))

requireMcpInput :: Ptr Word8 -> Word64 -> IO Text
requireMcpInput pointer length =
    decodeMcpInput pointer length >>=
        either (ioError . userError . Text.unpack) pure

maxMcpTextBytes :: Word64
maxMcpTextBytes = 1024 * 1024

maxMcpFields :: CSize
maxMcpFields = 4096

peekEnvEntry :: Ptr () -> Int -> IO (Text, Text)
peekEnvEntry pointer index = do
    let sliceSize =
            sizeOf (nullPtr :: Ptr ()) + sizeOf (undefined :: CSize)
        base = pointer `plusPtr` (index * sliceSize * 2)
    key <- peekUtf8Slice base 0
    value <- peekUtf8Slice (base `plusPtr` sliceSize) 0
    pure (key, value)

emitMcpServers
    :: FunPtr McpServerCallback -> FunPtr McpServerFieldCallback -> Ptr ()
    -> Either McpAdminError (McpAdminSnapshot [McpAdminServer]) -> IO ()
emitMcpServers callback fieldCallback context = \case
    Left err -> emitMcpServerError callback context err
    Right snapshot -> do
        forM_ snapshot.mcpAdminValue \server ->
            emitMcpServerItem
                callback fieldCallback context snapshot.mcpAdminRevision server
        invokeMcpServerCallback callback context 1 snapshot.mcpAdminRevision
            nullPtr 0 0 nullPtr 0 nullPtr 0 0 0 0 0 nullPtr 0

emitMcpServer
    :: FunPtr McpServerCallback -> FunPtr McpServerFieldCallback -> Ptr ()
    -> Either McpAdminError (McpAdminSnapshot McpAdminServer) -> IO ()
emitMcpServer callback fieldCallback context = \case
    Left err -> emitMcpServerError callback context err
    Right snapshot ->
        emitMcpServerItem
            callback fieldCallback context snapshot.mcpAdminRevision
            snapshot.mcpAdminValue

emitMcpServerItem
    :: FunPtr McpServerCallback -> FunPtr McpServerFieldCallback -> Ptr ()
    -> Word64 -> McpAdminServer -> IO ()
emitMcpServerItem callback fieldCallback context revision server = do
    withText server.mcpAdminName \name nameLength -> do
        forM_ (zip [0..] server.mcpAdminArgs) \(index, argument) ->
            withText argument $
                invokeMcpServerFieldCallback fieldCallback context
                    name nameLength 0 index
        forM_ (zip [0..] server.mcpAdminEnvKeys) \(index, key) ->
            withText key $
                invokeMcpServerFieldCallback fieldCallback context
                    name nameLength 1 index
        withText server.mcpAdminCommand \command commandLength ->
            withOptionalText server.mcpAdminCwd \cwd cwdLength ->
                invokeMcpServerCallback callback context 0 revision
                    name nameLength
                    (if server.mcpAdminEnabled then 1 else 0)
                    command commandLength cwd cwdLength
                    (fromIntegral server.mcpAdminStartupTimeoutSeconds)
                    (fromIntegral server.mcpAdminRequestTimeoutSeconds)
                    (fromIntegral (length server.mcpAdminArgs))
                    (fromIntegral (length server.mcpAdminEnvKeys))
                    nullPtr 0

emitMcpServerError
    :: FunPtr McpServerCallback -> Ptr () -> McpAdminError -> IO ()
emitMcpServerError callback context err =
    withText (mcpAdminErrorText err) \errorPtr errorLength ->
        invokeMcpServerCallback callback context (-1)
            (mcpAdminErrorRevision err)
            nullPtr 0 0 nullPtr 0 nullPtr 0 0 0 0 0
            errorPtr errorLength

emitMcpResult
    :: FunPtr McpResultCallback -> Ptr ()
    -> Either McpAdminError (McpAdminSnapshot a) -> IO ()
emitMcpResult callback context = \case
    Left err ->
        withText (mcpAdminErrorText err) $
            invokeMcpResultCallback callback context (-1)
                (mcpAdminErrorRevision err)
    Right snapshot ->
        invokeMcpResultCallback callback context 0 snapshot.mcpAdminRevision
            nullPtr 0

mcpAdminErrorRevision :: McpAdminError -> Word64
mcpAdminErrorRevision = \case
    McpAdminConflict revision -> revision
    _ -> 0

mcpAdminErrorText :: McpAdminError -> Text
mcpAdminErrorText = \case
    McpAdminConflict _ -> "MCP catalog changed; reload before editing"
    McpAdminNotFound name -> "MCP server not found: " <> name
    McpAdminAlreadyExists name -> "MCP server already exists: " <> name
    McpAdminInvalid err -> err

mcpAdminTry
    :: IO (Either McpAdminError a)
    -> IO (Either McpAdminError a)
mcpAdminTry action =
    tryAny action >>= \case
        Left exception ->
            pure (Left (McpAdminInvalid (Text.pack (show exception))))
        Right result -> pure result
