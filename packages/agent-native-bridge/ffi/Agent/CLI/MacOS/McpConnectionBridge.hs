{-# LANGUAGE ForeignFunctionInterface #-}

-- | Narrow typed remote MCP catalog and authorization entry points.
module Agent.CLI.MacOS.McpConnectionBridge
    ( McpConnectionCallback
    , McpConnectionAuthorizationCallback
    , emitConnection
    , emitConnectionTerminal
    , observedConnectionState
    ) where

import Agent.CLI.MacOS.Marshalling (decodeUtf8Input, withText)
import Agent.CLI.MacOS.McpConnectionOperation (startMcpConnectionOperation)
import Agent.CLI.MacOS.McpCredentialStore (ensureNativeMcpCredentialStore)
import Agent.CLI.McpAdmin (McpAdminError(..), McpAdminSnapshot(..))
import Agent.CLI.McpConnection
import Agent.Runtime.McpConnectionRuntime (readMcpConnectionIcons)
import qualified Agent.MCP as MCP
import Data.Maybe (fromMaybe)
import Control.Monad (forM_, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8, Word64)
import Foreign (FunPtr, Ptr, nullFunPtr, nullPtr, poke)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CSize(..))
import System.Directory.OsPath (getHomeDirectory)
import System.IO.Unsafe (unsafePerformIO)

type McpConnectionCallback =
    Ptr () -> CInt -> Word64
    -> CString -> CSize -> CString -> CSize -> CString -> CSize
    -> CInt -> CInt -> CString -> CSize -> IO ()

type McpConnectionAuthorizationCallback =
    Ptr () -> CString -> CSize -> IO ()

type McpConnectionIconCallback =
    Ptr () -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize -> IO ()

foreign import ccall "dynamic"
    invokeIconCallback :: FunPtr McpConnectionIconCallback -> McpConnectionIconCallback

foreign export ccall ha_mcp_connections_list_with_icons
    :: FunPtr McpConnectionCallback -> FunPtr McpConnectionIconCallback
    -> Ptr () -> Ptr (Ptr ()) -> IO CInt

ha_mcp_connections_list_with_icons
    :: FunPtr McpConnectionCallback -> FunPtr McpConnectionIconCallback
    -> Ptr () -> Ptr (Ptr ()) -> IO CInt
ha_mcp_connections_list_with_icons callback icons context output = do
    when (output /= nullPtr) (poke output nullPtr)
    if icons == nullFunPtr then pure 1 else
        withConnectionInputs callback output [] \_ ->
            startConnectionResultWithIcons icons callback context output 0 do
                home <- getHomeDirectory
                listMcpConnections home

foreign import ccall "dynamic"
    invokeConnectionCallback
        :: FunPtr McpConnectionCallback -> McpConnectionCallback
foreign import ccall "dynamic"
    invokeAuthorizationCallback
        :: FunPtr McpConnectionAuthorizationCallback
        -> McpConnectionAuthorizationCallback

foreign export ccall ha_mcp_connections_list
    :: FunPtr McpConnectionCallback -> Ptr () -> Ptr (Ptr ()) -> IO CInt
foreign export ccall ha_mcp_connection_create
    :: Word64 -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr McpConnectionCallback -> Ptr () -> Ptr (Ptr ()) -> IO CInt
foreign export ccall ha_mcp_connection_rename
    :: Word64 -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr McpConnectionCallback -> Ptr () -> Ptr (Ptr ()) -> IO CInt
foreign export ccall ha_mcp_connection_set_enabled
    :: Word64 -> Ptr Word8 -> CSize -> CInt
    -> FunPtr McpConnectionCallback -> Ptr () -> Ptr (Ptr ()) -> IO CInt
foreign export ccall ha_mcp_connection_remove
    :: Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpConnectionCallback -> Ptr () -> Ptr (Ptr ()) -> IO CInt
foreign export ccall ha_mcp_connection_authorize
    :: Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpConnectionAuthorizationCallback
    -> FunPtr McpConnectionCallback -> Ptr () -> Ptr (Ptr ()) -> IO CInt

ha_mcp_connections_list
    :: FunPtr McpConnectionCallback -> Ptr () -> Ptr (Ptr ()) -> IO CInt
ha_mcp_connections_list callback context output =
    withConnectionInputs callback output [] \_ ->
        startConnectionResult callback context output 0 do
            home <- getHomeDirectory
            listMcpConnections home

ha_mcp_connection_create
    :: Word64 -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr McpConnectionCallback -> Ptr () -> Ptr (Ptr ()) -> IO CInt
ha_mcp_connection_create expected label labelLength endpoint endpointLength
        callback context output =
    withConnectionInputs callback output
        [(label, labelLength), (endpoint, endpointLength)] \case
            [displayName, url] -> startConnectionResult callback context output 0 do
                home <- getHomeDirectory
                fmap (fmap singletonSnapshot) $
                    createMcpConnection home expected displayName url
            _ -> pure 2

ha_mcp_connection_rename
    :: Word64 -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr McpConnectionCallback -> Ptr () -> Ptr (Ptr ()) -> IO CInt
ha_mcp_connection_rename expected identifier identifierLength label labelLength
        callback context output =
    withConnectionInputs callback output
        [(identifier, identifierLength), (label, labelLength)] \case
            [connectionId, displayName] -> startConnectionResult callback context output 0 do
                home <- getHomeDirectory
                fmap (fmap singletonSnapshot) $
                    renameMcpConnection home expected connectionId displayName
            _ -> pure 2

ha_mcp_connection_set_enabled
    :: Word64 -> Ptr Word8 -> CSize -> CInt
    -> FunPtr McpConnectionCallback -> Ptr () -> Ptr (Ptr ()) -> IO CInt
ha_mcp_connection_set_enabled expected identifier identifierLength enabled
        callback context output =
    withConnectionInputs callback output [(identifier, identifierLength)] \case
        [connectionId] | enabled == 0 || enabled == 1 ->
            startConnectionResult callback context output 0 do
                home <- getHomeDirectory
                fmap (fmap singletonSnapshot) $
                    setMcpConnectionEnabled home expected connectionId (enabled == 1)
        _ -> pure 2

ha_mcp_connection_remove
    :: Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpConnectionCallback -> Ptr () -> Ptr (Ptr ()) -> IO CInt
ha_mcp_connection_remove expected identifier identifierLength callback context output =
    withConnectionInputs callback output [(identifier, identifierLength)] \case
        [connectionId] -> startConnectionResult callback context output 0 do
            home <- getHomeDirectory
            fmap (fmap (\snapshot -> snapshot { mcpAdminValue = [] })) $
                removeMcpConnection home expected connectionId
        _ -> pure 2

ha_mcp_connection_authorize
    :: Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpConnectionAuthorizationCallback
    -> FunPtr McpConnectionCallback -> Ptr () -> Ptr (Ptr ()) -> IO CInt
ha_mcp_connection_authorize expected identifier identifierLength authorization
        callback context output = do
    when (output /= nullPtr) (poke output nullPtr)
    if authorization == nullFunPtr then pure 1 else
        withConnectionInputs callback output [(identifier, identifierLength)] \case
            [connectionId] -> startConnectionResult callback context output 3 do
                home <- getHomeDirectory
                fmap (fmap singletonSnapshot) $
                    authorizeMcpConnection home expected connectionId \url ->
                        withText url (invokeAuthorizationCallback authorization context)
                            >> pure (Right ())
            _ -> pure 2

singletonSnapshot :: McpAdminSnapshot a -> McpAdminSnapshot [a]
singletonSnapshot snapshot = snapshot { mcpAdminValue = [snapshot.mcpAdminValue] }

-- Validate/copy every byte before creating the worker. No filesystem or network
-- work happens on a rejected request.
withConnectionInputs
    :: FunPtr McpConnectionCallback
    -> Ptr (Ptr ())
    -> [(Ptr Word8, CSize)]
    -> ([Text] -> IO CInt)
    -> IO CInt
withConnectionInputs callback output inputs continue = do
    when (output /= nullPtr) (poke output nullPtr)
    if callback == nullFunPtr || output == nullPtr then pure 1
    else if any invalid inputs then pure 2
    else do
        decoded <- traverse (\(bytes, length) ->
            decodeUtf8Input bytes (fromIntegral length)) inputs
        case sequence decoded of
            Left _ -> pure 2
            Right values
                | any (\value -> Text.null value || Text.any (== '\0') value) values -> pure 2
                | otherwise -> continue values
  where
    invalid (bytes, length) =
        bytes == nullPtr || length == 0 || length > 1024 * 1024

startConnectionResult
    :: FunPtr McpConnectionCallback -> Ptr () -> Ptr (Ptr ()) -> CInt
    -> IO (Either McpAdminError (McpAdminSnapshot [McpConnection]))
    -> IO CInt
startConnectionResult = startConnectionResultWithIcons nullFunPtr

startConnectionResultWithIcons
    :: FunPtr McpConnectionIconCallback
    -> FunPtr McpConnectionCallback -> Ptr () -> Ptr (Ptr ()) -> CInt
    -> IO (Either McpAdminError (McpAdminSnapshot [McpConnection]))
    -> IO CInt
startConnectionResultWithIcons icons callback context output state action =
    startMcpConnectionOperation output (ensureNativeMcpCredentialStore >> action) complete
        (emitConnectionTerminal callback context (-1) 0
            "MCP connection operation failed.")
  where
    complete Nothing =
        emitConnectionTerminal callback context (-2) 0 "Connection operation cancelled."
    complete (Just (Left failure)) =
        let (revision, message) = connectionError failure
        in emitConnectionTerminal callback context (-1) revision message
    complete (Just (Right snapshot)) = do
        forM_ snapshot.mcpAdminValue \connection -> do
            observed <- observedConnectionState state connection
            emitConnection callback context snapshot.mcpAdminRevision observed connection
            when (icons /= nullFunPtr && connection.connectionEnabled) do
                metadata <- readMcpConnectionIcons connection.connectionId
                    connection.connectionGeneration connection.connectionUrl
                forM_ metadata \icon ->
                    withText connection.connectionId \identifier identifierLength ->
                    withText icon.iconSrc \src srcLength ->
                    withText (fromMaybe "" icon.iconMimeType) \mime mimeLength ->
                    withText (fromMaybe "" icon.iconTheme) \theme themeLength ->
                        invokeIconCallback icons context identifier identifierLength
                            src srcLength mime mimeLength theme themeLength
        emitConnectionTerminal callback context 1 snapshot.mcpAdminRevision ""

-- A configured endpoint (or a saved credential) is not evidence of readiness.
-- Remember only successful initialize/tools-list verification in this process.
-- Generation matching prevents disable, replacement, and failed reauthorization
-- from inheriting a prior observation. Catalog revisions are opaque hashes,
-- not sequence numbers; independent generations never overwrite each other.
{-# NOINLINE readyConnections #-}
readyConnections :: IORef (Set.Set (Text, Text))
readyConnections = unsafePerformIO (newIORef Set.empty)

observedConnectionState :: CInt -> McpConnection -> IO CInt
observedConnectionState state connection =
    atomicModifyIORef' readyConnections \observations ->
        let key = (connection.connectionId,) <$> connection.connectionGeneration
            updated
                | state == 3 && connection.connectionEnabled =
                    maybe observations (`Set.insert` observations) key
                | otherwise = observations
            ready = connection.connectionEnabled
                && maybe False (`Set.member` updated) key
        in (updated, if ready then 3 else 0)

connectionError :: McpAdminError -> (Word64, Text)
connectionError = \case
    McpAdminConflict revision ->
        (revision, "Connections changed. Reload and try again.")
    McpAdminNotFound _ -> (0, "The MCP connection no longer exists.")
    McpAdminAlreadyExists _ -> (0, "The MCP connection already exists.")
    McpAdminInvalid message -> (0, message)

emitConnection
    :: FunPtr McpConnectionCallback -> Ptr () -> Word64 -> CInt
    -> McpConnection -> IO ()
emitConnection callback context revision state connection =
    withText connection.connectionId \identifier identifierLength ->
    withText connection.connectionDisplayName \label labelLength ->
    withText connection.connectionUrl \endpoint endpointLength ->
        invokeConnectionCallback callback context 0 revision
            identifier identifierLength label labelLength endpoint endpointLength
            (if connection.connectionEnabled then 1 else 0) state nullPtr 0

emitConnectionTerminal
    :: FunPtr McpConnectionCallback -> Ptr () -> CInt -> Word64 -> Text -> IO ()
emitConnectionTerminal callback context status revision message =
    withText message \errorText errorLength ->
        invokeConnectionCallback callback context status revision
            nullPtr 0 nullPtr 0 nullPtr 0 0 0 errorText errorLength
