{-# LANGUAGE ForeignFunctionInterface #-}

-- | macOS implementation of opaque per-connection credential storage.
-- Keychain values never cross the host ABI or appear in process arguments.
module Agent.CLI.MacOS.McpKeychain
    ( readMcpKeychain
    , writeMcpKeychain
    , deleteMcpKeychain
    ) where

import Control.Exception.Safe (bracket)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as Encoding
import Foreign (Ptr, alloca, castPtr, nullPtr, peek, poke)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CSize(..))

foreign import ccall safe "agent_mcp_keychain_read"
    keychainRead :: CString -> CSize -> Ptr CString -> Ptr CSize -> IO CInt
foreign import ccall safe "agent_mcp_keychain_write"
    keychainWrite :: CString -> CSize -> CString -> CSize -> IO CInt
foreign import ccall safe "agent_mcp_keychain_delete"
    keychainDelete :: CString -> CSize -> IO CInt
foreign import ccall unsafe "agent_mcp_keychain_release"
    keychainRelease :: CString -> CSize -> IO ()

readMcpKeychain :: Text -> IO (Either Text (Maybe BS.ByteString))
readMcpKeychain identifier =
    BS.useAsCStringLen (Encoding.encodeUtf8 identifier) \(account, accountLength) ->
    alloca \output ->
    alloca \outputLength -> do
        poke output nullPtr
        poke outputLength 0
        let acquire = do
                status <- keychainRead account (fromIntegral accountLength) output outputLength
                bytes <- peek output
                length <- peek outputLength
                pure (status, bytes, length)
            release (_, bytes, length) = keychainRelease bytes length
        bracket acquire release \(status, bytes, length) ->
            case status of
                0 | bytes /= nullPtr && length > 0 && length <= 1024 * 1024 ->
                    Right . Just <$> BS.packCStringLen (castPtr bytes, fromIntegral length)
                1 -> pure (Right Nothing)
                _ -> pure (Left "Cannot read MCP credentials from Keychain. Unlock your login keychain and try again.")

writeMcpKeychain :: Text -> BS.ByteString -> IO (Either Text ())
writeMcpKeychain identifier value =
    BS.useAsCStringLen (Encoding.encodeUtf8 identifier) \(account, accountLength) ->
    BS.useAsCStringLen value \(bytes, length) -> do
        status <- keychainWrite account (fromIntegral accountLength) bytes (fromIntegral length)
        pure $ if status == 0 then Right ()
            else Left "Cannot save MCP credentials in Keychain. Unlock your login keychain and try again."

deleteMcpKeychain :: Text -> IO (Either Text ())
deleteMcpKeychain identifier =
    BS.useAsCStringLen (Encoding.encodeUtf8 identifier) \(account, accountLength) -> do
        status <- keychainDelete account (fromIntegral accountLength)
        pure $ if status == 0 then Right ()
            else Left "Cannot remove MCP credentials from Keychain. Unlock your login keychain and try again."
