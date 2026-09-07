-- | Bounded, explicit artifact handling. Resource URIs are sent back only to
-- the originating MCP client; they are never interpreted as HTTP URLs.
module Agent.MCP.Artifact (materializeArtifacts, maximumArtifactBytes) where

import Agent.Json (RawJson, rawJsonBytes, rawJsonDecoder)
import qualified Agent.Json.Decode as Json
import Agent.MCP.Types (McpResourceContent(..))
import Control.Exception.Safe (bracketOnError, tryAny)
import Control.Monad (forM, unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.Char (isControl)
import Data.Bits ((.&.))
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Directory (removeFile, renameFile)
import System.IO (openBinaryTempFile, hClose)
import System.Posix.Files (setFileMode, getSymbolicLinkStatus, isDirectory, fileMode, fileOwner)
import System.Posix.User (getEffectiveUserID)

-- | Errors are deliberately generic: server payloads may contain secrets.
-- The supplied directory must be private, stable and owned by the caller.
materializeArtifacts
    :: FilePath
    -> (Text -> IO (Either Text [McpResourceContent]))
    -> RawJson
    -> IO (Either Text [Text])
materializeArtifacts directory readResource raw =
    case Json.decodeEither descriptors (rawJsonBytes raw) of
        Left _ -> pure (Left "Invalid MCP artifact metadata.")
        Right [] -> pure (Right [])
        Right artifacts -> do
            outcome <- tryAny $ bracketOnError
              (do
                  status <- getSymbolicLinkStatus directory
                  owner <- getEffectiveUserID
                  unless (isDirectory status && fileOwner status == owner
                      && fileMode status .&. 0o077 == 0) $
                      fail "artifact directory must be private"
                  newIORef [])
              (\files -> readIORef files >>= mapM_ (\path -> do
                  _ <- tryAny (removeFile path)
                  pure ()))
              \files -> forM artifacts \(uri, name, size) -> do
                resources <- readResource uri
                bytes <- case resources of
                    Right [resource]
                        | resource.mcpResourceUri == uri
                        , Nothing <- resource.mcpResourceText
                        , Just blob <- resource.mcpResourceBlob
                        , Text.length blob <= 4 * ((size + 2) `div` 3) ->
                            case Base64.decode (TextEncoding.encodeUtf8 blob) of
                                Right value | BS.length value == size -> pure value
                                _ -> fail "invalid artifact bytes"
                    _ -> fail "invalid artifact resource"
                -- The unique temporary basename remains part of the final
                -- filename, preventing collisions between concurrent calls.
                bracketOnError
                    (openBinaryTempFile directory ".mcp-artifact-")
                    (\(path, handle) -> do
                        _ <- tryAny (hClose handle)
                        _ <- tryAny (removeFile path)
                        pure ())
                    \(path, handle) -> do
                        setFileMode path 0o600
                        BS.hPut handle bytes
                        hClose handle
                        let destination = path <> "-" <> Text.unpack name
                        modifyIORef' files (destination :)
                        renameFile path destination
                        pure ("[artifact] " <> Text.pack destination)
            pure $ case outcome of
                Left _ -> Left "MCP artifact download failed."
                Right paths -> Right paths

descriptors :: Json.Decoder [(Text, Text, Int)]
descriptors = Json.object do
    failed <- Json.defaultKey False "isError" Json.bool
    if failed then pure [] else do
        values <- concat <$> Json.defaultKey [] "content" (Json.list descriptor)
        when (length values > 8 || sum (map (\(_, _, n) -> toInteger n) values) > toInteger maximumArtifactBytes) $
            fail "artifact aggregate limit"
        pure values

descriptor :: Json.Decoder [(Text, Text, Int)]
descriptor = Json.object do
    metadata <- Json.optionalKey "_meta" rawJsonDecoder
    let marker = metadata >>= \value -> case Json.decodeEither
            (Json.object (Json.optionalKey "dev.haskell-agent/artifact" rawJsonDecoder))
            (rawJsonBytes value) of
                Right result -> result
                Left _ -> Nothing
    case marker of
        Nothing -> pure []
        Just value -> do
            unless (Json.decodeEither Json.bool (rawJsonBytes value) == Right True) $
                fail "artifact marker"
            kind <- Json.atKey "type" Json.text
            unless (kind == "resource_link") (fail "artifact type")
            uri <- Json.atKey "uri" Json.text
            name <- Json.atKey "name" Json.text
            unless (not (Text.null uri) && Text.length uri <= 4096
                && not (Text.any isControl uri)) (fail "artifact uri")
            unless (not (Text.null name) && BS.length (TextEncoding.encodeUtf8 name) <= 128
                && name /= "." && name /= ".."
                && not (Text.any (\c -> c == '/' || c == '\\' || isControl c) name)) $
                fail "artifact filename"
            size <- Json.atKey "size" Json.int
            unless (size >= 0 && size <= maximumArtifactBytes) (fail "artifact size")
            pure [(uri, name, size)]

maximumArtifactBytes :: Int
maximumArtifactBytes = 20 * 1024 * 1024
