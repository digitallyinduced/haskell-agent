module Agent.CLI.Lsp.Protocol
    ( IncomingMessage(..)
    , encodeLspFrame
    , readMessage
    , sendMessage
    ) where

import Agent.Json
    ( RawJson
    , rawJsonBytes
    , rawJsonDecoder
    )
import Agent.Json.Decode
    ( JsonError(..)
    , decodeEither
    , optionalKey
    )
import Agent.Json.Decode qualified as Hermes
import Control.Exception.Safe (displayException, tryAny)
import Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO (Handle, hFlush)

data IncomingMessage = IncomingMessage
    { incomingId :: !(Maybe RawJson)
    , incomingNumericId :: !(Maybe Int)
    , incomingMethod :: !(Maybe Text)
    , incomingParams :: !(Maybe RawJson)
    , incomingError :: !(Maybe RawJson)
    , incomingResult :: !(Maybe RawJson)
    }

incomingMessageDecoder :: Hermes.Decoder IncomingMessage
incomingMessageDecoder =
    Hermes.object do
        identifier <- optionalKey "id" rawJsonDecoder
        IncomingMessage
            identifier
            (identifier >>= either (const Nothing) Just
                . decodeRawJson Hermes.int)
            <$> optionalKey "method" Hermes.text
            <*> optionalKey "params" rawJsonDecoder
            <*> optionalKey "error" rawJsonDecoder
            <*> optionalKey "result" rawJsonDecoder

decodeRawJson :: Hermes.Decoder a -> RawJson -> Either Text a
decodeRawJson decoder =
    either (Left . jsonErrorMessage) Right
        . decodeEither decoder
        . rawJsonBytes

sendMessage :: Handle -> Aeson.Value -> IO ()
sendMessage handle value = do
    BS.hPut handle (encodeLspFrame value)
    hFlush handle

encodeLspFrame :: Aeson.Value -> BS.ByteString
encodeLspFrame value =
    let body = LBS.toStrict (Aeson.encode value)
    in BS8.pack
        ("Content-Length: " <> show (BS.length body) <> "\r\n\r\n")
            <> body

readMessage :: Handle -> IO (Either Text IncomingMessage)
readMessage handle = do
    headers <- tryAny (readHeaders handle 0 0 Map.empty)
    case headers of
        Left exception ->
            pure . Left $
                "failed to read LSP response headers: "
                    <> Text.pack (displayException exception)
        Right values ->
            case Map.lookup "content-length" values >>= readMaybeInt of
                Nothing ->
                    pure (Left "LSP response omitted a valid Content-Length")
                Just bodyLength
                    | bodyLength < 0 ->
                        pure (Left "LSP response had a negative Content-Length")
                    | bodyLength > maxLspMessageBytes ->
                        pure . Left $
                            "LSP response exceeds "
                                <> Text.pack (show maxLspMessageBytes)
                                <> " bytes"
                    | otherwise -> do
                        bodyResult <- tryAny (BS.hGet handle bodyLength)
                        pure case bodyResult of
                            Left exception ->
                                Left
                                    ( "failed to read LSP response body: "
                                        <> Text.pack
                                            (displayException exception)
                                    )
                            Right body
                                | BS.length body /= bodyLength ->
                                    Left "LSP response ended before Content-Length"
                                | otherwise ->
                                    case decodeEither incomingMessageDecoder body of
                                        Left (JsonError err) ->
                                            Left
                                                ( "LSP response was invalid JSON: "
                                                    <> err
                                                )
                                        Right value -> Right value

readHeaders
    :: Handle
    -> Int
    -> Int
    -> Map String String
    -> IO (Map String String)
readHeaders handle count totalBytes headers = do
    when (count >= maxLspHeaderCount) $
        ioError (userError "LSP response sent too many headers")
    when (totalBytes >= maxLspHeaderBytes) $
        ioError (userError "LSP response headers are too large")
    rawLine <- BS8.hGetLine handle
    let line = BS8.unpack (BS8.takeWhile (/= '\r') rawLine)
        nextBytes = totalBytes + BS.length rawLine
    when (nextBytes > maxLspHeaderBytes) $
        ioError (userError "LSP response headers are too large")
    if null line
        then pure headers
        else
            case break (== ':') line of
                (name, ':' : value) ->
                    readHeaders handle (count + 1) nextBytes $
                        Map.insert
                            (map toLower name)
                            (dropWhile (== ' ') value)
                            headers
                _ ->
                    readHeaders handle (count + 1) nextBytes headers

maxLspHeaderCount, maxLspHeaderBytes, maxLspMessageBytes :: Int
maxLspHeaderCount = 100
maxLspHeaderBytes = 64 * 1024
maxLspMessageBytes = 16 * 1024 * 1024

readMaybeInt :: String -> Maybe Int
readMaybeInt raw =
    case reads raw of
        [(value, "")] -> Just value
        _ -> Nothing
