{-# LANGUAGE OverloadedStrings #-}
-- Verify a running loopback Ollama Responses endpoint, not the agent harness.
-- No model downloads, shell commands, project reads or hosted model requests.
module Main where

import Control.Monad (foldM, unless, when)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Maybe (fromMaybe)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word64)
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Network.URI (URI (..), URIAuth (..), parseURI)
import Numeric (showHex)
import System.Environment (getArgs)
import System.Exit (die)
import System.Random (randomIO)

main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        ["--help"] -> putStrLn usage
        ["--self-test"] -> selfTest
        _ -> do
            (endpoint, model) <- either die pure (parseArguments arguments)
            unless (validEndpoint endpoint) (die "--endpoint must be an unauthenticated loopback HTTP origin")
            manager <- newManager (managerSetProxy noProxy defaultManagerSettings)
            version <- getJson manager (endpoint <> "/api/version")
            versionText <- field "version" version
            putStrLn ("Ollama version: " <> Text.unpack (versionText :: Text))
            catalog <- getJson manager (endpoint <> "/api/tags")
            models <- field "models" catalog :: IO [Value]
            names <- mapM (field "name") models :: IO [Text]
            selected <- case [entry | (name, entry) <- zip names models, name == model] of
                entry : _ -> pure entry
                [] -> die ("Model is not installed: " <> Text.unpack model)
            digest <- field "digest" selected
            putStrLn ("Model: " <> Text.unpack model <> " digest: " <> Text.unpack digest)
            greeting <- createResponse manager endpoint model (String "Reply with a short greeting.") Nothing
            greetingText <- outputText greeting
            when (Text.null (Text.strip greetingText)) (die "Generation completed without visible text")
            putStrLn "Streaming generation: passed"
            let tools = [object
                    [ "type" .= ("function" :: Text), "name" .= ("read_verification_value" :: Text)
                    , "description" .= ("Return the verification value. Call with no arguments." :: Text)
                    , "parameters" .= object ["type" .= ("object" :: Text), "properties" .= object [], "additionalProperties" .= False]]]
                instructions = [object ["role" .= ("user" :: Text), "content" .=
                    ("Call read_verification_value to obtain the verification value. Do not guess it. After receiving the result, repeat it exactly." :: Text)]]
            callResponse <- createResponse manager endpoint model (toJSON instructions) (Just tools)
            outputs <- field "output" callResponse :: IO [Value]
            calls <- filterCalls outputs
            call <- case calls of
                [entry] -> do
                    name <- field "name" entry
                    unless (name == ("read_verification_value" :: Text)) (die "Unexpected verification function name")
                    pure entry
                _ -> die ("Expected one verification function call, got " <> show calls)
            encodedArguments <- field "arguments" call :: IO Text
            decodedArguments <- either die pure (eitherDecodeStrict' (Text.encodeUtf8 encodedArguments) :: Either String Value)
            unless (decodedArguments == object []) (die "Verification function received unexpected arguments")
            randomValue <- randomIO :: IO Word64
            let value = "verification-" <> Text.pack (showHex randomValue "")
            callIdentifier <- field "call_id" call :: IO Text
            let continuation = instructions <> outputs <> [object
                    ["type" .= ("function_call_output" :: Text), "call_id" .= callIdentifier, "output" .= value]]
            result <- createResponse manager endpoint model (toJSON continuation) Nothing
            resultText <- outputText result
            unless (value `Text.isInfixOf` resultText) (die "Model did not report the supplied function result")
            putStrLn "Function-call and result replay: passed"
            putStrLn "This verifies the wire API only; run the documented agent README test separately."

usage :: String
usage = "VerifyLocalModel.hs [--endpoint http://127.0.0.1:11434] [--model qwen3:0.6b] [--self-test]"

parseArguments :: [String] -> Either String (String, Text)
parseArguments = go "http://127.0.0.1:11434" "qwen3:0.6b"
  where
    go endpoint model [] = Right (reverse (dropWhile (== '/') (reverse endpoint)), model)
    go _ model ("--endpoint" : endpoint : rest) = go endpoint model rest
    go endpoint _ ("--model" : model : rest) = go endpoint (Text.pack model) rest
    go _ _ _ = Left usage

validEndpoint :: String -> Bool
validEndpoint endpoint = case parseURI endpoint of
    Just uri | uriScheme uri == "http:", null (uriPath uri), null (uriQuery uri), null (uriFragment uri) ->
        case uriAuthority uri of
            Just authority -> null (uriUserInfo authority)
                && uriRegName authority `elem` ["127.0.0.1", "localhost", "[::1]"]
            Nothing -> False
    _ -> False

field :: FromJSON value => Key -> Value -> IO value
field name value = either die pure (parseEither (withObject "object" (.: name)) value)

getJson :: Manager -> String -> IO Value
getJson manager url = do
    request <- parseRequest url
    response <- httpLbs request {responseTimeout = responseTimeoutMicro 10000000, redirectCount = 0} manager
    unless (statusCode (responseStatus response) == 200) (die "Local endpoint returned a non-success response")
    either die pure (eitherDecode (responseBody response))

createResponse :: Manager -> String -> Text -> Value -> Maybe [Value] -> IO Value
createResponse manager endpoint model input tools = do
    initial <- parseRequest (endpoint <> "/v1/responses")
    let payload = object $
            ["model" .= model, "input" .= input, "stream" .= True, "store" .= False,
             "max_output_tokens" .= (256 :: Int), "reasoning" .= object ["effort" .= ("none" :: Text)]]
            <> maybe [] (\values -> ["tools" .= values]) tools
        request = initial {method = "POST", requestBody = RequestBodyLBS (encode payload),
            requestHeaders = [("Content-Type", "application/json")],
            responseTimeout = responseTimeoutMicro 120000000, redirectCount = 0}
    withResponse request manager $ \response -> do
        unless (statusCode (responseStatus response) == 200) (die "Local response endpoint returned a non-success response")
        consumeStream (responseBody response) ByteString.empty Nothing

-- Read incrementally; HTTP body chunks need not align with SSE lines.
consumeStream :: BodyReader -> ByteString.ByteString -> Maybe Value -> IO Value
consumeStream reader pending completed = do
    chunk <- brRead reader
    if ByteString.null chunk then do
        final <- either die pure (consumeLine completed pending)
        maybe (die "Stream ended without response.completed") pure final
    else do
        let segments = ByteString.Char8.split '\n' (pending <> chunk)
        updated <- either die pure (foldM consumeLine completed (init segments))
        consumeStream reader (last segments) updated

consumeLine :: Maybe Value -> ByteString.ByteString -> Either String (Maybe Value)
consumeLine completed line
    | not ("data:" `ByteString.isPrefixOf` line) = Right completed
    | payload == "[DONE]" = Right completed
    | otherwise = do
        event <- eitherDecodeStrict' payload
        eventType <- parseEither (withObject "event" (.:? "type")) event
        case eventType :: Maybe Text of
            Just "error" -> Left ("Response failed: " <> show event)
            Just "response.failed" -> Left ("Response failed: " <> show event)
            Just "response.completed" -> Just <$> parseEither (withObject "event" (.: "response")) event
            _ -> Right completed
  where
    payload = ByteString.Char8.dropWhile (`elem` [' ', '\t', '\r']) $
        ByteString.Char8.dropWhileEnd (`elem` [' ', '\t', '\r']) (ByteString.drop 5 line)

filterCalls :: [Value] -> IO [Value]
filterCalls values = do
    types <- mapM (field "type") values :: IO [Text]
    pure [value | (kind, value) <- zip types values, kind == "function_call"]

outputText :: Value -> IO Text
outputText response = do
    outputs <- field "output" response :: IO [Value]
    Text.concat <$> mapM itemText outputs
  where
    itemText item = do
        kind <- field "type" item :: IO Text
        if kind /= "message" then pure "" else do
            content <- either die pure (parseEither (withObject "message" (.:? "content")) item)
            Text.concat <$> mapM partText (fromMaybe [] content)
    partText part = do
        kind <- field "type" part :: IO Text
        if kind /= "output_text" then pure "" else field "text" part

selfTest :: IO ()
selfTest = do
    unless (all validEndpoint ["http://127.0.0.1:11434", "http://localhost", "http://[::1]:11434"]) (die "Loopback validation regression")
    when (any validEndpoint ["https://localhost", "http://example.com", "http://user@localhost",
        "http://localhost/path", "http://localhost?token=x", "http://localhost#fragment",
        "http://localhost.example.com", "http://127.0.0.1@evil.invalid"]) (die "Unsafe endpoint accepted")
    let completed = object ["output" .= ([] :: [Value])]
        event = LazyByteString.toStrict (encode (object ["type" .= ("response.completed" :: Text), "response" .= completed]))
    unless (consumeLine Nothing ("data: " <> event <> "\r") == Right (Just completed)) (die "SSE completion regression")
    unless (consumeLine (Just completed) "data: [DONE]" == Right (Just completed)) (die "SSE terminator regression")
    chunks <- newIORef ["event: response.completed\nda", "ta: " <> ByteString.take 7 event,
        ByteString.drop 7 event <> "\r\n", "data: [DONE]\n"]
    let reader = atomicModifyIORef' chunks $ \remaining -> case remaining of
            [] -> ([], ByteString.empty)
            chunk : rest -> (rest, chunk)
    received <- consumeStream reader ByteString.empty Nothing
    unless (received == completed) (die "Fragmented stream fixture failed")
    case consumeLine Nothing "data: {\"type\":\"response.failed\"}" of
        Left _ -> pure ()
        Right _ -> die "SSE failure accepted"
    putStrLn "Loopback validation and SSE fixtures passed. No model was contacted."
