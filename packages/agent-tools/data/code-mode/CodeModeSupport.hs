{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Runtime support loaded by the experimental, unsandboxed GHCi worker.
module CodeModeSupport
    ( callTool, text, json, image, generatedImage, audio, store, load, decodeJson
    , runCell, barrier
    ) where

import Control.Concurrent.MVar
import Control.Concurrent.Async (race_)
import Control.Exception.Safe (bracket, bracketOnError, displayException, tryAny, onException)
import Control.Monad (void, forever, when)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Map.Strict as Map
import System.Environment (getEnv)
import System.Exit (ExitCode (ExitFailure))
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.IO (dup, fdToHandle, closeFd)
import System.Posix.Process (exitImmediately)
import Text.Read (readMaybe)

-- Serialize writes only; a scoped reader routes replies to request mailboxes.
{-# NOINLINE connection #-}
connection :: MVar (Maybe (Text, MVar Handle, MVar (Integer, Map.Map Integer (MVar Value))))
connection = unsafePerformIO (newMVar Nothing)

-- Handles must not survive a runtime scope: GHCi reloads this module each cell and
-- otherwise loses ownership of the duplicated descriptors with its old CAFs.
withConnection :: ((Handle, Handle) -> IO a) -> IO a
withConnection action =
    bracket (descriptor "HASKELL_AGENT_CODE_INPUT") hClose $ \input ->
        bracket (descriptor "HASKELL_AGENT_CODE_OUTPUT") hClose $ \output -> do
            mapM_ (\handle -> hSetBinaryMode handle True >> hSetBuffering handle NoBuffering)
                [input, output]
            action (input, output)
  where
    descriptor name = do
        value <- getEnv name
        case readMaybe value of
            Nothing -> fail ("invalid runtime descriptor: " <> name)
            Just number -> bracketOnError
                (dup (fromIntegral (number :: Int))) closeFd fdToHandle

request :: Text -> Value -> IO Value
request method arguments = do
    active <- readMVar connection
    (scope, writer, pending) <- maybe (fail "code-mode runtime is not active") pure active
    slot <- newEmptyMVar
    let register = modifyMVar pending $ \(next, slots) ->
            pure ((next + 1, Map.insert next slot slots), next)
        unregister identifier = modifyMVar_ pending $ \(next, slots) ->
            pure (next, Map.delete identifier slots)
    reply <- bracket register unregister $ \identifier -> do
        withMVar writer $ \output -> flip onException (exitImmediately (ExitFailure 70)) $ do
            LBS.hPutStr output (encode (object
                [ "cell" .= runtimeCellIdentifier, "scope" .= scope, "id" .= identifier
                , "method" .= method, "arguments" .= arguments ]))
            BS.hPut output "\n"
            hFlush output
        takeMVar slot
    case reply of
        Object fields
            | Just (String message) <- KeyMap.lookup "error" fields ->
                fail (Text.unpack message)
            | Just result <- KeyMap.lookup "result" fields -> pure result
        _ -> fail "invalid code-mode host response"

withRuntime :: Text -> IO () -> IO ()
withRuntime phase action = withConnection $ \(input, output) -> do
    writer <- newMVar output
    pending <- newMVar (0, Map.empty)
    let scope = runtimeCellIdentifier <> ":" <> phase
        receive = forever $ do
            line <- BS8.hGetLine input
            reply <- either fail pure (eitherDecodeStrict' line)
            identifier <- case reply of
                Object fields | Just value <- KeyMap.lookup "id" fields ->
                    case fromJSON value of
                        Success number -> pure number
                        Error message -> fail message
                _ -> fail "missing response identifier"
            (_, slots) <- readMVar pending
            -- A cancelled caller may already have removed its reply slot.
            when (case reply of
                    Object fields -> KeyMap.lookup "scope" fields == Just (String scope)
                    _ -> False) $
                mapM_ (\slot -> void (tryPutMVar slot reply)) (Map.lookup identifier slots)
        install = modifyMVar_ connection (const (pure (Just (scope, writer, pending))))
        uninstall _ = modifyMVar_ connection (const (pure Nothing))
    bracket install uninstall $ \() -> race_ action receive

-- Substituted by the host before loading this module. Requests from closures
-- belonging to an unloaded cell cannot acquire the next cell's tool snapshot.
runtimeCellIdentifier :: Text
runtimeCellIdentifier = "__HASKELL_CODE_CELL__"

callTool :: Text -> Value -> IO Value
callTool name arguments =
    request "tool" (object ["name" .= name, "arguments" .= arguments])

text :: Text -> IO ()
text value = void $ request "content" (object ["type" .= ("text" :: Text), "text" .= value])

json :: Value -> IO ()
json = text . Text.decodeUtf8 . LBS.toStrict . encode

-- Accept a URL value or the image object returned by the tool dispatcher.
image :: Value -> IO ()
image (String url) = void $ request "content"
    (object ["type" .= ("image" :: Text), "image_url" .= url])
image (Object fields)
    | Just (String _) <- KeyMap.lookup "image_url" fields =
        void $ request "content" (Object (KeyMap.insert "type" (String "image") fields))
image _ = fail "image expects an image URL string or an object containing image_url"

generatedImage :: Value -> IO ()
generatedImage = image

-- Forward an explicitly constructed audio content envelope.
audio :: Value -> IO ()
audio (Object fields)
    | Just (String "audio") <- KeyMap.lookup "type" fields =
        void $ request "content" (Object fields)
audio _ = fail "audio expects an audio content object with type = audio"

store :: Text -> Value -> IO ()
store key value = void $ request "store" (object ["key" .= key, "value" .= value])

load :: Text -> IO Value
load = request "load" . String

decodeJson :: FromJSON value => Value -> Either Text value
decodeJson (String value) =
    either (Left . Text.pack) Right (eitherDecodeStrict' (Text.encodeUtf8 value))
decodeJson value = case fromJSON value of
    Error message -> Left (Text.pack message)
    Success result -> Right result

runCell :: Text -> IO () -> IO ()
runCell identifier action = withRuntime "run" $ do
    result <- tryAny action
    void $ request "completed" $ object
        [ "cell" .= identifier
        , "error" .= either (Just . Text.pack . displayException) (const Nothing) result
        ]

-- A separately loaded support module supplies a barrier even if the user's
-- module failed to compile. Both ordinary output streams are drained as well.
barrier :: Text -> Text -> IO ()
barrier identifier marker = withRuntime "barrier" $ do
    void $ request "barrier" (String identifier)
    hPutStrLn stdout ("\n" <> Text.unpack marker)
    hFlush stdout
    hPutStrLn stderr ("\n" <> Text.unpack marker)
    hFlush stderr
