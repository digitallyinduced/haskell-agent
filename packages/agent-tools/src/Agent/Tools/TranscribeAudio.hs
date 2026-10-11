module Agent.Tools.TranscribeAudio
    ( transcribeAudioTool
    , supportedAudioMime
    ) where

import Agent.Json.Decode (Decoder)
import Agent.OsPath (fromText, toText, unsafeToFilePath)
import Agent.ToolArgs (objectArgs, reqText)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch
    ( ToolCall(..), ToolHandlerResult(..), decodeToolArguments
    , toolArgumentsValue, typedRichToolWithCall
    )
import Agent.Tools.IO (displayPathInWorkspace, resolveForRead)
import Agent.Tools.Scheduling (ToolAccess(..), ToolResource(..), ToolResourceClaim(..))
import Agent.Tools.Types
    ( AppTool, ToolEnv(..), ToolExecutionPolicy(..), jsonTool
    , withSharedToolResourceClaims
    )
import Control.Exception.Safe (SomeException, try)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory.OsPath (doesFileExist)
import System.IO (IOMode(ReadMode), withBinaryFile)
import System.OsPath (takeFileName)

audioPathDecoder :: Decoder Text
audioPathDecoder = objectArgs \object -> reqText object "path"

transcribeAudioTool :: ToolEnv -> AppTool
transcribeAudioTool env = withSharedToolResourceClaims env (audioClaims env) $
    jsonTool "transcribe_audio"
        "Transcribe speech in a local WAV or MP3 recording through the configured transcription provider and return the transcript as text. This is speech-to-text, not native audio input to the conversation model. Files are detected by content, including extensionless files, and must not exceed 20 MiB. Use this tool instead of installing local transcription software."
        [PropertySchema "path" PropertyString True $ Just
            "Path to the audio recording. Relative paths use the workspace; absolute paths must be within an allowed filesystem root."]
        True
        ParallelSafe
        (typedRichToolWithCall "transcribe_audio" audioPathDecoder (\_ -> runTranscribeAudio env))

audioClaims :: ToolEnv -> ToolCall -> IO (Either Text [ToolResourceClaim])
audioClaims env call =
    case decodeToolArguments audioPathDecoder (toolArgumentsValue call.arguments) of
        Left err -> pure (Left err)
        Right path -> fmap (fmap (\resolved -> [ToolResourceClaim ToolRead (ToolPath resolved)]))
            (resolveForRead env (fromText path))

runTranscribeAudio :: ToolEnv -> Text -> IO (Either Text ToolHandlerResult)
runTranscribeAudio env target = resolveForRead env (fromText target) >>= \case
    Left err -> pure (Left err)
    Right path -> doesFileExist path >>= \case
        False -> do
            display <- displayPathInWorkspace env path
            pure (Left ("File not found: " <> display))
        True -> do
            display <- displayPathInWorkspace env path
            try @_ @SomeException (withBinaryFile (unsafeToFilePath path) ReadMode \handle -> do
                prefix <- BS.hGet handle 1024
                case supportedAudioMime prefix of
                    Nothing -> pure (Left "The file does not contain a supported WAV or MP3 audio header.")
                    Just (mime, extension) -> case env.toolTranscribeAudio of
                        Nothing -> pure (Left "Audio transcription is unavailable for this session. Configure a supported transcription provider; do not install local transcription software automatically.")
                        Just transcribe -> do
                            remainder <- BS.hGet handle (maxAudioBytes + 1 - BS.length prefix)
                            let bytes = prefix <> remainder
                                name = toText (takeFileName path)
                                filename =
                                    if extension `Text.isSuffixOf` Text.toLower name
                                        then Text.take (255 - Text.length extension) (Text.dropEnd (Text.length extension) name)
                                            <> Text.takeEnd (Text.length extension) name
                                        else Text.take (255 - Text.length extension) name <> extension
                            if BS.length bytes > maxAudioBytes
                                then pure (Left "Audio file is too large to transcribe (maximum 20 MiB).")
                                else transcribe filename mime bytes >>= \case
                                    Left err -> pure (Left ("Audio transcription failed: " <> err))
                                    Right transcript -> pure (Right (ToolHandlerResult
                                        ("Audio transcript: " <> display <> "\n\n" <> transcript) []))
                ) >>= \case
                    Left err -> pure (Left ("Failed to transcribe audio: " <> Text.pack (show err)))
                    Right result -> pure result

maxAudioBytes :: Int
maxAudioBytes = 20 * 1024 * 1024

-- The backend validates decoding. Exclude non-WAVE RIFF containers and AAC
-- sync words by checking the MPEG audio version, layer, and sample rate.
supportedAudioMime :: BS.ByteString -> Maybe (Text, Text)
supportedAudioMime prefix
    | BS.length prefix >= 12
    , "RIFF" `BS.isPrefixOf` prefix
    , BS.take 4 (BS.drop 8 prefix) == "WAVE" = Just ("audio/wav", ".wav")
    | BS.length prefix >= 10
    , "ID3" `BS.isPrefixOf` prefix
    , BS.index prefix 3 `elem` [2, 3, 4]
    , BS.index prefix 4 /= 255
    , BS.all (< 128) (BS.take 4 (BS.drop 6 prefix)) = Just ("audio/mpeg", ".mp3")
    | BS.length prefix >= 4
    , BS.index prefix 0 == 255
    , let versionLayer = BS.index prefix 1
    , versionLayer .&. 224 == 224
    , versionLayer .&. 24 /= 8
    , versionLayer .&. 6 == 2
    , let rates = BS.index prefix 2
    , rates .&. 240 /= 240
    , rates .&. 12 /= 12 = Just ("audio/mpeg", ".mp3")
    | otherwise = Nothing
