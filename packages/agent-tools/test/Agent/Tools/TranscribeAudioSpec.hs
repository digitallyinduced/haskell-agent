module Agent.Tools.TranscribeAudioSpec (spec) where

import Agent.ToolDispatch
    ( ToolCallResult(..), ToolDispatchConfig(..), dispatchToolCall
    , functionToolCall, toolCallResultFiles
    )
import Agent.Tools.TranscribeAudio (transcribeAudioTool)
import Agent.Tools.Types (AppTool(..), ToolEnv(..), defaultToolEnv, setToolSessionTmp)
import Control.Exception.Safe (bracket, throwString)
import qualified Data.ByteString as BS
import Data.IORef (newIORef, modifyIORef', readIORef)
import qualified Data.Text as Text
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive, createFileLink)
import System.FilePath ((</>))
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "transcribeAudioTool" do
    it "transcribes extensionless WAV and MP3 bytes and returns only text" do
        calls <- newIORef []
        let transcribe name mime bytes = do
                modifyIORef' calls (<> [(name, mime, bytes)])
                pure (Right "Hallo Welt")
        withTool (\env -> env {toolTranscribeAudio = Just transcribe}) \workspace tool -> do
            BS.writeFile (workspace </> "recording") wavBytes
            BS.writeFile (workspace </> "song") mp3Bytes
            result <- runTool tool "{\"path\":\"recording\"}"
            result.output `shouldBe` "Audio transcript: recording\n\nHallo Welt"
            result.toolResultImages `shouldBe` []
            toolCallResultFiles result `shouldBe` []
            _ <- runTool tool "{\"path\":\"song\"}"
            readIORef calls `shouldReturn`
                [("recording.wav", "audio/wav", wavBytes), ("song.mp3", "audio/mpeg", mp3Bytes)]

    it "rejects oversized audio before invoking transcription" do
        withTool unexpectedTranscription \workspace tool -> do
            BS.writeFile (workspace </> "large") (wavBytes <> BS.replicate (20 * 1024 * 1024) 0)
            result <- runTool tool "{\"path\":\"large\"}"
            result.output `shouldSatisfy` Text.isInfixOf "maximum 20 MiB"

    it "preserves exact bytes at the 20 MiB size limit" do
        let bytes = wavBytes <> BS.replicate (20 * 1024 * 1024 - BS.length wavBytes) 0
            transcribe _ _ actual = do
                actual `shouldBe` bytes
                pure (Right "complete")
        withTool (\env -> env {toolTranscribeAudio = Just transcribe}) \workspace tool -> do
            BS.writeFile (workspace </> "limit.wav") bytes
            result <- runTool tool "{\"path\":\"limit.wav\"}"
            result.output `shouldBe` "Audio transcript: limit.wav\n\ncomplete"

    it "bounds extensionless provider filenames and does not expose absolute paths" do
        calls <- newIORef []
        let transcribe name _ _ = modifyIORef' calls (<> [name]) >> pure (Right "")
        withTool (\env -> env {toolTranscribeAudio = Just transcribe}) \workspace tool -> do
            let name = Text.replicate 255 "a"
            BS.writeFile (workspace </> Text.unpack name) wavBytes
            _ <- runTool tool ("{\"path\":\"" <> Text.pack workspace <> "/" <> name <> "\"}")
            BS.writeFile (workspace </> "Report.WAV") wavBytes
            _ <- runTool tool "{\"path\":\"Report.WAV\"}"
            readIORef calls `shouldReturn` [Text.replicate 251 "a" <> ".wav", "Report.WAV"]

    it "reports unavailable transcription explicitly" do
        withTool id \workspace tool -> do
            BS.writeFile (workspace </> "recording.wav") wavBytes
            result <- runTool tool "{\"path\":\"recording.wav\"}"
            result.output `shouldSatisfy` Text.isInfixOf "Audio transcription is unavailable"

    it "reports provider failures and synchronous exceptions as tool errors" do
        withTool (\env -> env {toolTranscribeAudio = Just (\_ _ _ -> pure (Left "Provider unavailable"))}) \workspace tool -> do
            BS.writeFile (workspace </> "recording.wav") wavBytes
            result <- runTool tool "{\"path\":\"recording.wav\"}"
            result.output `shouldBe` "ERR Audio transcription failed: Provider unavailable"
        withTool (\env -> env {toolTranscribeAudio = Just (\_ _ _ -> throwString "Provider exception")}) \workspace tool -> do
            BS.writeFile (workspace </> "recording.wav") wavBytes
            result <- runTool tool "{\"path\":\"recording.wav\"}"
            result.output `shouldSatisfy` Text.isPrefixOf "ERR Failed to transcribe audio:"

    it "rejects other RIFF containers, AAC, truncated ID3 headers, and ordinary text" do
        withTool unexpectedTranscription \workspace tool -> do
            mapM_ (\bytes -> do
                BS.writeFile (workspace </> "recording.mp3") bytes
                result <- runTool tool "{\"path\":\"recording.mp3\"}"
                result.output `shouldSatisfy` Text.isInfixOf "supported WAV or MP3 audio header")
                ["RIFF\0\0\0\0AVI ", BS.pack [255, 241, 80, 128], "ID3", "ordinary text"]

    it "recognizes ID3-tagged MP3 recordings by content" do
        let bytes = "ID3" <> BS.pack [4, 0, 0, 0, 0, 0, 0] <> mp3Bytes
            transcribe name mime actual = do
                (name, mime, actual) `shouldBe` ("tagged.mp3", "audio/mpeg", bytes)
                pure (Right "tagged")
        withTool (\env -> env {toolTranscribeAudio = Just transcribe}) \workspace tool -> do
            BS.writeFile (workspace </> "tagged") bytes
            result <- runTool tool "{\"path\":\"tagged\"}"
            result.output `shouldBe` "Audio transcript: tagged\n\ntagged"

    it "rejects missing paths and symlinks outside allowed roots before invoking transcription" do
        withTool unexpectedTranscription \_ tool -> do
            missing <- runTool tool "{\"path\":\"missing.wav\"}"
            missing.output `shouldBe` "ERR File not found: missing.wav"
        root <- getTemporaryDirectory
        bracket (mkdtemp (root </> "agent-audio-external-")) removeDirectoryRecursive \outside -> do
            BS.writeFile (outside </> "recording.wav") wavBytes
            withTool unexpectedTranscription \workspace tool -> do
                createFileLink (outside </> "recording.wav") (workspace </> "external.wav")
                result <- runTool tool "{\"path\":\"external.wav\"}"
                result.output `shouldSatisfy` Text.isPrefixOf "ERR "

unexpectedTranscription :: ToolEnv -> ToolEnv
unexpectedTranscription env = env {toolTranscribeAudio = Just (\_ _ _ ->
    expectationFailure "Unexpected transcription" >> pure (Right ""))}

withTool :: (ToolEnv -> ToolEnv) -> (FilePath -> AppTool -> IO a) -> IO a
withTool configure action = do
    root <- getTemporaryDirectory
    bracket (mkdtemp (root </> "agent-transcribe-audio-test-")) removeDirectoryRecursive \workspace -> do
        temp <- mkdtemp (workspace </> "session-temp-")
        env <- defaultToolEnv (unsafeEncodeUtf workspace)
        setToolSessionTmp env (Just (unsafeEncodeUtf temp))
        action workspace (transcribeAudioTool (configure env))

runTool :: AppTool -> Text.Text -> IO ToolCallResult
runTool tool arguments =
    dispatchToolCall testDispatchConfig [tool.appToolHandler]
        (functionToolCall "transcribe-1" "transcribe_audio" arguments)

testDispatchConfig :: ToolDispatchConfig
testDispatchConfig = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "unknown:" <> name
    , toolDispatchFormatResult = either ("ERR " <>) id
    , toolDispatchFormatException = \name _ -> "EX " <> name
    , toolDispatchOnException = \_ _ -> pure ()
    , toolDispatchOnOutput = \_ _ -> pure ()
    , toolDispatchFinalizeOutput = \_ output -> pure output
    }

wavBytes :: BS.ByteString
wavBytes = "RIFF" <> BS.pack [38, 0, 0, 0] <> "WAVEfmt "
    <> BS.pack [16, 0, 0, 0, 1, 0, 1, 0, 128, 62, 0, 0, 0, 125, 0, 0, 2, 0, 16, 0]
    <> "data" <> BS.pack [2, 0, 0, 0, 0, 0]

mp3Bytes :: BS.ByteString
mp3Bytes = BS.pack [255, 251, 144, 0] <> BS.replicate 413 0
