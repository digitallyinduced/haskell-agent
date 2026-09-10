module Agent.Tools.FileSystem.ReadFileSpec (spec) where

import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolDispatchConfig(..)
    , ToolResultImage(..)
    , dispatchToolCall
    , functionToolCall
    )
import Agent.Tools.FileSystem.ReadFile
    ( ReadFileArgs(..)
    , formatReadFile
    , readFileTool
    , streamReadFile
    )
import Agent.Tools.Types (AppTool(..), defaultToolEnv, setToolSessionTmp)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import qualified Data.Text as Text
import Control.Exception.Safe (bracket)
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "formatReadFile" do
    it "treats offset -1 as the last content line when the file ends with a newline" do
        formatReadFile "a\nb\nc\n" (readArgs (Just (-1)) Nothing)
            `shouldBe` Right "3\8594c"

    it "treats offset -1 as the last content line when the file has no trailing newline" do
        formatReadFile "a\nb\nc" (readArgs (Just (-1)) Nothing)
            `shouldBe` Right "3\8594c"

    it "reads the last N lines with a negative offset" do
        formatReadFile "a\nb\nc\n" (readArgs (Just (-2)) (Just 2))
            `shouldBe` Right "2\8594b\nc"

    it "clamps an offset past the start of the file to the first line" do
        formatReadFile "a\nb\nc\n" (readArgs (Just (-80)) (Just 1))
            `shouldBe` Right "1\8594a"

    it "does not count a trailing newline as an extra empty line" do
        formatReadFile "a\nb\n" (readArgs (Just 1) Nothing)
            `shouldBe` Right "1\8594a\nb"

    it "reports when a positive offset is past the last line" do
        formatReadFile "a\nb\nc\n" (readArgs (Just 4) Nothing)
            `shouldBe` Right "Offset 4 is beyond the end of the file (3 lines)."

    it "rejects a non-positive limit" do
        formatReadFile "a\nb\n" (readArgs (Just 1) (Just (-1)))
            `shouldSatisfy` isLeft
        formatReadFile "a\nb\n" (readArgs (Just 1) (Just 0))
            `shouldSatisfy` isLeft

    it "numbers the first line and every tenth line" do
        let content = Text.unlines (map (Text.pack . show) [1 .. 12 :: Int])
        formatReadFile content (readArgs Nothing Nothing)
            `shouldBe` Right
                ( Text.intercalate "\n"
                    [ "1\8594" <> "1"
                    , "2"
                    , "3"
                    , "4"
                    , "5"
                    , "6"
                    , "7"
                    , "8"
                    , "9"
                    , "10\8594" <> "10"
                    , "11"
                    , "12"
                    ]
                )

    describe "streamReadFile" do
        it "matches empty-file offset semantics" do
            withFile "" \path -> do
                streamReadFile (unsafeEncodeUtf path) (readArgs Nothing Nothing)
                    `shouldReturn` Right "1\8594"
                streamReadFile (unsafeEncodeUtf path) (readArgs (Just 2) Nothing)
                    `shouldReturn` Right "Offset 2 is beyond the end of the file (1 lines)."

        it "preserves CRLF and trailing newline behavior" do
            withFile "a\r\nb\r\n" \path ->
                streamReadFile (unsafeEncodeUtf path) (readArgs Nothing Nothing)
                    `shouldReturn` Right "1\8594a\r\nb\r"

        it "supports negative offsets" do
            withFile "a\nb\nc\n" \path ->
                streamReadFile (unsafeEncodeUtf path) (readArgs (Just (-2)) (Just 2))
                    `shouldReturn` Right "2\8594b\nc"

        it "decodes invalid UTF-8 leniently across chunks" do
            withBytes (BS.replicate 65535 97 <> BS.pack [0xc3, 0x28] <> "\n") \path ->
                streamReadFile (unsafeEncodeUtf path) (readArgs Nothing Nothing)
                    >>= (`shouldSatisfy`
                        either (const False) (Text.isInfixOf "\xfffd("))

        it "rejects NUL bytes in the first 8 KiB" do
            withBytes "prefix\0suffix" \path ->
                streamReadFile (unsafeEncodeUtf path) (readArgs Nothing Nothing)
                    `shouldReturn` Left "Cannot read binary file"

        it "skips giant unselected lines without retaining them" do
            withBytes (BS.replicate 300000 120 <> "\nsmall\n") \path ->
                streamReadFile (unsafeEncodeUtf path) (readArgs (Just 2) (Just 1))
                    `shouldReturn` Right "2\8594small"

        it "fails early on a giant selected line" do
            withBytes (BS.replicate 300000 120 <> "\n") \path ->
                streamReadFile (unsafeEncodeUtf path) (readArgs Nothing Nothing)
                    >>= (`shouldSatisfy` isLeft)

    describe "readFileTool" do
        it "still returns numbered text for ordinary files" do
            withTool \workspace tool -> do
                writeFile (workspace </> "notes.txt") "hello\n"
                result <- runReadTool tool "{\"target_file\":\"notes.txt\"}"
                result.output `shouldBe` "1\8594hello"
                result.toolResultImages `shouldBe` []

        it "attaches a PNG to the model-facing result" do
            withTool \workspace tool -> do
                BS.writeFile (workspace </> "shot.png") pngBytes
                result <- runReadTool tool "{\"target_file\":\"shot.png\"}"
                case result of
                    ToolCallResult{output, toolResultImages = [image]} -> do
                        output `shouldBe` "Viewed image file: shot.png"
                        image.imageUrl `shouldSatisfy`
                            Text.isPrefixOf "data:image/png;base64,"
                    _ -> expectationFailure ("expected image result, got " <> show result)

        it "still rejects non-image binary files" do
            withTool \workspace tool -> do
                BS.writeFile (workspace </> "blob.bin") "prefix\0suffix"
                result <- runReadTool tool "{\"target_file\":\"blob.bin\"}"
                result.output `shouldSatisfy` Text.isInfixOf "Cannot read binary file"
                result.toolResultImages `shouldBe` []

readArgs :: Maybe Int -> Maybe Int -> ReadFileArgs
readArgs offset limit =
    ReadFileArgs
        { targetFile = "example.txt"
        , offset = offset
        , limit = limit
        , pages = Nothing
        , format = Nothing
        }

withFile :: String -> (FilePath -> IO a) -> IO a
withFile content = withBytes (BS.pack (map (fromIntegral . fromEnum) content))

withBytes :: BS.ByteString -> (FilePath -> IO a) -> IO a
withBytes bytes action = do
    root <- getTemporaryDirectory
    bracket (mkdtemp (root </> "agent-read-file-test-")) removeDirectoryRecursive \dir -> do
        let path = dir </> "input.txt"
        BS.writeFile path bytes
        action path

pngBytes :: BS.ByteString
pngBytes = BS.pack
    [ 0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a
    , 0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52
    , 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01
    , 0x08, 0x02, 0x00, 0x00, 0x00, 0x90, 0x77, 0x53, 0xde
    , 0x00, 0x00, 0x00, 0x0c, 0x49, 0x44, 0x41, 0x54
    , 0x08, 0xd7, 0x63, 0xf8, 0xcf, 0xc0, 0x00, 0x00
    , 0x03, 0x01, 0x01, 0x00, 0x18, 0xdd, 0x8d, 0xb0
    , 0x00, 0x00, 0x00, 0x00, 0x49, 0x45, 0x4e, 0x44
    , 0xae, 0x42, 0x60, 0x82
    ]

withTool :: (FilePath -> AppTool -> IO a) -> IO a
withTool action = do
    root <- getTemporaryDirectory
    bracket
        (mkdtemp (root </> "agent-read-file-tool-"))
        removeDirectoryRecursive
        \workspace -> do
            temp <- mkdtemp (workspace </> "session-tmp-")
            env <- defaultToolEnv (unsafeEncodeUtf workspace)
            setToolSessionTmp env (Just (unsafeEncodeUtf temp))
            action workspace (readFileTool env)

runReadTool :: AppTool -> Text.Text -> IO ToolCallResult
runReadTool tool arguments =
    dispatchToolCall testDispatchConfig [tool.appToolHandler]
        (functionToolCall "read-1" "read_file" arguments)

testDispatchConfig :: ToolDispatchConfig
testDispatchConfig = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "unknown:" <> name
    , toolDispatchFormatResult = either ("ERR " <>) id
    , toolDispatchFormatException = \name _ -> "EX " <> name
    , toolDispatchOnException = \_ _ -> pure ()
    , toolDispatchOnOutput = \_ _ -> pure ()
    , toolDispatchFinalizeOutput = \_ output -> pure output
    }
