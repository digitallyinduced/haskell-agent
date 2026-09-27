module Agent.Tools.FileSystem.ReadFileSpec (spec) where

import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolResultFile(..)
    , ToolResultImage(..)
    , toolCallResultFiles
    , ToolDispatchConfig(..)
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
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive, createFileLink)
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
                toolCallResultFiles result `shouldBe` []

        it "loads images into context using the same validation as view_image" do
            withTool \workspace tool -> do
                BS.writeFile (workspace </> "shot.png") pngBytes
                result <- runReadTool tool "{\"target_file\":\"shot.png\"}"
                result.output `shouldBe` "Viewed image file: shot.png"
                map (.imageDetail) result.toolResultImages `shouldBe` [Just "high"]
                map (.imageUrl) result.toolResultImages `shouldSatisfy`
                    all (Text.isPrefixOf "data:image/png;base64,")
                toolCallResultFiles result `shouldBe` []

        it "loads extensionless PDF bytes as a native attachment, not text" do
            withTool \workspace tool -> do
                BS.writeFile (workspace </> "document-identifier") pdfBytes
                result <- runReadTool tool "{\"target_file\":\"document-identifier\"}"
                toolCallResultFiles result `shouldBe`
                    [ToolResultFile "document-identifier.pdf" "application/pdf" pdfBytes]
                result.output `shouldBe` "Loaded PDF file into model context: document-identifier"
                result.toolResultImages `shouldBe` []

        it "bounds the attachment filename when adding a PDF extension" do
            withTool \workspace tool -> do
                let name = Text.replicate 255 "a"
                BS.writeFile (workspace </> Text.unpack name) pdfBytes
                result <- runReadTool tool ("{\"target_file\":\"" <> name <> "\"}")
                toolCallResultFiles result `shouldBe`
                    [ToolResultFile (Text.replicate 251 "a" <> ".pdf") "application/pdf" pdfBytes]

        it "detects an image stored under an extensionless document identifier" do
            withTool \workspace tool -> do
                BS.writeFile (workspace </> "image-identifier") pngBytes
                result <- runReadTool tool "{\"target_file\":\"image-identifier\"}"
                length result.toolResultImages `shouldBe` 1
                toolCallResultFiles result `shouldBe` []

        it "retains a PDF basename rather than leaking its absolute path" do
            withTool \workspace tool -> do
                let path = workspace </> "Report.PDF"
                BS.writeFile path pdfBytes
                result <- runReadTool tool
                    ("{\"target_file\":\"" <> Text.pack path <> "\"}")
                map (.fileName) (toolCallResultFiles result) `shouldBe` ["Report.PDF"]

        it "does not mistake a misleading PDF extension for valid content" do
            withTool \workspace tool -> do
                BS.writeFile (workspace </> "report.pdf") "not a PDF"
                result <- runReadTool tool "{\"target_file\":\"report.pdf\"}"
                result.output `shouldSatisfy` Text.isInfixOf "does not contain a PDF header"
                toolCallResultFiles result `shouldBe` []

        it "rejects oversized native attachments before returning content" do
            withTool \workspace tool -> do
                BS.writeFile (workspace </> "large") ("%PDF-" <> BS.replicate (20 * 1024 * 1024) 32)
                result <- runReadTool tool "{\"target_file\":\"large\"}"
                result.output `shouldSatisfy` Text.isInfixOf "maximum 20 MiB"
                toolCallResultFiles result `shouldBe` []

        it "accepts native attachments at the exact size limit without truncating" do
            withTool \workspace tool -> do
                let bytes = "%PDF-" <> BS.replicate (20 * 1024 * 1024 - 5) 32
                BS.writeFile (workspace </> "limit.pdf") bytes
                result <- runReadTool tool "{\"target_file\":\"limit.pdf\"}"
                map (.fileData) (toolCallResultFiles result) `shouldBe` [bytes]

        it "does not silently ignore text ranges for binary attachments" do
            withTool \workspace tool -> do
                BS.writeFile (workspace </> "report") pdfBytes
                result <- runReadTool tool "{\"target_file\":\"report\",\"limit\":2}"
                result.output `shouldSatisfy` Text.isInfixOf "loaded whole"
                toolCallResultFiles result `shouldBe` []

        it "rejects corrupt images rather than attaching the signature alone" do
            withTool \workspace tool -> do
                BS.writeFile (workspace </> "image") (BS.take 12 pngBytes)
                result <- runReadTool tool "{\"target_file\":\"image\"}"
                result.output `shouldSatisfy` Text.isInfixOf "invalid or unsupported image"
                result.toolResultImages `shouldBe` []

        it "rejects a PDF reached through a symlink outside allowed roots" do
            withBytes pdfBytes \outside ->
                withTool \workspace tool -> do
                    createFileLink outside (workspace </> "external.pdf")
                    result <- runReadTool tool "{\"target_file\":\"external.pdf\"}"
                    result.output `shouldSatisfy` Text.isPrefixOf "ERR "
                    toolCallResultFiles result `shouldBe` []

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

-- The provider owns full PDF decoding; read_file recognizes the native header
-- and preserves the exact bytes instead of parsing or rendering the document.
pdfBytes :: BS.ByteString
pdfBytes = "%PDF-1.7\n1 0 obj\n<< /Type /Catalog >>\nendobj\n%%EOF\n"

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
