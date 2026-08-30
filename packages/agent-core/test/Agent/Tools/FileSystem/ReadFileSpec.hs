module Agent.Tools.FileSystem.ReadFileSpec (spec) where

import Agent.Tools.FileSystem.ReadFile
    ( ReadFileArgs(..)
    , formatReadFile
    , streamReadFile
    )
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
                    `shouldSatisfy` either (const False) (Text.isInfixOf "\xfffd(")

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
                    `shouldSatisfy` isLeft

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
