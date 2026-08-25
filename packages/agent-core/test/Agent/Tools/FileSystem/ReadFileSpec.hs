module Agent.Tools.FileSystem.ReadFileSpec (spec) where

import Agent.Tools.FileSystem.ReadFile
    ( ReadFileArgs(..)
    , formatReadFile
    )
import Data.Either (isLeft)
import qualified Data.Text as Text
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

readArgs :: Maybe Int -> Maybe Int -> ReadFileArgs
readArgs offset limit =
    ReadFileArgs
        { targetFile = "example.txt"
        , offset = offset
        , limit = limit
        , pages = Nothing
        , format = Nothing
        }
