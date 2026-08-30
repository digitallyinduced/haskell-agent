module Agent.CLI.ExternalProgramSpec (spec) where

import Agent.CLI.ExternalProgram
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text.IO as Text
import System.Directory (doesFileExist)
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.ExternalProgram" do
    describe "parseProgramWords" do
        it "handles quotes, escapes, and empty arguments" do
            parseProgramWords
                "editor --title=\"two words\" 'file name' one\\ two ''"
                `shouldBe`
                    Right
                        [ "editor"
                        , "--title=two words"
                        , "file name"
                        , "one two"
                        , ""
                        ]

        it "rejects incomplete quoting and escapes" do
            parseProgramWords "editor 'unfinished"
                `shouldBe`
                    Left "program specification contains an unterminated quote"
            parseProgramWords "editor trailing\\"
                `shouldBe`
                    Left "program specification ends with an incomplete escape"

    describe "parseExternalProgram" do
        it "splits an executable from its arguments" do
            parseExternalProgram "less -R"
                `shouldBe`
                    Right
                        ExternalProgram
                            { externalProgramExecutable = "less"
                            , externalProgramArguments = ["-R"]
                            }

        it "rejects an empty specification" do
            parseExternalProgram "   "
                `shouldBe` Left "program specification must not be empty"

    describe "normalizeEditedText" do
        it "removes at most one editor-added final newline" do
            normalizeEditedText "draft\n" `shouldBe` "draft"
            normalizeEditedText "draft\r\n" `shouldBe` "draft"
            normalizeEditedText "draft\n\n" `shouldBe` "draft\n"
            normalizeEditedText "draft" `shouldBe` "draft"

    describe "temporary program execution" do
        it "appends the temporary path as a direct process argument" do
            withTemporaryTextFile "agent-source-" "edited" \source ->
                withTemporaryTextFile "agent-target-" "before" \target -> do
                    runExternalProgramOnFile
                        ExternalProgram
                            { externalProgramExecutable = "cp"
                            , externalProgramArguments = [source]
                            }
                        target
                        `shouldReturn` Right ()
                    Text.readFile target `shouldReturn` "edited"

        it "removes the temporary file after the action returns" do
            pathRef <- newIORef ""
            withTemporaryTextFile "agent-cleanup-" "draft" \path -> do
                writeIORef pathRef path
                doesFileExist path `shouldReturn` True
            path <- readIORef pathRef
            doesFileExist path `shouldReturn` False
