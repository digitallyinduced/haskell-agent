module Agent.CLI.AppleTitleSpec (spec) where

import Agent.CLI.AppleTitle
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.AppleTitle" do
    describe "parseAppleModelInfoAvailable" do
        it "accepts apfel model-info output that reports availability" do
            parseAppleModelInfoAvailable
                "apfel v1.9.1 — model info\n\
                \├ model:      apple-foundationmodel\n\
                \├ on-device:  true (always)\n\
                \├ available:  yes\n"
                `shouldBe` True

        it "rejects unavailable or missing availability" do
            parseAppleModelInfoAvailable
                "├ available:  no\n"
                `shouldBe` False
            parseAppleModelInfoAvailable "model: apple-foundationmodel"
                `shouldBe` False
            parseAppleModelInfoAvailable "├ available:  yesterday\n"
                `shouldBe` False

    describe "parseAppleTitleJson" do
        it "reads a JSON title object" do
            parseAppleTitleJson "{\"title\":\"Fix auth races\"}"
                `shouldBe` Just "Fix auth races"

        it "unwraps a fenced JSON payload" do
            parseAppleTitleJson
                "```json\n{\"title\":\"Compact session history\"}\n```"
                `shouldBe` Just "Compact session history"

        it "falls back to the first plain line" do
            parseAppleTitleJson "Session title: Postgres pool exhaustion\nmore"
                `shouldBe` Just "Postgres pool exhaustion"

        it "rejects empty output" do
            parseAppleTitleJson "   \n"
                `shouldBe` Nothing

    describe "appleTitleUserPrompt" do
        it "includes the conversation excerpt" do
            appleTitleUserPrompt "User:\nFix auth"
                `shouldSatisfy` Text.isInfixOf "User:\nFix auth"

    describe "generateAppleFoundationTitleTimed" do
        it "reads a title from a stub helper" do
            withHelper
                "#!/bin/sh\nprintf '%s\\n' '{\"title\":\"Stub title\"}'\n"
                \executable ->
                    generateAppleFoundationTitleTimed
                        2_000_000
                        executable
                        "User:\nFix auth"
                        `shouldReturn` Right "Stub title"

        it "times out a stuck helper" do
            withHelper
                "#!/bin/sh\nsleep 5\nprintf '%s\\n' '{\"title\":\"Late title\"}'\n"
                \executable ->
                    generateAppleFoundationTitleTimed
                        200_000
                        executable
                        "conversation"
                        `shouldReturn`
                            Left "Apple Intelligence title request timed out"

withHelper :: String -> (FilePath -> IO ()) -> IO ()
withHelper script action =
    withSystemTempDirectory "apple-title-spec-" \directory -> do
        let executable = directory </> "helper"
        Text.writeFile executable (Text.pack script)
        setFileMode executable 0o755
        action executable
