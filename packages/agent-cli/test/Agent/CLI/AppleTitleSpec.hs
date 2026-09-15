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
    describe "parseAppleAvailableJson" do
        it "accepts a JSON availability object" do
            parseAppleAvailableJson "{\"available\":true}\n"
                `shouldBe` True

        it "rejects unavailable or missing availability" do
            parseAppleAvailableJson "{\"available\":false,\"reason\":\"modelNotReady\"}\n"
                `shouldBe` False
            parseAppleAvailableJson "model: apple-foundationmodel"
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

    describe "generateAppleFoundationTitleTimed" do
        it "reads a title from a stub helper on stdin" do
            withHelper
                "#!/bin/sh\ncat >/dev/null\nprintf '%s\\n' '{\"title\":\"Stub title\"}'\n"
                \executable ->
                    generateAppleFoundationTitleTimed
                        2_000_000
                        executable
                        "User:\nFix auth"
                        `shouldReturn` Right "Stub title"

        it "times out a stuck helper" do
            withHelper
                "#!/bin/sh\nexec sleep 5\n"
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
