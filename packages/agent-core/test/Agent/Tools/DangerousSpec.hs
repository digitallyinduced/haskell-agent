module Agent.Tools.DangerousSpec (spec) where

import Agent.Tools.Dangerous
    ( commandLooksLikeRmRf
    , forbiddenRmRfReason
    , shellCommandBlocked
    )
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "commandLooksLikeRmRf" do
        it "detects clustered and split short flags" do
            commandLooksLikeRmRf "rm -rf /tmp/x" `shouldBe` True
            commandLooksLikeRmRf "rm -fr ./build" `shouldBe` True
            commandLooksLikeRmRf "rm -r -f out" `shouldBe` True
            commandLooksLikeRmRf "rm -f -r out" `shouldBe` True
            commandLooksLikeRmRf "rm -Rf /tmp/x" `shouldBe` True
            commandLooksLikeRmRf "rm -vfr cache" `shouldBe` True

        it "detects long options" do
            commandLooksLikeRmRf "rm --recursive --force out" `shouldBe` True
            commandLooksLikeRmRf "rm --force --recursive out" `shouldBe` True

        it "detects absolute rm paths and .exe" do
            commandLooksLikeRmRf "/bin/rm -rf /tmp/x" `shouldBe` True
            commandLooksLikeRmRf "rm.exe -rf C:\\Temp\\x" `shouldBe` True

        it "detects chained and piped forms" do
            commandLooksLikeRmRf "ls && rm -rf tmp" `shouldBe` True
            commandLooksLikeRmRf "true; rm -rf tmp" `shouldBe` True
            commandLooksLikeRmRf "echo hi | rm -rf tmp" `shouldBe` True
            commandLooksLikeRmRf "rm -rf a || true" `shouldBe` True

        it "detects common wrappers and env assignments" do
            commandLooksLikeRmRf "sudo rm -rf /var/tmp/x" `shouldBe` True
            commandLooksLikeRmRf "env rm -rf ./cache" `shouldBe` True
            commandLooksLikeRmRf "FOO=1 BAR=2 rm -rf ./cache" `shouldBe` True
            commandLooksLikeRmRf "nohup rm -rf ./cache" `shouldBe` True
            commandLooksLikeRmRf "command rm -rf ./cache" `shouldBe` True

        it "allows safe rm and unrelated commands" do
            commandLooksLikeRmRf "rm -r build" `shouldBe` False
            commandLooksLikeRmRf "rm -f file.txt" `shouldBe` False
            commandLooksLikeRmRf "rm file.txt" `shouldBe` False
            commandLooksLikeRmRf "ls -la" `shouldBe` False
            commandLooksLikeRmRf "git status" `shouldBe` False
            commandLooksLikeRmRf "echo 'rm -rf tmp'" `shouldBe` False
            commandLooksLikeRmRf "echo \"rm -rf tmp\"" `shouldBe` False
            commandLooksLikeRmRf "rmdir empty-dir" `shouldBe` False
            commandLooksLikeRmRf "rclone sync --force" `shouldBe` False
            -- Nested shells are out of scope for this best-effort gate.
            commandLooksLikeRmRf "bash -lc 'rm -rf tmp'" `shouldBe` False

    describe "shellCommandBlocked" do
        it "blocks shell tools with a clear reason" do
            shouldBlock "run_terminal_cmd" "{\"command\":\"rm -rf /tmp/x\"}"
            shouldBlock "run_terminal_command" "{\"command\":\"rm -rf /tmp/x\"}"
            shouldBlock "shell_command" "{\"command\":\"rm -fr ./build\"}"
            shouldBlock "run_terminal_cmd" "{\"command\":\"ls && rm -rf tmp\"}"

        it "includes a truncated command snippet in the reason" do
            let msg = forbiddenRmRfReason "rm -rf /tmp/very-important"
            msg `shouldSatisfy` Text.isInfixOf "Blocked dangerous shell command"
            msg `shouldSatisfy` Text.isInfixOf "rm -rf /tmp/very-important"

        it "parses JSON with whitespace around the command value" do
            shouldBlock "run_terminal_cmd" "{\"command\" : \"rm -rf x\"}"

        it "allows non-matching shell commands" do
            shouldAllow "run_terminal_cmd" "{\"command\":\"rm -r build\"}"
            shouldAllow "run_terminal_cmd" "{\"command\":\"rm file.txt\"}"
            shouldAllow "shell_command" "{\"command\":\"ls -la\"}"

        it "ignores non-shell tools even if arguments look dangerous" do
            shellCommandBlocked "search_replace" "{\"command\":\"rm -rf x\"}"
                `shouldBe` Nothing
            shellCommandBlocked "read_file" "{\"command\":\"rm -rf x\"}"
                `shouldBe` Nothing

        it "does nothing when the command field is missing" do
            shellCommandBlocked "run_terminal_cmd" "{\"description\":\"nope\"}"
                `shouldBe` Nothing

  where
    shouldBlock tool args =
        case shellCommandBlocked tool args of
            Just msg -> msg `shouldSatisfy` Text.isInfixOf "Blocked dangerous shell command"
            Nothing -> expectationFailure ("expected block for " <> Text.unpack args)
    shouldAllow tool args =
        shellCommandBlocked tool args `shouldBe` Nothing
