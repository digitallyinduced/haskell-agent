module Agent.Tools.DangerousSpec (spec) where

import Agent.Tools.Dangerous (shellCommandBlocked)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "shellCommandBlocked" do
    it "blocks rm -rf / rm -fr / separate flags" do
        shouldBlock "run_terminal_cmd" "{\"command\":\"rm -rf /tmp/x\"}"
        shouldBlock "shell_command" "{\"command\":\"rm -fr ./build\"}"
        shouldBlock "run_terminal_cmd" "{\"command\":\"rm -r -f out\"}"
        shouldBlock "run_terminal_cmd" "{\"command\":\"rm --recursive --force out\"}"

    it "blocks chained and wrapped forms" do
        shouldBlock "run_terminal_cmd" "{\"command\":\"ls && rm -rf tmp\"}"
        shouldBlock "run_terminal_cmd" "{\"command\":\"sudo rm -rf /var/tmp/x\"}"
        shouldBlock "run_terminal_cmd" "{\"command\":\"FOO=1 rm -rf ./cache\"}"

    it "allows non-force recursive rm and unrelated commands" do
        shouldAllow "run_terminal_cmd" "{\"command\":\"rm -r build\"}"
        shouldAllow "run_terminal_cmd" "{\"command\":\"rm file.txt\"}"
        shouldAllow "run_terminal_cmd" "{\"command\":\"ls -la\"}"
        shouldAllow "run_terminal_cmd" "{\"command\":\"git status\"}"

    it "ignores non-shell tools" do
        shellCommandBlocked "search_replace" "{\"command\":\"rm -rf x\"}"
            `shouldBe` Nothing

  where
    shouldBlock tool args =
        case shellCommandBlocked tool args of
            Just msg -> msg `shouldSatisfy` Text.isInfixOf "Blocked dangerous shell command"
            Nothing -> expectationFailure ("expected block for " <> Text.unpack args)
    shouldAllow tool args =
        shellCommandBlocked tool args `shouldBe` Nothing
