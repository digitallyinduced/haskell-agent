{-# LANGUAGE OverloadedStrings #-}

module Claude.Agent.SDK.CapabilitiesSpec (spec) where

import Claude.Agent.SDK.Capabilities
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "Claude Code capabilities" do
    it "parses the installed version format" do
        parseClaudeVersion "2.1.209 (Claude Code)" `shouldBe` Just "2.1.209"
        parseClaudeVersion "Claude Code version v2.1.209" `shouldBe` Just "2.1.209"

    it "discovers stream and security flags from help" do
        let capabilities = parseClaudeHelp fixtureHelp
        capabilities.supportsStreaming `shouldBe` True
        capabilities.supportsSafeMode `shouldBe` True
        capabilitySupportsPermissionMode capabilities "manual" `shouldBe` True
        capabilitySupportsFlag capabilities "--strict-mcp-config" `shouldBe` True

    it "fails closed when required streaming flags are absent" do
        let capabilities = parseClaudeHelp "Usage: claude\n  --output-format text"
        validateSubprocessArguments capabilities
            ["-p", "--input-format", "stream-json", "--output-format", "stream-json", "--verbose"]
            `shouldSatisfy` isLeft

fixtureHelp :: Text
fixtureHelp = Text.unlines
    [ "Usage: claude [options]"
    , "  -p, --print"
    , "  --input-format <format> (choices: text, stream-json)"
    , "  --output-format <format> (choices: text, json, stream-json)"
    , "  --verbose"
    , "  --safe-mode"
    , "  --strict-mcp-config"
    , "  --permission-mode <mode> (choices: acceptEdits, auto, bypassPermissions, manual, dontAsk, plan)"
    ]

isLeft :: Either a b -> Bool
isLeft = \case
    Left _ -> True
    Right _ -> False
