{-# LANGUAGE OverloadedStrings #-}

module Claude.Agent.SDK.CapabilitiesSpec (spec) where

import Claude.Agent.SDK.Capabilities
import Claude.Agent.SDK.TestSupport
    ( withFakeClaude )
import Control.Exception.Safe (bracket, finally, onException)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Posix.IO
    ( closeFd
    , createPipe
    , dup
    , dupTo
    , stdInput
    )
import System.Timeout (timeout)
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

    it "does not inherit caller stdin for capability subprocesses" $
        withFakeClaude stdinReadingProbeScript \directory executable -> do
            result <-
                withBlockedStandardInput $
                    timeout 5_000_000 $
                        probeClaudeCapabilitiesIn executable directory
            result `shouldSatisfy` \case
                Just (Right capabilities) ->
                    capabilities.supportsStreaming
                _ -> False

    it "does not cache a failed capability probe" $
        withFakeClaude failFirstHelpProbeScript \directory executable -> do
            first <- probeClaudeCapabilitiesIn executable directory
            first `shouldSatisfy` isLeft
            second <- probeClaudeCapabilitiesIn executable directory
            second `shouldSatisfy` \case
                Right capabilities ->
                    capabilities.supportsStreaming
                Left _ -> False

    it "returns a cached successful capability probe without re-running help" $
        withFakeClaude cacheHitProbeScript \directory executable -> do
            first <- probeClaudeCapabilitiesIn executable directory
            first `shouldSatisfy` \case
                Right capabilities -> capabilities.supportsStreaming
                Left _ -> False
            second <- probeClaudeCapabilitiesIn executable directory
            second `shouldBe` first

    it "fails the probe when a descendant keeps stdout open" $
        withFakeClaude heldOpenHelpOutputScript \directory executable -> do
            result <-
                timeout 3_000_000 $
                    probeClaudeCapabilitiesIn executable directory
            result `shouldSatisfy` \case
                Just (Left message) ->
                    "stdout read timed out" `Text.isInfixOf` message
                _ -> False

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

stdinReadingProbeScript :: String
stdinReadingProbeScript =
    unlines
        [ "#!/bin/sh"
        , "IFS= read -r _input"
        , "case \"$1\" in"
        , "  --version)"
        , "    printf '%s\\n' '2.1.251 (Claude Code)'"
        , "    ;;"
        , "  --help)"
        , "    printf '%s\\n' \\"
        , "      'Usage: claude [options]' \\"
        , "      '  -p, --print' \\"
        , "      '  --input-format <format> (choices: text, stream-json)' \\"
        , "      '  --output-format <format> (choices: text, json, stream-json)' \\"
        , "      '  --verbose' \\"
        , "      '  --safe-mode' \\"
        , "      '  --strict-mcp-config' \\"
        , "      '  --permission-mode <mode> (choices: acceptEdits, auto, bypassPermissions, manual, dontAsk, plan)'"
        , "    ;;"
        , "esac"
        ]

heldOpenHelpOutputScript :: String
heldOpenHelpOutputScript =
    unlines
        [ "#!/bin/sh"
        , "case \"$1\" in"
        , "  --version)"
        , "    printf '%s\\n' '2.1.253 (Claude Code)'"
        , "    ;;"
        , "  --help)"
        , "    sleep 2 2>/dev/null &"
        , "    printf '%s\\n' \\"
        , "      'Usage: claude [options]' \\"
        , "      '  -p, --print' \\"
        , "      '  --input-format <format> (choices: text, stream-json)' \\"
        , "      '  --output-format <format> (choices: text, json, stream-json)' \\"
        , "      '  --verbose'"
        , "    ;;"
        , "esac"
        ]

failFirstHelpProbeScript :: String
failFirstHelpProbeScript =
    unlines
        [ "#!/bin/sh"
        , "case \"$1\" in"
        , "  --version)"
        , "    printf '%s\\n' '2.1.252 (Claude Code)'"
        , "    ;;"
        , "  --help)"
        , "    if [ ! -e \"$0.help-probed\" ]; then"
        , "      printf 'failed' > \"$0.help-probed\""
        , "      exit 1"
        , "    fi"
        , "    printf '%s\\n' \\"
        , "      'Usage: claude [options]' \\"
        , "      '  -p, --print' \\"
        , "      '  --input-format <format> (choices: text, stream-json)' \\"
        , "      '  --output-format <format> (choices: text, json, stream-json)' \\"
        , "      '  --verbose' \\"
        , "      '  --safe-mode' \\"
        , "      '  --strict-mcp-config' \\"
        , "      '  --permission-mode <mode> (choices: acceptEdits, auto, bypassPermissions, manual, dontAsk, plan)'"
        , "    ;;"
        , "esac"
        ]

cacheHitProbeScript :: String
cacheHitProbeScript =
    unlines
        [ "#!/bin/sh"
        , "case \"$1\" in"
        , "  --version)"
        , "    printf '%s\\n' '2.1.254 (Claude Code)'"
        , "    ;;"
        , "  --help)"
        , "    if [ -e \"$0.help-probed\" ]; then"
        , "      exit 1"
        , "    fi"
        , "    printf 'succeeded' > \"$0.help-probed\""
        , "    printf '%s\\n' \\"
        , "      'Usage: claude [options]' \\"
        , "      '  -p, --print' \\"
        , "      '  --input-format <format> (choices: text, stream-json)' \\"
        , "      '  --output-format <format> (choices: text, json, stream-json)' \\"
        , "      '  --verbose' \\"
        , "      '  --safe-mode' \\"
        , "      '  --strict-mcp-config' \\"
        , "      '  --permission-mode <mode> (choices: acceptEdits, auto, bypassPermissions, manual, dontAsk, plan)'"
        , "    ;;"
        , "esac"
        ]

-- | Replace stdin with a pipe whose writer stays open. A child that inherits
-- stdin blocks in @read@, while a child with a private closed stdin receives
-- EOF immediately.
withBlockedStandardInput :: IO a -> IO a
withBlockedStandardInput action =
    bracket acquire release (const action)
  where
    acquire = do
        original <- dup stdInput
        (reader, writer) <- createPipe
        let cleanup = do
                closeFd original
                closeFd reader
                closeFd writer
        (do
            _ <- dupTo reader stdInput
            closeFd reader
            pure (original, writer)
            ) `onException` cleanup
    release (original, writer) =
        (void (dupTo original stdInput) `finally` closeFd original)
            `finally` closeFd writer

isLeft :: Either a b -> Bool
isLeft = \case
    Left _ -> True
    Right _ -> False
