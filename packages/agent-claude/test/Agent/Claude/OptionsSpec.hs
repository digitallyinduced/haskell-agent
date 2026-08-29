module Agent.Claude.OptionsSpec (spec) where

import Agent.Claude.Options
import Agent.Claude.Transport
import Claude.Agent.SDK.Types (ClaudeAgentOptions(..), PermissionMode(..))
import Control.Exception.Safe (finally)
import Data.List (isInfixOf)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

spec :: Spec
spec =
    describe "toClaudeAgentOptions" do
        it "preserves the Claude Code manual permission spelling" do
            sdk <- toClaudeAgentOptions
                ClaudeCodeNoTools
                (defaultClaudeCodeOptions "/bin/claude" "/tmp")
                    { permission = ClaudeCodeManual }
            sdk.permissionMode `shouldBe` Just PermissionManual

        it "injects only the explicit gateway provider variables" $
            withEnvironmentVariables
                [ ("ANTHROPIC_API_KEY", Just "ambient-api-key")
                , ("ANTHROPIC_AUTH_TOKEN", Just "ambient-auth-token")
                , ("ANTHROPIC_BASE_URL", Just "https://ambient.invalid")
                , ("CLAUDE_CODE_USE_BEDROCK", Just "1")
                , ("HASKELL_AGENT_GATEWAY_TOKEN", Just "ambient-gateway-token")
                ]
                do
                    let options =
                            (defaultClaudeCodeOptions "/bin/claude" "/tmp")
                                { transport =
                                    ClaudeCodeGateway
                                        { gatewayBaseUrl = "https://gateway.example/"
                                        , gatewayToken = "gateway-token"
                                        }
                                }
                    sdk <- toClaudeAgentOptions ClaudeCodeDefaultTools options
                    let environment = maybe [] id sdk.environment
                    lookup "ANTHROPIC_BASE_URL" environment
                        `shouldBe` Just "https://gateway.example/anthropic"
                    lookup "ANTHROPIC_AUTH_TOKEN" environment
                        `shouldBe` Just "gateway-token"
                    lookup "ANTHROPIC_API_KEY" environment `shouldBe` Nothing
                    lookup "CLAUDE_CODE_USE_BEDROCK" environment `shouldBe` Nothing
                    lookup "HASKELL_AGENT_GATEWAY_TOKEN" environment
                        `shouldBe` Nothing
                    show sdk `shouldNotSatisfy` isInfixOf "gateway-token"
                    show sdk `shouldSatisfy` isInfixOf "environment = <redacted>"

        it "does not inject provider credentials for a local subscription" do
            sdk <-
                toClaudeAgentOptions
                    ClaudeCodeDefaultTools
                    (defaultClaudeCodeOptions "/bin/claude" "/tmp")
            let environment = maybe [] id sdk.environment
            lookup "ANTHROPIC_BASE_URL" environment `shouldBe` Nothing
            lookup "ANTHROPIC_AUTH_TOKEN" environment `shouldBe` Nothing

withEnvironmentVariables
    :: [(String, Maybe String)]
    -> IO a
    -> IO a
withEnvironmentVariables variables action = do
    previous <- mapM
        (\(name, _) -> do
            value <- lookupEnv name
            pure (name, value))
        variables
    mapM_ (uncurry install) variables
    action `finally` mapM_ (uncurry install) previous
  where
    install name = \case
        Nothing -> unsetEnv name
        Just value -> setEnv name value
