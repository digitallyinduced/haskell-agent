module Agent.CLI.EnvironmentSpec (spec) where

import Agent.CLI.Environment (lookupNonEmpty)
import Control.Exception (bracket_)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

spec :: Spec
spec = describe "lookupNonEmpty" do
    it "treats empty values as absent" do
        withEnvironment "AGENT_CLI_ENVIRONMENT_SPEC" (Just "") do
            lookupNonEmpty "AGENT_CLI_ENVIRONMENT_SPEC"
                `shouldReturn` Nothing

    it "decodes UTF-8 environment values" do
        withEnvironment "AGENT_CLI_ENVIRONMENT_SPEC" (Just "café 🚀") do
            lookupNonEmpty "AGENT_CLI_ENVIRONMENT_SPEC"
                `shouldReturn` Just "café 🚀"

withEnvironment :: String -> Maybe String -> IO a -> IO a
withEnvironment name value action = do
    previous <- lookupEnv name
    bracket_ (setValue value) (setValue previous) action
  where
    setValue = \case
        Nothing -> unsetEnv name
        Just content -> setEnv name content
