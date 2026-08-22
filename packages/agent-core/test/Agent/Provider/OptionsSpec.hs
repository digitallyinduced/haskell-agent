module Agent.Provider.OptionsSpec (spec) where

import Agent.Provider.Options
import Control.Exception.Safe (bracket)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

spec :: Spec
spec = describe "Agent.Provider.Options" do
    describe "parseModelOverrides" do
        it "parses valid entries, trims whitespace, and skips malformed ones" do
            parseModelOverrides "local = remote,broken,=missing,also=,x=y=z"
                `shouldBe`
                    [ ("local", "remote")
                    , ("x", "y=z")
                    ]

    describe "environment lookup" do
        it "distinguishes empty, invalid, and valid values" $
            withEnvironment "AGENT_PROVIDER_OPTIONS_SPEC" do
                unsetEnv "AGENT_PROVIDER_OPTIONS_SPEC"
                lookupNonEmptyEnv "AGENT_PROVIDER_OPTIONS_SPEC"
                    `shouldReturn` Nothing

                setEnv "AGENT_PROVIDER_OPTIONS_SPEC" ""
                lookupNonEmptyEnv "AGENT_PROVIDER_OPTIONS_SPEC"
                    `shouldReturn` Nothing

                setEnv "AGENT_PROVIDER_OPTIONS_SPEC" "  "
                lookupNonEmptyEnv "AGENT_PROVIDER_OPTIONS_SPEC"
                    `shouldReturn` Just "  "

                setEnv "AGENT_PROVIDER_OPTIONS_SPEC" "42s"
                lookupIntEnv "AGENT_PROVIDER_OPTIONS_SPEC"
                    `shouldReturn` Nothing

                setEnv "AGENT_PROVIDER_OPTIONS_SPEC" "42"
                lookupIntEnv "AGENT_PROVIDER_OPTIONS_SPEC"
                    `shouldReturn` Just 42

withEnvironment :: String -> IO a -> IO a
withEnvironment name =
    bracket (lookupEnv name) restore . const
  where
    restore Nothing = unsetEnv name
    restore (Just value) = setEnv name value
