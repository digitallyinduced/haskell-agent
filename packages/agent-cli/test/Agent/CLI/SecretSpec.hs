module Agent.CLI.SecretSpec (spec) where

import Agent.CLI.Secret
    ( sanitizeSecretPromptText
    , secretPromptMessage
    )
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "line-mode secret prompt" do
    it "identifies trusted secret entry without including a value" do
        let rendered =
                secretPromptMessage
                    "Enter the BotFather token"
                    (Just "Configure the Telegram gateway")
        rendered `shouldSatisfy`
            Text.isInfixOf "Secret requested by agent"
        rendered `shouldSatisfy`
            Text.isInfixOf "Purpose: Configure the Telegram gateway"
        rendered `shouldSatisfy`
            Text.isInfixOf "Enter the BotFather token"
        rendered `shouldSatisfy`
            Text.isSuffixOf "secret> "

    it "does not render an absent or blank purpose" do
        secretPromptMessage "API key" Nothing
            `shouldNotSatisfy` Text.isInfixOf "Purpose:"
        secretPromptMessage "API key" (Just "  ")
            `shouldNotSatisfy` Text.isInfixOf "Purpose:"

    it "neutralizes terminal control characters in model-supplied chrome" do
        let rendered =
                secretPromptMessage
                    "Token:\ESC]52;c;payload\BEL"
                    (Just "Deploy\ESC[2J")
        rendered `shouldNotSatisfy` Text.isInfixOf "\ESC"
        rendered `shouldNotSatisfy` Text.isInfixOf "\BEL"

    it "constrains model-controlled chrome to one safe line" do
        let rendered =
                sanitizeSecretPromptText
                    "first\nsecond\tthird\r\ESC[2J\BEL"
        rendered `shouldNotSatisfy` Text.any (`elem` ['\n', '\t', '\r', '\ESC', '\BEL'])
        rendered `shouldBe` "first second third  [2J "
