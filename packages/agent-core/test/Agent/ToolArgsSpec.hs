module Agent.ToolArgsSpec (spec) where

import Agent.ToolArgs
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = describe "ToolArgs" do
    it "extracts required text with model-facing errors" do
        extractText (Aeson.object []) "query"
            `shouldBe` Left "Missing parameter: query"

    it "accepts a bare string for string-list parameters" do
        extractTextList (Aeson.object ["ids" .= ("abc" :: Text)]) "ids"
            `shouldBe` Right ["abc"]

    it "accepts string booleans" do
        extractMaybeBool (Aeson.object ["flag" .= ("true" :: Text)]) "flag"
            `shouldBe` Just True

    it "backs typed FromJSON records" do
        Aeson.eitherDecode "{\"query\":\"invoice\",\"limit\":5,\"dry_run\":\"false\"}"
            `shouldBe`
                Right (SearchArgs "invoice" 5 (Just False))

    it "keeps optList absent distinct from present empty" do
        parseEither (objectArgs $ \o -> optList @Text o "items" "items must be an array") (Aeson.object [])
            `shouldBe` Right Nothing
        parseEither (objectArgs $ \o -> optList @Text o "items" "items must be an array") (Aeson.object ["items" .= ([] :: [Text])])
            `shouldBe` Right (Just [])

data SearchArgs = SearchArgs
    { query :: Text
    , limit :: Int
    , dryRun :: Maybe Bool
    } deriving (Eq, Show)

instance Aeson.FromJSON SearchArgs where
    parseJSON = objectArgs $ \o -> SearchArgs
        <$> reqText o "query"
        <*> intOr o "limit" 10
        <*> optBool o "dry_run"
