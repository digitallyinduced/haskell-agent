module Agent.XAI.ContextTrimSpec (spec) where

import Agent.XAI.ContextTrim
import Agent.OpenAI.Responses.Types
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Test.Hspec

spec :: Spec
spec = describe "trimResponsesItemsToTokenBudget" do
    it "keeps the newest incident when older history exceeds the budget" do
        trimResponsesItemsToTokenBudget 1_000
            [ (time "2026-01-01T10:00:00Z", True, oversizedItem)
            , (time "2026-01-01T10:20:00Z", True, callItem)
            , (time "2026-01-01T10:20:01Z", False, outputItem)
            ]
            `shouldBe` [callItem, outputItem]

    it "drops unpaired tool call outputs" do
        trimResponsesItemsToTokenBudget 1_000
            [ (time "2026-01-01T10:00:00Z", True, oversizedItem)
            , (time "2026-01-01T10:20:00Z", True, outputItem)
            ]
            `shouldBe` []

oversizedItem :: ResponseItem
oversizedItem = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts
        [ InputTextPart
            { text = Text.replicate 5_000 "x"
            , promptCacheBreakpoint = Nothing
            , extraFields = mempty
            }
        ]
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , extraFields = mempty
    }

callItem :: ResponseItem
callItem = FunctionCallItem FunctionCall
    { itemId = Just "fc_1"
    , callId = "call_1"
    , name = "echo_text"
    , arguments = "{\"text\":\"ok\"}"
    , status = Nothing
    , extraFields = mempty
    }

outputItem :: ResponseItem
outputItem = FunctionCallOutputItem FunctionCallOutput
    { itemId = Nothing
    , callId = "call_1"
    , output = Aeson.String "ok"
    , status = Nothing
    , extraFields = mempty
    }

time :: String -> UTCTime
time value =
    case iso8601ParseM value of
        Just t -> t
        Nothing -> error ("invalid test time: " <> value)
