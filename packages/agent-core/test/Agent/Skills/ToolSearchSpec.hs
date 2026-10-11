module Agent.Skills.ToolSearchSpec (spec) where

import Agent.Loop (defaultLoopDispatch)
import Agent.Loop.InputItems (toolResultToItem)
import Agent.Responses.Types (ResponseItem(..), ToolSearchOutput(..))
import Agent.Skills.ToolSearch
import Agent.ToolDispatch
    ( ToolCallKind(..)
    , ToolCallMode(..)
    , ToolCallResult(..)
    , ToolCall(..)
    , ToolDispatchConfig(..)
    , toolCallResultStructured
    , toolSearchToolCall
    )
import Agent.Tools.Types (dispatchRegisteredToolCall, mkToolRegistry)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "Agent.Skills.ToolSearch" do
    it "advertises a client tool search with the catalog and an enum of skill names" do
        let definition = toolSkillSearchDefinition "# Skills" [invoices, meals]
        field "type" definition `shouldBe` Just (String "tool_search")
        field "execution" definition `shouldBe` Just (String "client")
        case field "description" definition of
            Just (String description) -> do
                description `shouldSatisfy` Text.isPrefixOf "# Skills\n"
                description `shouldSatisfy` Text.isInfixOf
                    "- invoices: Write and issue outgoing invoices. Tools: upsert_invoice, issue_invoice"
                description `shouldSatisfy` Text.isInfixOf
                    "- meals: Record business meal receipts. Tools: create_meal_receipt"
            other -> expectationFailure ("unexpected description: " <> show other)
        (field "parameters" definition >>= field "properties" >>= field "skills" >>= field "items" >>= field "enum")
            `shouldBe` Just (Aeson.toJSON ["invoices", "meals" :: String])

    it "renders a loaded skill as a namespace of deferred tools" do
        toolSkillNamespace meals `shouldBe` object
            [ "type" .= ("namespace" :: String)
            , "name" .= ("meals" :: String)
            , "description" .= ("Ask for the attendees first." :: String)
            , "tools" .=
                [ object
                    [ "type" .= ("function" :: String)
                    , "name" .= ("create_meal_receipt" :: String)
                    , "parameters" .= object []
                    , "defer_loading" .= True
                    ]
                ]
            ]

    it "resolves requested skills in request order, once, and skips unknown names" do
        fmap (map (.toolSkillName))
            (resolveToolSkillSearch [invoices, meals] "{\"skills\":[\"meals\",\"travel\",\"invoices\",\"meals\"]}")
            `shouldBe` Right ["meals", "invoices"]
        resolveToolSkillSearch [invoices] "{}" `shouldSatisfy` isLeft

    it "dispatches a tool search call and encodes the result as a tool search output" do
        registry <- either (fail . Text.unpack) pure (mkToolRegistry [toolSkillSearchTool [invoices, meals]])
        result <- dispatchRegisteredToolCall defaultLoopDispatch registry
            (toolSearchToolCall "call-1" "{\"skills\":[\"meals\"]}")
        result.callKind `shouldBe` ToolSearchCallKind
        toolSearchOutputTools result.output `shouldBe` [toolSkillNamespace meals]
        case toolResultToItem result of
            ToolSearchOutputItem output -> do
                output.callId `shouldBe` Just "call-1"
                output.execution `shouldBe` Just "client"
                map Aeson.toJSON output.tools `shouldBe` [toolSkillNamespace meals]
            other -> expectationFailure ("unexpected item: " <> show other)

    it "preserves large tool definitions when text formatting and finalization replace the output" do
        let largeSkill = meals
                { toolSkillInstructions = Text.replicate 100000 "Instruction. "
                , toolSkillTools =
                    [object
                        [ "type" .= ("function" :: Text.Text)
                        , "name" .= ("create_meal_receipt" :: Text.Text)
                        , "description" .= Text.replicate 100000 "Schema documentation. "
                        , "parameters" .= object []
                        ]]
                }
            dispatch = defaultLoopDispatch
                { toolDispatchFormatResult = either id ("Formatted output: " <>)
                , toolDispatchFinalizeOutput = \_ text -> do
                    Text.length text `shouldSatisfy` (> 1000000)
                    pure "Full output retained as an artifact."
                }
        registry <- either (fail . Text.unpack) pure (mkToolRegistry [toolSkillSearchTool [largeSkill]])
        result <- dispatchRegisteredToolCall dispatch registry
            (toolSearchToolCall "large-search" "{\"skills\":[\"meals\"]}")
        result.output `shouldBe` "Full output retained as an artifact."
        toolCallResultStructured result `shouldBe` Just (Aeson.toJSON [toolSkillNamespace largeSkill])
        case toolResultToItem result of
            ToolSearchOutputItem output ->
                map Aeson.toJSON output.tools `shouldBe` [toolSkillNamespace largeSkill]
            other -> expectationFailure ("unexpected item: " <> show other)

    it "loads nothing when a tool search fails" do
        let failed = ToolCallResult
                { callId = "call-2"
                , output = "tool_search failed"
                , callKind = ToolSearchCallKind
                , toolResultMode = BlockingToolCall
                , toolResultImages = []
                , toolResultOutcome = Nothing
                }
        case toolResultToItem failed of
            ToolSearchOutputItem output -> output.tools `shouldBe` []
            other -> expectationFailure ("unexpected item: " <> show other)

    it "rejects catalogs the provider or the model could not use" do
        validateToolSkills [invoices, meals] `shouldBe` Right [invoices, meals]
        validateToolSkills [invoices, invoices] `shouldSatisfy` isLeft
        validateToolSkills [meals { toolSkillName = "meal receipts" }] `shouldSatisfy` isLeft
        validateToolSkills [meals { toolSkillTools = [] }] `shouldSatisfy` isLeft
        validateToolSkills [meals { toolSkillTools = [object []] }] `shouldSatisfy` isLeft

    it "builds tool search calls with the reserved name" do
        let call = toolSearchToolCall "call-3" "{}"
        call.name `shouldBe` toolSearchToolName
        call.callKind `shouldBe` ToolSearchCallKind
  where
    field key = \case
        Object fields -> KeyMap.lookup key fields
        _ -> Nothing
    isLeft = either (const True) (const False)

invoices :: ToolSkill
invoices = ToolSkill
    { toolSkillName = "invoices"
    , toolSkillDescription = "Write and issue\n  outgoing invoices."
    , toolSkillInstructions = "Confirm the recipient."
    , toolSkillTools = [function "upsert_invoice", function "issue_invoice"]
    }

meals :: ToolSkill
meals = ToolSkill
    { toolSkillName = "meals"
    , toolSkillDescription = "Record business meal receipts."
    , toolSkillInstructions = "Ask for the attendees first."
    , toolSkillTools = [function "create_meal_receipt"]
    }

function :: String -> Value
function name = object
    [ "type" .= ("function" :: String)
    , "name" .= name
    , "parameters" .= object []
    ]
