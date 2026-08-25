module Agent.CLI.RequestSpec (spec) where

import Agent.CLI.Request
    ( requestParams
    , setRequestInstructionsAndTools
    , setRequestModel
    )
import Agent.CLI.Tools (webSearchTool)
import Agent.Provider (Provider(..))
import Agent.Responses.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (toList)
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = describe "requestParams" do
    it "constructs a non-storing request with model, instructions, tools, and effort" do
        let params =
                requestParams
                    OpenAIProvider
                    "test-model"
                    "test instructions"
                    [webSearchTool]
                    "high"
        params.model `shouldBe` Just "test-model"
        params.instructions `shouldBe` Just "test instructions"
        params.tools `shouldBe` Just [webSearchTool]
        params.include `shouldBe`
            Just [ResponseInclude "reasoning.encrypted_content"]
        params.stream `shouldBe` Just True
        params.toolChoice `shouldBe` Just (ToolChoiceMode ToolChoiceAuto)
        params.parallelToolCalls `shouldBe` Just True
        params.store `shouldBe` Just False
        case params.reasoning of
            Nothing -> expectationFailure "expected reasoning configuration"
            Just reasoning -> do
                reasoning.effort `shouldBe` Just "high"
                reasoning.context `shouldBe` Nothing
                reasoning.generateSummary `shouldBe` Nothing
                reasoning.reasoningMode `shouldBe` Nothing
                reasoning.summary `shouldBe` Just "auto"
                reasoning.extraFields `shouldBe` KeyMap.empty

    it "leaves reasoning summaries provider-controlled outside OpenAI" do
        let params =
                requestParams
                    OpenRouterProvider
                    "test-model"
                    "test instructions"
                    []
                    "medium"
        (params.reasoning >>= (.summary)) `shouldBe` Nothing

    it "uses the Responses Lite input shape for the local frontier models" do
        let params =
                requestParams
                    OpenAIProvider
                    "gpt-5.6-sol"
                    "base instructions"
                    [webSearchTool]
                    "high"
        params.instructions `shouldBe` Nothing
        params.tools `shouldBe` Nothing
        params.include `shouldBe`
            Just [ResponseInclude "reasoning.encrypted_content"]
        params.stream `shouldBe` Just True
        params.store `shouldBe` Just False
        params.toolChoice `shouldBe` Just (ToolChoiceMode ToolChoiceAuto)
        params.parallelToolCalls `shouldBe` Just False
        params.text `shouldBe` Just ResponseTextConfig
            { format = Nothing
            , verbosity = Just "low"
            , extraFields = KeyMap.empty
            }
        case params.reasoning of
            Nothing -> expectationFailure "expected reasoning configuration"
            Just reasoning -> reasoning.context `shouldBe` Just "all_turns"
        case params.input of
            Just (ResponseInputItems (additional : base : _)) -> do
                additional `shouldSatisfy` isAdditionalTools
                base `shouldSatisfy` isBaseInstructions
            _ -> expectationFailure
                "expected additional_tools and base-instructions input prefix"

    it "groups function and custom tools into the functions namespace" do
        let params =
                requestParams
                    OpenAIProvider
                    "gpt-5.6-sol"
                    "base instructions"
                    [ webSearchTool
                    , functionTool "lookup"
                    , namespaceTool "editor" Nothing [functionTool "edit"]
                    , customTool "exec"
                    , namespaceTool
                        "functions"
                        (Just "existing functions")
                        [functionTool "existing"]
                    , functionTool "last"
                    ]
                    "high"
            tools = additionalToolValues params

        map toolIdentity tools `shouldBe`
            [ (Just "web_search", Nothing)
            , (Just "namespace", Just "functions")
            , (Just "namespace", Just "editor")
            ]
        case tools of
            _web : functions : _editor : [] -> do
                jsonTextField "description" functions
                    `shouldBe` Just "existing functions"
                map (jsonTextField "name") (jsonArrayField "tools" functions)
                    `shouldBe`
                        map Just ["lookup", "exec", "existing", "last"]
            _ -> expectationFailure "expected three top-level Lite tools"

    it "refreshes the Lite prefix without dropping pending input" do
        let pending = userMessage "keep me"
            original = appendInputItem pending $
                requestParams
                    OpenAIProvider
                    "gpt-5.6-sol"
                    "old instructions"
                    [functionTool "old"]
                    "high"
            refreshed =
                setRequestInstructionsAndTools
                    "new instructions"
                    (Just [functionTool "new"])
                    original

        refreshed.instructions `shouldBe` Nothing
        refreshed.tools `shouldBe` Nothing
        map (jsonTextField "name")
            (functionsNamespaceTools (additionalToolValues refreshed))
            `shouldBe` [Just "new"]
        case refreshed.input of
            Just (ResponseInputItems [_additional, base, suffix]) -> do
                base `shouldSatisfy` isInstructionText "new instructions"
                suffix `shouldBe` pending
            _ -> expectationFailure
                "expected refreshed prefix followed by pending input"

    it "rebuilds request dialect fields when models cross the Lite boundary" do
        let pending = userMessage "pending"
            genericText = ResponseTextConfig
                { format = Nothing
                , verbosity = Just "medium"
                , extraFields = KeyMap.singleton
                    (Key.fromText "vendor_option")
                    (Aeson.Bool True)
                }
            generic =
                withText (Just genericText) $
                    appendInputItem pending $
                    requestParams
                        OpenAIProvider
                        "gpt-generic"
                        "base instructions"
                        [webSearchTool, functionTool "lookup"]
                        "high"
            lite =
                setRequestModel OpenAIProvider "gpt-5.6-terra" generic
            restored =
                setRequestModel OpenAIProvider "gpt-generic-2" lite

        lite.instructions `shouldBe` Nothing
        lite.tools `shouldBe` Nothing
        lite.parallelToolCalls `shouldBe` Just False
        fmap (.context) lite.reasoning `shouldBe` Just (Just "all_turns")
        fmap (.verbosity) lite.text `shouldBe` Just (Just "low")

        restored.model `shouldBe` Just "gpt-generic-2"
        restored.instructions `shouldBe` Just "base instructions"
        restored.tools `shouldBe` Just
            [webSearchTool, functionTool "lookup"]
        restored.parallelToolCalls `shouldBe` Just True
        fmap (.context) restored.reasoning `shouldBe` Just Nothing
        restored.text `shouldBe` Just
            (genericText { verbosity = Nothing })
        restored.input `shouldBe` Just (ResponseInputItems [pending])

isAdditionalTools :: ResponseItem -> Bool
isAdditionalTools = \case
    UnknownResponseItem TaggedObject{tag = "additional_tools"} -> True
    _ -> False

isBaseInstructions :: ResponseItem -> Bool
isBaseInstructions = \case
    MessageItem message ->
        message.role == RoleDeveloper
            && message.extraFields /= KeyMap.empty
            && case message.content of
                MessageContentParts [InputTextPart{ text = "base instructions" }] ->
                    True
                _ -> False
    _ -> False

isInstructionText :: Text -> ResponseItem -> Bool
isInstructionText expected = \case
    MessageItem message -> case message.content of
        MessageContentParts [InputTextPart{ text }] ->
            text == expected
        _ -> False
    _ -> False

functionTool :: Text -> ResponseTool
functionTool toolName = FunctionToolValue FunctionTool
    { name = toolName
    , description = Nothing
    , parameters = Nothing
    , strict = Just True
    , extraFields = KeyMap.empty
    }

customTool :: Text -> ResponseTool
customTool toolName = KnownResponseTool ToolCustom TaggedObject
    { tag = "custom"
    , fields = KeyMap.singleton
        (Key.fromText "name")
        (Aeson.String toolName)
    }

namespaceTool :: Text -> Maybe Text -> [ResponseTool] -> ResponseTool
namespaceTool namespaceName namespaceDescription nestedTools =
    KnownResponseTool ToolNamespace TaggedObject
        { tag = "namespace"
        , fields = KeyMap.fromList $
            [ (Key.fromText "name", Aeson.String namespaceName)
            , (Key.fromText "tools", Aeson.toJSON nestedTools)
            ]
            <> case namespaceDescription of
                Just description ->
                    [ (Key.fromText "description"
                      , Aeson.String description)
                    ]
                Nothing -> []
        }

userMessage :: Text -> ResponseItem
userMessage value = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts
        [InputTextPart value Nothing KeyMap.empty]
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , extraFields = KeyMap.empty
    }

appendInputItem
    :: ResponseItem
    -> ResponseCreateParams
    -> ResponseCreateParams
appendInputItem item ResponseCreateParams{..} =
    let nextItems = case input of
            Just (ResponseInputItems items) -> items <> [item]
            Just (ResponseInputText text) -> [userMessage text, item]
            Nothing -> [item]
    in ResponseCreateParams
        { input = Just (ResponseInputItems nextItems)
        , ..
        }

withText
    :: Maybe ResponseTextConfig
    -> ResponseCreateParams
    -> ResponseCreateParams
withText nextText ResponseCreateParams { text = _, .. } =
    ResponseCreateParams { text = nextText, .. }

additionalToolValues :: ResponseCreateParams -> [Aeson.Value]
additionalToolValues params = case params.input of
    Just (ResponseInputItems
        (UnknownResponseItem TaggedObject{tag = "additional_tools", fields} : _)) ->
            case KeyMap.lookup (Key.fromText "tools") fields of
                Just (Aeson.Array values) -> toList values
                _ -> []
    _ -> []

functionsNamespaceTools :: [Aeson.Value] -> [Aeson.Value]
functionsNamespaceTools =
    maybe [] (jsonArrayField "tools")
        . findValue
            (\value -> toolIdentity value
                == (Just "namespace", Just "functions"))

findValue :: (value -> Bool) -> [value] -> Maybe value
findValue predicate = \case
    value : rest
        | predicate value -> Just value
        | otherwise -> findValue predicate rest
    [] -> Nothing

toolIdentity :: Aeson.Value -> (Maybe Text, Maybe Text)
toolIdentity value =
    ( jsonTextField "type" value
    , jsonTextField "name" value
    )

jsonTextField :: Text -> Aeson.Value -> Maybe Text
jsonTextField fieldName = \case
    Aeson.Object object ->
        case KeyMap.lookup (Key.fromText fieldName) object of
            Just (Aeson.String value) -> Just value
            _ -> Nothing
    _ -> Nothing

jsonArrayField :: Text -> Aeson.Value -> [Aeson.Value]
jsonArrayField fieldName = \case
    Aeson.Object object ->
        case KeyMap.lookup (Key.fromText fieldName) object of
            Just (Aeson.Array values) -> toList values
            _ -> []
    _ -> []
