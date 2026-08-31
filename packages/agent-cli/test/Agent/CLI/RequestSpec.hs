module Agent.CLI.RequestSpec (spec) where

import Agent.CLI.Request
    ( requestParams
    , requestPromptParts
    , requestToolIdentities
    , setRequestInstructionsAndTools
    , setRequestModel
    , setRequestPromptCacheKey
    )
import Agent.CLI.Tools (webSearchTool)
import Agent.Provider (Provider(..))
import Agent.Responses.Types
import Agent.Responses.Types.Items (responseItemDecoder)
import Agent.Json.Decode qualified as Hermes
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
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

    it "sets the prompt cache key" do
        let params = setRequestPromptCacheKey "session-123" $
                requestParams
                    OpenAIProvider
                    "test-model"
                    "test instructions"
                    []
                    "high"
        params.promptCacheKey `shouldBe` Just "session-123"

    it "decodes Responses metadata as a string map" do
        let decoded = Hermes.decodeEither responseCreateParamsDecoder $
                Text.encodeUtf8
                    "{\"metadata\":{\"team\":\"agent\"}}"
        fmap (.metadata) decoded `shouldBe`
            Right (Just (ResponseMetadata (Map.fromList [("team", "agent")])))

    it "decodes local-shell environment variables as a string map" do
        let decoded = Hermes.decodeEither responseItemDecoder $ Text.encodeUtf8
                "{\"type\":\"local_shell_call\",\"action\":{\"type\":\"exec\",\"command\":[\"env\"],\"env\":{\"LANG\":\"C\"}}}"
        case decoded of
            Right (LocalShellCallItem LocalShellCall
                { action = Just LocalShellExec{env} }) ->
                    env `shouldBe` Just
                        (EnvironmentVariables (Map.fromList [("LANG", "C")]))
            Right item -> expectationFailure
                ("unexpected decoded item: " <> show item)
            Left err -> expectationFailure
                ("could not decode local shell item: " <> show err)

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

    it "projects hosted computer into the Responses Lite function harness" do
        let computerTool = knownResponseTool ToolComputer
            params =
                requestParams
                    OpenAIProvider
                    "gpt-5.6-sol"
                    "base instructions"
                    [webSearchTool, computerTool, functionTool "lookup"]
                    "high"

        params.tools `shouldBe` Nothing
        jsonArrayField "tools" (Aeson.toJSON params) `shouldBe` []
        map toolIdentity (additionalToolValues params)
            `shouldBe`
                [ (Just "web_search", Nothing)
                , (Just "namespace", Just computerFunctionNamespace)
                , (Just "namespace", Just "functions")
                ]
        map toolIdentity
            (computerNamespaceTools (additionalToolValues params))
            `shouldBe` [(Just "function", Just computerFunctionName)]

    it "keeps hosted computer native for the standard Responses API" do
        let computerTool = knownResponseTool ToolComputer
            params =
                requestParams
                    OpenAIProvider
                    "gpt-5.6"
                    "base instructions"
                    [computerTool]
                    "high"

        params.tools `shouldBe` Just [computerTool]
        jsonArrayField "tools" (Aeson.toJSON params)
            `shouldBe` [Aeson.toJSON computerTool]

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
        let computerTool = knownResponseTool ToolComputer
            pending = userMessage "keep me"
            original = appendInputItem pending $
                requestParams
                    OpenAIProvider
                    "gpt-5.6-sol"
                    "old instructions"
                    [functionTool "old", computerTool]
                    "high"
            refreshed =
                setRequestInstructionsAndTools
                    "new instructions"
                    (Just [functionTool "new", computerTool])
                    original
            instructionsOnly =
                setRequestInstructionsAndTools
                    "newer instructions"
                    Nothing
                    refreshed

        refreshed.instructions `shouldBe` Nothing
        refreshed.tools `shouldBe` Nothing
        map (jsonTextField "name")
            (functionsNamespaceTools (additionalToolValues refreshed))
            `shouldBe` [Just "new"]
        map toolIdentity (additionalToolValues refreshed)
            `shouldBe`
                [ (Just "namespace", Just "functions")
                , (Just "namespace", Just computerFunctionNamespace)
                ]
        instructionsOnly.tools `shouldBe` Nothing
        computerNamespaceTools (additionalToolValues instructionsOnly)
            `shouldSatisfy` (not . null)
        case refreshed.input of
            Just (ResponseInputItems [_additional, base, suffix]) -> do
                base `shouldSatisfy` isInstructionText "new instructions"
                suffix `shouldBe` pending
            _ -> expectationFailure
                "expected refreshed prefix followed by pending input"

    it "restores an exact persisted Lite instruction/tool prefix" do
        let original =
                requestParams
                    OpenAIProvider
                    "gpt-5.6-sol"
                    "persisted instructions"
                    [ webSearchTool
                    , functionTool "lookup"
                    , customTool "exec"
                    , namespaceTool "editor" Nothing [functionTool "edit"]
                    ]
                    "high"
            persisted@(instructionText, toolSchemas) =
                requestPromptParts original
            regenerated =
                requestParams
                    OpenAIProvider
                    "gpt-5.6-sol"
                    "new binary instructions"
                    [ webSearchTool
                    , documentedFunctionTool
                        "lookup"
                        "new description"
                    , customTool "exec"
                    , namespaceTool "editor" Nothing [functionTool "edit"]
                    ]
                    "high"
            restored =
                setRequestInstructionsAndTools
                    instructionText
                    (Just toolSchemas)
                    regenerated

        requestToolIdentities toolSchemas
            `shouldBe`
                requestToolIdentities
                    (snd (requestPromptParts regenerated))
        requestPromptParts restored `shouldBe` persisted

    it "treats tool identity and order changes as new prompt epochs" do
        let old =
                [ functionTool "lookup"
                , customTool "exec"
                ]
            descriptionOnly =
                [ documentedFunctionTool
                    "lookup"
                    "updated documentation"
                , customTool "exec"
                ]
            reordered = reverse old
            renamed =
                [ functionTool "search"
                , customTool "exec"
                ]
        requestToolIdentities descriptionOnly
            `shouldBe` requestToolIdentities old
        requestToolIdentities reordered
            `shouldNotBe` requestToolIdentities old
        requestToolIdentities renamed
            `shouldNotBe` requestToolIdentities old

    it "rebuilds request dialect fields when models cross the Lite boundary" do
        let computerTool = knownResponseTool ToolComputer
            pending = userMessage "pending"
            genericText = ResponseTextConfig
                { format = Just ResponseFormatJsonObject
                , verbosity = Just "medium"
                }
            generic =
                withText (Just genericText) $
                    appendInputItem pending $
                    requestParams
                        OpenAIProvider
                        "gpt-generic"
                        "base instructions"
                        [webSearchTool, functionTool "lookup", computerTool]
                        "high"
            lite =
                setRequestModel OpenAIProvider "gpt-5.6-terra" generic
            restored =
                setRequestModel OpenAIProvider "gpt-generic-2" lite

        lite.instructions `shouldBe` Nothing
        lite.tools `shouldBe` Nothing
        map toolIdentity (additionalToolValues lite)
            `shouldBe`
                [ (Just "web_search", Nothing)
                , (Just "namespace", Just "functions")
                , (Just "namespace", Just computerFunctionNamespace)
                ]
        lite.parallelToolCalls `shouldBe` Just False
        fmap (.context) lite.reasoning `shouldBe` Just (Just "all_turns")
        fmap (.verbosity) lite.text `shouldBe` Just (Just "low")

        restored.model `shouldBe` Just "gpt-generic-2"
        restored.instructions `shouldBe` Just "base instructions"
        restored.tools `shouldBe` Just
            [webSearchTool, functionTool "lookup", computerTool]
        restored.parallelToolCalls `shouldBe` Just True
        fmap (.context) restored.reasoning `shouldBe` Just Nothing
        restored.text `shouldBe` Just
            (genericText { verbosity = Nothing })
        restored.input `shouldBe` Just (ResponseInputItems [pending])

    it "clears Fast mode when switching models" do
        let params :: ResponseCreateParams
            params =
                (requestParams OpenAIProvider "gpt-generic"
                    "instructions" [] "high" :: ResponseCreateParams)
                    { serviceTier = Just "priority" }
        setRequestModel OpenAIProvider "gpt-5.6-terra" params
            `shouldSatisfy` \result -> result.serviceTier == Nothing

isAdditionalTools :: ResponseItem -> Bool
isAdditionalTools = \case
    AdditionalToolsItemValue{} -> True
    UnknownResponseItem TaggedObject{tag = "additional_tools"} -> True
    _ -> False

isBaseInstructions :: ResponseItem -> Bool
isBaseInstructions = \case
    MessageItem message ->
        message.role == RoleDeveloper
            && maybe False
                ("model.base_instructions" `elem`)
                (message.passthrough >>= (.contentItemKinds))
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
    }

documentedFunctionTool :: Text -> Text -> ResponseTool
documentedFunctionTool toolName documentation =
    FunctionToolValue FunctionTool
        { name = toolName
        , description = Just documentation
        , parameters = Nothing
        , strict = Just True
        }

customTool :: Text -> ResponseTool
customTool toolName = CustomToolValue CustomTool
    { name = toolName
    , description = Nothing
    , format = Nothing
    }

namespaceTool :: Text -> Maybe Text -> [ResponseTool] -> ResponseTool
namespaceTool namespaceName namespaceDescription nestedTools =
    NamespaceToolValue NamespaceTool
        { name = namespaceName
        , description = namespaceDescription
        , tools = nestedTools
        }

userMessage :: Text -> ResponseItem
userMessage value = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts
        [InputTextPart value Nothing]
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
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
    Just (ResponseInputItems (AdditionalToolsItemValue item : _)) ->
        map Aeson.toJSON item.tools
    _ -> []

functionsNamespaceTools :: [Aeson.Value] -> [Aeson.Value]
functionsNamespaceTools =
    maybe [] (jsonArrayField "tools")
        . findValue
            (\value -> toolIdentity value
                == (Just "namespace", Just "functions"))

computerNamespaceTools :: [Aeson.Value] -> [Aeson.Value]
computerNamespaceTools =
    maybe [] (jsonArrayField "tools")
        . findValue
            (\value -> toolIdentity value
                == (Just "namespace", Just computerFunctionNamespace))

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
