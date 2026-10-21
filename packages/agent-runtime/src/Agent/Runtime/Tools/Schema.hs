-- | Convert registered AppTools into Responses tool schemas.
module Agent.Runtime.Tools.Schema
    ( requireToolRegistry
    , lookupAppTool
    , codexToolsForModel
    , isClaudeBridgeTool
    , schemasFromAppTools
    , schemasFromAppToolsWithAsyncCapability
    , schemasFromAppToolsWithHostedSearch
    , schemasFromAppToolsWithHostedSearchAndAsyncCapability
    , schemasFromAppToolsCodeMode
    , schemasFromAppToolsCodeModeWithAsyncCapability
    , schemasFromAppToolsCodeModeWithHostedSearch
    , schemasFromAppToolsCodeModeWithHostedSearchAndAsyncCapability
    , hostedSearchToolNames
    , hostedSearchToolNamesWhen
    , hostedSearchToolCollisions
    , webSearchTool
    , xSearchTool
    ) where

import Agent.ComputerUse (computerFunctionParameters)
import Agent.Responses.Types.Tools
    ( ResponseTool(..)
    , ResponseToolType(..)
    , FunctionTool(..)
    , CustomTool(..)
    , NamespaceTool(..)
    , computerFunctionName
    , knownResponseTool
    , responseToolTypeText
    )
import Agent.Json (rawJsonFromEncoding)
import Agent.Dialect
    ( Dialect
    , DialectId(..)
    , FunctionSchemaStyle(..)
    , ToolLayout(..)
    , dialectId
    , dialectFunctionSchemaStyle
    , dialectToolLayout
    , grokBuildPublicToolName
    )
import Agent.OpenAI.ImageGeneration
    ( imageGenerationNamespace
    , imageGenerationNamespaceDescription
    , imageGenerationToolName
    )
import Agent.OpenAI.ToolDSL (buildGrokTool, buildTool)
import Agent.OpenAI.Models.Types
    ( ModelInfo(..), ModelMessages(..), ModelToolMessages(..)
    , ModelToolMessage(..), MultiAgentToolMessages(..), MultiAgentVersion(..) )
import Agent.ToolDSL (PropertySchema(..), parametersObjectLoose)
import Agent.ToolDispatch (canonicalToolName)
import Agent.Codex.Dialect.ApplyPatch (applyPatchGrammar)
import Agent.Tools.MultiAgents (multiAgentNamespace, multiAgentToolNames)
import Agent.Tools.Types
    ( AppTool(..)
    , ToolSchema(..)
    , ToolRegistry
    , mkToolRegistry
    , appToolSupportsAsync
    )
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Control.Applicative ((<|>))
import Control.Monad (foldM, guard)
import qualified Data.Foldable as Foldable
import Data.List (partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Info (os)

-- | Claude supplies these native tools itself. Only bridge the remaining
-- JSON functions so the provider does not see duplicate tool implementations.
isClaudeBridgeTool :: AppTool -> Bool
isClaudeBridgeTool tool =
    canonicalToolName tool.appToolName
        `notElem`
            [ "shell_command", "run_terminal_cmd"
            , "read_file", "write_file", "grep", "glob"
            , "search_replace", "apply_patch", "write_stdin"
            , "web_fetch", "web_search"
            ]
        && case tool.appToolSchema of
            JsonFunctionSchema{} -> True
            RawJsonFunctionSchema{} -> True
            _ -> False

requireToolRegistry :: [AppTool] -> IO ToolRegistry
requireToolRegistry tools
    | any reservesComputerFunction tools =
        ioError . userError . Text.unpack $
            "tool name " <> computerFunctionName
                <> " is reserved for local computer control"
    | otherwise =
        either (ioError . userError . Text.unpack) pure (mkToolRegistry tools)
  where
    reservesComputerFunction tool =
        canonicalToolName tool.appToolName == computerFunctionName
            && not (isHostedComputerSchema tool.appToolSchema)

lookupAppTool :: Text -> [AppTool] -> Maybe AppTool
lookupAppTool name tools =
    Map.lookup (canonicalToolName name) byName
  where
    byName =
        Map.fromList
            [ (canonicalToolName tool.appToolName, tool)
            | tool <- tools
            ]

-- | Resolve the selected native Codex model's collaboration surface before
-- namespace and code-mode projection. Metadata changes declarations only;
-- handlers, approval rules and execution policies remain host-owned.
-- Missing capability metadata retains the established fallback surface.
codexToolsForModel :: Maybe ModelInfo -> [AppTool] -> [AppTool]
codexToolsForModel Nothing = id
codexToolsForModel (Just info) = map applyMessage . filter supported
  where
    supported tool =
        tool.appToolName `notElem`
            (multiAgentToolNames <> ["spawn_agent_in_worktree"])
            || maybe True (== MultiAgentV2) info.multiAgentVersion
    messages = info.modelMessages >>= (.tools) >>= (.multiAgent)
    messageFor name = messages >>= \overrides -> case name of
        "spawn_agent" -> overrides.spawnAgent
        "send_message" -> overrides.sendMessage
        "followup_task" -> overrides.followupTask
        "wait_agent" -> overrides.waitAgent
        "interrupt_agent" -> overrides.interruptAgent
        "list_agents" -> overrides.listAgents
        _ -> Nothing
    applyMessage tool = case messageFor tool.appToolName of
        Nothing -> tool
        Just message -> tool
            { appToolDescription =
                maybe tool.appToolDescription id message.description
            , appToolSchema = maybe tool.appToolSchema RawJsonFunctionSchema $
                message.parameters >>= catalogParameters tool
            }

catalogParameters :: AppTool -> Text -> Maybe Aeson.Value
catalogParameters tool encoded = do
    candidate@(Aeson.Object root) <-
        Aeson.decodeStrict' (TextEncoding.encodeUtf8 encoded)
    guard (KeyMap.lookup "type" root == Just (Aeson.String "object"))
    normalized@(Aeson.Object fields) <- normalizeCatalogSchema candidate
    let original = case tool.appToolSchema of
            JsonFunctionSchema properties -> parametersObjectLoose properties
            RawJsonFunctionSchema schema -> schema
            _ -> Aeson.Null
        propertiesOf (Aeson.Object schema) = case KeyMap.lookup "properties" schema of
            Just (Aeson.Object properties) -> properties
            _ -> KeyMap.empty
        propertiesOf _ = KeyMap.empty
        encryptedNames =
            [ name
            | (name, Aeson.Object property) <- KeyMap.toList (propertiesOf original)
            , KeyMap.lookup "encrypted" property == Just (Aeson.Bool True)
            ]
    if null encryptedNames then pure normalized else do
        properties <- foldM preserveEncryption
            (propertiesOf normalized) encryptedNames
        pure (Aeson.Object (KeyMap.insert "properties" (Aeson.Object properties) fields))
  where
    preserveEncryption properties name = do
        Aeson.Object property <- KeyMap.lookup name properties
        pure (KeyMap.insert name
            (Aeson.Object (KeyMap.insert "encrypted" (Aeson.Bool True) property))
            properties)

-- Match the Codex JsonSchema subset: malformed known fields reject the
-- override; unknown keywords and null optional fields are omitted.
normalizeCatalogSchema :: Aeson.Value -> Maybe Aeson.Value
normalizeCatalogSchema (Aeson.Object fields) =
    Aeson.Object . KeyMap.fromList <$> traverse normalizeField
        [ (key, value)
        | (key, value) <- KeyMap.toList fields
        , value /= Aeson.Null
        , Key.toText key `elem` supportedFields
        ]
  where
    supportedFields =
        [ "$ref", "type", "description", "encrypted", "enum", "items"
        , "minItems", "properties", "required", "additionalProperties"
        , "anyOf", "oneOf", "allOf", "$defs", "definitions"
        ]
    normalizeField (key, value) = (key,) <$> case Key.toText key of
        "$ref" -> string value
        "description" -> string value
        "encrypted" -> boolean value
        "type" -> primitive value <|> array primitive value
        "enum" -> array Just value
        "items" -> normalizeCatalogSchema value
        "minItems" -> case value of
            Aeson.Number number
                | Just (_ :: Word) <- toBoundedInteger number -> Just value
            _ -> Nothing
        "properties" -> schemaMap value
        "$defs" -> schemaMap value
        "definitions" -> schemaMap value
        "required" -> array string value
        "additionalProperties" -> boolean value <|> normalizeCatalogSchema value
        "anyOf" -> array normalizeCatalogSchema value
        "oneOf" -> array normalizeCatalogSchema value
        "allOf" -> array normalizeCatalogSchema value
        _ -> Nothing
    primitive value@(Aeson.String name)
        | name `elem` ["string", "number", "boolean", "integer", "object", "array", "null"] = Just value
    primitive _ = Nothing
    string value@(Aeson.String _) = Just value
    string _ = Nothing
    boolean value@(Aeson.Bool _) = Just value
    boolean _ = Nothing
    array f (Aeson.Array values) = Aeson.Array <$> traverse f values
    array _ _ = Nothing
    schemaMap (Aeson.Object values) = Aeson.Object <$> traverse normalizeCatalogSchema values
    schemaMap _ = Nothing
normalizeCatalogSchema _ = Nothing

-- | Built-in Responses @web_search@ tool, enabled for every provider by default.
-- The host runs the search server-side; the agent loop never dispatches it.
webSearchTool :: ResponseTool
webSearchTool = knownResponseTool ToolWebSearch

-- | Grok Build hosted @x_search@ tool. Server-side; the loop never dispatches it.
xSearchTool :: ResponseTool
xSearchTool = knownResponseTool ToolXSearch

hostedSearchToolTypes :: Dialect -> [ResponseToolType]
hostedSearchToolTypes dialect =
    ToolWebSearch
        : [ToolXSearch | dialectId dialect == GrokBuildDialect]

hostedSearchTools :: Dialect -> [ResponseTool]
hostedSearchTools dialect =
    map knownResponseTool (hostedSearchToolTypes dialect)

hostedSearchToolsWhen :: Bool -> Dialect -> [ResponseTool]
hostedSearchToolsWhen includeHostedSearch dialect
    | includeHostedSearch = hostedSearchTools dialect
    | otherwise = []

-- | Model-facing names for hosted search tools advertised in this dialect.
hostedSearchToolNames :: Dialect -> [Text]
hostedSearchToolNames =
    map responseToolTypeText . hostedSearchToolTypes

-- | Model-facing hosted search names, optionally omitted at an execution
-- boundary which requires every model-controlled network action to be routed
-- through an application tool.
hostedSearchToolNamesWhen :: Bool -> Dialect -> [Text]
hostedSearchToolNamesWhen includeHostedSearch dialect
    | includeHostedSearch = hostedSearchToolNames dialect
    | otherwise = []

-- | Names reserved so MCP servers cannot shadow hosted search tools.
hostedSearchToolCollisions :: [(Text, Text)]
hostedSearchToolCollisions =
    [ (responseToolTypeText ToolWebSearch, "built-in web search")
    , (responseToolTypeText ToolXSearch, "built-in X search")
    ]

schemasFromAppTools :: Dialect -> [AppTool] -> [ResponseTool]
schemasFromAppTools = schemasFromAppToolsWithHostedSearch True

schemasFromAppToolsWithAsyncCapability
    :: Bool
    -> Dialect
    -> [AppTool]
    -> [ResponseTool]
schemasFromAppToolsWithAsyncCapability =
    schemasFromAppToolsWithHostedSearchAndAsyncCapability True

-- | Project application tools while explicitly controlling provider-hosted
-- search. Hosted search bypasses application-tool dispatch, so embeddings
-- without that capability must pass 'False'.
schemasFromAppToolsWithHostedSearch
    :: Bool
    -> Dialect
    -> [AppTool]
    -> [ResponseTool]
schemasFromAppToolsWithHostedSearch includeHostedSearch =
    schemasFromAppToolsWithHostedSearchAndAsyncCapability
        includeHostedSearch
        False

schemasFromAppToolsWithHostedSearchAndAsyncCapability
    :: Bool
    -> Bool
    -> Dialect
    -> [AppTool]
    -> [ResponseTool]
schemasFromAppToolsWithHostedSearchAndAsyncCapability
        includeHostedSearch modelSupportsAsync dialect tools =
    case dialectToolLayout dialect of
        CollaborationNamespaceLayout ->
            let (multi, nonMulti) = partition isMultiAgentTool tools
                (imageGeneration, rest) =
                    partition isImageGenerationTool nonMulti
                base = hostedSearchToolsWhen includeHostedSearch dialect
                    ++ mapMaybe
                        (schemaFromAppTool modelSupportsAsync dialect)
                        rest
                imageNamespaces =
                    [ imageGenerationNamespaceTool
                        modelSupportsAsync
                        imageGeneration
                    | not (null imageGeneration)
                    ]
                collaborationNamespaces =
                    [ multiAgentNamespaceTool modelSupportsAsync multi
                    | not (null multi)
                    ]
            in base ++ imageNamespaces ++ collaborationNamespaces
        FlatToolLayout ->
            hostedSearchToolsWhen includeHostedSearch dialect
                ++ mapMaybe
                    (schemaFromAppTool modelSupportsAsync dialect)
                    tools
        NoHostToolLayout ->
            []

isMultiAgentTool :: AppTool -> Bool
isMultiAgentTool tool = tool.appToolName `elem` multiAgentToolNames

isImageGenerationTool :: AppTool -> Bool
isImageGenerationTool tool =
    tool.appToolName == imageGenerationToolName

schemaFromAppTool :: Bool -> Dialect -> AppTool -> Maybe ResponseTool
schemaFromAppTool _ _ tool
    | canonicalToolName tool.appToolName == computerFunctionName
    , not (isHostedComputerSchema tool.appToolSchema) =
        Nothing
schemaFromAppTool modelSupportsAsync dialect tool =
    fmap (setAsyncCapability supportsAsync) $
        case tool.appToolSchema of
            HostedComputerSchema ->
                if os `elem` ["darwin", "linux"]
                        && dialectId dialect == CodexDialect
                    then Just (FunctionToolValue FunctionTool
                        { name = computerFunctionName
                        , description = Just tool.appToolDescription
                        , parameters = Just
                            (rawJsonFromEncoding
                                (Aeson.toEncoding computerFunctionParameters))
                        , strict = Just True
                        , async = Nothing
                        })
                    else Nothing
            HostedComputerFunctionSchema parameters ->
                if os == "darwin" && dialectId dialect == CodexDialect
                    then Just (FunctionToolValue FunctionTool
                        { name = computerFunctionName
                        , description = Just tool.appToolDescription
                        , parameters = Just
                            (rawJsonFromEncoding
                                (Aeson.toEncoding parameters))
                        , strict = Just
                            (isStrictHostedFunctionSchema parameters)
                        , async = Nothing
                        })
                    else Nothing
            JsonFunctionSchema parameters ->
                case dialectFunctionSchemaStyle dialect of
                    NoFunctionSchemas ->
                        Nothing
                    StrictFunctionSchemas ->
                        Just (buildSchema buildTool parameters)
                    LooseFunctionSchemas ->
                        Just (buildSchema buildGrokTool parameters)
            RawJsonFunctionSchema parameters ->
                Just (FunctionToolValue FunctionTool
                    { name = tool.appToolName
                    , description = Just tool.appToolDescription
                    , parameters =
                        Just (rawJsonFromEncoding (Aeson.toEncoding parameters))
                    , strict = case dialectFunctionSchemaStyle dialect of
                        StrictFunctionSchemas -> Just False
                        LooseFunctionSchemas -> Nothing
                        NoFunctionSchemas -> Nothing
                    , async = Nothing
                    })
            FreeformApplyPatchSchema ->
                case dialectFunctionSchemaStyle dialect of
                    NoFunctionSchemas -> Nothing
                    _ -> Just
                        (applyPatchCustomTool
                            tool.appToolName
                            tool.appToolDescription)
            FreeformGrammarSchema syntax definition ->
                case dialectFunctionSchemaStyle dialect of
                    NoFunctionSchemas -> Nothing
                    _ -> Just (grammarCustomTool
                        tool.appToolName
                        tool.appToolDescription
                        syntax
                        definition)
  where
    supportsAsync = modelSupportsAsync && appToolSupportsAsync tool

    buildSchema build parameters =
        let (name, description, projectedParameters) =
                projectFunctionTool dialect tool parameters
        in build name description projectedParameters

projectFunctionTool
    :: Dialect
    -> AppTool
    -> [PropertySchema]
    -> (Text, Text, [PropertySchema])
projectFunctionTool dialect tool parameters
    | dialectId dialect == GrokBuildDialect =
        ( grokBuildPublicToolName tool.appToolName
        , grokPublicText tool.appToolDescription
        , map (projectProperty tool.appToolName) parameters
        )
    | otherwise =
        (tool.appToolName, tool.appToolDescription, parameters)

projectProperty :: Text -> PropertySchema -> PropertySchema
projectProperty toolName property =
    property
        { propertyName =
            if toolName == "task"
                && property.propertyName == "run_in_background"
                then "background"
                else property.propertyName
        , description = grokPublicText <$> property.description
        }

setAsyncCapability :: Bool -> ResponseTool -> ResponseTool
setAsyncCapability enabled = \case
    FunctionToolValue tool ->
        FunctionToolValue tool
            { async = if enabled then Just True else Nothing }
    CustomToolValue tool ->
        CustomToolValue tool
            { async = if enabled then Just True else Nothing }
    tool -> tool

grokPublicText :: Text -> Text
grokPublicText =
    replace "run_in_background" "background"
        . replace "run_terminal_cmd" "run_terminal_command"
        . replace "get_task_output" "get_command_or_subagent_output"
        . replace "wait_tasks" "wait_commands_or_subagents"
        . replace "kill_task" "kill_command_or_subagent"
        . replaceTaskName
  where
    replace = Text.replace
    -- Avoid replacing ordinary prose uses of "task"; only the common
    -- backtick-delimited tool reference is unambiguous.
    replaceTaskName = Text.replace "`task`" "`spawn_subagent`"

-- | Codex collaboration namespace: nested non-strict function tools.
multiAgentNamespaceTool :: Bool -> [AppTool] -> ResponseTool
multiAgentNamespaceTool modelSupportsAsync tools =
    namespaceTool
        modelSupportsAsync
        multiAgentNamespace
        "Tools for spawning and managing sub-agents."
        tools

imageGenerationNamespaceTool :: Bool -> [AppTool] -> ResponseTool
imageGenerationNamespaceTool modelSupportsAsync =
    namespaceTool
        modelSupportsAsync
        imageGenerationNamespace
        imageGenerationNamespaceDescription

namespaceTool :: Bool -> Text -> Text -> [AppTool] -> ResponseTool
namespaceTool modelSupportsAsync namespaceName namespaceDescription tools =
    NamespaceToolValue NamespaceTool
        { name = namespaceName
        , description = Just namespaceDescription
        , tools = map nestedFunction tools
        }
  where
    nestedFunction tool =
        setAsyncCapability
            (modelSupportsAsync && appToolSupportsAsync tool) $
        FunctionToolValue FunctionTool
            { name = tool.appToolName
            , description = Just tool.appToolDescription
            , strict = Just False
            , async = Nothing
            , parameters = Just . rawJsonFromEncoding . Aeson.toEncoding $
                namespaceParameters tool
            }

    namespaceParameters tool = case parametersValue tool of
        Aeson.Object schema
            | Just (Aeson.Array required) <- KeyMap.lookup "required" schema
            , null required -> Aeson.Object (KeyMap.delete "required" schema)
        schema -> schema

    parametersValue tool = case tool.appToolSchema of
        JsonFunctionSchema parameters -> parametersObjectLoose parameters
        RawJsonFunctionSchema parameters -> parameters
        FreeformApplyPatchSchema -> Aeson.object []
        FreeformGrammarSchema _ _ -> Aeson.object []
        HostedComputerSchema -> Aeson.object []
        HostedComputerFunctionSchema _ -> Aeson.object []

isHostedComputerSchema :: ToolSchema -> Bool
isHostedComputerSchema = \case
    HostedComputerSchema -> True
    HostedComputerFunctionSchema _ -> True
    _ -> False

isStrictHostedFunctionSchema :: Aeson.Value -> Bool
isStrictHostedFunctionSchema value@(Aeson.Object object) =
    KeyMap.lookup "type" object == Just (Aeson.String "object")
        && strictSchemaNode value
isStrictHostedFunctionSchema _ = False

strictSchemaNode :: Aeson.Value -> Bool
strictSchemaNode (Aeson.Object object)
    | any (`KeyMap.member` object)
        ["allOf", "oneOf", "not", "if", "then", "else", "$ref"] =
        False
    | schemaIncludesType "object" object =
        case
            ( KeyMap.lookup "properties" object
            , KeyMap.lookup "required" object
            , KeyMap.lookup "additionalProperties" object
            ) of
            ( Just (Aeson.Object properties)
                , Just (Aeson.Array required)
                , Just (Aeson.Bool False)
                ) ->
                    let propertyNames =
                            map (Key.toText) (KeyMap.keys properties)
                        requiredNames =
                            [ name
                            | Aeson.String name <- Foldable.toList required
                            ]
                    in length requiredNames == Foldable.length required
                        && length requiredNames == length propertyNames
                        && all (`elem` requiredNames) propertyNames
                        && all strictSchemaNode properties
            _ -> False
    | schemaIncludesType "array" object =
        maybe False strictSchemaNode (KeyMap.lookup "items" object)
            && strictAlternatives object
    | otherwise = strictAlternatives object
strictSchemaNode (Aeson.Array values) =
    all strictSchemaNode values
strictSchemaNode _ = True

strictAlternatives :: Aeson.Object -> Bool
strictAlternatives object =
    case KeyMap.lookup "anyOf" object of
        Nothing -> True
        Just (Aeson.Array alternatives) ->
            not (Foldable.null alternatives)
                && all strictSchemaNode alternatives
        Just _ -> False

schemaIncludesType :: Text -> Aeson.Object -> Bool
schemaIncludesType expected object =
    case KeyMap.lookup "type" object of
        Just (Aeson.String actual) -> actual == expected
        Just (Aeson.Array actual) ->
            Aeson.String expected `elem` Foldable.toList actual
        _ -> False

-- | Codex registers apply_patch as a Responses custom tool with a Lark grammar.
applyPatchCustomTool :: Text -> Text -> ResponseTool
applyPatchCustomTool name description =
    grammarCustomTool name description "lark" applyPatchGrammar

-- | Freeform custom tool with an explicit provider grammar, matching the
-- Codex wire shape for tools such as code-mode @exec@.
grammarCustomTool :: Text -> Text -> Text -> Text -> ResponseTool
grammarCustomTool name description syntax definition =
    CustomToolValue CustomTool
        { name
        , description = Just description
        , async = Nothing
        , format = Just . rawJsonFromEncoding . Aeson.toEncoding $
            Aeson.object
                [ "type" .= ("grammar" :: Text)
                , "syntax" .= syntax
                , "definition" .= definition
                ]
        }

-- | Code-mode tool surface: the @exec@/@wait@ entry points first, hosted
-- search tools last, matching the upstream Codex wire order.
schemasFromAppToolsCodeMode :: Dialect -> [AppTool] -> [ResponseTool]
schemasFromAppToolsCodeMode =
    schemasFromAppToolsCodeModeWithHostedSearch True

schemasFromAppToolsCodeModeWithAsyncCapability
    :: Bool
    -> Dialect
    -> [AppTool]
    -> [ResponseTool]
schemasFromAppToolsCodeModeWithAsyncCapability =
    schemasFromAppToolsCodeModeWithHostedSearchAndAsyncCapability True

schemasFromAppToolsCodeModeWithHostedSearch
    :: Bool
    -> Dialect
    -> [AppTool]
    -> [ResponseTool]
schemasFromAppToolsCodeModeWithHostedSearch
        includeHostedSearch =
    schemasFromAppToolsCodeModeWithHostedSearchAndAsyncCapability
        includeHostedSearch
        False

schemasFromAppToolsCodeModeWithHostedSearchAndAsyncCapability
    :: Bool
    -> Bool
    -> Dialect
    -> [AppTool]
    -> [ResponseTool]
schemasFromAppToolsCodeModeWithHostedSearchAndAsyncCapability
        includeHostedSearch modelSupportsAsync dialect tools =
    mapMaybe (schemaFromAppTool modelSupportsAsync dialect) tools
        ++ hostedSearchToolsWhen includeHostedSearch dialect
