-- | Request-scoped catalog preparation. The returned state is only a candidate
-- until the caller commits the provider response and transcript together.
module Agent.OpenAI.ToolCatalog.Request
    ( CatalogRequest(..)
    , prepareCatalogRequest
    , isCatalogContextItem
    ) where

import Agent.Json (RawJson, rawJsonFromEncoding)
import Agent.Loop.Backend
import Agent.OpenAI.ModelMetadata (isCodexResponsesLiteModel)
import Agent.OpenAI.ToolCatalog
import Agent.Responses.LoopBackend (withRequestInput)
import Agent.Responses.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (toList)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text

data CatalogRequest = CatalogRequest
    { catalogDeltaRequest :: !ResponseCreateParams
    , catalogFullRequest :: !ResponseCreateParams
    , catalogCommittedInput :: ![ResponseItem]
    , catalogCandidateState :: !BackendProviderState
    , catalogRequiresReplay :: !Bool
    } deriving stock (Show)

-- | A Lite template is an explicit capability boundary. Models without the
-- Lite protocol and ordinary Responses templates are deliberately unchanged.
prepareCatalogRequest
    :: ResponseCreateParams
    -> BackendSnapshot
    -> [ResponseItem]
    -> Maybe CatalogRequest
prepareCatalogRequest params snapshot newItems = do
    model <- params.model
    if isCodexResponsesLiteModel model then pure () else Nothing
    (rawDefinitions, instructions) <- templateParts params.input
    let definitions = map Aeson.toJSON rawDefinitions
    current <- either (const Nothing) Just (buildToolCatalog definitions)
    let previous = do
            stored <- snapshot.backendProviderState
            if stored.providerStateNamespace == catalogNamespace then pure () else Nothing
            Aeson.Object fields <- Just (Aeson.toJSON stored.providerStatePayload)
            if KeyMap.lookup "model" fields == Just (Aeson.String model) then pure () else Nothing
            Aeson.Array oldDefinitions <- KeyMap.lookup "tools" fields
            old <- either (const Nothing) Just (buildToolCatalog (toList oldDefinitions))
            oldInstructions <- KeyMap.lookup "instructions" fields
            -- Removing base instructions cannot be represented by appending
            -- an empty message: replay without the obsolete instruction prefix.
            if null instructions && oldInstructions /= Aeson.toJSON instructions
                then Nothing
                else pure ()
            pure (old, oldInstructions)
        instructionJson = Aeson.toJSON instructions
        delta = diffToolCatalog (fst <$> previous) current
        catalogItems = renderCatalogDelta delta
        instructionItems =
            if (snd <$> previous) == Just instructionJson then [] else instructions
        contextItems = catalogItems <> instructionItems
        restarting = isNothing previous
        history = if restarting
            then filter (not . isCatalogContextItem) snapshot.backendItems
            else snapshot.backendItems
        committedInput = if restarting
            then contextItems <> history <> newItems
            else history <> contextItems <> newItems
        candidate = BackendProviderState catalogNamespace $
            rawJsonFromEncoding $ Aeson.toEncoding $ Aeson.object
                [ "model" Aeson..= model
                , "tools" Aeson..= definitions
                , "instructions" Aeson..= instructionJson
                ]
    pure CatalogRequest
        { catalogDeltaRequest = withoutTemplatePrefix params (contextItems <> newItems)
        , catalogFullRequest = withoutTemplatePrefix params committedInput
        , catalogCommittedInput = committedInput
        , catalogCandidateState = candidate
        , catalogRequiresReplay = restarting && not (null snapshot.backendItems)
        }

catalogNamespace :: Text
catalogNamespace = "openai.responses_lite.tool_catalog.v1"

templateParts :: Maybe ResponseInput -> Maybe ([RawJson], [ResponseItem])
templateParts (Just (ResponseInputItems (AdditionalToolsItemValue catalog : rest))) =
    Just (catalog.tools, takeWhile isBaseInstructions rest)
templateParts _ = Nothing

withoutTemplatePrefix :: ResponseCreateParams -> [ResponseItem] -> ResponseCreateParams
withoutTemplatePrefix params items =
    case withRequestInput params items of
        result@ResponseCreateParams { input = Just (ResponseInputItems prepared) } ->
            result { input = Just (ResponseInputItems (drop prefixLength prepared)) }
        result -> result
  where
    prefixLength = case templateParts params.input of
        Just (_, instructions) -> 1 + length instructions
        Nothing -> 0

isBaseInstructions :: ResponseItem -> Bool
isBaseInstructions = hasKind "model.base_instructions"

isCatalogContextItem :: ResponseItem -> Bool
isCatalogContextItem = isModelContextItem

hasKind :: Text -> ResponseItem -> Bool
hasKind kind (MessageItem message) =
    maybe False (maybe False (kind `elem`) . (.contentItemKinds)) message.passthrough
hasKind _ _ = False

renderCatalogDelta :: ToolCatalogDelta -> [ResponseItem]
renderCatalogDelta delta =
    [ notice "The following tool definitions update the available catalog. Previously defined tools remain available unless explicitly removed; new definitions replace earlier definitions with the same qualified name."
    | delta.requiresNamespaceNotice
    ]
    <> [ AdditionalToolsItemValue (AdditionalToolsItem Nothing "developer"
            (map (rawJsonFromEncoding . Aeson.toEncoding) delta.addedDefinitions))
       | not (null delta.addedDefinitions)
       ]
    <> [ notice (if Text.null instructions
            then "Instructions for namespace " <> name <> " have been cleared."
            else "Updated instructions for namespace " <> name <> ":\n" <> instructions)
       | (name, instructions) <- delta.namespaceInstructions
       ]
    <> [ notice ("The following tools or namespaces are no longer available: "
            <> Text.intercalate ", " delta.removedDeclarations <> ".")
       | not (null delta.removedDeclarations)
       ]
  where
    notice text = MessageItem ResponseMessage
        { messageId = Nothing
        , content = MessageContentText text
        , role = RoleDeveloper
        , status = Nothing
        , phase = Nothing
        , passthrough = Just InternalChatMetadata
            { turnId = Nothing
            , createTime = Nothing
            , contentItemKinds = Just ["model.tool_catalog"]
            , executedToolCalls = Nothing
            }
        }
