-- | Responses Lite catalog transitions. Catalogs are immutable: a caller must
-- publish the next catalog with the history which contains its rendered delta,
-- never when a request is merely attempted.
module Agent.OpenAI.ToolCatalog
    ( ToolCatalog
    , ToolCatalogDelta(..)
    , buildToolCatalog
    , diffToolCatalog
    ) where

import Control.Monad (foldM)
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

data ToolCatalog = ToolCatalog
    { definitions :: ![Value]
    , declarations :: !(Map Text Value)
    } deriving stock (Eq, Show)

data ToolCatalogDelta = ToolCatalogDelta
    { addedDefinitions :: ![Value]
    , removedDeclarations :: ![Text]
    , namespaceInstructions :: ![(Text, Text)]
    -- | Exactly one notice is required before an incremental namespace batch.
    , requiresNamespaceNotice :: !Bool
    } deriving stock (Eq, Show)

-- | Validate one-level namespace declarations and reject ambiguous names.
-- JSON object equality is structural, so key ordering is not a schema change.
buildToolCatalog :: [Value] -> Either Text ToolCatalog
buildToolCatalog definitions = do
    declarations <- foldM insertDefinition Map.empty definitions
    pure ToolCatalog { definitions, declarations }
  where
    insertDefinition declarations definition@(Object fields) = do
        name <- declarationName definition
        case KeyMap.lookup "tools" fields of
            Nothing -> insertUnique name definition declarations
            Just (Array members)
                | KeyMap.lookup "type" fields == Just (String "namespace") -> do
                    withHeader <- insertUnique name
                        (Object (KeyMap.delete "tools" fields)) declarations
                    foldM (insertMember name) withHeader (toList members)
            _ -> Left ("Invalid namespace members: " <> name)
    insertDefinition _ _ = Left "Tool declarations must be JSON objects"

    insertMember namespace declarations member@(Object fields)
        | KeyMap.member "tools" fields =
            Left "Nested tool namespaces are not supported"
        | otherwise = do
            name <- declarationName member
            insertUnique (namespace <> "." <> name) member declarations
    insertMember _ _ _ = Left "Namespace members must be JSON objects"

    insertUnique name value declarations
        | Map.member name declarations = Left ("Duplicate tool declaration: " <> name)
        | otherwise = Right (Map.insert name value declarations)

declarationName :: Value -> Either Text Text
declarationName (Object fields) =
    case KeyMap.lookup "name" fields of
        Just (String name) -> validate name
        Nothing -> case KeyMap.lookup "type" fields of
            Just (String name) -> validate name
            _ -> Left "Tool declaration has no name or type"
        _ -> Left "Tool declaration name must be text"
  where
    validate name
        | Text.null name || Text.any (== '.') name =
            Left "Tool declaration names must be nonempty and unqualified"
        | otherwise = Right name
declarationName _ = Left "Tool declaration must be a JSON object"

diffToolCatalog :: Maybe ToolCatalog -> ToolCatalog -> ToolCatalogDelta
diffToolCatalog previous current =
    ToolCatalogDelta
        { addedDefinitions = additions
        , removedDeclarations = removals
        , namespaceInstructions = instructions
        , requiresNamespaceNotice =
            maybe False (const True) previous && any isNamespace additions
        }
  where
    old = maybe Map.empty (.declarations) previous
    changed name = Map.lookup name old /= Map.lookup name current.declarations
    transitions = mapMaybe transition current.definitions
    additions = [definition | Left definition <- transitions]
    instructions = [instruction | Right instruction <- transitions]
    removedNames = Map.keys (Map.difference old current.declarations)
    removals = filter (not . memberOfRemovedNamespace) removedNames
    memberOfRemovedNamespace name =
        case Text.breakOn "." name of
            (namespace, suffix) ->
                not (Text.null suffix) && namespace `elem` removedNames

    transition definition@(Object fields) = case declarationName definition of
        Left _ -> Nothing -- Impossible for a validated catalog.
        Right name -> case KeyMap.lookup "tools" fields of
            Just (Array members) ->
                let changedMembers = filter (memberChanged name) (toList members)
                    structuralHeader = Object . KeyMap.delete "description" . KeyMap.delete "tools"
                    headerChanged = case Map.lookup name old of
                        Just (Object oldFields) ->
                            structuralHeader oldFields /= structuralHeader fields
                        _ -> True
                in if headerChanged
                    then Just (Left definition)
                    else if not (null changedMembers)
                    then Just (Left (Object (KeyMap.insert "tools"
                        (Array (Vector.fromList changedMembers)) fields)))
                    else if changed name
                        then Just (Right (name, description fields))
                        else Nothing
            _ | changed name -> Just (Left definition)
            _ -> Nothing
    transition _ = Nothing

    memberChanged namespace member = case declarationName member of
        Right name -> changed (namespace <> "." <> name)
        Left _ -> False
    description fields = case KeyMap.lookup "description" fields of
        Just (String value) -> value
        _ -> ""
    isNamespace (Object fields) =
        KeyMap.lookup "type" fields == Just (String "namespace")
    isNamespace _ = False
