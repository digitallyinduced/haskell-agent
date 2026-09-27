-- | Deterministic Haskell declarations for an immutable nested-tool snapshot.
--
-- These bindings describe structural input/output types, not complete JSON Schema
-- validation. Constraints such as enum membership remain the tool's concern.
module Agent.Tools.CodeMode.Haskell.Bindings
    ( HaskellBindings(..)
    , generateHaskellBindings
    , generateHaskellBindingsWithOutputs
    , normalizeHaskellIdentifier
    ) where

import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (sortOn)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

data HaskellBindings = HaskellBindings
    { bindingsModuleSource :: !Text
    , bindingsDeclarations :: !Text
    } deriving (Eq, Show)

-- | Lower-case ASCII identifiers with explicit handling of reserved words.
-- Collisions are rejected by the generator rather than resolved silently.
normalizeHaskellIdentifier :: Text -> Text
normalizeHaskellIdentifier input =
    let cleaned = Text.map replace input
        started = case Text.uncons cleaned of
            Just (first, _) | isAsciiLower first -> cleaned
            _ -> "tool_" <> cleaned
    in if Set.member started reservedIdentifiers
        then "tool_" <> started
        else started
  where
    replace character
        | isAsciiLower character || isAsciiUpper character
            || isDigit character || character == '_' = character
        | otherwise = '_'

reservedIdentifiers :: Set.Set Text
reservedIdentifiers = Set.fromList
    [ "as", "case", "class", "data", "default", "deriving", "do", "else"
    , "family", "forall", "foreign", "hiding", "if", "import", "in"
    , "infix", "infixl", "infixr", "instance", "let", "mdo", "module"
    , "newtype", "of", "pattern", "qualified", "role", "safe", "then"
    , "type", "unsafe", "where", "stock", "anyclass", "via"
    ]

generateHaskellBindings
    :: [(Text, Maybe Value)] -> Either Text HaskellBindings
generateHaskellBindings = generateHaskellBindingsWithOutputs
    . map (\(name, schema) -> (name, schema, Nothing))

generateHaskellBindingsWithOutputs
    :: [(Text, Maybe Value, Maybe Value)] -> Either Text HaskellBindings
generateHaskellBindingsWithOutputs specifications = do
    ensureUnique "tool" (map (normalizeHaskellIdentifier . toolName) specifications)
    generated <- traverse renderTool (sortOn toolName specifications)
    let declarations = Text.intercalate "\n\n" (map fst generated)
        implementation = Text.intercalate "\n\n" (map snd generated)
        resultDeclaration = if any (\(_, _, output) -> output /= Nothing) specifications
            then toolResultDeclaration <> "\n\n" else ""
    pure HaskellBindings
        { bindingsModuleSource = moduleHeader <> resultDeclaration <> implementation <> "\n"
        , bindingsDeclarations =
            "-- Call these bindings qualified as Tools.<name>.\n"
            <> "-- Optional fields: Nothing omits the property; Just Nothing is explicit null for nullable fields.\n"
            <> "-- Bindings describe structural types, not complete JSON Schema validation; input constraints remain the tool's responsibility.\n"
            <> "-- Declared outputs return ToolResult: inspect .decodedResult (Either Text output) or .rawResult (Value).\n"
            <> "-- Decode failures never repeat a tool call. Tools without an output schema retain their raw Value result.\n"
            <> resultDeclaration <> declarations
        }
  where
    toolName (name, _, _) = name

toolResultDeclaration :: Text
toolResultDeclaration = Text.unlines
    [ "data ToolResult a = ToolResult"
    , "    { rawResult :: !Value"
    , "    , decodedResult :: !(Either Text a)"
    , "    } deriving (Show, Eq)"
    ]

moduleHeader :: Text
moduleHeader = Text.unlines
    [ "{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, NoFieldSelectors #-}"
    , "module Tools where"
    , "import qualified Prelude as P"
    , "import Prelude (IO, Integer, Bool, Maybe(..), Either, Show, Eq)"
    , "import Data.Text (Text)"
    , "import qualified Data.Text as Text"
    , "import Data.Scientific (Scientific)"
    , "import Data.Aeson (Value)"
    , "import qualified Data.Aeson as Aeson"
    , "import qualified Data.Aeson.KeyMap as KeyMap"
    , "import qualified CodeModeSupport"
    , ""
    ]

renderTool :: (Text, Maybe Value, Maybe Value) -> Either Text (Text, Text)
renderTool (originalName, schema, outputSchema) = do
    let functionName = normalizeHaskellIdentifier originalName
        argumentName = "ToolArguments_" <> functionName
    (argumentType, declarations, implementations) <- case schema of
        Nothing -> pure ("Text", [], [])
        Just value -> renderType False argumentName value
    (outputType, outputDeclarations, outputImplementations) <-
        maybe (pure fallbackType) (renderType True ("ToolOutput_" <> functionName)) outputSchema
    let call = "CodeModeSupport.callTool "
            <> haskellLiteral originalName <> " (Aeson.toJSON arguments)"
        resultType = case outputSchema of
            Nothing -> "Value"
            Just _ -> "(ToolResult " <> parenthesize outputType <> ")"
        signature = functionName <> " :: " <> parenthesize argumentType <> " -> IO " <> resultType
        implementation = functionName <> " arguments = " <> case outputSchema of
            Nothing -> call
            Just _ -> call <> " P.>>= \\raw -> P.pure (ToolResult raw (case Aeson.fromJSON raw of"
                <> " { Aeson.Error message -> P.Left (Text.pack message); Aeson.Success value -> P.Right value }))"
    pure
        ( Text.intercalate "\n\n" (declarations <> outputDeclarations <> [signature])
        , Text.intercalate "\n\n" (implementations <> outputImplementations <> [signature, implementation])
        )

-- Type expression, model-facing declarations, executable definitions.
type RenderedType = (Text, [Text], [Text])

renderType :: Bool -> Text -> Value -> Either Text RenderedType
renderType output name (Object schema)
    | any (`KeyMap.member` schema)
        ["$ref", "anyOf", "oneOf", "allOf", "not", "if", "dependentSchemas"] =
        pure fallbackType
    | otherwise = case KeyMap.lookup "type" schema of
        Just (String "string") -> scalar "Text"
        Just (String "integer") -> scalar "Integer"
        Just (String "number") -> scalar "Scientific"
        Just (String "boolean") -> scalar "Bool"
        Just (String "array") -> do
            (elementType, declarations, implementations) <-
                maybe (pure fallbackType) (renderType output (name <> "'Element"))
                    (KeyMap.lookup "items" schema)
            pure ("[" <> elementType <> "]", declarations, implementations)
        Just (String "object") -> renderObject output name schema
        Just (Array alternatives) ->
            case Vector.toList alternatives of
                [String "null", String other] -> nullable other
                [String other, String "null"] -> nullable other
                _ -> pure fallbackType
        _ -> pure fallbackType
  where
    scalar value = pure (value, [], [])
    nullable other = do
        (valueType, declarations, implementations) <-
            renderType output name (Object (KeyMap.insert "type" (String other) schema))
        pure ("Maybe " <> parenthesize valueType, declarations, implementations)
renderType _ _ _ = pure fallbackType

fallbackType :: RenderedType
fallbackType = ("Value", [], [])

renderObject :: Bool -> Text -> KeyMap.KeyMap Value -> Either Text RenderedType
renderObject output name schema =
    case (KeyMap.lookup "additionalProperties" schema, KeyMap.lookup "properties" schema) of
        (_, Just (Object properties)) | output -> renderProperties properties
        (Just (Bool True), _) -> pure fallbackType
        (Just (Object _), _) -> pure fallbackType
        (_, Just (Object properties)) -> renderProperties properties
        (Just (Bool False), Nothing) -> renderProperties KeyMap.empty
        _ -> pure fallbackType
  where
    required = case KeyMap.lookup "required" schema of
        Just (Array values) -> Set.fromList [value | String value <- Vector.toList values]
        _ -> Set.empty
    renderProperties properties = do
        let sorted = sortOn (Key.toText . fst) (KeyMap.toList properties)
            fieldNames = map (normalizeHaskellIdentifier . Key.toText . fst) sorted
            propertyNames = Set.fromList (map (Key.toText . fst) sorted)
        if not (required `Set.isSubsetOf` propertyNames)
            then pure fallbackType
            else do
                ensureUnique ("field in " <> name) fieldNames
                fields <- traverse renderField (zip [0 :: Int ..] sorted)
                let fieldDeclarations =
                        [ field <> " :: !(" <> valueType <> ")"
                        | (field, valueType, _, _, _) <- fields
                        ]
                    declaration = "data " <> name <> " = " <> name
                        <> (if null fields then "" else
                            "\n    { " <> Text.intercalate "\n    , " fieldDeclarations <> "\n    }")
                        <> "\n    deriving (Show, Eq)"
                    variables = ["argument" <> Text.pack (show index) | index <- [0 .. length fields - 1]]
                    encoder = "instance Aeson.ToJSON " <> name <> " where\n"
                        <> "    toJSON (" <> Text.unwords (name : variables) <> ") = Aeson.object (P.concat\n"
                        <> "        [ " <> Text.intercalate "\n        , "
                            [ encodeField original optional variable
                            | ((_, _, original, optional, _), variable) <- zip fields variables
                            ]
                        <> "\n        ])"
                    decoder = "instance Aeson.FromJSON " <> name <> " where\n"
                        <> "    parseJSON = Aeson.withObject " <> haskellLiteral name <> " (\\object -> "
                        <> "P.pure " <> name
                        <> Text.concat
                            [ " P.<*> (" <> decodeField original optional <> ")"
                            | (_, _, original, optional, _) <- fields
                            ]
                        <> ")\n"
                    nestedDeclarations = concat [declarations | (_, _, _, _, (declarations, _)) <- fields]
                    nestedImplementations = concat [implementations | (_, _, _, _, (_, implementations)) <- fields]
                pure
                    ( name
                    , nestedDeclarations <> [declaration]
                    , nestedImplementations <> [declaration, encoder] <> [decoder | output]
                    )
    renderField (index, (key, value)) = do
        (valueType, declarations, implementations) <-
            renderType output (name <> "'Field" <> Text.pack (show index)) value
        let original = Key.toText key
            optional = Set.notMember original required
        pure
            ( normalizeHaskellIdentifier original
            , if optional then "Maybe " <> parenthesize valueType else valueType
            , original
            , optional
            , (declarations, implementations)
            )

encodeField :: Text -> Bool -> Text -> Text
encodeField original optional variable
    | optional =
        "P.maybe [] (\\value -> [(" <> haskellLiteral original
        <> ", Aeson.toJSON value)]) " <> variable
    | otherwise =
        "[(" <> haskellLiteral original <> ", Aeson.toJSON " <> variable <> ")]"

decodeField :: Text -> Bool -> Text
decodeField original optional
    | optional = "case KeyMap.lookup " <> haskellLiteral original <> " object of "
        <> "{ Nothing -> P.pure Nothing; Just value -> P.fmap Just (Aeson.parseJSON value) }"
    | otherwise = "object Aeson..: " <> haskellLiteral original

ensureUnique :: Text -> [Text] -> Either Text ()
ensureUnique description = go Set.empty
  where
    go _ [] = Right ()
    go seen (name : remaining)
        | Set.member name seen =
            Left ("Haskell " <> description <> " identifier collision: " <> name)
        | otherwise = go (Set.insert name seen) remaining

parenthesize :: Text -> Text
parenthesize value = "(" <> value <> ")"

-- 'show' on Text emits a Haskell string literal, including escaping of quotes,
-- control characters and newlines. Never interpolate external names as source.
haskellLiteral :: Text -> Text
haskellLiteral = Text.pack . show
