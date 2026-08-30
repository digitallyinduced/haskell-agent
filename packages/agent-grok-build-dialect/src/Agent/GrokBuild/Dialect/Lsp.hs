-- | Grok Build's model-facing @lsp@ contract.
module Agent.GrokBuild.Dialect.Lsp
    ( LspOperation(..)
    , LspPosition(..)
    , LspPositionOperation(..)
    , LspRequest(..)
    , lspOperationName
    , lspPositionOperation
    , lspTool
    ) where

import Agent.GrokBuild.Dialect.Common (jsonTool)
import Agent.GrokBuild.Dialect.Json (optionalInt, optionalTextValue)
import qualified Agent.Json.Decode as Json
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch (typedTool)
import Agent.Tools.Types (AppTool, ToolExecutionPolicy(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Natural (Natural)

data LspOperation
    = GoToDefinition
    | FindReferences
    | Hover
    | GoToImplementation
    | DocumentSymbol
    | WorkspaceSymbol
    deriving (Eq, Show)

-- | An LSP operation that requires a source position.
data LspPositionOperation
    = LspGoToDefinition
    | LspFindReferences
    | LspHover
    | LspGoToImplementation
    deriving (Eq, Show)

data LspPosition = LspPosition
    { positionLine :: !Natural
    , positionCharacter :: !Natural
    }
    deriving (Eq, Show)

lspPositionOperation :: LspPositionOperation -> LspOperation
lspPositionOperation = \case
    LspGoToDefinition -> GoToDefinition
    LspFindReferences -> FindReferences
    LspHover -> Hover
    LspGoToImplementation -> GoToImplementation

lspOperationName :: LspOperation -> Text
lspOperationName = \case
    GoToDefinition -> "goToDefinition"
    FindReferences -> "findReferences"
    Hover -> "hover"
    GoToImplementation -> "goToImplementation"
    DocumentSymbol -> "documentSymbol"
    WorkspaceSymbol -> "workspaceSymbol"

lspOperationDecoder :: Json.Decoder LspOperation
lspOperationDecoder = Json.withText \value ->
        case value of
            "goToDefinition" -> pure GoToDefinition
            "findReferences" -> pure FindReferences
            "hover" -> pure Hover
            "goToImplementation" -> pure GoToImplementation
            "documentSymbol" -> pure DocumentSymbol
            "workspaceSymbol" -> pure WorkspaceSymbol
            _ ->
                fail
                    ( "Unknown LSP operation: "
                        <> Text.unpack value
                    )

-- | A structurally valid request decoded from the flat model-facing schema.
data LspRequest
    = LspAtPosition !LspPositionOperation !Text !LspPosition
    | LspDocumentSymbols !Text
    | LspWorkspaceSymbols !Text
    deriving (Eq, Show)

lspRequestDecoder :: Json.Decoder LspRequest
lspRequestDecoder = Json.object do
    operation <- Json.atKey "operation" lspOperationDecoder
    case operation of
        GoToDefinition ->
            positionRequest LspGoToDefinition operation
        FindReferences ->
            positionRequest LspFindReferences operation
        Hover ->
            positionRequest LspHover operation
        GoToImplementation ->
            positionRequest LspGoToImplementation operation
        DocumentSymbol ->
            LspDocumentSymbols <$> requiredFilePath operation
        WorkspaceSymbol ->
            workspaceSymbolsRequest
  where
    positionRequest positionOperation operation =
        LspAtPosition positionOperation
            <$> requiredFilePath operation
            <*> (LspPosition
                <$> coordinate operation "line"
                <*> coordinate operation "character")
    coordinate operation key =
        optionalInt key >>= \case
            Just value | value >= 0 ->
                pure (fromIntegral value)
            _ ->
                fail . Text.unpack $
                    lspOperationName operation
                        <> " requires non-negative line and character"
    requiredFilePath operation =
        optionalTextValue "file_path" >>= \case
            Just filePath -> pure filePath
            Nothing ->
                fail . Text.unpack $
                    lspOperationName operation <> " requires file_path"
    workspaceSymbolsRequest =
        optionalTextValue "query" >>= \case
            Nothing ->
                fail "workspaceSymbol requires query"
            Just rawQuery
                | Text.null (Text.strip rawQuery) ->
                    fail "workspaceSymbol requires a non-empty query"
                | otherwise ->
                    pure (LspWorkspaceSymbols (Text.strip rawQuery))

lspTool
    :: (LspRequest -> IO (Either Text Text))
    -> AppTool
lspTool run =
    jsonTool
        "lsp"
        lspDescription
        [ PropertySchema "operation"
            (PropertyEnum
                [ "goToDefinition"
                , "findReferences"
                , "hover"
                , "goToImplementation"
                , "documentSymbol"
                , "workspaceSymbol"
                ])
            True
            (Just "The LSP operation to perform.")
        , PropertySchema "file_path" PropertyString False $
            Just "Absolute path to the file."
        , PropertySchema "line" PropertyInteger False $
            Just "0-indexed line number."
        , PropertySchema "character" PropertyInteger False $
            Just "0-indexed column number."
        , PropertySchema "query" PropertyString False $
            Just
                "Symbol name or partial name (workspaceSymbol only)."
        ]
        True
        TurnSequential
        (typedTool "lsp" lspRequestDecoder run)

lspDescription :: Text
lspDescription =
    "Code intelligence via configured language servers. Prefer it over text \
    \search for symbol-aware navigation.\n\
    \Operations: goToDefinition, findReferences, hover, goToImplementation, \
    \documentSymbol, workspaceSymbol. Position-based operations require \
    \file_path + line + character; workspaceSymbol requires query."
