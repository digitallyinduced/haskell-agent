-- | Grok Build's model-facing @lsp@ contract.
module Agent.GrokBuild.Dialect.Lsp
    ( LspOperation(..)
    , LspRequest(..)
    , lspOperationName
    , lspTool
    ) where

import Agent.GrokBuild.Dialect.Common (jsonTool)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch (typedTool)
import Agent.Tools.Types (AppTool, ToolExecutionPolicy(..))
import Data.Aeson
    ( FromJSON(..)
    , (.:)
    , (.:?)
    , withObject
    , withText
    )
import Data.Text (Text)
import qualified Data.Text as Text

data LspOperation
    = GoToDefinition
    | FindReferences
    | Hover
    | GoToImplementation
    | DocumentSymbol
    | WorkspaceSymbol
    deriving (Eq, Show)

lspOperationName :: LspOperation -> Text
lspOperationName = \case
    GoToDefinition -> "goToDefinition"
    FindReferences -> "findReferences"
    Hover -> "hover"
    GoToImplementation -> "goToImplementation"
    DocumentSymbol -> "documentSymbol"
    WorkspaceSymbol -> "workspaceSymbol"

instance FromJSON LspOperation where
    parseJSON = withText "LSP operation" \value ->
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

data LspRequest = LspRequest
    { lspOperation :: !LspOperation
    , lspFilePath :: !(Maybe Text)
    , lspLine :: !(Maybe Int)
    , lspCharacter :: !(Maybe Int)
    , lspQuery :: !(Maybe Text)
    }
    deriving (Eq, Show)

instance FromJSON LspRequest where
    parseJSON = withObject "lsp" \object ->
        LspRequest
            <$> object .: "operation"
            <*> object .:? "file_path"
            <*> object .:? "line"
            <*> object .:? "character"
            <*> object .:? "query"

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
        (typedTool "lsp" run)

lspDescription :: Text
lspDescription =
    "Code intelligence via configured language servers. Prefer it over text \
    \search for symbol-aware navigation.\n\
    \Operations: goToDefinition, findReferences, hover, goToImplementation, \
    \documentSymbol, workspaceSymbol. Position-based operations require \
    \file_path + line + character; workspaceSymbol requires query."
