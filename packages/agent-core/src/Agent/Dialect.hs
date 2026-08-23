-- | The model-facing agent protocol, separate from provider transport.
--
-- A provider owns authentication, billing, endpoints, request projection, and
-- streaming. A dialect owns the prompt and tool conventions a model sees.
module Agent.Dialect
    ( DialectId(..)
    , Dialect
    , dialectId
    , dialectToolSurface
    , dialectFunctionSchemaStyle
    , dialectToolLayout
    , dialectPromptStyle
    , dialectProjectInstructionStyle
    , dialectInstructionHomeStyle
    , dialectChildAgentProtocol
    , ToolSurface(..)
    , FunctionSchemaStyle(..)
    , ToolLayout(..)
    , PromptStyle(..)
    , ProjectInstructionStyle(..)
    , InstructionHomeStyle(..)
    , ChildAgentProtocol(..)
    , codexDialect
    , grokBuildDialect
    , genericResponsesDialect
    , dialectForId
    , dialectIdForModel
    , dialectForModel
    , legacyDialectIdForProvider
    , providerSupportsDialect
    , dialectSlug
    , parseDialect
    ) where

import Agent.Provider (Provider(..))
import Data.Text (Text)
import qualified Data.Text as Text

-- | Stable identity persisted in session and project metadata.
data DialectId
    = CodexDialect
    | GrokBuildDialect
    | GenericResponsesDialect
    deriving (Eq, Ord, Show)

-- | Runtime tool implementation family.
--
-- The generic Responses dialect initially uses the portable Grok-style
-- function tool surface without Grok's identity prompt.
data ToolSurface
    = CodexToolSurface
    | GrokBuildToolSurface
    deriving (Eq, Show)

-- | JSON function-schema convention presented to the model.
data FunctionSchemaStyle
    = StrictFunctionSchemas
    | LooseFunctionSchemas
    deriving (Eq, Show)

-- | Whether model-facing tools are flat or grouped into a namespace.
data ToolLayout
    = FlatToolLayout
    | CollaborationNamespaceLayout
    deriving (Eq, Show)

-- | Base system-prompt family.
data PromptStyle
    = CodexPromptStyle
    | GrokBuildPromptStyle
    | GenericResponsesPromptStyle
    deriving (Eq, Show)

-- | How discovered project instructions are presented to the model.
data ProjectInstructionStyle
    = CodexProjectInstructions
    | GrokProjectInstructions
    deriving (Eq, Show)

-- | Compatibility directory used for global project instructions.
data InstructionHomeStyle
    = CodexInstructionHome
    | GrokInstructionHome
    | HarnessInstructionHome
    deriving (Eq, Show)

-- | Model-facing child-agent tool and prompt protocol.
data ChildAgentProtocol
    = CodexCollaborationProtocol
    | GrokTaskProtocol
    | GenericTaskProtocol
    deriving (Eq, Show)

-- | Complete static selection of model-facing behavior.
--
-- Effectful tools are constructed later from 'dialectToolSurface'; they
-- cannot be stored directly because they own shell, GHCi, plan-mode, and
-- subagent resources.
data Dialect = Dialect
    { dialectId :: !DialectId
    , dialectToolSurface :: !ToolSurface
    , dialectFunctionSchemaStyle :: !FunctionSchemaStyle
    , dialectToolLayout :: !ToolLayout
    , dialectPromptStyle :: !PromptStyle
    , dialectProjectInstructionStyle :: !ProjectInstructionStyle
    , dialectInstructionHomeStyle :: !InstructionHomeStyle
    , dialectChildAgentProtocol :: !ChildAgentProtocol
    }
    deriving (Eq, Show)

dialectId :: Dialect -> DialectId
dialectId Dialect{dialectId = value} = value

dialectToolSurface :: Dialect -> ToolSurface
dialectToolSurface Dialect{dialectToolSurface = value} = value

dialectFunctionSchemaStyle :: Dialect -> FunctionSchemaStyle
dialectFunctionSchemaStyle Dialect{dialectFunctionSchemaStyle = value} = value

dialectToolLayout :: Dialect -> ToolLayout
dialectToolLayout Dialect{dialectToolLayout = value} = value

dialectPromptStyle :: Dialect -> PromptStyle
dialectPromptStyle Dialect{dialectPromptStyle = value} = value

dialectProjectInstructionStyle :: Dialect -> ProjectInstructionStyle
dialectProjectInstructionStyle Dialect{dialectProjectInstructionStyle = value} =
    value

dialectInstructionHomeStyle :: Dialect -> InstructionHomeStyle
dialectInstructionHomeStyle Dialect{dialectInstructionHomeStyle = value} = value

dialectChildAgentProtocol :: Dialect -> ChildAgentProtocol
dialectChildAgentProtocol Dialect{dialectChildAgentProtocol = value} = value

codexDialect :: Dialect
codexDialect = Dialect
    { dialectId = CodexDialect
    , dialectToolSurface = CodexToolSurface
    , dialectFunctionSchemaStyle = StrictFunctionSchemas
    , dialectToolLayout = CollaborationNamespaceLayout
    , dialectPromptStyle = CodexPromptStyle
    , dialectProjectInstructionStyle = CodexProjectInstructions
    , dialectInstructionHomeStyle = CodexInstructionHome
    , dialectChildAgentProtocol = CodexCollaborationProtocol
    }

grokBuildDialect :: Dialect
grokBuildDialect = Dialect
    { dialectId = GrokBuildDialect
    , dialectToolSurface = GrokBuildToolSurface
    , dialectFunctionSchemaStyle = LooseFunctionSchemas
    , dialectToolLayout = FlatToolLayout
    , dialectPromptStyle = GrokBuildPromptStyle
    , dialectProjectInstructionStyle = GrokProjectInstructions
    , dialectInstructionHomeStyle = GrokInstructionHome
    , dialectChildAgentProtocol = GrokTaskProtocol
    }

genericResponsesDialect :: Dialect
genericResponsesDialect = Dialect
    { dialectId = GenericResponsesDialect
    , dialectToolSurface = GrokBuildToolSurface
    , dialectFunctionSchemaStyle = LooseFunctionSchemas
    , dialectToolLayout = FlatToolLayout
    , dialectPromptStyle = GenericResponsesPromptStyle
    , dialectProjectInstructionStyle = GrokProjectInstructions
    , dialectInstructionHomeStyle = HarnessInstructionHome
    , dialectChildAgentProtocol = GenericTaskProtocol
    }

dialectForId :: DialectId -> Dialect
dialectForId = \case
    CodexDialect -> codexDialect
    GrokBuildDialect -> grokBuildDialect
    GenericResponsesDialect -> genericResponsesDialect

-- | Resolve the default dialect for a provider/model target.
--
-- OpenRouter is a transport for several model families, so its dialect is
-- selected from the model slug rather than from the provider alone.
dialectIdForModel :: Provider -> Text -> DialectId
dialectIdForModel provider model = case provider of
    OpenAIProvider -> CodexDialect
    XAIProvider -> GrokBuildDialect
    OpenRouterProvider
        | "x-ai/" `Text.isPrefixOf` normalized -> GrokBuildDialect
        | "openai/" `Text.isPrefixOf` normalized -> CodexDialect
        | otherwise -> GenericResponsesDialect
  where
    normalized = Text.toLower (Text.strip model)

dialectForModel :: Provider -> Text -> Dialect
dialectForModel provider =
    dialectForId . dialectIdForModel provider

-- | Dialect used before dialect identity was persisted explicitly.
--
-- OpenRouter historically always used the Grok Build surface, including for
-- OpenAI-family model slugs. Legacy sessions must preserve that contract.
legacyDialectIdForProvider :: Provider -> DialectId
legacyDialectIdForProvider = \case
    OpenAIProvider -> CodexDialect
    XAIProvider -> GrokBuildDialect
    OpenRouterProvider -> GrokBuildDialect

-- | Whether a provider transport can carry a model-facing dialect.
--
-- Direct provider transports expose one native contract. OpenRouter can host
-- models from all supported families, so the model target selects its dialect.
providerSupportsDialect :: Provider -> DialectId -> Bool
providerSupportsDialect provider dialect = case provider of
    OpenAIProvider -> dialect == CodexDialect
    XAIProvider -> dialect == GrokBuildDialect
    OpenRouterProvider -> True

dialectSlug :: DialectId -> Text
dialectSlug = \case
    CodexDialect -> "codex"
    GrokBuildDialect -> "grok-build"
    GenericResponsesDialect -> "generic-responses"

parseDialect :: Text -> Maybe DialectId
parseDialect raw = case Text.toLower (Text.strip raw) of
    "codex" -> Just CodexDialect
    "grok-build" -> Just GrokBuildDialect
    "grok" -> Just GrokBuildDialect
    "generic-responses" -> Just GenericResponsesDialect
    "generic" -> Just GenericResponsesDialect
    _ -> Nothing
