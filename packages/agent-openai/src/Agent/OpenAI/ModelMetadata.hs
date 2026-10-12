-- | Minimal Codex model metadata used by transport-level context management.
module Agent.OpenAI.ModelMetadata
    ( CodexModelMetadata(..)
    , codexModelMetadata
    , isCodexResponsesLiteModel
    , codexAutoCompactTokenLimitFor
    , codexEffectiveContextWindowFor
    , defaultCodexAutoCompactTokenLimit
    , defaultCodexEffectiveContextWindow
    ) where

import Data.Text (Text)

data CodexModelMetadata = CodexModelMetadata
    { modelContextWindow :: !Int
    , modelEffectiveContextWindow :: !Int
    , modelAutoCompactTokenLimit :: !Int
    } deriving (Eq, Show)

-- | The current Codex fallback model metadata uses a 272k context window and
-- compacts at 90% of that window.
defaultCodexAutoCompactTokenLimit :: Int
defaultCodexAutoCompactTokenLimit = (codexMetadataForWindow 272_000).modelAutoCompactTokenLimit

defaultCodexEffectiveContextWindow :: Int
defaultCodexEffectiveContextWindow = (codexMetadataForWindow 272_000).modelEffectiveContextWindow

-- | Codex compaction is based on a model's default @context_window@, not
-- its larger @max_context_window@.
codexModelMetadata :: Text -> Maybe CodexModelMetadata
codexModelMetadata modelName
    -- Codex takes model metadata from the backend's live catalog
    -- (GET /backend-api/codex/models), which overrides the bundled
    -- models.json. On 2026-10-12 the live catalog reported
    -- context_window = 373000 for GPT-6.1 Sol (max_context_window 872000,
    -- no auto_compact_token_limit), while the bundled catalog still listed
    -- 272000.
    | modelName == "gpt-6.1-sol" = Just (codexMetadataForWindow 373_000)
    -- The other Lite models report context_window = 272000 in both.
    | isCodexResponsesLiteModel modelName = Just (codexMetadataForWindow 272_000)
    | otherwise = Nothing

-- | Codex's defaults for a catalog entry without an explicit
-- auto_compact_token_limit or effective_context_window_percent: compact at
-- 90% of the window, and treat 95% of it as usable.
codexMetadataForWindow :: Int -> CodexModelMetadata
codexMetadataForWindow window =
    CodexModelMetadata
        { modelContextWindow = window
        , modelEffectiveContextWindow = window * 95 `div` 100
        , modelAutoCompactTokenLimit = window * 9 `div` 10
        }

-- | Models routed through the Codex Responses Lite request dialect.
--
-- Keep this predicate in the shared OpenAI metadata module so the CLI,
-- HTTP transport, and WebSocket transport cannot silently disagree about
-- which requests need the Lite headers and input shape.
isCodexResponsesLiteModel :: Text -> Bool
isCodexResponsesLiteModel modelName =
    modelName `elem`
        [ "gpt-6.1-sol"
        , "gpt-6-sol"
        , "gpt-6-luna"
        , "gpt-6-astra"
        , "gpt-5.6-sol"
        , "gpt-5.6-terra"
        , "gpt-5.6-luna"
        ]

codexAutoCompactTokenLimitFor :: Maybe Text -> Int
codexAutoCompactTokenLimitFor modelName =
    maybe defaultCodexAutoCompactTokenLimit
        (.modelAutoCompactTokenLimit)
        (modelName >>= codexModelMetadata)

codexEffectiveContextWindowFor :: Maybe Text -> Int
codexEffectiveContextWindowFor modelName =
    maybe defaultCodexEffectiveContextWindow
        (.modelEffectiveContextWindow)
        (modelName >>= codexModelMetadata)
