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
defaultCodexAutoCompactTokenLimit = 244_800

defaultCodexEffectiveContextWindow :: Int
defaultCodexEffectiveContextWindow = 258_400

codexModelMetadata :: Text -> Maybe CodexModelMetadata
codexModelMetadata modelName
    | isCodexResponsesLiteModel modelName =
        Just CodexModelMetadata
            { modelContextWindow = 272_000
            , modelEffectiveContextWindow = defaultCodexEffectiveContextWindow
            , modelAutoCompactTokenLimit = defaultCodexAutoCompactTokenLimit
            }
    | otherwise = Nothing

-- | Models routed through the Codex Responses Lite request dialect.
--
-- Keep this predicate in the shared OpenAI metadata module so the CLI,
-- HTTP transport, and WebSocket transport cannot silently disagree about
-- which requests need the Lite headers and input shape.
isCodexResponsesLiteModel :: Text -> Bool
isCodexResponsesLiteModel modelName =
    modelName `elem`
        [ "gpt-5.6-sol"
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
