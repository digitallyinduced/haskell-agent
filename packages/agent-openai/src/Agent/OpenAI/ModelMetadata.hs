-- | Minimal Codex model metadata used by transport-level context management.
module Agent.OpenAI.ModelMetadata
    ( CodexModelMetadata(..)
    , codexModelMetadata
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
    | modelName `elem`
        [ "gpt-5.6-sol"
        , "gpt-5.6-terra"
        , "gpt-5.6-luna"
        ] =
        Just CodexModelMetadata
            { modelContextWindow = 272_000
            , modelEffectiveContextWindow = defaultCodexEffectiveContextWindow
            , modelAutoCompactTokenLimit = defaultCodexAutoCompactTokenLimit
            }
    | otherwise = Nothing

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
