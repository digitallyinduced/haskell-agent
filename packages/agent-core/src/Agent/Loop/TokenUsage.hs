-- | Provider token accounting and live generation-rate estimates.
module Agent.Loop.TokenUsage
    ( TokenUsage(..)
    , emptyTokenUsage
    , tokenUsageDecoder
    , addTokenUsage
    , estimateTokensFromChars
    , tokensPerSecond
    , generationTokensPerSecond
    , liveTokenRateMinMillis
    , liveTokensPerSecond
    ) where

import qualified Agent.Json.Decode as Json
import Data.Aeson (ToJSON(..), object, (.=))

-- | Provider-reported token counts for one model response. @inputTokens@
-- typically includes any cached prefix; @cachedTokens@ is that subset when
-- the provider reports it.
data TokenUsage = TokenUsage
    { inputTokens :: !Int
    , outputTokens :: !Int
    , cachedTokens :: !Int
    } deriving (Eq, Show)

instance Semigroup TokenUsage where
    left <> right = addTokenUsage left right

instance Monoid TokenUsage where
    mempty = emptyTokenUsage

emptyTokenUsage :: TokenUsage
emptyTokenUsage = TokenUsage
    { inputTokens = 0
    , outputTokens = 0
    , cachedTokens = 0
    }

instance ToJSON TokenUsage where
    toJSON usage = object
        [ "input" .= usage.inputTokens
        , "output" .= usage.outputTokens
        , "cached" .= usage.cachedTokens
        ]

tokenUsageDecoder :: Json.Decoder TokenUsage
tokenUsageDecoder = Json.object $
    TokenUsage
        <$> Json.atKey "input" Json.int
        <*> Json.atKey "output" Json.int
        <*> (maybe 0 id <$> Json.atKeyOptional "cached" Json.int)

addTokenUsage :: TokenUsage -> TokenUsage -> TokenUsage
addTokenUsage a b = TokenUsage
    { inputTokens = a.inputTokens + b.inputTokens
    , outputTokens = a.outputTokens + b.outputTokens
    , cachedTokens = a.cachedTokens + b.cachedTokens
    }

-- | Rough streamed-text estimate: about four Unicode scalars per token.
estimateTokensFromChars :: Int -> Int
estimateTokensFromChars chars = (max 0 chars + 3) `div` 4

-- | Output tokens per second from a token count and elapsed milliseconds.
tokensPerSecond :: Int -> Int -> Maybe Double
tokensPerSecond tokens millis
    | tokens <= 0 = Nothing
    | millis <= 0 = Nothing
    | otherwise =
        Just (fromIntegral tokens * 1000 / fromIntegral millis)

-- | Completed generation speed from provider-reported output-token metadata.
-- Character-derived estimates are reserved for the live streaming display.
generationTokensPerSecond :: Int -> Int -> Maybe Double
generationTokensPerSecond = tokensPerSecond

-- | Live tok/s is noisy on the first few hundred milliseconds of a stream.
liveTokenRateMinMillis :: Int
liveTokenRateMinMillis = 400

liveTokensPerSecond :: Int -> Int -> Maybe Double
liveTokensPerSecond chars millis
    | millis < liveTokenRateMinMillis = Nothing
    | otherwise = tokensPerSecond (estimateTokensFromChars chars) millis
