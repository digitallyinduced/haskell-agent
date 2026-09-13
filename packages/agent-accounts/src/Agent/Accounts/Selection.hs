-- | Provider-local account ranking, independent of discovery and presentation.
module Agent.Accounts.Selection
    ( AccountCandidate(..)
    , SelectedAccount(..)
    , providerSupportsUsageAccountSelection
    , loadedAuthSupportsUsageAccountSelection
    , selectCandidates
    ) where

import Agent.Accounts.Auth (LoadedAuth(..), isGatewayLoadedAuth)
import Agent.Provider (BillingMode(..), Provider(..))
import Data.List (find, sortOn)
import Data.Ord (Down(..))
import Data.Text (Text)

data SelectedAccount = SelectedAccount
    { selectedProvider :: !Provider
    , selectedSelectionId :: !Text
    , selectedAccountId :: !Text
    , selectedBillingMode :: !BillingMode
    , selectedLabel :: !Text
    }
    deriving (Eq, Show)

-- | Claude Code owns its authentication, and Gemini does not expose the
-- account usage needed by this ranking policy.
providerSupportsUsageAccountSelection :: Provider -> Bool
providerSupportsUsageAccountSelection = \case
    OpenAIProvider -> True
    XAIProvider -> True
    OpenRouterProvider -> True
    GeminiProvider -> False
    ClaudeCodeProvider -> False

loadedAuthSupportsUsageAccountSelection :: LoadedAuth -> Bool
loadedAuthSupportsUsageAccountSelection loaded =
    not (isGatewayLoadedAuth loaded)
        && providerSupportsUsageAccountSelection loaded.loadedProvider

data AccountCandidate = AccountCandidate
    { candidateProvider :: !Provider
    , candidateSelectionId :: !Text
    , candidateAccountId :: !Text
    , candidateBillingMode :: !BillingMode
    , candidateLabel :: !Text
    , candidateCapacity :: !(Maybe Double)
    }
    deriving (Eq, Show)

-- | A usable remembered account wins. Otherwise choose the greatest capacity,
-- preserving discovery order for ties. Missing capacity is unverifiable.
selectCandidates
    :: Maybe (Text, Text)
    -> [AccountCandidate]
    -> Maybe AccountCandidate
selectCandidates remembered candidates =
    case remembered >>= rememberedCandidate usable of
        Just candidate -> Just candidate
        Nothing -> case sortOn ranking usable of
            candidate : _ -> Just candidate
            [] -> Nothing
  where
    usable = filter (maybe False (> 0) . (.candidateCapacity)) candidates
    rememberedCandidate available (selectionId, accountId) =
        find
            (\candidate ->
                candidate.candidateAccountId == accountId
                    && (candidate.candidateSelectionId == selectionId
                        || selectionId == accountId))
            available
    ranking candidate = Down <$> candidate.candidateCapacity
