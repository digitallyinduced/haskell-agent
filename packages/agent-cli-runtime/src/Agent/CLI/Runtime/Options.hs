-- | Runtime policy values shared by non-terminal frontends.
module Agent.CLI.Runtime.Options
    ( ApprovalPolicy(..)
    , GatewayCommand(..)
    , defaultEffortFor
    ) where

import Agent.Provider (Provider(..))
import Agent.ReasoningEffort (ReasoningEffort(..))
import Data.Text (Text)

data ApprovalPolicy
    = ApproveAll
    | DenyMutating
    | PromptMutating
    deriving (Eq, Show)

data GatewayCommand
    = GatewayConnect Text
    | GatewayStatus
    | GatewayDisconnect
    deriving (Eq, Show)

defaultEffortFor :: Provider -> ReasoningEffort
defaultEffortFor = \case
    XAIProvider -> EffortHigh
    OpenAIProvider -> EffortMedium
    OpenRouterProvider -> EffortMedium
    GeminiProvider -> EffortMedium
    ClaudeCodeProvider -> EffortXHigh
