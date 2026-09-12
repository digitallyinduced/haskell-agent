-- | Frontend-neutral local-tool capability policy and scoped acquisitions.
-- These acquisitions remain separate so the shared startup scheduler retains
-- concurrency and the existing dependency-ordered resource domains.
module Agent.Runtime.Tools.LocalStartup
    ( LocalToolSettings(..)
    , LocalToolPolicy(..)
    , LocalToolAcquisitions(..)
    , resolveLocalToolPolicy
    , selectLocalToolAcquisitions
    , localToolAcquisitions
    ) where

import qualified Agent.ComputerUse as ComputerUse
import Agent.Dialect (DialectId(GrokBuildDialect))
import Agent.Provider (Provider(OpenAIProvider))
import Agent.ResourceScope (logSlowCleanup)
import Agent.Runtime.Config (WebFetchConfig, LspConfig)
import Agent.Runtime.Lsp
    ( LspStartup(..), newLspRuntime, closeLspRuntime )
import Agent.Runtime.WebFetch
    ( WebFetchRuntime, newWebFetchRuntime, closeWebFetchRuntime )
import Agent.Tools.Types (ToolEnv)
import Data.Acquire (Acquire, mkAcquire)
import Data.Text (Text)

data LocalToolSettings = LocalToolSettings
    { localHostExtensions :: !Bool
    , localDialectId :: !DialectId
    , localProvider :: !Provider
    , localPlatform :: !Text
    , localComputerUseEnabled :: !Bool
    }

data LocalToolPolicy = LocalToolPolicy
    { localGrokToolsEnabled :: !Bool
    , localComputerUseAvailable :: !Bool
    , localComputerToolExposed :: !Bool
    }
    deriving (Eq, Show)

-- | Preserve the distinction between computer-use availability and exposure.
-- Its runtime has historically been acquired for supported OpenAI hosts even
-- when the session does not expose its tool; host-extension permission gates
-- Grok's web/LSP tools, not this independent availability decision.
resolveLocalToolPolicy :: LocalToolSettings -> LocalToolPolicy
resolveLocalToolPolicy settings = LocalToolPolicy
    { localGrokToolsEnabled =
        settings.localHostExtensions && settings.localDialectId == GrokBuildDialect
    , localComputerUseAvailable = available
    , localComputerToolExposed = available && settings.localComputerUseEnabled
    }
  where
    available =
        settings.localProvider == OpenAIProvider
            && settings.localPlatform `elem` ["darwin", "linux"]

data LocalToolAcquisitions web lsp computer = LocalToolAcquisitions
    { localAcquireWebFetch :: Acquire (Maybe web)
    , localAcquireLsp :: Acquire lsp
    , localAcquireComputerUse :: Acquire (Maybe computer)
    }

-- | Disabled capabilities do not evaluate their acquisition or finalizer.
-- Concrete factories remain injectable for lifecycle tests and alternate hosts.
selectLocalToolAcquisitions
    :: LocalToolPolicy
    -> lsp
    -> LocalToolAcquisitions web lsp computer
    -> LocalToolAcquisitions web lsp computer
selectLocalToolAcquisitions policy emptyLsp acquisitions = LocalToolAcquisitions
    { localAcquireWebFetch =
        if policy.localGrokToolsEnabled
            then acquisitions.localAcquireWebFetch
            else pure Nothing
    , localAcquireLsp =
        if policy.localGrokToolsEnabled
            then acquisitions.localAcquireLsp
            else pure emptyLsp
    , localAcquireComputerUse =
        if policy.localComputerUseAvailable
            then acquisitions.localAcquireComputerUse
            else pure Nothing
    }

-- | Pair concrete resources with their finalizers at the ownership boundary.
-- The host supplies web-fetch failure presentation; it must either abort or
-- explicitly recover with a replacement runtime.
localToolAcquisitions
    :: LocalToolSettings
    -> WebFetchConfig
    -> LspConfig
    -> ToolEnv
    -> (Text -> IO (Maybe WebFetchRuntime))
    -> LocalToolAcquisitions WebFetchRuntime LspStartup ComputerUse.ComputerUseRuntime
localToolAcquisitions settings webConfig lspConfig toolEnv onWebFetchFailure =
    selectLocalToolAcquisitions (resolveLocalToolPolicy settings)
        LspStartup { lspStartupRuntime = Nothing, lspStartupWarnings = [] }
        LocalToolAcquisitions
            { localAcquireWebFetch = mkAcquire
                (newWebFetchRuntime webConfig toolEnv >>= either onWebFetchFailure pure)
                (logSlowCleanup "web fetch runtime" . mapM_ closeWebFetchRuntime)
            , localAcquireLsp = mkAcquire
                (newLspRuntime lspConfig toolEnv)
                (logSlowCleanup "language server runtime"
                    . mapM_ closeLspRuntime . (.lspStartupRuntime))
            , localAcquireComputerUse = mkAcquire
                (Just <$> ComputerUse.newComputerUseRuntime)
                (logSlowCleanup "computer use runtime"
                    . mapM_ ComputerUse.closeComputerUseRuntime)
            }
