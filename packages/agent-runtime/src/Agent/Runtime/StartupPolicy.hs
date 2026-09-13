-- | Host-owned startup permissions, independent of command-line options.
module Agent.Runtime.StartupPolicy
    ( NativeStartupPolicy(..)
    , NativeContextSources(..)
    , NativeExecutionFacilities(..)
    , hostNativeStartupPolicy
    , restrictedNativeStartupPolicy
    ) where

-- | Whether startup may augment supplied context with workspace instruction
-- files and skills. Workspace discovery itself is configured separately.
data NativeContextSources
    = WorkspaceContextAllowed
    | SuppliedContextOnly
    deriving (Eq, Show)

-- | Legacy host embeddings may request host startup facilities. A restricted
-- embedding executes only the supplied turn: no workspace switching, startup
-- input files, automatic approval, computer use, or code-mode startup.
data NativeExecutionFacilities
    = HostStartupFacilities
    | TurnScopedFacilities
    deriving (Eq, Show)

data NativeStartupPolicy = NativeStartupPolicy
    { nativeContextSources :: !NativeContextSources
    , nativeExecutionFacilities :: !NativeExecutionFacilities
    }
    deriving (Eq, Show)

hostNativeStartupPolicy :: NativeStartupPolicy
hostNativeStartupPolicy = NativeStartupPolicy
    { nativeContextSources = WorkspaceContextAllowed
    , nativeExecutionFacilities = HostStartupFacilities
    }

restrictedNativeStartupPolicy :: NativeStartupPolicy
restrictedNativeStartupPolicy = NativeStartupPolicy
    { nativeContextSources = SuppliedContextOnly
    , nativeExecutionFacilities = TurnScopedFacilities
    }
