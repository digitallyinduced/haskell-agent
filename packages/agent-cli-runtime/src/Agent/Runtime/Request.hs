-- | Frontend-neutral inputs for a native runtime turn.
--
-- This module deliberately does not depend on command-line options, terminal
-- presentation, or transport adapters. Adapters may lower these inputs into
-- the existing CLI orchestration while that implementation is extracted.
module Agent.Runtime.Request
    ( NativeInteractionMode(..)
    , NativeShellMode(..)
    , NativeSessionTarget(..)
    , NativeTurnRequest(..)
    , validateNativeTurnRequest
    ) where

import Agent.Loop (ImageAttachment)
import Agent.Provider (Provider)
import Agent.ReasoningEffort (ReasoningEffort)
import Data.Text (Text)
import Data.Text qualified as Text
import System.OsPath (OsPath)

data NativeInteractionMode
    = NativeAsk
    -- ^ Prompt before mutating tools.
    | NativePlan
    -- ^ Begin this turn with plan mode active.
    | NativeYolo
    -- ^ Auto-approve mutating tools. Typed native turns reject this mode;
    -- legacy embedding hooks still use the shared interaction vocabulary.
    deriving (Eq, Show)

data NativeShellMode
    = NativeShellNone
    | NativeShellBash
    | NativeShellGhci
    | NativeShellBoth
    deriving (Eq, Show)

-- | A native turn either creates a durable session or resumes one.
-- The sum type prevents ambiguous combinations of save and resume flags.
data NativeSessionTarget
    = NativeNewSession
    | NativeResumeSession !Text
    deriving (Eq, Show)

-- | Native turns exclude CLI-only capabilities such as worktree creation,
-- computer use, prompt files, and implicit non-interactive auto-approval.
-- The executing adapter must supply interactive approval and plan callbacks.
data NativeTurnRequest = NativeTurnRequest
    { nativeTurnPrompt :: !Text
    , nativeTurnImages :: ![ImageAttachment]
    , nativeTurnSession :: !NativeSessionTarget
    , nativeTurnProvider :: !(Maybe Provider)
    , nativeTurnModel :: !(Maybe Text)
    , nativeTurnCwd :: !OsPath
    , nativeTurnEffort :: !(Maybe ReasoningEffort)
    , nativeTurnInteractionMode :: !NativeInteractionMode
    , nativeTurnShellMode :: !NativeShellMode
    }
    deriving (Eq, Show)

-- | Validate the shared native-turn policy before transport queue admission.
-- Workspace, provider, and session resolution remain execution-time concerns.
validateNativeTurnRequest :: NativeTurnRequest -> Either Text ()
validateNativeTurnRequest request
    | request.nativeTurnInteractionMode == NativeYolo =
        Left "typed native turns do not support auto-approval"
    | NativeResumeSession sessionId <- request.nativeTurnSession
    , Text.null (Text.strip sessionId) =
        Left "native resume session id must not be empty"
    | otherwise = Right ()
