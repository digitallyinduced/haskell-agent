-- | Idle REPL mode transitions and compact status-line formatting.
module Agent.CLI.Status
    ( applyReplMode
    , cycleReplInteraction
    , formatReplStatusLine
    , formatTokenUsage
    ) where

import Agent.CLI.Input (terminalTextWidth, truncateDisplayText)
import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.CLI.Project (saveProjectAutoApprove)
import Agent.CLI.ReplMode
    ( ReplMode(..)
    , cycleReplMode
    , replModeFromState
    , replModeLabel
    )
import Agent.CLI.Style (roleMuted)
import Agent.Loop (TokenUsage(..), emptyTokenUsage)
import System.OsPath (OsPath)
import Agent.Tools.PlanMode
    ( PlanModeEnv(..)
    , PlanModeState(..)
    , deactivatePlanMode
    , setPlanModeState
    )
import Control.Monad (when)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Idle prompt chrome: model, reasoning effort, interaction mode on the
-- left; session token totals right-aligned when the TTY width is known.
formatReplStatusLine
    :: Bool
    -> Maybe Int
    -> Text
    -> Text
    -> ReplMode
    -> TokenUsage
    -> Text
formatReplStatusLine color width model effort mode usage =
    let left = "  " <> model <> " · " <> effort <> " · " <> replModeLabel mode
        right = formatTokenUsage usage
        padded = case width of
            Just cols | cols > 0 ->
                fitStatusLine cols left right
            _ | Text.null right -> left
            _ -> left <> "  " <> right
    in roleMuted color padded

-- | Keep the status on one physical row so composer redraws never inherit a
-- wrapped status line. Prefer interaction state over token totals.
fitStatusLine :: Int -> Text -> Text -> Text
fitStatusLine cols left right
    | cols <= 0 = ""
    | Text.null right = truncateDisplayText cols left
    | leftWidth + 1 + rightWidth <= cols =
        left <> Text.replicate (cols - leftWidth - rightWidth) " " <> right
    | leftWidth <= cols = left
    | otherwise = truncateDisplayText cols left
  where
    leftWidth = terminalTextWidth left
    rightWidth = terminalTextWidth right

-- | Apply a cycled idle mode: plan pending vs always-approve vs ask.
-- Always-approve still persists to project settings, matching /always-approve.
applyReplMode
    :: PlanModeEnv
    -> IORef ApprovalPolicy
    -> OsPath
    -> ReplMode
    -> IO ()
applyReplMode planMode policyRef projectRoot = \case
    ReplModePlan ->
        setPlanModeState planMode PlanPending
    ReplModeAlwaysApprove -> do
        deactivatePlanMode planMode
        writeIORef policyRef ApproveAll
        saveProjectAutoApprove projectRoot True
    ReplModeNormal -> do
        deactivatePlanMode planMode
        current <- readIORef policyRef
        when (current == ApproveAll) do
            writeIORef policyRef PromptMutating
            saveProjectAutoApprove projectRoot False

-- | Pure next-mode helper used by tests and the Shift+Tab handler.
cycleReplInteraction :: PlanModeState -> ApprovalPolicy -> ReplMode
cycleReplInteraction planState policy =
    cycleReplMode (replModeFromState planState policy)

-- | Compact session totals: @1.2k in · 340 out@. Cached tokens are shown
-- only when the provider reported a non-zero cache hit.
formatTokenUsage :: TokenUsage -> Text
formatTokenUsage usage
    | usage == emptyTokenUsage = ""
    | otherwise =
        formatTokenCount usage.inputTokens
            <> " in · "
            <> formatTokenCount usage.outputTokens
            <> " out"
            <> cachedSuffix
  where
    cachedSuffix
        | usage.cachedTokens > 0 =
            " · " <> formatTokenCount usage.cachedTokens <> " cached"
        | otherwise = ""

formatTokenCount :: Int -> Text
formatTokenCount n
    | n < 0 = "0"
    | n < 1000 = Text.pack (show n)
    | n < 10000 =
        let tenths = (n + 50) `div` 100
            whole = tenths `div` 10
            frac = tenths `mod` 10
        in Text.pack (show whole <> "." <> show frac <> "k")
    | n < 1000000 =
        Text.pack (show ((n + 500) `div` 1000) <> "k")
    | n < 10000000 =
        let tenths = (n + 50000) `div` 100000
            whole = tenths `div` 10
            frac = tenths `mod` 10
        in Text.pack (show whole <> "." <> show frac <> "M")
    | otherwise =
        Text.pack (show ((n + 500000) `div` 1000000) <> "M")
