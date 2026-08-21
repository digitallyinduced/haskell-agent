-- | Interactive plan-mode prompts: enter confirmation and approve /
-- request-changes / cancel when a plan is presented.
module Agent.CLI.Plan
    ( cliPlanHooks
    , extractProposedPlan
    , stripProposedPlan
    , renderPlanMarkdown
    , parsePlanDecisionAnswer
    ) where

import Agent.CLI.CancelWatch (withStdinPaused)
import Agent.CLI.Input
    ( ReplLine(..)
    , readApprovalLine
    , readChoiceSelection
    , readReplLine
    )
import Agent.CLI.Interrupt (InterruptState)
import Agent.CLI.Markdown (renderMarkdown)
import Agent.CLI.Style
    ( agentBackground
    , paintBackgroundLines
    , roleMuted
    , rolePrompt
    , roleSuccess
    , roleWarn
    , solarizedCyan
    , style
    )
import Agent.Tools.PlanMode (PlanDecision(..), PlanModeHooks(..))
import Control.Exception (AsyncException(UserInterrupt))
import Control.Exception.Safe (throwIO)
import Data.IORef (IORef)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Console.ANSI
    ( ConsoleIntensity(..)
    , ConsoleLayer(..)
    , SGR(..)
    )
import System.Console.ANSI.Codes (clearFromCursorToLineEndCode)
import System.IO (Handle, hFlush, hIsTerminalDevice, stderr, stdin)

-- | Build plan-mode prompts. @escPaused@ pauses the Esc cancel watcher so
-- arrow keys / single-key answers are not stolen mid-turn.
cliPlanHooks :: InterruptState -> IORef Bool -> IO Bool -> PlanModeHooks
cliPlanHooks interrupt escPaused resolveColor = PlanModeHooks
    { planConfirmEnter = withStdinPaused escPaused . confirmEnter resolveColor
    , planDecideExit = withStdinPaused escPaused . decideExit interrupt resolveColor
    , planAskQuestion = \q opts ->
        withStdinPaused escPaused (askQuestion interrupt resolveColor q opts)
    }

confirmEnter :: IO Bool -> Text -> IO Bool
confirmEnter resolveColor reason = do
    color <- resolveColor
    isTty <- hIsTerminalDevice stdin
    if not isTty
        then pure False
        else do
            putTextLn stderr (roleMuted color reason)
            let question = roleWarn color "Enter plan mode? [y/N] "
            readApprovalLine question >>= \case
                Nothing -> pure False
                Just raw ->
                    pure $ Text.toLower (Text.strip raw) `elem` ["y", "yes"]

decideExit :: InterruptState -> IO Bool -> Text -> IO PlanDecision
decideExit interrupt resolveColor planBody = do
    color <- resolveColor
    isTty <- hIsTerminalDevice stdin
    putTextLn stderr ""
    putTextLn stderr (roleMuted color "── plan ──")
    Text.hPutStrLn stderr (renderPlanMarkdown color planBody)
    hFlush stderr
    putTextLn stderr (roleMuted color "──────────")
    if not isTty
        then pure PlanCancel
        else promptDecision interrupt color

promptDecision :: InterruptState -> Bool -> IO PlanDecision
promptDecision interrupt color = do
    let question =
            roleWarn color
                "Plan: [a]pprove / [s] request changes / [q] cancel? "
    readApprovalLine question >>= \case
        Nothing -> pure PlanCancel
        Just raw -> case parsePlanDecisionAnswer raw of
            Just PlanApprove -> do
                putTextLn stderr (roleSuccess color "plan approved")
                pure PlanApprove
            Just PlanCancel -> do
                putTextLn stderr (roleMuted color "plan cancelled")
                pure PlanCancel
            Just (PlanRequestChanges _) -> do
                notes <- readChangeNotes interrupt color
                pure (PlanRequestChanges notes)
            Nothing -> do
                putTextLn stderr (roleMuted color "enter a, s, or q")
                promptDecision interrupt color

readChangeNotes :: InterruptState -> Bool -> IO Text
readChangeNotes interrupt color = do
    let chrome =
            rolePrompt color "changes> "
                <> if color
                    then Text.pack clearFromCursorToLineEndCode
                    else mempty
    readReplLine interrupt chrome >>= \case
        ReplEof -> pure "(no notes)"
        ReplQuitInterrupt -> throwIO UserInterrupt
        ReplPasted text ->
            if Text.null (Text.strip text) then pure "(no notes)" else pure (Text.strip text)
        ReplCycleMode _ ->
            -- Shift+Tab is idle-prompt only; keep asking for notes.
            readChangeNotes interrupt color
        ReplText text
            | Text.null (Text.strip text) -> pure "(no notes)"
            | otherwise -> pure (Text.strip text)

askQuestion :: InterruptState -> IO Bool -> Text -> [Text] -> IO (Maybe Text)
askQuestion interrupt resolveColor question options = do
    color <- resolveColor
    isTty <- hIsTerminalDevice stdin
    putTextLn stderr (roleMuted color question)
    if not isTty
        then pure Nothing
        else case options of
            [] -> do
                let chrome =
                        rolePrompt color "answer> "
                            <> if color
                                then Text.pack clearFromCursorToLineEndCode
                                else mempty
                readReplLine interrupt chrome >>= \case
                    ReplEof -> pure Nothing
                    ReplQuitInterrupt -> throwIO UserInterrupt
                    ReplPasted text ->
                        if Text.null (Text.strip text)
                            then pure Nothing
                            else pure (Just (Text.strip text))
                    ReplCycleMode _ ->
                        askQuestion interrupt resolveColor question []
                    ReplText text
                        | Text.null (Text.strip text) -> pure Nothing
                        | otherwise -> pure (Just (Text.strip text))
            opts ->
                readChoiceSelection (formatChoiceLine color) opts

formatChoiceLine :: Bool -> Bool -> Text -> Text
formatChoiceLine color selected label
    | selected =
        style color
            [ SetConsoleIntensity BoldIntensity
            , SetRGBColor Foreground solarizedCyan
            ]
            label
    | otherwise = roleMuted color label

renderPlanMarkdown :: Bool -> Text -> Text
renderPlanMarkdown color text =
    paintBackgroundLines color agentBackground (renderMarkdown color text)

parsePlanDecisionAnswer :: Text -> Maybe PlanDecision
parsePlanDecisionAnswer raw = case Text.toLower (Text.strip raw) of
    "a" -> Just PlanApprove
    "approve" -> Just PlanApprove
    "y" -> Just PlanApprove
    "yes" -> Just PlanApprove
    "s" -> Just (PlanRequestChanges "")
    "c" -> Just (PlanRequestChanges "")
    "changes" -> Just (PlanRequestChanges "")
    "r" -> Just (PlanRequestChanges "")
    "q" -> Just PlanCancel
    "cancel" -> Just PlanCancel
    "n" -> Just PlanCancel
    "no" -> Just PlanCancel
    _ -> Nothing

-- | Pull the first @\<proposed_plan\>…\</proposed_plan\>@ block (Codex).
extractProposedPlan :: Text -> Maybe Text
extractProposedPlan text =
    case Text.breakOn openTag text of
        (_, afterOpen)
            | Text.null afterOpen -> Nothing
            | otherwise ->
                let rest = Text.drop (Text.length openTag) afterOpen
                in case Text.breakOn closeTag rest of
                    (inner, afterClose)
                        | Text.null afterClose -> Nothing
                        | otherwise -> Just (Text.strip inner)
  where
    openTag = "<proposed_plan>"
    closeTag = "</proposed_plan>"

-- | Remove proposed_plan tags for display after the approval UI shows the body.
stripProposedPlan :: Text -> Text
stripProposedPlan text =
    case Text.breakOn "<proposed_plan>" text of
        (before, afterOpen)
            | Text.null afterOpen -> text
            | otherwise ->
                let rest = Text.drop (Text.length "<proposed_plan>") afterOpen
                    (_, afterClose) = Text.breakOn "</proposed_plan>" rest
                    after =
                        if Text.null afterClose
                            then ""
                            else Text.drop (Text.length "</proposed_plan>") afterClose
                in Text.strip (before <> after)

putTextLn :: Handle -> Text -> IO ()
putTextLn handle text = do
    Text.hPutStrLn handle text
    hFlush handle
