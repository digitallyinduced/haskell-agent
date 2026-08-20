-- | Interactive plan-mode prompts: enter confirmation and approve /
-- request-changes / cancel when a plan is presented.
module Agent.CLI.Plan
    ( cliPlanHooks
    , extractProposedPlan
    , stripProposedPlan
    , parsePlanDecisionAnswer
    ) where

import Agent.CLI.CancelWatch (withStdinPaused)
import Agent.CLI.Input
    ( readApprovalLine
    , readChoiceSelection
    , readReplLine
    )
import Agent.CLI.Style
    ( roleMuted
    , rolePrompt
    , roleSuccess
    , roleWarn
    , solarizedCyan
    , style
    )
import Agent.Tools.PlanMode (PlanDecision(..), PlanModeHooks(..))
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
cliPlanHooks :: IORef Bool -> IO Bool -> PlanModeHooks
cliPlanHooks escPaused resolveColor = PlanModeHooks
    { planConfirmEnter = withStdinPaused escPaused . confirmEnter resolveColor
    , planDecideExit = withStdinPaused escPaused . decideExit resolveColor
    , planAskQuestion = \q opts ->
        withStdinPaused escPaused (askQuestion resolveColor q opts)
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

decideExit :: IO Bool -> Text -> IO PlanDecision
decideExit resolveColor planBody = do
    color <- resolveColor
    isTty <- hIsTerminalDevice stdin
    putTextLn stderr ""
    putTextLn stderr (roleMuted color "── plan ──")
    Text.hPutStrLn stderr planBody
    hFlush stderr
    putTextLn stderr (roleMuted color "──────────")
    if not isTty
        then pure PlanCancel
        else promptDecision color

promptDecision :: Bool -> IO PlanDecision
promptDecision color = do
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
                notes <- readChangeNotes color
                pure (PlanRequestChanges notes)
            Nothing -> do
                putTextLn stderr (roleMuted color "enter a, s, or q")
                promptDecision color

readChangeNotes :: Bool -> IO Text
readChangeNotes color = do
    let chrome =
            rolePrompt color "changes> "
                <> if color
                    then Text.pack clearFromCursorToLineEndCode
                    else mempty
    readReplLine chrome >>= \case
        Nothing -> pure "(no notes)"
        Just text
            | Text.null (Text.strip text) -> pure "(no notes)"
            | otherwise -> pure (Text.strip text)

askQuestion :: IO Bool -> Text -> [Text] -> IO (Maybe Text)
askQuestion resolveColor question options = do
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
                readReplLine chrome >>= \case
                    Nothing -> pure Nothing
                    Just text
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
