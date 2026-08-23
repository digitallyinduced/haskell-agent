-- | Interactive plan-mode prompts: enter confirmation and approve /
-- request-changes / cancel when a plan is presented.
module Agent.CLI.Plan
    ( cliPlanHooks
    , PlanEnterChoice(..)
    , PlanEnterState(..)
    , PlanExitState(..)
    , applyPlanEnterKey
    , applyPlanExitKey
    , initialPlanEnterState
    , initialPlanExitState
    , renderPlanEnterFrame
    , renderPlanExitFrame
    , extractProposedPlan
    , stripProposedPlan
    , renderPlanMarkdown
    , parsePlanDecisionAnswer
    , planDecisionFollowUp
    ) where

import Agent.CLI.CancelWatch (withStdinPaused)
import Agent.CLI.Input
    ( ReplLine(..)
    , readChoiceSelection
    , readReplLine
    )
import Agent.CLI.Interrupt (InterruptState)
import Agent.CLI.Markdown (renderMarkdown)
import Agent.CLI.Notification
    ( AttentionRequest(InputRequested, PlanModeRequested)
    , notifyAttention
    )
import Agent.CLI.Picker (PickerKey(..), runOverlay)
import Agent.CLI.Style
    ( agentBackground
    , glyphWarn
    , paintBackgroundLines
    , roleMuted
    , rolePrompt
    , roleSuccess
    , roleWarn
    , solarizedCyan
    , style
    )
import Agent.Tools.PlanMode
    ( PlanDecision(..)
    , PlanModeHooks(..)
    , planApprovedContinuation
    )
import Control.Exception (AsyncException(UserInterrupt))
import Control.Exception.Safe (throwIO)
import Data.Char (toLower)
import Data.IORef (IORef)
import Data.Maybe (fromMaybe)
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

data PlanEnterChoice = PlanEnter | PlanStayNormal
    deriving (Eq, Show)

data PlanEnterState = PlanEnterState Text Int
    deriving (Eq, Show)

data PlanExitState = PlanExitState Int
    deriving (Eq, Show)

initialPlanEnterState :: Text -> PlanEnterState
initialPlanEnterState reason = PlanEnterState reason 0

initialPlanExitState :: PlanExitState
initialPlanExitState = PlanExitState 0

confirmEnter :: IO Bool -> Text -> IO Bool
confirmEnter resolveColor reason = do
    color <- resolveColor
    isTty <- hIsTerminalDevice stdin
    if not isTty
        then pure False
        else do
            notifyAttention stderr PlanModeRequested
            result <-
                runOverlay
                    (renderPlanEnterFrame color)
                    applyPlanEnterKey
                    (initialPlanEnterState reason)
            pure (fromMaybe PlanStayNormal result == PlanEnter)

applyPlanEnterKey
    :: PickerKey
    -> PlanEnterState
    -> Either PlanEnterChoice PlanEnterState
applyPlanEnterKey key state@(PlanEnterState reason index) = case key of
    PickerKeyCancel -> Left PlanStayNormal
    PickerKeyConfirm -> Left (enterChoiceFromIndex index)
    PickerKeyUp -> Right (PlanEnterState reason (movePlanIndex 2 (-1) index))
    PickerKeyDown -> Right (PlanEnterState reason (movePlanIndex 2 1 index))
    PickerKeyChar c -> case planDecisionForKey c of
        Just PlanApprove -> Left PlanEnter
        Just PlanCancel -> Left PlanStayNormal
        _ -> Right state
    PickerKeyBackspace -> Right state

renderPlanEnterFrame :: Bool -> PlanEnterState -> Text
renderPlanEnterFrame color (PlanEnterState reason index) =
    Text.intercalate "\n"
        [ roleWarn color (glyphWarn <> "Enter plan mode?")
        , roleMuted color reason
        , renderPlanRow color (index == 0) "Enter plan mode"
        , renderPlanRow color (index == 1) "Stay in normal mode"
        , roleMuted color
            "↑↓/jk or scroll · click/enter · a/y enter · n/q/esc stay"
        ]

enterChoiceFromIndex :: Int -> PlanEnterChoice
enterChoiceFromIndex 0 = PlanEnter
enterChoiceFromIndex _ = PlanStayNormal

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
        else do
            notifyAttention stderr InputRequested
            promptDecision interrupt color

promptDecision :: InterruptState -> Bool -> IO PlanDecision
promptDecision interrupt color = do
    result <-
        runOverlay
            (renderPlanExitFrame color)
            applyPlanExitKey
            initialPlanExitState
    case fromMaybe PlanCancel result of
        PlanApprove -> do
            putTextLn stderr (roleSuccess color "plan approved")
            pure PlanApprove
        PlanCancel -> do
            putTextLn stderr (roleMuted color "plan cancelled")
            pure PlanCancel
        PlanRequestChanges _ -> do
            notes <- readChangeNotes interrupt color
            pure (PlanRequestChanges notes)

applyPlanExitKey
    :: PickerKey
    -> PlanExitState
    -> Either PlanDecision PlanExitState
applyPlanExitKey key state@(PlanExitState index) = case key of
    PickerKeyCancel -> Left PlanCancel
    PickerKeyConfirm -> Left (exitChoiceFromIndex index)
    PickerKeyUp -> Right (PlanExitState (movePlanIndex 3 (-1) index))
    PickerKeyDown -> Right (PlanExitState (movePlanIndex 3 1 index))
    PickerKeyChar c ->
        maybe (Right state) Left (planDecisionForKey c)
    PickerKeyBackspace -> Right state

renderPlanExitFrame :: Bool -> PlanExitState -> Text
renderPlanExitFrame color (PlanExitState index) =
    Text.intercalate "\n"
        [ roleWarn color (glyphWarn <> "Ready to implement this plan?")
        , renderPlanRow color (index == 0) "Approve and implement"
        , renderPlanRow color (index == 1) "Request changes"
        , renderPlanRow color (index == 2) "Cancel plan"
        , roleMuted color
            "↑↓/jk or scroll · click/enter · a approve · s changes · q/esc cancel"
        ]

exitChoiceFromIndex :: Int -> PlanDecision
exitChoiceFromIndex = \case
    0 -> PlanApprove
    1 -> PlanRequestChanges ""
    _ -> PlanCancel

movePlanIndex :: Int -> Int -> Int -> Int
movePlanIndex count delta index = (index + delta) `mod` count

renderPlanRow :: Bool -> Bool -> Text -> Text
renderPlanRow color selected label =
    let cursor = if selected then roleWarn color "› " else "  "
        body = if selected then roleSuccess color label else roleMuted color label
    in cursor <> body

readChangeNotes :: InterruptState -> Bool -> IO Text
readChangeNotes interrupt color = do
    notifyAttention stderr InputRequested
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
        ReplClipboardPaste text _ ->
            if Text.null (Text.strip text)
                then readChangeNotes interrupt color
                else pure (Text.strip text)
        ReplClipboardPasteOrText _ text ->
            if Text.null (Text.strip text)
                then readChangeNotes interrupt color
                else pure (Text.strip text)
        ReplCycleMode _ ->
            -- Shift+Tab is idle-prompt only; keep asking for notes.
            readChangeNotes interrupt color
        ReplChooseModel _ ->
            readChangeNotes interrupt color
        ReplChooseEffort _ ->
            readChangeNotes interrupt color
        ReplChooseAccount _ ->
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
        else do
            notifyAttention stderr InputRequested
            case options of
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
                        ReplClipboardPaste text _ ->
                            if Text.null (Text.strip text)
                                then askQuestion interrupt resolveColor question []
                                else pure (Just (Text.strip text))
                        ReplClipboardPasteOrText _ text ->
                            if Text.null (Text.strip text)
                                then askQuestion interrupt resolveColor question []
                                else pure (Just (Text.strip text))
                        ReplCycleMode _ ->
                            askQuestion interrupt resolveColor question []
                        ReplChooseModel _ ->
                            askQuestion interrupt resolveColor question []
                        ReplChooseEffort _ ->
                            askQuestion interrupt resolveColor question []
                        ReplChooseAccount _ ->
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
parsePlanDecisionAnswer raw =
    case Text.toLower (Text.strip raw) of
        "approve" -> Just PlanApprove
        "yes" -> Just PlanApprove
        "changes" -> Just (PlanRequestChanges "")
        "q" -> Just PlanCancel
        "cancel" -> Just PlanCancel
        "no" -> Just PlanCancel
        answer -> case Text.unpack answer of
            [key] -> planDecisionForKey key
            _ -> Nothing

planDecisionForKey :: Char -> Maybe PlanDecision
planDecisionForKey key = case toLower key of
    'a' -> Just PlanApprove
    'y' -> Just PlanApprove
    's' -> Just (PlanRequestChanges "")
    'c' -> Just (PlanRequestChanges "")
    'r' -> Just (PlanRequestChanges "")
    'n' -> Just PlanCancel
    _ -> Nothing

-- | Build the synthetic turn that follows a plan decision.
-- Approval and requested changes continue immediately; cancellation stops.
planDecisionFollowUp :: PlanDecision -> Maybe Text
planDecisionFollowUp PlanApprove = Just planApprovedContinuation
planDecisionFollowUp (PlanRequestChanges notes) =
    Just $ Text.intercalate "\n"
        [ "The user requested changes to the plan. Stay in plan mode and revise."
        , "Feedback:"
        , notes
        ]
planDecisionFollowUp PlanCancel = Nothing

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
