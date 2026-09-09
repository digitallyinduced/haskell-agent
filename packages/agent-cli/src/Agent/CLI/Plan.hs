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
    , ProposedPlanSegment(..)
    , ProposedPlanStream
    , initialProposedPlanStream
    , feedProposedPlanStream
    , finishProposedPlanStream
    , proposedPlanVisibleText
    , extractProposedPlan
    , resumedPlanNeedsApproval
    , stripProposedPlan
    , renderPlanMarkdown
    , parsePlanDecisionAnswer
    , planDecisionFollowUp
    ) where

import Agent.CLI.CancelWatch (StdinControl, withStdinPaused)
import Agent.CLI.Input
    ( ReplLine(..)
    , readChoiceSelection
    , readReplLineForProvider
    )
import Agent.CLI.Interrupt (InterruptState)
import Agent.CLI.Markdown (renderMarkdown)
import Agent.CLI.Notification
    ( AttentionRequest(InputRequested, PlanModeRequested)
    , notifyAttention
    )
import Agent.CLI.Picker (PickerKey(..), runOverlay)
import Agent.CLI.Render (renderAssistantTextForHandle)
import Agent.CLI.Style
    ( agentBackground
    , glyphWarn
    , paintBackgroundLines
    , roleMuted
    , rolePrompt
    , roleSuccess
    , roleWarn
    , terminalCyan
    , style
    )
import Agent.Tools.PlanMode
    ( PlanDecision(..)
    , PlanModeHooks(..)
    , planApprovedContinuation
    )
import Agent.Provider (Provider)
import Control.Exception (AsyncException(UserInterrupt))
import Control.Exception.Safe (throwIO)
import Data.Char (toLower)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Console.ANSI
    ( ConsoleIntensity(..)
    , SGR(..)
    )
import System.Console.ANSI.Codes (clearFromCursorToLineEndCode)
import System.IO (Handle, hFlush, hIsTerminalDevice, stderr, stdin)

-- | Build plan-mode prompts. @stdinControl@ pauses the Esc cancel watcher so
-- arrow keys / single-key answers are not stolen mid-turn.
cliPlanHooks :: Provider -> InterruptState -> StdinControl -> IO Bool -> PlanModeHooks
cliPlanHooks provider interrupt stdinControl resolveColor = PlanModeHooks
    { planConfirmEnter = withStdinPaused stdinControl . confirmEnter resolveColor
    , planDecideExit =
        withStdinPaused stdinControl . decideExit provider interrupt resolveColor
    , planAskQuestion = \q opts ->
        withStdinPaused stdinControl
            (askQuestion provider interrupt resolveColor q opts)
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
    PickerKeyLeft -> Right state
    PickerKeyRight -> Right state
    PickerKeyChar c -> case planDecisionForKey c of
        Just PlanApprove -> Left PlanEnter
        Just PlanCancel -> Left PlanStayNormal
        _ -> Right state
    PickerKeyBackspace -> Right state
    PickerKeyTab -> Right state
    PickerKeyBackTab -> Right state

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

decideExit :: Provider -> InterruptState -> IO Bool -> Text -> IO PlanDecision
decideExit provider interrupt resolveColor planBody = do
    color <- resolveColor
    isTty <- hIsTerminalDevice stdin
    putTextLn stderr ""
    putTextLn stderr (roleMuted color "── plan ──")
    renderedPlan <-
        renderAssistantTextForHandle stderr color planBody
    Text.hPutStrLn stderr renderedPlan
    hFlush stderr
    putTextLn stderr (roleMuted color "──────────")
    if not isTty
        then pure PlanCancel
        else do
            notifyAttention stderr InputRequested
            promptDecision provider interrupt color

promptDecision :: Provider -> InterruptState -> Bool -> IO PlanDecision
promptDecision provider interrupt color = do
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
            notes <- readChangeNotes provider interrupt color
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
    PickerKeyLeft -> Right state
    PickerKeyRight -> Right state
    PickerKeyChar c ->
        maybe (Right state) Left (planDecisionForKey c)
    PickerKeyBackspace -> Right state
    PickerKeyTab -> Right state
    PickerKeyBackTab -> Right state

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

readChangeNotes :: Provider -> InterruptState -> Bool -> IO Text
readChangeNotes provider interrupt color = do
    notifyAttention stderr InputRequested
    let chrome =
            rolePrompt color "changes> "
                <> if color
                    then Text.pack clearFromCursorToLineEndCode
                    else mempty
    readReplLineForProvider provider interrupt chrome >>= \case
        ReplEof -> pure "(no notes)"
        ReplQuitInterrupt -> throwIO UserInterrupt
        ReplPasted text ->
            if Text.null (Text.strip text) then pure "(no notes)" else pure (Text.strip text)
        ReplClipboardPaste text _ ->
            if Text.null (Text.strip text)
                then readChangeNotes provider interrupt color
                else pure (Text.strip text)
        ReplClipboardPasteCaptured _ ->
            readChangeNotes provider interrupt color
        ReplRemoveCapturedImage _ ->
            readChangeNotes provider interrupt color
        ReplClipboardPasteOrText _ _ text ->
            if Text.null (Text.strip text)
                then readChangeNotes provider interrupt color
                else pure (Text.strip text)
        ReplCycleMode _ ->
            -- Shift+Tab is idle-prompt only; keep asking for notes.
            readChangeNotes provider interrupt color
        ReplChooseModel _ ->
            readChangeNotes provider interrupt color
        ReplChooseEffort _ ->
            readChangeNotes provider interrupt color
        ReplChooseAccount _ ->
            readChangeNotes provider interrupt color
        ReplRemovePendingImage _ _ ->
            readChangeNotes provider interrupt color
        ReplMeta _ ->
            readChangeNotes provider interrupt color
        ReplText text
            | Text.null (Text.strip text) -> pure "(no notes)"
            | otherwise -> pure (Text.strip text)

askQuestion
    :: Provider
    -> InterruptState
    -> IO Bool
    -> Text
    -> [Text]
    -> IO (Maybe Text)
askQuestion provider interrupt resolveColor question options = do
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
                    readReplLineForProvider provider interrupt chrome >>= \case
                        ReplEof -> pure Nothing
                        ReplQuitInterrupt -> throwIO UserInterrupt
                        ReplPasted text ->
                            if Text.null (Text.strip text)
                                then pure Nothing
                                else pure (Just (Text.strip text))
                        ReplClipboardPaste text _ ->
                            if Text.null (Text.strip text)
                                then askQuestion
                                    provider interrupt resolveColor question []
                                else pure (Just (Text.strip text))
                        ReplClipboardPasteCaptured _ ->
                            askQuestion
                                provider interrupt resolveColor question []
                        ReplRemoveCapturedImage _ ->
                            askQuestion
                                provider interrupt resolveColor question []
                        ReplClipboardPasteOrText _ _ text ->
                            if Text.null (Text.strip text)
                                then askQuestion
                                    provider interrupt resolveColor question []
                                else pure (Just (Text.strip text))
                        ReplCycleMode _ ->
                            askQuestion
                                provider interrupt resolveColor question []
                        ReplChooseModel _ ->
                            askQuestion
                                provider interrupt resolveColor question []
                        ReplChooseEffort _ ->
                            askQuestion
                                provider interrupt resolveColor question []
                        ReplChooseAccount _ ->
                            askQuestion
                                provider interrupt resolveColor question []
                        ReplRemovePendingImage _ _ ->
                            askQuestion
                                provider interrupt resolveColor question []
                        ReplMeta _ ->
                            askQuestion
                                provider interrupt resolveColor question []
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
            , terminalCyan
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

-- | Whether a resumed transcript ends with a plan that still needs the
-- user's decision.  Codex presents its plan in assistant text rather than
-- through an explicit exit tool, so this is the durable signal available
-- when rebuilding the in-memory plan-mode state on resume.
resumedPlanNeedsApproval :: [Maybe Text] -> Bool
resumedPlanNeedsApproval assistantTexts =
    case reverse [text | Just text <- assistantTexts] of
        lastText : _ -> isJust (extractProposedPlan lastText)
        _ -> False

-- | Typed pieces produced by the Codex plan protocol stream.
data ProposedPlanSegment
    = AssistantText !Text
    | ProposedPlanStart
    | ProposedPlanDelta !Text
    | ProposedPlanEnd
    deriving (Eq, Show)

-- | Incremental parser state. A possibly incomplete line is held until the
-- parser can decide whether it is protocol markup. This mirrors Codex's
-- tagged-line protocol: tags only have control meaning when alone on a line.
data ProposedPlanStream = ProposedPlanStream
    { proposedPlanInside :: !Bool
    , proposedPlanPendingLine :: !Text
    , proposedPlanPassingLine :: !Bool
    } deriving (Eq, Show)

initialProposedPlanStream :: ProposedPlanStream
initialProposedPlanStream = ProposedPlanStream False "" False

feedProposedPlanStream
    :: ProposedPlanStream
    -> Text
    -> (ProposedPlanStream, [ProposedPlanSegment])
feedProposedPlanStream state chunk =
    consumeStream
        state.proposedPlanInside
        state.proposedPlanPassingLine
        (state.proposedPlanPendingLine <> chunk)

finishProposedPlanStream :: ProposedPlanStream -> [ProposedPlanSegment]
finishProposedPlanStream state =
    segments <> [ProposedPlanEnd | inside]
  where
    (inside, segments)
        | state.proposedPlanPassingLine =
            ( state.proposedPlanInside
            , textSegment
                state.proposedPlanInside
                state.proposedPlanPendingLine
            )
        | otherwise =
            consumeLine
                state.proposedPlanInside
                state.proposedPlanPendingLine

proposedPlanVisibleText :: [ProposedPlanSegment] -> Text
proposedPlanVisibleText = Text.concat . foldr collect []
  where
    collect (AssistantText text) rest = text : rest
    collect _ rest = rest

consumeStream
    :: Bool
    -> Bool
    -> Text
    -> (ProposedPlanStream, [ProposedPlanSegment])
consumeStream inside passing input
    | passing =
        case Text.breakOn "\n" input of
            (line, rest)
                | Text.null rest ->
                    ( ProposedPlanStream inside "" True
                    , textSegment inside line
                    )
                | otherwise ->
                    let completeLine = line <> "\n"
                        remaining = Text.drop 1 rest
                        (nextState, following) =
                            consumeStream inside False remaining
                    in ( nextState
                       , textSegment inside completeLine <> following
                       )
    | otherwise =
    case Text.breakOn "\n" input of
        (_, rest)
            | Text.null rest && couldBecomeControlLine inside input ->
                (ProposedPlanStream inside input False, [])
            | Text.null rest ->
                ( ProposedPlanStream inside "" True
                , textSegment inside input
                )
        (line, rest) ->
            let completeLine = line <> "\n"
                remaining = Text.drop 1 rest
                (nextInside, segments) = consumeLine inside completeLine
                (nextState, following) =
                    consumeStream nextInside False remaining
            in (nextState, segments <> following)

couldBecomeControlLine :: Bool -> Text -> Bool
couldBecomeControlLine inside line =
    couldBecomeTag expected (Text.dropWhile isHorizontalSpace line)
  where
    expected = if inside then closeTag else openTag

couldBecomeTag :: Text -> Text -> Bool
couldBecomeTag tag candidate =
    candidate `Text.isPrefixOf` tag
        || ( tag `Text.isPrefixOf` candidate
            && Text.all isHorizontalSpace
                (Text.drop (Text.length tag) candidate)
           )

isHorizontalSpace :: Char -> Bool
isHorizontalSpace character =
    character == ' ' || character == '\t' || character == '\r'

consumeLine :: Bool -> Text -> (Bool, [ProposedPlanSegment])
consumeLine inside line
    | stripped == openTag && not inside =
        (True, [ProposedPlanStart])
    | stripped == closeTag && inside =
        (False, [ProposedPlanEnd])
    | otherwise =
        (inside, textSegment inside line)
  where
    stripped = Text.strip line

textSegment :: Bool -> Text -> [ProposedPlanSegment]
textSegment _ text | Text.null text = []
textSegment True text = [ProposedPlanDelta text]
textSegment False text = [AssistantText text]

openTag :: Text
openTag = "<proposed_plan>"

closeTag :: Text
closeTag = "</proposed_plan>"

-- | Remove Codex plan protocol blocks from assistant presentation. The plan
-- body is presented through the typed plan approval hook instead.
stripProposedPlan :: Text -> Text
stripProposedPlan text =
    proposedPlanVisibleText
        (segments <> finishProposedPlanStream state)
  where
    (state, segments) =
        feedProposedPlanStream initialProposedPlanStream text

putTextLn :: Handle -> Text -> IO ()
putTextLn handle text = do
    Text.hPutStrLn handle text
    hFlush handle
