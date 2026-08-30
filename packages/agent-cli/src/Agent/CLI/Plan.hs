-- | Interactive plan-mode prompts: enter confirmation and approve /
-- revise / abandon when a plan is presented.
module Agent.CLI.Plan
    ( cliPlanHooks
    , PlanEnterChoice(..)
    , PlanEnterState(..)
    , PlanExitState(..)
    , TerminalPlanReviewState(..)
    , TerminalPlanReviewDecision(..)
    , PlanReviewAdapter(..)
    , PlanReviewAdapterError(..)
    , PlanReviewResolution(..)
    , PlanReviewFeedback(..)
    , PlanLineComment(..)
    , runPlanReviewAdapter
    , resolveTerminalPlanReview
    , renderPlanReviewAdapterError
    , renderPlanReviewFeedback
    , parsePlanReviewFeedback
    , applyPlanEnterKey
    , applyPlanExitKey
    , applyTerminalPlanReviewKey
    , initialPlanEnterState
    , initialPlanExitState
    , initialTerminalPlanReviewState
    , renderPlanEnterFrame
    , renderPlanExitFrame
    , renderTerminalPlanReviewFrame
    , ProposedPlanParseError(..)
    , parseProposedPlan
    , renderProposedPlanParseError
    , extractProposedPlan
    , stripProposedPlan
    , formatPlanPreview
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
    , terminalCyan
    , style
    )
import Agent.OsPath (toText)
import Agent.Tools.PlanMode
    ( PlanDecision(..)
    , PlanModeHooks(..)
    , planApprovedContinuation
    )
import Control.Applicative ((<|>))
import Control.Exception (AsyncException(UserInterrupt))
import Control.Exception.Safe (throwIO)
import Data.Char (toLower)
import Data.IORef (IORef)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Console.ANSI
    ( ConsoleIntensity(..)
    , SGR(..)
    )
import System.Console.ANSI.Codes (clearFromCursorToLineEndCode)
import System.IO (Handle, hFlush, hIsTerminalDevice, stderr, stdin)
import System.OsPath (OsPath)

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

-- | Typed outcomes for the terminal reviewer. The current core hook still
-- consumes 'PlanDecision'; this type preserves the distinctions needed by the
-- durable interaction adapter without overloading abandon for a closed UI.
data TerminalPlanReviewDecision
    = TerminalPlanApprove
    | TerminalPlanApproveAnyway
      -- ^ Advisory validation warnings were explicitly accepted.
    | TerminalPlanRevise !PlanReviewFeedback
    | TerminalPlanAbandon
    | TerminalPlanDefer
    deriving (Eq, Show)

data TerminalPlanReviewState = TerminalPlanReviewState
    { terminalReviewWarnings :: ![Text]
    , terminalReviewIndex :: !Int
    }
    deriving (Eq, Show)

-- | Persistence and presentation boundary for a plan review. The snapshot is
-- written and read back before the presenter is invoked, so write failures,
-- unreadable plans, and stale contents all fail closed without opening an
-- approval surface.
data PlanReviewAdapter = PlanReviewAdapter
    { planReviewWriteSnapshot :: !(Text -> IO (Either Text ()))
    , planReviewReadSnapshot :: !(IO (Either Text Text))
    , planReviewPresentSnapshot
        :: !(Text -> [Text] -> IO TerminalPlanReviewDecision)
    }

data PlanReviewAdapterError
    = PlanReviewWriteFailed !Text
    | PlanReviewReadFailed !Text
    | PlanReviewStale
    | PlanReviewWarningsNotAcknowledged ![Text]
    deriving (Eq, Show)

-- | Lifecycle effects of a typed review outcome. Closing the review is
-- intentionally distinct from abandoning it: defer leaves Plan Mode active.
data PlanReviewResolution = PlanReviewResolution
    { planReviewShouldDeactivate :: !Bool
    , planReviewContinuation :: !(Maybe Text)
    }
    deriving (Eq, Show)

data PlanReviewFeedback = PlanReviewFeedback
    { planFeedbackOverall :: !Text
    , planFeedbackLineComments :: ![PlanLineComment]
    }
    deriving (Eq, Show)

data PlanLineComment = PlanLineComment
    { planCommentStartLine :: !Int
    , planCommentEndLine :: !Int
    , planCommentBody :: !Text
    }
    deriving (Eq, Show)

initialPlanEnterState :: Text -> PlanEnterState
initialPlanEnterState reason = PlanEnterState reason 0

initialPlanExitState :: PlanExitState
initialPlanExitState = PlanExitState 0

initialTerminalPlanReviewState :: [Text] -> TerminalPlanReviewState
initialTerminalPlanReviewState warnings =
    TerminalPlanReviewState
        { terminalReviewWarnings = warnings
        , terminalReviewIndex = 0
        }

-- | Persist, verify, then present one immutable plan snapshot. Advisory
-- warnings change the approval label/outcome but never make the document
-- invalid.
runPlanReviewAdapter
    :: PlanReviewAdapter
    -> [Text]
    -> Text
    -> IO (Either PlanReviewAdapterError TerminalPlanReviewDecision)
runPlanReviewAdapter adapter warnings expectedPlan =
    adapter.planReviewWriteSnapshot expectedPlan >>= \case
        Left writeError ->
            pure (Left (PlanReviewWriteFailed writeError))
        Right () ->
            adapter.planReviewReadSnapshot >>= \case
                Left readError ->
                    pure (Left (PlanReviewReadFailed readError))
                Right persistedPlan
                    | persistedPlan /= expectedPlan ->
                        pure (Left PlanReviewStale)
                    | otherwise -> do
                        decision <-
                            adapter.planReviewPresentSnapshot
                                persistedPlan
                                warnings
                        case decision of
                            TerminalPlanDefer ->
                                pure (Right TerminalPlanDefer)
                            TerminalPlanAbandon ->
                                pure (Right TerminalPlanAbandon)
                            TerminalPlanApprove
                                | not (null warnings) ->
                                    pure
                                        (Left
                                            (PlanReviewWarningsNotAcknowledged
                                                warnings))
                            _ ->
                                adapter.planReviewReadSnapshot >>= \case
                                    Left readError ->
                                        pure
                                            (Left
                                                (PlanReviewReadFailed
                                                    readError))
                                    Right currentPlan
                                        | currentPlan /= persistedPlan ->
                                            pure (Left PlanReviewStale)
                                        | otherwise ->
                                            pure (Right decision)

renderPlanReviewAdapterError :: PlanReviewAdapterError -> Text
renderPlanReviewAdapterError = \case
    PlanReviewWriteFailed detail ->
        "the plan could not be written: " <> detail
    PlanReviewReadFailed detail ->
        "the written plan could not be read back: " <> detail
    PlanReviewStale ->
        "the plan changed before the review decision could be accepted"
    PlanReviewWarningsNotAcknowledged _ ->
        "the plan has advisory warnings that require Approve anyway"

resolveTerminalPlanReview
    :: TerminalPlanReviewDecision
    -> PlanReviewResolution
resolveTerminalPlanReview = \case
    TerminalPlanApprove ->
        approvedResolution
    TerminalPlanApproveAnyway ->
        approvedResolution
    TerminalPlanRevise feedback ->
        PlanReviewResolution
            { planReviewShouldDeactivate = False
            , planReviewContinuation =
                planDecisionFollowUp
                    (PlanRequestChanges
                        (renderPlanReviewFeedback feedback))
            }
    TerminalPlanAbandon ->
        PlanReviewResolution
            { planReviewShouldDeactivate = True
            , planReviewContinuation = Nothing
            }
    TerminalPlanDefer ->
        PlanReviewResolution
            { planReviewShouldDeactivate = False
            , planReviewContinuation = Nothing
            }
  where
    approvedResolution =
        PlanReviewResolution
            { planReviewShouldDeactivate = True
            , planReviewContinuation =
                planDecisionFollowUp PlanApprove
            }

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

-- | Warnings-ready terminal review reducer. This stays separate from the
-- legacy 'PlanDecision' picker until the durable core hook carries typed
-- outcomes; notably, Esc/Close defers instead of abandoning the plan.
applyTerminalPlanReviewKey
    :: PickerKey
    -> TerminalPlanReviewState
    -> Either TerminalPlanReviewDecision TerminalPlanReviewState
applyTerminalPlanReviewKey key state = case key of
    PickerKeyCancel -> Left TerminalPlanDefer
    PickerKeyConfirm -> Left (terminalReviewDecisionAt state)
    PickerKeyUp ->
        Right state
            { terminalReviewIndex =
                movePlanIndex 4 (-1) state.terminalReviewIndex
            }
    PickerKeyDown ->
        Right state
            { terminalReviewIndex =
                movePlanIndex 4 1 state.terminalReviewIndex
            }
    PickerKeyChar character -> case toLower character of
        'a' -> Left (terminalReviewApproveDecision state)
        'r' -> Left
            (TerminalPlanRevise
                (PlanReviewFeedback "" []))
        's' -> Left
            (TerminalPlanRevise
                (PlanReviewFeedback "" []))
        'b' -> Left TerminalPlanAbandon
        'q' -> Left TerminalPlanDefer
        _ -> Right state
    PickerKeyBackspace -> Right state

terminalReviewDecisionAt
    :: TerminalPlanReviewState
    -> TerminalPlanReviewDecision
terminalReviewDecisionAt state = case state.terminalReviewIndex of
    0 -> terminalReviewApproveDecision state
    1 -> TerminalPlanRevise (PlanReviewFeedback "" [])
    2 -> TerminalPlanAbandon
    _ -> TerminalPlanDefer

terminalReviewApproveDecision
    :: TerminalPlanReviewState
    -> TerminalPlanReviewDecision
terminalReviewApproveDecision state
    | null state.terminalReviewWarnings = TerminalPlanApprove
    | otherwise = TerminalPlanApproveAnyway

renderTerminalPlanReviewFrame
    :: Bool
    -> TerminalPlanReviewState
    -> Text
renderTerminalPlanReviewFrame color state =
    Text.intercalate "\n" $
        [roleWarn color (glyphWarn <> "Ready to implement this plan?")]
            <> warningRows
            <> [ renderPlanRow color (selected 0) approveLabel
               , renderPlanRow color (selected 1) "Revise plan"
               , renderPlanRow color (selected 2) "Abandon plan"
               , renderPlanRow color (selected 3) "Close review"
               , roleMuted color
                    "↑↓/jk or scroll · click/enter · a approve · r revise · b abandon · q/esc close"
               ]
  where
    selected index = state.terminalReviewIndex == index
    approveLabel
        | null state.terminalReviewWarnings = "Approve and implement"
        | otherwise = "Approve anyway and implement"
    warningRows =
        [ roleWarn color ("Warning: " <> warning)
        | warning <- state.terminalReviewWarnings
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
            putTextLn stderr (roleMuted color "plan abandoned")
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
        , renderPlanRow color (index == 1) "Revise plan"
        , renderPlanRow color (index == 2) "Abandon plan"
        , roleMuted color
            "↑↓/jk or scroll · click/enter · a approve · r revise · q/esc abandon"
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
        ReplClipboardPasteOrText _ _ text ->
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
        ReplRemovePendingImage _ _ ->
            readChangeNotes interrupt color
        ReplText text
            | Text.null (Text.strip text) -> pure "(no notes)"
            | otherwise -> pure (Text.strip text)

-- | Parse freeform terminal feedback while recognizing line comments of the
-- form @L12: ...@ or @L12-L15: ...@. Invalid references remain ordinary
-- overall feedback rather than being silently discarded.
parsePlanReviewFeedback :: Text -> PlanReviewFeedback
parsePlanReviewFeedback input =
    let parsed = map parseFeedbackLine (Text.lines input)
        comments = mapMaybe fst parsed
        overall =
            Text.strip
                (Text.unlines
                    [ line
                    | (Nothing, line) <- parsed
                    , not (Text.null (Text.strip line))
                    ])
    in PlanReviewFeedback
        { planFeedbackOverall = overall
        , planFeedbackLineComments = comments
        }

renderPlanReviewFeedback :: PlanReviewFeedback -> Text
renderPlanReviewFeedback feedback =
    case overallRows <> commentRows of
        [] -> "(no notes)"
        rows -> Text.intercalate "\n" rows
  where
    overallRows =
        [ feedback.planFeedbackOverall
        | not (Text.null (Text.strip feedback.planFeedbackOverall))
        ]
    commentRows = map renderLineComment feedback.planFeedbackLineComments

renderLineComment :: PlanLineComment -> Text
renderLineComment comment =
    reference <> ": " <> comment.planCommentBody
  where
    startLine = Text.pack (show comment.planCommentStartLine)
    endLine = Text.pack (show comment.planCommentEndLine)
    reference
        | comment.planCommentStartLine == comment.planCommentEndLine =
            "L" <> startLine
        | otherwise =
            "L" <> startLine <> "-L" <> endLine

parseFeedbackLine :: Text -> (Maybe PlanLineComment, Text)
parseFeedbackLine raw =
    case Text.breakOn ":" (Text.strip raw) of
        (reference, suffix)
            | not (Text.null suffix)
            , Just (startLine, endLine) <- parseLineReference reference
            , let body = Text.strip (Text.drop 1 suffix)
            , not (Text.null body) ->
                ( Just PlanLineComment
                    { planCommentStartLine = startLine
                    , planCommentEndLine = endLine
                    , planCommentBody = body
                    }
                , ""
                )
        _ -> (Nothing, raw)

parseLineReference :: Text -> Maybe (Int, Int)
parseLineReference raw =
    case Text.splitOn "-" (Text.toUpper (Text.strip raw)) of
        [single] -> do
            line <- parsePositiveLine single
            pure (line, line)
        [start, end] -> do
            startLine <- parsePositiveLine start
            endLine <- parsePositiveLine end
            if endLine < startLine
                then Nothing
                else pure (startLine, endLine)
        _ -> Nothing

parsePositiveLine :: Text -> Maybe Int
parsePositiveLine raw = do
    digits <- Text.stripPrefix "L" raw
    case reads (Text.unpack digits) of
        [(line, "")] | line > 0 -> Just line
        _ -> Nothing

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
                        ReplClipboardPasteOrText _ _ text ->
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
                        ReplRemovePendingImage _ _ ->
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
            , terminalCyan
            ]
            label
    | otherwise = roleMuted color label

renderPlanMarkdown :: Bool -> Text -> Text
renderPlanMarkdown color text =
    paintBackgroundLines color agentBackground (renderMarkdown color text)

-- | Markdown shown by @/view-plan@. Keeping the empty state explicit avoids
-- presenting a blank system block and makes it clear which artifact is being
-- inspected.
formatPlanPreview :: OsPath -> Text -> Text
formatPlanPreview path content
    | Text.null (Text.strip content) =
        Text.unlines
            [ "# Plan"
            , ""
            , "_No plan has been written yet._"
            , ""
            , "Plan file: `" <> toText path <> "`"
            ]
    | otherwise =
        content
            <> "\n\n---\nPlan file: `"
            <> toText path
            <> "`"

parsePlanDecisionAnswer :: Text -> Maybe PlanDecision
parsePlanDecisionAnswer raw =
    case Text.toLower (Text.strip raw) of
        "approve" -> Just PlanApprove
        "yes" -> Just PlanApprove
        "changes" -> Just (PlanRequestChanges "")
        "revise" -> Just (PlanRequestChanges "")
        "q" -> Just PlanCancel
        "cancel" -> Just PlanCancel
        "abandon" -> Just PlanCancel
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
    'q' -> Just PlanCancel
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

-- | Why a Codex @\<proposed_plan\>@ envelope could not be accepted.
--
-- Tags inside fenced Markdown code blocks are deliberately ignored. A valid
-- proposal contains exactly one complete, non-nested envelope.
data ProposedPlanParseError
    = ProposedPlanNotFound
    | ProposedPlanUnclosed
    | ProposedPlanUnexpectedClose
    | ProposedPlanNested
    | ProposedPlanMultiple
    deriving (Eq, Show)

renderProposedPlanParseError :: ProposedPlanParseError -> Text
renderProposedPlanParseError = \case
    ProposedPlanNotFound ->
        "no <proposed_plan> block was found outside fenced code"
    ProposedPlanUnclosed ->
        "the <proposed_plan> block is missing </proposed_plan>"
    ProposedPlanUnexpectedClose ->
        "found </proposed_plan> without a matching opening tag"
    ProposedPlanNested ->
        "<proposed_plan> blocks must not be nested"
    ProposedPlanMultiple ->
        "expected exactly one <proposed_plan> block"

-- | Parse exactly one complete, non-nested proposal outside fenced Markdown
-- code. Unlike the compatibility 'extractProposedPlan' wrapper, callers can
-- report the concrete malformed-envelope reason.
parseProposedPlan :: Text -> Either ProposedPlanParseError Text
parseProposedPlan text =
    proposalBody text <$> parseProposedPlanMatch text

-- | Compatibility wrapper for callers that only need presence/absence.
extractProposedPlan :: Text -> Maybe Text
extractProposedPlan = either (const Nothing) Just . parseProposedPlan

-- | Remove proposed_plan tags for display after the approval UI shows the body.
stripProposedPlan :: Text -> Text
stripProposedPlan text =
    case parseProposedPlanMatch text of
        Left _ -> text
        Right match ->
            Text.strip
                ( Text.take match.proposalOpenOffset text
                    <> Text.drop match.proposalEndOffset text
                )

data ProposalTagKind = ProposalOpen | ProposalClose
    deriving (Eq, Show)

data LocatedProposalTag = LocatedProposalTag
    { proposalTagOffset :: !Int
    , proposalTagKind :: !ProposalTagKind
    }
    deriving (Eq, Show)

data ProposedPlanMatch = ProposedPlanMatch
    { proposalOpenOffset :: !Int
    , proposalContentOffset :: !Int
    , proposalCloseOffset :: !Int
    , proposalEndOffset :: !Int
    }
    deriving (Eq, Show)

data MarkdownFence = MarkdownFence !Char !Int
    deriving (Eq, Show)

openProposalTag :: Text
openProposalTag = "<proposed_plan>"

closeProposalTag :: Text
closeProposalTag = "</proposed_plan>"

parseProposedPlanMatch
    :: Text
    -> Either ProposedPlanParseError ProposedPlanMatch
parseProposedPlanMatch text =
    go Nothing Nothing (proposalTagsOutsideFences text)
  where
    go
        :: Maybe Int
        -> Maybe ProposedPlanMatch
        -> [LocatedProposalTag]
        -> Either ProposedPlanParseError ProposedPlanMatch
    go open completed = \case
        []
            | Just _ <- open -> Left ProposedPlanUnclosed
            | Just match <- completed -> Right match
            | otherwise -> Left ProposedPlanNotFound
        tag : rest -> case tag.proposalTagKind of
            ProposalOpen
                | Just _ <- open -> Left ProposedPlanNested
                | Just _ <- completed -> Left ProposedPlanMultiple
                | otherwise ->
                    go (Just tag.proposalTagOffset) completed rest
            ProposalClose -> case open of
                Nothing -> Left ProposedPlanUnexpectedClose
                Just openOffset ->
                    let match = ProposedPlanMatch
                            { proposalOpenOffset = openOffset
                            , proposalContentOffset =
                                openOffset + Text.length openProposalTag
                            , proposalCloseOffset = tag.proposalTagOffset
                            , proposalEndOffset =
                                tag.proposalTagOffset
                                    + Text.length closeProposalTag
                            }
                    in go Nothing (Just match) rest

proposalBody :: Text -> ProposedPlanMatch -> Text
proposalBody text match =
    Text.strip
        (Text.take
            (match.proposalCloseOffset - match.proposalContentOffset)
            (Text.drop match.proposalContentOffset text))

proposalTagsOutsideFences :: Text -> [LocatedProposalTag]
proposalTagsOutsideFences =
    go 0 Nothing . Text.splitOn "\n"
  where
    go _ _ [] = []
    go offset fence (line : rest) =
        let nextOffset = offset + Text.length line + 1
        in case fence of
            Nothing -> case openingFence line of
                Just opened -> go nextOffset (Just opened) rest
                Nothing ->
                    proposalTagsOnLine offset line
                        <> go nextOffset Nothing rest
            Just opened
                | closesFence opened line ->
                    go nextOffset Nothing rest
                | otherwise ->
                    go nextOffset fence rest

proposalTagsOnLine :: Int -> Text -> [LocatedProposalTag]
proposalTagsOnLine base line =
    sortOn (.proposalTagOffset)
        (locate ProposalOpen openProposalTag <> locate ProposalClose closeProposalTag)
  where
    locate kind needle =
        [ LocatedProposalTag
            { proposalTagOffset = base + Text.length prefix
            , proposalTagKind = kind
            }
        | (prefix, _) <- Text.breakOnAll needle line
        ]

openingFence :: Text -> Maybe MarkdownFence
openingFence line = do
    rest <- fenceLineStart line
    (marker, _) <- Text.uncons rest
    if marker /= '`' && marker /= '~'
        then Nothing
        else
            let width = Text.length (Text.takeWhile (== marker) rest)
            in if width >= 3
                then Just (MarkdownFence marker width)
                else Nothing

closesFence :: MarkdownFence -> Text -> Bool
closesFence (MarkdownFence marker openingWidth) line =
    case fenceLineStart line of
        Nothing -> False
        Just rest ->
            let width = Text.length (Text.takeWhile (== marker) rest)
                trailing = Text.drop width rest
            in width >= openingWidth
                && Text.all isFenceTrailingSpace trailing

-- Conservatively recognize fences inside block quotes, list items, and
-- indented code. False negatives here could authorize a proposal example as
-- the real plan, while a false positive merely asks the model to restate it.
fenceLineStart :: Text -> Maybe Text
fenceLineStart = Just . stripContainers . dropIndent
  where
    dropIndent = Text.dropWhile isIndent
    isIndent character = character == ' ' || character == '\t'

    stripContainers input
        | Just rest <- Text.stripPrefix ">" input =
            stripContainers (dropIndent rest)
        | Just rest <- stripListMarker input =
            stripContainers (dropIndent rest)
        | otherwise = input

stripListMarker :: Text -> Maybe Text
stripListMarker input =
    bullet <|> ordered
  where
    bullet = do
        (marker, rest) <- Text.uncons input
        if marker `elem` ['-', '+', '*'] && startsWithIndent rest
            then Just rest
            else Nothing
    ordered =
        let (digits, suffix) = Text.span isAsciiDigit input
        in if Text.null digits || Text.length digits > 9
            then Nothing
            else do
                (marker, rest) <- Text.uncons suffix
                if marker `elem` ['.', ')'] && startsWithIndent rest
                    then Just rest
                    else Nothing

startsWithIndent :: Text -> Bool
startsWithIndent text =
    case Text.uncons text of
        Just (character, _) -> character == ' ' || character == '\t'
        Nothing -> False

isAsciiDigit :: Char -> Bool
isAsciiDigit character =
    character >= '0' && character <= '9'

isFenceTrailingSpace :: Char -> Bool
isFenceTrailingSpace char =
    char == ' ' || char == '\t' || char == '\r'

putTextLn :: Handle -> Text -> IO ()
putTextLn handle text = do
    Text.hPutStrLn handle text
    hFlush handle
