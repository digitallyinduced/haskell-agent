-- | Pure state and Brick rendering for a correlated plan-review surface.
--
-- The CLI owns transport and lifecycle. This module deliberately keeps the
-- review reducer independent from a particular reply mechanism so the same
-- state can be driven by a local keyboard, a managed client, or an external
-- first-answer-wins resolution.
module Agent.TUI.PlanReview
    ( PlanReviewId(..)
    , PlanReviewMode(..)
    , PlanReviewFocus(..)
    , PlanReviewWarning(..)
    , PlanLineRange(..)
    , PlanReviewComment(..)
    , PlanReviewRequest(..)
    , PlanReviewAction(..)
    , PlanReviewControl(..)
    , PlanReviewCommand(..)
    , PlanApproval(..)
    , PlanRevision(..)
    , PlanReviewOutcome(..)
    , PlanReviewState(..)
    , PlanReviewTransition(..)
    , initialPlanReview
    , stepPlanReview
    , planReviewActions
    , planReviewActionLabel
    , planSourceLines
    , selectedPlanLineRange
    , numberedPlanSource
    , planReviewCommandForEvent
    , planReviewCommandForControl
    , planReviewWidget
    ) where

import Agent.TUI.Markdown (markdownWidget)
import qualified Agent.TUI.Theme as Theme
import Brick
import Data.List (findIndex)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as V

newtype PlanReviewId = PlanReviewId Text
    deriving (Eq, Ord, Show)

data PlanReviewMode
    = PlanReviewPreview
    | PlanReviewSource
    deriving (Eq, Ord, Show)

data PlanReviewFocus
    = PlanReviewDocument
    | PlanReviewFeedback
    | PlanReviewCommentEditor
    | PlanReviewActions
    deriving (Eq, Ord, Enum, Bounded, Show)

data PlanReviewWarning = PlanReviewWarning
    { warningCode :: !Text
    , warningMessage :: !Text
    }
    deriving (Eq, Show)

data PlanLineRange = PlanLineRange
    { rangeStart :: !Int
    , rangeEnd :: !Int
    }
    deriving (Eq, Ord, Show)

data PlanReviewComment = PlanReviewComment
    { commentRange :: !PlanLineRange
    , commentBody :: !Text
    }
    deriving (Eq, Show)

data PlanReviewRequest = PlanReviewRequest
    { requestId :: !PlanReviewId
    , requestTitle :: !Text
    , requestMarkdown :: !Text
    , requestDigest :: !Text
    , requestWarnings :: ![PlanReviewWarning]
    }
    deriving (Eq, Show)

data PlanReviewAction
    = PlanReviewApprove
    | PlanReviewRevise
    | PlanReviewAbandon
    | PlanReviewDefer
    deriving (Eq, Ord, Enum, Bounded, Show)

data PlanReviewControl
    = ReviewViewport
    | ReviewModeControl !PlanReviewMode
    | ReviewLineControl !Int
    | ReviewFeedbackControl
    | ReviewCommentControl
    | ReviewActionControl !PlanReviewAction
    deriving (Eq, Ord, Show)

data PlanReviewCommand
    = ReviewSetMode !PlanReviewMode
    | ReviewToggleMode
    | ReviewSetFocus !PlanReviewFocus
    | ReviewMoveFocus !Int
    | ReviewMoveLine !Int
    | ReviewSelectLine !Int
    | ReviewToggleRangeAnchor
    | ReviewClearRange
    | ReviewSetCommentDraft !Text
    | ReviewAddComment
    | ReviewDeleteComment !Int
    | ReviewSetFeedbackDraft !Text
    | ReviewMoveAction !Int
    | ReviewChooseAction !PlanReviewAction
    | ReviewActivateAction
    | ReviewDismissExternal !PlanReviewId
    deriving (Eq, Show)

data PlanApproval = PlanApproval
    { approvalRequestId :: !PlanReviewId
    , approvalDigest :: !Text
    , approvalAcceptedWarnings :: !Bool
    }
    deriving (Eq, Show)

data PlanRevision = PlanRevision
    { revisionRequestId :: !PlanReviewId
    , revisionDigest :: !Text
    , revisionFeedback :: !Text
    , revisionComments :: ![PlanReviewComment]
    }
    deriving (Eq, Show)

data PlanReviewOutcome
    = PlanApproved !PlanApproval
    | PlanRevisionRequested !PlanRevision
    | PlanAbandoned !PlanReviewId !Text
    | PlanDeferred !PlanReviewId
    | PlanReviewExternallyResolved !PlanReviewId
    deriving (Eq, Show)

data PlanReviewState = PlanReviewState
    { reviewRequest :: !PlanReviewRequest
    , reviewMode :: !PlanReviewMode
    , reviewFocus :: !PlanReviewFocus
    , reviewSelectedLine :: !Int
    , reviewRangeAnchor :: !(Maybe Int)
    , reviewCommentDraft :: !Text
    , reviewComments :: ![PlanReviewComment]
    , reviewFeedbackDraft :: !Text
    , reviewActionIndex :: !Int
    , reviewNotice :: !(Maybe Text)
    }
    deriving (Eq, Show)

data PlanReviewTransition
    = PlanReviewContinue !PlanReviewState
    | PlanReviewComplete !PlanReviewOutcome
    deriving (Eq, Show)

initialPlanReview :: PlanReviewRequest -> PlanReviewState
initialPlanReview request =
    PlanReviewState
        { reviewRequest = request
        , reviewMode = PlanReviewPreview
        , reviewFocus = PlanReviewDocument
        , reviewSelectedLine =
            if null (planSourceLines request.requestMarkdown) then 0 else 1
        , reviewRangeAnchor = Nothing
        , reviewCommentDraft = ""
        , reviewComments = []
        , reviewFeedbackDraft = ""
        , reviewActionIndex = 0
        , reviewNotice = Nothing
        }

stepPlanReview
    :: PlanReviewCommand
    -> PlanReviewState
    -> PlanReviewTransition
stepPlanReview command state =
    case command of
        ReviewSetMode mode ->
            continue state
                { reviewMode = mode
                , reviewNotice = Nothing
                }
        ReviewToggleMode ->
            continue state
                { reviewMode = case state.reviewMode of
                    PlanReviewPreview -> PlanReviewSource
                    PlanReviewSource -> PlanReviewPreview
                , reviewNotice = Nothing
                }
        ReviewSetFocus focus ->
            continue state { reviewFocus = focus }
        ReviewMoveFocus delta ->
            continue state
                { reviewFocus = moveEnum delta state.reviewFocus
                }
        ReviewMoveLine delta ->
            continue $
                moveSelectedLine delta
                    state
                        { reviewMode = PlanReviewSource
                        , reviewFocus = PlanReviewDocument
                        , reviewNotice = Nothing
                        }
        ReviewSelectLine lineNumber ->
            continue $
                selectLine lineNumber
                    state
                        { reviewMode = PlanReviewSource
                        , reviewFocus = PlanReviewDocument
                        , reviewNotice = Nothing
                        }
        ReviewToggleRangeAnchor
            | state.reviewSelectedLine <= 0 ->
                continue state
                    { reviewNotice = Just "The empty plan has no source lines to select."
                    }
            | otherwise ->
                continue state
                    { reviewRangeAnchor =
                        case state.reviewRangeAnchor of
                            Nothing -> Just state.reviewSelectedLine
                            Just _ -> Nothing
                    , reviewNotice = Nothing
                    }
        ReviewClearRange ->
            continue state
                { reviewRangeAnchor = Nothing
                , reviewNotice = Nothing
                }
        ReviewSetCommentDraft draft ->
            continue state
                { reviewCommentDraft = draft
                , reviewFocus = PlanReviewCommentEditor
                , reviewNotice = Nothing
                }
        ReviewAddComment ->
            addComment state
        ReviewDeleteComment index ->
            continue state
                { reviewComments = deleteAt index state.reviewComments
                , reviewNotice = Nothing
                }
        ReviewSetFeedbackDraft draft ->
            continue state
                { reviewFeedbackDraft = draft
                , reviewFocus = PlanReviewFeedback
                , reviewNotice = Nothing
                }
        ReviewMoveAction delta ->
            continue state
                { reviewActionIndex =
                    wrapIndex
                        (length (planReviewActions state))
                        (state.reviewActionIndex + delta)
                , reviewFocus = PlanReviewActions
                , reviewNotice = Nothing
                }
        ReviewChooseAction action ->
            chooseAction action state
        ReviewActivateAction ->
            case atMay (planReviewActions state) state.reviewActionIndex of
                Nothing -> continue state
                Just action -> chooseAction action state
        ReviewDismissExternal ident
            | ident == state.reviewRequest.requestId ->
                PlanReviewComplete (PlanReviewExternallyResolved ident)
            | otherwise ->
                continue state
  where
    continue = PlanReviewContinue

planReviewActions :: PlanReviewState -> [PlanReviewAction]
planReviewActions _ =
    [ PlanReviewApprove
    , PlanReviewRevise
    , PlanReviewAbandon
    , PlanReviewDefer
    ]

planReviewActionLabel :: PlanReviewState -> PlanReviewAction -> Text
planReviewActionLabel state = \case
    PlanReviewApprove
        | null state.reviewRequest.requestWarnings -> "Approve"
        | otherwise -> "Approve anyway"
    PlanReviewRevise -> "Revise"
    PlanReviewAbandon -> "Abandon"
    PlanReviewDefer -> "Close"

planSourceLines :: Text -> [Text]
planSourceLines source
    | Text.null source = []
    | otherwise = Text.splitOn "\n" source

selectedPlanLineRange :: PlanReviewState -> Maybe PlanLineRange
selectedPlanLineRange state
    | state.reviewSelectedLine <= 0 = Nothing
    | otherwise =
        let anchor =
                maybe state.reviewSelectedLine id state.reviewRangeAnchor
            selected = state.reviewSelectedLine
        in Just PlanLineRange
            { rangeStart = min anchor selected
            , rangeEnd = max anchor selected
            }

numberedPlanSource :: PlanReviewState -> [Text]
numberedPlanSource state =
    let
        source = planSourceLines state.reviewRequest.requestMarkdown
        width = max 1 (length (show (length source)))
    in
    [ marker lineNumber
        <> Text.justifyRight width ' ' (Text.pack (show lineNumber))
        <> " │ "
        <> body
    | (lineNumber, body) <- zip [1 ..] source
    ]
  where
    marker lineNumber
        | lineInSelectedRange lineNumber state = "›"
        | lineHasComment lineNumber state.reviewComments = "•"
        | otherwise = " "

-- | Translate non-text Vty input into a pure reducer command. Text editing is
-- intentionally left to the host composer, which feeds draft replacement
-- commands back into this reducer.
planReviewCommandForEvent
    :: PlanReviewState
    -> V.Event
    -> Maybe PlanReviewCommand
planReviewCommandForEvent state = \case
    V.EvKey (V.KChar '\t') [] -> Just (ReviewMoveFocus 1)
    V.EvKey V.KBackTab [] -> Just (ReviewMoveFocus (-1))
    V.EvKey V.KEsc []
        | state.reviewFocus
            `elem` [PlanReviewFeedback, PlanReviewCommentEditor] ->
                Just (ReviewSetFocus PlanReviewDocument)
        | otherwise ->
            Just (ReviewChooseAction PlanReviewDefer)
    V.EvKey V.KUp []
        | state.reviewFocus == PlanReviewDocument ->
            Just (ReviewMoveLine (-1))
        | otherwise -> Nothing
    V.EvKey V.KDown []
        | state.reviewFocus == PlanReviewDocument ->
            Just (ReviewMoveLine 1)
        | otherwise -> Nothing
    V.EvKey V.KLeft []
        | state.reviewFocus == PlanReviewActions ->
            Just (ReviewMoveAction (-1))
        | otherwise -> Just (ReviewSetMode PlanReviewPreview)
    V.EvKey V.KRight []
        | state.reviewFocus == PlanReviewActions ->
            Just (ReviewMoveAction 1)
        | otherwise -> Just (ReviewSetMode PlanReviewSource)
    V.EvKey V.KEnter [] ->
        case state.reviewFocus of
            PlanReviewActions -> Just ReviewActivateAction
            PlanReviewCommentEditor -> Just ReviewAddComment
            PlanReviewFeedback -> Just (ReviewSetFocus PlanReviewActions)
            PlanReviewDocument
                | state.reviewMode == PlanReviewSource ->
                    Just ReviewToggleRangeAnchor
                | otherwise -> Just (ReviewSetMode PlanReviewSource)
    V.EvKey (V.KChar ' ') []
        | state.reviewFocus == PlanReviewDocument
        , state.reviewMode == PlanReviewSource ->
            Just ReviewToggleRangeAnchor
    V.EvKey (V.KChar 'p') []
        | not (reviewEditing state) ->
            Just (ReviewSetMode PlanReviewPreview)
    V.EvKey (V.KChar 's') []
        | not (reviewEditing state) ->
            Just (ReviewSetMode PlanReviewSource)
    V.EvKey (V.KChar 'c') []
        | not (reviewEditing state) ->
            Just (ReviewSetFocus PlanReviewCommentEditor)
    V.EvKey (V.KChar 'f') []
        | not (reviewEditing state) ->
            Just (ReviewSetFocus PlanReviewFeedback)
    V.EvKey (V.KChar 'a') []
        | not (reviewEditing state) ->
            Just (ReviewChooseAction PlanReviewApprove)
    V.EvKey (V.KChar 'r') []
        | not (reviewEditing state) ->
            Just (ReviewChooseAction PlanReviewRevise)
    V.EvKey (V.KChar 'x') []
        | not (reviewEditing state) ->
            Just (ReviewChooseAction PlanReviewAbandon)
    _ -> Nothing

planReviewCommandForControl :: PlanReviewControl -> PlanReviewCommand
planReviewCommandForControl = \case
    ReviewViewport -> ReviewSetFocus PlanReviewDocument
    ReviewModeControl mode -> ReviewSetMode mode
    ReviewLineControl lineNumber -> ReviewSelectLine lineNumber
    ReviewFeedbackControl -> ReviewSetFocus PlanReviewFeedback
    ReviewCommentControl -> ReviewSetFocus PlanReviewCommentEditor
    ReviewActionControl action -> ReviewChooseAction action

planReviewWidget
    :: (Ord n, Show n)
    => (PlanReviewControl -> n)
    -> PlanReviewState
    -> Widget n
planReviewWidget controlName state =
    vBox $
        [ withAttr Theme.headerAttr $
            txt $
                nonEmptyOr "Plan review" state.reviewRequest.requestTitle
        , withAttr Theme.mutedAttr $
            txt $
                "Digest " <> abbreviatedDigest state.reviewRequest.requestDigest
        ]
            <> warningWidgets state.reviewRequest.requestWarnings
            <> [ modeWidget controlName state
               , viewport
                    (controlName ReviewViewport)
                    Vertical
                    (documentWidget controlName state)
               ]
            <> commentsWidgets state.reviewComments
            <> [ clickable (controlName ReviewFeedbackControl) $
                    editorWidget
                    "Overall feedback"
                    "Add feedback for a revision request"
                    state.reviewFeedbackDraft
               , clickable (controlName ReviewCommentControl) $
                    editorWidget
                    (commentEditorTitle state)
                    "Select source lines and add a comment"
                    state.reviewCommentDraft
               , actionWidget controlName state
               ]
            <> maybe [] (pure . withAttr Theme.errorAttr . txtWrap)
                state.reviewNotice

documentWidget
    :: Ord n
    => (PlanReviewControl -> n)
    -> PlanReviewState
    -> Widget n
documentWidget controlName state =
    case state.reviewMode of
        PlanReviewPreview
            | Text.null (Text.strip state.reviewRequest.requestMarkdown) ->
                withAttr Theme.mutedAttr $
                    txtWrap "No plan has been written yet."
            | otherwise ->
                markdownWidget state.reviewRequest.requestMarkdown
        PlanReviewSource ->
            case zip [1 ..] (numberedPlanSource state) of
                [] ->
                    withAttr Theme.mutedAttr $
                        txtWrap "No plan source lines are available."
                rows ->
                    vBox (map renderSourceRow rows)
  where
    renderSourceRow (lineNumber, body) =
        let widget = txt body
            styled
                | lineInSelectedRange lineNumber state =
                    withAttr Theme.selectedAttr widget
                | lineHasComment lineNumber state.reviewComments =
                    withAttr Theme.successAttr widget
                | otherwise = widget
            interactive =
                clickable (controlName (ReviewLineControl lineNumber)) styled
        in if lineNumber == state.reviewSelectedLine
            then visible interactive
            else interactive

warningWidgets :: [PlanReviewWarning] -> [Widget n]
warningWidgets [] = []
warningWidgets warnings =
    withAttr Theme.errorAttr (txt "Plan warnings")
        : [ withAttr Theme.errorAttr $
                txtWrap ("• " <> warning.warningMessage)
          | warning <- warnings
          ]

modeWidget
    :: Ord n
    => (PlanReviewControl -> n)
    -> PlanReviewState
    -> Widget n
modeWidget controlName state =
    hBox
        [ modeLabel PlanReviewPreview "Preview"
        , txt "  "
        , modeLabel PlanReviewSource "Source"
        ]
  where
    modeLabel mode label =
        clickable (controlName (ReviewModeControl mode)) $
            (if state.reviewMode == mode
                then withAttr Theme.selectedAttr
                else withAttr Theme.mutedAttr)
                (txt label)

commentsWidgets :: [PlanReviewComment] -> [Widget n]
commentsWidgets [] = []
commentsWidgets comments =
    withAttr Theme.headerAttr (txt "Line comments")
        : [ txtWrap $
                formatRange comment.commentRange
                    <> " — "
                    <> comment.commentBody
          | comment <- comments
          ]

editorWidget :: Text -> Text -> Text -> Widget n
editorWidget title placeholder draft =
    vBox
        [ withAttr Theme.mutedAttr (txt title)
        , txtWrap (nonEmptyOr placeholder draft)
        ]

commentEditorTitle :: PlanReviewState -> Text
commentEditorTitle state =
    case selectedPlanLineRange state of
        Nothing -> "Line comment"
        Just range -> "Line comment (" <> formatRange range <> ")"

actionWidget
    :: Ord n
    => (PlanReviewControl -> n)
    -> PlanReviewState
    -> Widget n
actionWidget controlName state =
    hBox $
        concat
            [ [ clickable (controlName (ReviewActionControl action)) $
                    if index == state.reviewActionIndex
                        then withAttr Theme.selectedAttr (txt label)
                        else txt label
              , txt "  "
              ]
            | (index, action) <- zip [0 ..] (planReviewActions state)
            , let label = planReviewActionLabel state action
            ]

chooseAction
    :: PlanReviewAction
    -> PlanReviewState
    -> PlanReviewTransition
chooseAction action state =
    case action of
        PlanReviewApprove ->
            PlanReviewComplete $
                PlanApproved PlanApproval
                    { approvalRequestId = request.requestId
                    , approvalDigest = request.requestDigest
                    , approvalAcceptedWarnings =
                        not (null request.requestWarnings)
                    }
        PlanReviewRevise ->
            PlanReviewComplete $
                PlanRevisionRequested PlanRevision
                    { revisionRequestId = request.requestId
                    , revisionDigest = request.requestDigest
                    , revisionFeedback = Text.strip state.reviewFeedbackDraft
                    , revisionComments = state.reviewComments
                    }
        PlanReviewAbandon ->
            PlanReviewComplete $
                PlanAbandoned request.requestId request.requestDigest
        PlanReviewDefer ->
            PlanReviewComplete (PlanDeferred request.requestId)
  where
    request = state.reviewRequest

addComment :: PlanReviewState -> PlanReviewTransition
addComment state =
    case (selectedPlanLineRange state, Text.strip state.reviewCommentDraft) of
        (Nothing, _) ->
            PlanReviewContinue state
                { reviewNotice = Just "Select one or more source lines first."
                }
        (_, "") ->
            PlanReviewContinue state
                { reviewNotice = Just "Enter a line comment before adding it."
                }
        (Just range, body) ->
            PlanReviewContinue state
                { reviewComments =
                    upsertComment
                        PlanReviewComment
                            { commentRange = range
                            , commentBody = body
                            }
                        state.reviewComments
                , reviewCommentDraft = ""
                , reviewRangeAnchor = Nothing
                , reviewFocus = PlanReviewDocument
                , reviewNotice = Nothing
                }

upsertComment
    :: PlanReviewComment
    -> [PlanReviewComment]
    -> [PlanReviewComment]
upsertComment comment comments =
    case
        findIndex
            ((== comment.commentRange) . (.commentRange))
            comments
    of
        Nothing -> comments <> [comment]
        Just index ->
            take index comments <> [comment] <> drop (index + 1) comments

moveSelectedLine :: Int -> PlanReviewState -> PlanReviewState
moveSelectedLine delta state =
    let count = length (planSourceLines state.reviewRequest.requestMarkdown)
    in state
        { reviewSelectedLine =
            if count == 0
                then 0
                else max 1 (min count (state.reviewSelectedLine + delta))
        }

selectLine :: Int -> PlanReviewState -> PlanReviewState
selectLine lineNumber state =
    let count = length (planSourceLines state.reviewRequest.requestMarkdown)
    in state
        { reviewSelectedLine =
            if count == 0
                then 0
                else max 1 (min count lineNumber)
        }

lineInSelectedRange :: Int -> PlanReviewState -> Bool
lineInSelectedRange lineNumber state =
    case selectedPlanLineRange state of
        Nothing -> False
        Just range ->
            lineNumber >= range.rangeStart
                && lineNumber <= range.rangeEnd

lineHasComment :: Int -> [PlanReviewComment] -> Bool
lineHasComment lineNumber =
    any \comment ->
        lineNumber >= comment.commentRange.rangeStart
            && lineNumber <= comment.commentRange.rangeEnd

formatRange :: PlanLineRange -> Text
formatRange range
    | range.rangeStart == range.rangeEnd =
        "line " <> Text.pack (show range.rangeStart)
    | otherwise =
        "lines "
            <> Text.pack (show range.rangeStart)
            <> "–"
            <> Text.pack (show range.rangeEnd)

abbreviatedDigest :: Text -> Text
abbreviatedDigest digest
    | Text.length digest <= 16 = digest
    | otherwise = Text.take 12 digest <> "…"

nonEmptyOr :: Text -> Text -> Text
nonEmptyOr fallback value
    | Text.null (Text.strip value) = fallback
    | otherwise = value

moveEnum :: (Enum a, Bounded a) => Int -> a -> a
moveEnum delta value =
    let
        low = fromEnum (minBound `asTypeOf` value)
        high = fromEnum (maxBound `asTypeOf` value)
        count = high - low + 1
        next = low + ((fromEnum value - low + delta) `mod` count)
    in toEnum next

wrapIndex :: Int -> Int -> Int
wrapIndex count index
    | count <= 0 = 0
    | otherwise = index `mod` count

deleteAt :: Int -> [a] -> [a]
deleteAt index values
    | index < 0 = values
    | otherwise = take index values <> drop (index + 1) values

atMay :: [a] -> Int -> Maybe a
atMay values index
    | index < 0 = Nothing
    | otherwise = case drop index values of
        value : _ -> Just value
        [] -> Nothing

reviewEditing :: PlanReviewState -> Bool
reviewEditing state =
    state.reviewFocus
        `elem` [PlanReviewFeedback, PlanReviewCommentEditor]
