-- | Pure state and Brick rendering for structured question batches.
--
-- A questionnaire is correlated by request id and preserves every draft while
-- the user moves between question tabs. Transport-specific timeout and
-- first-answer-wins handling stays in the caller; typed timeout, cancellation,
-- clarification, and external-resolution outcomes are represented here.
module Agent.TUI.Questionnaire
    ( QuestionnaireId(..)
    , QuestionnaireOption(..)
    , QuestionnaireQuestion(..)
    , QuestionnaireRequest(..)
    , QuestionnaireFocus(..)
    , QuestionnaireAction(..)
    , QuestionnaireControl(..)
    , QuestionnaireCommand(..)
    , QuestionnaireAnswer(..)
    , QuestionnaireSubmission(..)
    , QuestionnaireOutcome(..)
    , QuestionnaireState(..)
    , QuestionnaireTransition(..)
    , initialQuestionnaire
    , stepQuestionnaire
    , questionnaireActions
    , questionnaireActionLabel
    , questionnaireCurrentQuestion
    , questionnaireFocusedOption
    , questionnaireAnswers
    , questionnaireCommandForEvent
    , questionnaireCommandForControl
    , questionnaireWidget
    ) where

import qualified Agent.TUI.Theme as Theme
import Brick
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as V

newtype QuestionnaireId = QuestionnaireId Text
    deriving (Eq, Ord, Show)

data QuestionnaireOption = QuestionnaireOption
    { optionLabel :: !Text
    , optionDescription :: !Text
    , optionPreview :: !(Maybe Text)
    }
    deriving (Eq, Show)

data QuestionnaireQuestion = QuestionnaireQuestion
    { questionText :: !Text
    , questionOptions :: ![QuestionnaireOption]
    , questionMultiSelect :: !Bool
    }
    deriving (Eq, Show)

data QuestionnaireRequest = QuestionnaireRequest
    { requestId :: !QuestionnaireId
    , requestQuestions :: ![QuestionnaireQuestion]
    }
    deriving (Eq, Show)

data QuestionnaireFocus
    = QuestionnaireTabs
    | QuestionnaireOptions
    | QuestionnaireOther
    | QuestionnaireChat
    | QuestionnaireActions
    deriving (Eq, Ord, Enum, Bounded, Show)

data QuestionnaireAction
    = QuestionnaireSubmit
    | QuestionnaireClarify
    | QuestionnaireFinish
    | QuestionnaireCancel
    deriving (Eq, Ord, Enum, Bounded, Show)

data QuestionnaireControl
    = QuestionsViewport
    | QuestionTabControl !Int
    | QuestionOptionControl !Int
    | QuestionOtherControl
    | QuestionOtherEditorControl
    | QuestionChatEditorControl
    | QuestionActionControl !QuestionnaireAction
    deriving (Eq, Ord, Show)

data QuestionnaireCommand
    = QuestionsSetFocus !QuestionnaireFocus
    | QuestionsMoveFocus !Int
    | QuestionsMoveTab !Int
    | QuestionsSelectTab !Int
    | QuestionsMoveOption !Int
    | QuestionsChooseOption !Int
    | QuestionsChooseOther
    | QuestionsActivateOption
    | QuestionsSetOtherDraft !Text
    | QuestionsSetChatDraft !Text
    | QuestionsMoveAction !Int
    | QuestionsChooseAction !QuestionnaireAction
    | QuestionsActivateAction
    | QuestionsTimedOut
    | QuestionsDismissExternal !QuestionnaireId
    deriving (Eq, Show)

data QuestionnaireAnswer = QuestionnaireAnswer
    { answerQuestionIndex :: !Int
    , answerQuestion :: !Text
    , answerLabels :: ![Text]
    , answerOther :: !(Maybe Text)
    }
    deriving (Eq, Show)

data QuestionnaireSubmission = QuestionnaireSubmission
    { submissionRequestId :: !QuestionnaireId
    , submissionAnswers :: ![QuestionnaireAnswer]
    }
    deriving (Eq, Show)

data QuestionnaireOutcome
    = QuestionnaireSubmitted !QuestionnaireSubmission
    | QuestionnaireClarificationRequested !QuestionnaireId !Text
    | QuestionnaireFinished !QuestionnaireId
    | QuestionnaireCancelled !QuestionnaireId
    | QuestionnaireTimeout !QuestionnaireId
    | QuestionnaireExternallyResolved !QuestionnaireId
    deriving (Eq, Show)

data QuestionnaireState = QuestionnaireState
    { questionnaireRequest :: !QuestionnaireRequest
    , questionnaireQuestionIndex :: !Int
    , questionnaireOptionIndex :: !Int
    , questionnaireSelections :: !(Map Int (Set Int))
    , questionnaireOtherSelected :: !(Set Int)
    , questionnaireOtherDrafts :: !(Map Int Text)
    , questionnaireChatDraft :: !Text
    , questionnaireFocus :: !QuestionnaireFocus
    , questionnaireActionIndex :: !Int
    , questionnaireNotice :: !(Maybe Text)
    }
    deriving (Eq, Show)

data QuestionnaireTransition
    = QuestionnaireContinue !QuestionnaireState
    | QuestionnaireComplete !QuestionnaireOutcome
    deriving (Eq, Show)

initialQuestionnaire :: QuestionnaireRequest -> QuestionnaireState
initialQuestionnaire request =
    QuestionnaireState
        { questionnaireRequest = request
        , questionnaireQuestionIndex = 0
        , questionnaireOptionIndex = 0
        , questionnaireSelections = Map.empty
        , questionnaireOtherSelected = Set.empty
        , questionnaireOtherDrafts = Map.empty
        , questionnaireChatDraft = ""
        , questionnaireFocus = QuestionnaireOptions
        , questionnaireActionIndex = 0
        , questionnaireNotice = Nothing
        }

stepQuestionnaire
    :: QuestionnaireCommand
    -> QuestionnaireState
    -> QuestionnaireTransition
stepQuestionnaire command state =
    case command of
        QuestionsSetFocus focus ->
            continue state { questionnaireFocus = focus }
        QuestionsMoveFocus delta ->
            continue state
                { questionnaireFocus = moveEnum delta state.questionnaireFocus
                }
        QuestionsMoveTab delta ->
            continue (moveTab delta state)
        QuestionsSelectTab index ->
            continue (selectTab index state)
        QuestionsMoveOption delta ->
            continue (moveOption delta state)
        QuestionsChooseOption index ->
            continue $
                case questionnaireCurrentQuestion state of
                    Just question
                        | index >= 0
                        , index < length question.questionOptions ->
                            activateOption
                                state
                                    { questionnaireOptionIndex = index
                                    , questionnaireFocus =
                                        QuestionnaireOptions
                                    , questionnaireNotice = Nothing
                                    }
                    _ -> state
        QuestionsChooseOther ->
            continue $
                case questionnaireCurrentQuestion state of
                    Nothing -> state
                    Just question ->
                        activateOption
                            state
                                { questionnaireOptionIndex =
                                    length question.questionOptions
                                , questionnaireFocus = QuestionnaireOptions
                                , questionnaireNotice = Nothing
                                }
        QuestionsActivateOption ->
            continue (activateOption state)
        QuestionsSetOtherDraft draft ->
            continue (setOtherDraft draft state)
        QuestionsSetChatDraft draft ->
            continue state
                { questionnaireChatDraft = draft
                , questionnaireFocus = QuestionnaireChat
                , questionnaireNotice = Nothing
                }
        QuestionsMoveAction delta ->
            continue state
                { questionnaireActionIndex =
                    wrapIndex
                        (length questionnaireActions)
                        (state.questionnaireActionIndex + delta)
                , questionnaireFocus = QuestionnaireActions
                , questionnaireNotice = Nothing
                }
        QuestionsChooseAction action ->
            chooseAction action state
        QuestionsActivateAction ->
            case atMay questionnaireActions state.questionnaireActionIndex of
                Nothing -> continue state
                Just action -> chooseAction action state
        QuestionsTimedOut ->
            QuestionnaireComplete $
                QuestionnaireTimeout state.questionnaireRequest.requestId
        QuestionsDismissExternal ident
            | ident == state.questionnaireRequest.requestId ->
                QuestionnaireComplete (QuestionnaireExternallyResolved ident)
            | otherwise ->
                continue state
  where
    continue = QuestionnaireContinue

questionnaireActions :: [QuestionnaireAction]
questionnaireActions =
    [ QuestionnaireSubmit
    , QuestionnaireClarify
    , QuestionnaireFinish
    , QuestionnaireCancel
    ]

questionnaireActionLabel :: QuestionnaireAction -> Text
questionnaireActionLabel = \case
    QuestionnaireSubmit -> "Submit answers"
    QuestionnaireClarify -> "Ask/clarify"
    QuestionnaireFinish -> "Finish interview"
    QuestionnaireCancel -> "Cancel"

questionnaireCurrentQuestion
    :: QuestionnaireState
    -> Maybe QuestionnaireQuestion
questionnaireCurrentQuestion state =
    atMay
        state.questionnaireRequest.requestQuestions
        state.questionnaireQuestionIndex

questionnaireFocusedOption
    :: QuestionnaireState
    -> Maybe QuestionnaireOption
questionnaireFocusedOption state = do
    question <- questionnaireCurrentQuestion state
    atMay question.questionOptions state.questionnaireOptionIndex

questionnaireAnswers
    :: QuestionnaireState
    -> Either Int [QuestionnaireAnswer]
questionnaireAnswers state =
    traverse answerAt (zip [0 ..] state.questionnaireRequest.requestQuestions)
  where
    answerAt (questionIndex, question) =
        let
            selected =
                Set.toAscList $
                    Map.findWithDefault
                        Set.empty
                        questionIndex
                        state.questionnaireSelections
            labels =
                [ option.optionLabel
                | optionIndex <- selected
                , Just option <- [atMay question.questionOptions optionIndex]
                ]
            other =
                Text.strip
                    <$> Map.lookup
                        questionIndex
                        state.questionnaireOtherDrafts
            validOther =
                case other of
                    Just value | not (Text.null value) -> Just value
                    _ -> Nothing
            includesOther =
                Set.member questionIndex state.questionnaireOtherSelected
        in if includesOther && validOther == Nothing
            then Left questionIndex
            else if null labels && not includesOther
            then Left questionIndex
            else Right QuestionnaireAnswer
                { answerQuestionIndex = questionIndex
                , answerQuestion = question.questionText
                , answerLabels = labels
                , answerOther =
                    if includesOther then validOther else Nothing
                }

-- | Translate non-text Vty input into a pure questionnaire command. The host
-- composer remains responsible for editing Other/chat text and sends complete
-- drafts back with 'QuestionsSetOtherDraft'/'QuestionsSetChatDraft'.
questionnaireCommandForEvent
    :: QuestionnaireState
    -> V.Event
    -> Maybe QuestionnaireCommand
questionnaireCommandForEvent state = \case
    V.EvKey (V.KChar '\t') [] -> Just (QuestionsMoveFocus 1)
    V.EvKey V.KBackTab [] -> Just (QuestionsMoveFocus (-1))
    V.EvKey V.KEsc []
        | state.questionnaireFocus
            `elem` [QuestionnaireOther, QuestionnaireChat] ->
                Just (QuestionsSetFocus QuestionnaireOptions)
        | otherwise ->
            Just (QuestionsChooseAction QuestionnaireCancel)
    V.EvKey V.KLeft []
        | state.questionnaireFocus == QuestionnaireActions ->
            Just (QuestionsMoveAction (-1))
        | questionnaireEditing state -> Nothing
        | otherwise -> Just (QuestionsMoveTab (-1))
    V.EvKey V.KRight []
        | state.questionnaireFocus == QuestionnaireActions ->
            Just (QuestionsMoveAction 1)
        | questionnaireEditing state -> Nothing
        | otherwise -> Just (QuestionsMoveTab 1)
    V.EvKey V.KUp []
        | state.questionnaireFocus == QuestionnaireOptions ->
            Just (QuestionsMoveOption (-1))
        | otherwise -> Nothing
    V.EvKey V.KDown []
        | state.questionnaireFocus == QuestionnaireOptions ->
            Just (QuestionsMoveOption 1)
        | otherwise -> Nothing
    V.EvKey V.KEnter [] ->
        case state.questionnaireFocus of
            QuestionnaireOptions -> Just QuestionsActivateOption
            QuestionnaireActions -> Just QuestionsActivateAction
            QuestionnaireOther ->
                Just (QuestionsSetFocus QuestionnaireActions)
            QuestionnaireChat ->
                Just (QuestionsChooseAction QuestionnaireClarify)
            QuestionnaireTabs ->
                Just (QuestionsSetFocus QuestionnaireOptions)
    V.EvKey (V.KChar ' ') []
        | state.questionnaireFocus == QuestionnaireOptions ->
            Just QuestionsActivateOption
    V.EvKey (V.KChar 'o') []
        | not (questionnaireEditing state) ->
            Just (QuestionsSetFocus QuestionnaireOther)
    V.EvKey (V.KChar 'c') []
        | not (questionnaireEditing state) ->
            Just (QuestionsSetFocus QuestionnaireChat)
    V.EvKey (V.KChar 'f') []
        | not (questionnaireEditing state) ->
            Just (QuestionsChooseAction QuestionnaireFinish)
    _ -> Nothing

questionnaireCommandForControl
    :: QuestionnaireControl
    -> QuestionnaireCommand
questionnaireCommandForControl = \case
    QuestionsViewport -> QuestionsSetFocus QuestionnaireOptions
    QuestionTabControl index -> QuestionsSelectTab index
    QuestionOptionControl index -> QuestionsChooseOption index
    QuestionOtherControl -> QuestionsChooseOther
    QuestionOtherEditorControl -> QuestionsSetFocus QuestionnaireOther
    QuestionChatEditorControl -> QuestionsSetFocus QuestionnaireChat
    QuestionActionControl action -> QuestionsChooseAction action

questionnaireWidget
    :: (Ord n, Show n)
    => (QuestionnaireControl -> n)
    -> QuestionnaireState
    -> Widget n
questionnaireWidget controlName state =
    vBox
        [ withAttr Theme.headerAttr (txt "Questions")
        , tabsWidget controlName state
        , viewport
            (controlName QuestionsViewport)
            Vertical
            (questionWidget controlName state)
        , clickable (controlName QuestionOtherEditorControl) $
            editorWidget
                "Other"
                "Write another answer"
                (currentOtherDraft state)
        , clickable (controlName QuestionChatEditorControl) $
            editorWidget
                "Ask or clarify"
                "Ask a freeform question before answering"
                state.questionnaireChatDraft
        , actionWidget controlName state
        , maybe emptyWidget (withAttr Theme.errorAttr . txtWrap)
            state.questionnaireNotice
        ]

tabsWidget
    :: Ord n
    => (QuestionnaireControl -> n)
    -> QuestionnaireState
    -> Widget n
tabsWidget controlName state =
    hBox $
        concat
            [ [ clickable (controlName (QuestionTabControl index)) $
                    (if index == state.questionnaireQuestionIndex
                        then withAttr Theme.selectedAttr
                        else withAttr Theme.mutedAttr)
                        (txt (" " <> Text.pack (show (index + 1)) <> " "))
              , txt " "
              ]
            | index <-
                [0 .. length state.questionnaireRequest.requestQuestions - 1]
            ]

questionWidget
    :: Ord n
    => (QuestionnaireControl -> n)
    -> QuestionnaireState
    -> Widget n
questionWidget controlName state =
    case questionnaireCurrentQuestion state of
        Nothing ->
            withAttr Theme.mutedAttr $
                txtWrap "No questions were provided."
        Just question ->
            vBox $
                [ txtWrap question.questionText
                , withAttr Theme.mutedAttr $
                    txt $
                        if question.questionMultiSelect
                            then "Select one or more options."
                            else "Select one option."
                ]
                    <> concat
                        [ optionWidgets
                            controlName
                            state
                            question
                            index
                            option
                        | (index, option) <- zip [0 ..] question.questionOptions
                        ]
                    <> otherWidgets controlName state question
                    <> previewWidgets state question

optionWidgets
    :: Ord n
    => (QuestionnaireControl -> n)
    -> QuestionnaireState
    -> QuestionnaireQuestion
    -> Int
    -> QuestionnaireOption
    -> [Widget n]
optionWidgets controlName state question index option =
    [ clickable (controlName (QuestionOptionControl index)) $
        styled $
            txt $
                selectionMarker state question index
                    <> " "
                    <> option.optionLabel
    ]
        <> [ withAttr Theme.mutedAttr $
                padLeft (Pad 4) (txtWrap option.optionDescription)
           | not (Text.null (Text.strip option.optionDescription))
           ]
  where
    styled
        | state.questionnaireOptionIndex == index =
            withAttr Theme.selectedAttr
        | otherwise = id

otherWidgets
    :: Ord n
    => (QuestionnaireControl -> n)
    -> QuestionnaireState
    -> QuestionnaireQuestion
    -> [Widget n]
otherWidgets controlName state question =
    [ clickable (controlName QuestionOtherControl) $
        styled $
            txt $
                otherMarker <> " Other"
    , withAttr Theme.mutedAttr $
        padLeft (Pad 4) $
            txtWrap $
                nonEmptyOr
                    "Provide a different answer below."
                    (currentOtherDraft state)
    ]
  where
    questionIndex = state.questionnaireQuestionIndex
    selected = Set.member questionIndex state.questionnaireOtherSelected
    focused =
        state.questionnaireOptionIndex == length question.questionOptions
    otherMarker
        | question.questionMultiSelect =
            if selected then "[x]" else "[ ]"
        | otherwise =
            if selected then "(•)" else "( )"
    styled
        | focused = withAttr Theme.selectedAttr
        | otherwise = id

previewWidgets
    :: QuestionnaireState
    -> QuestionnaireQuestion
    -> [Widget n]
previewWidgets state question
    | question.questionMultiSelect = []
    | otherwise =
        case questionnaireFocusedOption state >>= (.optionPreview) of
            Just preview | not (Text.null (Text.strip preview)) ->
                [ withAttr Theme.headerAttr (txt "Preview")
                , padLeft (Pad 2) (txtWrap preview)
                ]
            _ -> []

selectionMarker
    :: QuestionnaireState
    -> QuestionnaireQuestion
    -> Int
    -> Text
selectionMarker state question optionIndex =
    let
        selected =
            Set.member optionIndex $
                Map.findWithDefault
                    Set.empty
                    state.questionnaireQuestionIndex
                    state.questionnaireSelections
    in if question.questionMultiSelect
        then if selected then "[x]" else "[ ]"
        else if selected then "(•)" else "( )"

editorWidget :: Text -> Text -> Text -> Widget n
editorWidget title placeholder draft =
    vBox
        [ withAttr Theme.mutedAttr (txt title)
        , txtWrap (nonEmptyOr placeholder draft)
        ]

actionWidget
    :: Ord n
    => (QuestionnaireControl -> n)
    -> QuestionnaireState
    -> Widget n
actionWidget controlName state =
    hBox $
        concat
            [ [ clickable (controlName (QuestionActionControl action)) $
                    (if index == state.questionnaireActionIndex
                        then withAttr Theme.selectedAttr
                        else id)
                        (txt (questionnaireActionLabel action))
              , txt "  "
              ]
            | (index, action) <- zip [0 ..] questionnaireActions
            ]

chooseAction
    :: QuestionnaireAction
    -> QuestionnaireState
    -> QuestionnaireTransition
chooseAction action state =
    case action of
        QuestionnaireSubmit ->
            case questionnaireAnswers state of
                Left questionIndex ->
                    QuestionnaireContinue state
                        { questionnaireQuestionIndex = questionIndex
                        , questionnaireOptionIndex = 0
                        , questionnaireFocus = QuestionnaireOptions
                        , questionnaireNotice =
                            Just $
                                "Answer question "
                                    <> Text.pack (show (questionIndex + 1))
                                    <> " before submitting."
                        }
                Right answers ->
                    QuestionnaireComplete $
                        QuestionnaireSubmitted QuestionnaireSubmission
                            { submissionRequestId =
                                state.questionnaireRequest.requestId
                            , submissionAnswers = answers
                            }
        QuestionnaireClarify ->
            let clarification = Text.strip state.questionnaireChatDraft
            in if Text.null clarification
                then QuestionnaireContinue state
                    { questionnaireFocus = QuestionnaireChat
                    , questionnaireNotice =
                        Just "Enter a question or clarification first."
                    }
                else QuestionnaireComplete $
                    QuestionnaireClarificationRequested
                        state.questionnaireRequest.requestId
                        clarification
        QuestionnaireFinish ->
            QuestionnaireComplete $
                QuestionnaireFinished state.questionnaireRequest.requestId
        QuestionnaireCancel ->
            QuestionnaireComplete $
                QuestionnaireCancelled state.questionnaireRequest.requestId

moveTab :: Int -> QuestionnaireState -> QuestionnaireState
moveTab delta state =
    let
        count = length state.questionnaireRequest.requestQuestions
        next = wrapIndex count (state.questionnaireQuestionIndex + delta)
    in state
        { questionnaireQuestionIndex = next
        , questionnaireOptionIndex = 0
        , questionnaireFocus = QuestionnaireTabs
        , questionnaireNotice = Nothing
        }

selectTab :: Int -> QuestionnaireState -> QuestionnaireState
selectTab index state =
    let count = length state.questionnaireRequest.requestQuestions
    in if index < 0 || index >= count
        then state
        else state
            { questionnaireQuestionIndex = index
            , questionnaireOptionIndex = 0
            , questionnaireFocus = QuestionnaireTabs
            , questionnaireNotice = Nothing
            }

moveOption :: Int -> QuestionnaireState -> QuestionnaireState
moveOption delta state =
    case questionnaireCurrentQuestion state of
        Nothing -> state
        Just question ->
            let count = length question.questionOptions + 1
            in state
                { questionnaireOptionIndex =
                    wrapIndex count (state.questionnaireOptionIndex + delta)
                , questionnaireFocus = QuestionnaireOptions
                , questionnaireNotice = Nothing
                }

activateOption :: QuestionnaireState -> QuestionnaireState
activateOption state =
    case questionnaireCurrentQuestion state of
        Nothing -> state
        Just question
            | state.questionnaireOptionIndex
                == length question.questionOptions ->
                toggleOther question state
            | otherwise ->
                toggleKnownOption question state

toggleKnownOption
    :: QuestionnaireQuestion
    -> QuestionnaireState
    -> QuestionnaireState
toggleKnownOption question state =
    let
        questionIndex = state.questionnaireQuestionIndex
        optionIndex = state.questionnaireOptionIndex
        old =
            Map.findWithDefault
                Set.empty
                questionIndex
                state.questionnaireSelections
        selected
            | question.questionMultiSelect =
                toggleSetMember optionIndex old
            | otherwise = Set.singleton optionIndex
    in state
        { questionnaireSelections =
            Map.insert questionIndex selected state.questionnaireSelections
        , questionnaireOtherSelected =
            if question.questionMultiSelect
                then state.questionnaireOtherSelected
                else Set.delete
                    questionIndex
                    state.questionnaireOtherSelected
        , questionnaireFocus = QuestionnaireOptions
        , questionnaireNotice = Nothing
        }

toggleOther
    :: QuestionnaireQuestion
    -> QuestionnaireState
    -> QuestionnaireState
toggleOther question state =
    let
        questionIndex = state.questionnaireQuestionIndex
        selected
            | question.questionMultiSelect =
                toggleSetMember
                    questionIndex
                    state.questionnaireOtherSelected
            | otherwise =
                Set.insert questionIndex state.questionnaireOtherSelected
    in state
        { questionnaireOtherSelected = selected
        , questionnaireSelections =
            if question.questionMultiSelect
                then state.questionnaireSelections
                else Map.delete
                    questionIndex
                    state.questionnaireSelections
        , questionnaireFocus = QuestionnaireOther
        , questionnaireNotice = Nothing
        }

setOtherDraft :: Text -> QuestionnaireState -> QuestionnaireState
setOtherDraft draft state =
    let
        questionIndex = state.questionnaireQuestionIndex
        selected =
            if Text.null (Text.strip draft)
                then Set.delete
                    questionIndex
                    state.questionnaireOtherSelected
                else Set.insert
                    questionIndex
                    state.questionnaireOtherSelected
        selections =
            case questionnaireCurrentQuestion state of
                Just question | not question.questionMultiSelect ->
                    Map.delete questionIndex state.questionnaireSelections
                _ -> state.questionnaireSelections
    in state
        { questionnaireOtherDrafts =
            Map.insert questionIndex draft state.questionnaireOtherDrafts
        , questionnaireOtherSelected = selected
        , questionnaireSelections = selections
        , questionnaireFocus = QuestionnaireOther
        , questionnaireNotice = Nothing
        }

currentOtherDraft :: QuestionnaireState -> Text
currentOtherDraft state =
    Map.findWithDefault
        ""
        state.questionnaireQuestionIndex
        state.questionnaireOtherDrafts

toggleSetMember :: Ord a => a -> Set a -> Set a
toggleSetMember value values
    | Set.member value values = Set.delete value values
    | otherwise = Set.insert value values

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

atMay :: [a] -> Int -> Maybe a
atMay values index
    | index < 0 = Nothing
    | otherwise = case drop index values of
        value : _ -> Just value
        [] -> Nothing

questionnaireEditing :: QuestionnaireState -> Bool
questionnaireEditing state =
    state.questionnaireFocus
        `elem` [QuestionnaireOther, QuestionnaireChat]
