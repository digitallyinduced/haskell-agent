module Agent.TUI.QuestionnaireSpec (spec) where

import Agent.TUI.Questionnaire
import qualified Agent.TUI.Theme as Theme
import Brick (Widget, renderWidget)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as V
import Test.Hspec

spec :: Spec
spec = describe "structured questionnaire state" do
    it "preserves selections and Other drafts across question tabs" do
        let
            state =
                applyCommands
                    [ QuestionsActivateOption
                    , QuestionsSetOtherDraft "A different deployment"
                    , QuestionsMoveTab 1
                    , QuestionsMoveOption 1
                    , QuestionsActivateOption
                    , QuestionsMoveTab (-1)
                    ]
                    (initialQuestionnaire request)
        Map.lookup 0 state.questionnaireSelections
            `shouldBe` Just (Set.singleton 0)
        Map.lookup 0 state.questionnaireOtherDrafts
            `shouldBe` Just "A different deployment"
        Set.member 0 state.questionnaireOtherSelected `shouldBe` True
        Map.lookup 1 state.questionnaireSelections
            `shouldBe` Just (Set.singleton 1)

    it "keeps single-select options mutually exclusive with Other" do
        let
            second =
                continuing $
                    stepQuestionnaire
                        (QuestionsSelectTab 1)
                        (initialQuestionnaire request)
            selected =
                continuing $
                    stepQuestionnaire
                        QuestionsActivateOption
                        second
            other =
                continuing $
                    stepQuestionnaire
                        (QuestionsSetOtherDraft "Custom")
                        selected
        Map.lookup 1 selected.questionnaireSelections
            `shouldBe` Just (Set.singleton 0)
        Map.lookup 1 other.questionnaireSelections `shouldBe` Nothing
        Set.member 1 other.questionnaireOtherSelected `shouldBe` True

    it "retains option descriptions and exposes single-select previews" do
        let state =
                continuing $
                    stepQuestionnaire
                        (QuestionsSelectTab 1)
                        (initialQuestionnaire request)
        questionnaireFocusedOption state
            `shouldBe`
                Just QuestionnaireOption
                    { optionLabel = "Safe"
                    , optionDescription = "Use the conservative rollout."
                    , optionPreview = Just "10% → 50% → 100%"
                    }

    it "moves to the first incomplete tab instead of submitting" do
        let state =
                continuing $
                    stepQuestionnaire
                        QuestionsActivateOption
                        (initialQuestionnaire request)
            result =
                stepQuestionnaire
                    (QuestionsChooseAction QuestionnaireSubmit)
                    state
        case result of
            QuestionnaireContinue next -> do
                next.questionnaireQuestionIndex `shouldBe` 1
                next.questionnaireNotice
                    `shouldBe` Just "Answer question 2 before submitting."
            QuestionnaireComplete outcome ->
                expectationFailure ("unexpected completion: " <> show outcome)

    it "requires text when Other is selected" do
        let
            selectedOther =
                applyCommands
                    [ QuestionsMoveOption 2
                    , QuestionsActivateOption
                    , QuestionsMoveTab 1
                    , QuestionsActivateOption
                    ]
                    (initialQuestionnaire request)
        questionnaireAnswers selectedOther `shouldBe` Left 0

    it "submits typed answers for multi- and single-select questions" do
        let
            answered =
                applyCommands
                    [ QuestionsActivateOption
                    , QuestionsMoveOption 1
                    , QuestionsActivateOption
                    , QuestionsMoveTab 1
                    , QuestionsActivateOption
                    ]
                    (initialQuestionnaire request)
        stepQuestionnaire
            (QuestionsChooseAction QuestionnaireSubmit)
            answered
            `shouldBe`
                QuestionnaireComplete
                    (QuestionnaireSubmitted QuestionnaireSubmission
                        { submissionRequestId =
                            QuestionnaireId "questions-1"
                        , submissionAnswers =
                            [ QuestionnaireAnswer
                                { answerQuestionIndex = 0
                                , answerQuestion = "Where should this run?"
                                , answerLabels = ["Local", "Cloud"]
                                , answerOther = Nothing
                                }
                            , QuestionnaireAnswer
                                { answerQuestionIndex = 1
                                , answerQuestion = "Which rollout?"
                                , answerLabels = ["Safe"]
                                , answerOther = Nothing
                                }
                            ]
                        })

    it "emits typed clarification, finish, cancellation, and timeout outcomes" do
        let
            initial = initialQuestionnaire request
            chat =
                continuing $
                    stepQuestionnaire
                        (QuestionsSetChatDraft "  What scale?  ")
                        initial
        stepQuestionnaire
            (QuestionsChooseAction QuestionnaireClarify)
            chat
            `shouldBe`
                QuestionnaireComplete
                    (QuestionnaireClarificationRequested
                        (QuestionnaireId "questions-1")
                        "What scale?")
        stepQuestionnaire
            (QuestionsChooseAction QuestionnaireFinish)
            initial
            `shouldBe`
                QuestionnaireComplete
                    (QuestionnaireFinished (QuestionnaireId "questions-1"))
        stepQuestionnaire
            (QuestionsChooseAction QuestionnaireCancel)
            initial
            `shouldBe`
                QuestionnaireComplete
                    (QuestionnaireCancelled (QuestionnaireId "questions-1"))
        stepQuestionnaire QuestionsTimedOut initial
            `shouldBe`
                QuestionnaireComplete
                    (QuestionnaireTimeout (QuestionnaireId "questions-1"))

    it "ignores stale external resolutions" do
        let state = initialQuestionnaire request
        stepQuestionnaire
            (QuestionsDismissExternal (QuestionnaireId "stale"))
            state
            `shouldBe` QuestionnaireContinue state
        stepQuestionnaire
            (QuestionsDismissExternal (QuestionnaireId "questions-1"))
            state
            `shouldBe`
                QuestionnaireComplete
                    (QuestionnaireExternallyResolved
                        (QuestionnaireId "questions-1"))

    it "renders tabs, descriptions, previews, Other, and actions" do
        let
            state =
                continuing $
                    stepQuestionnaire
                        (QuestionsSelectTab 1)
                        (initialQuestionnaire request)
            rendered =
                show $
                    renderWidget
                        (Just Theme.terminalDefault)
                        [ questionnaireWidget
                            (Text.pack . show)
                            state
                            :: Widget Text
                        ]
                        (80, 24)
        rendered `shouldSatisfy` isInfixOf "Which rollout?"
        rendered `shouldSatisfy` isInfixOf "Use the conservative rollout."
        rendered `shouldSatisfy` isInfixOf "Finish interview"

    it "maps tab, option, action, and editor input without stealing draft text" do
        let
            state = initialQuestionnaire request
            editing =
                state { questionnaireFocus = QuestionnaireOther }
        questionnaireCommandForEvent state (V.EvKey V.KRight [])
            `shouldBe` Just (QuestionsMoveTab 1)
        questionnaireCommandForEvent state (V.EvKey (V.KChar ' ') [])
            `shouldBe` Just QuestionsActivateOption
        questionnaireCommandForEvent editing (V.EvKey (V.KChar 'f') [])
            `shouldBe` Nothing
        questionnaireCommandForEvent editing (V.EvKey V.KEsc [])
            `shouldBe` Just (QuestionsSetFocus QuestionnaireOptions)
        questionnaireCommandForControl (QuestionOptionControl 1)
            `shouldBe` QuestionsChooseOption 1

request :: QuestionnaireRequest
request =
    QuestionnaireRequest
        { requestId = QuestionnaireId "questions-1"
        , requestQuestions =
            [ QuestionnaireQuestion
                { questionText = "Where should this run?"
                , questionOptions =
                    [ QuestionnaireOption
                        { optionLabel = "Local"
                        , optionDescription = "Run on this machine."
                        , optionPreview = Nothing
                        }
                    , QuestionnaireOption
                        { optionLabel = "Cloud"
                        , optionDescription = "Run in managed infrastructure."
                        , optionPreview = Nothing
                        }
                    ]
                , questionMultiSelect = True
                }
            , QuestionnaireQuestion
                { questionText = "Which rollout?"
                , questionOptions =
                    [ QuestionnaireOption
                        { optionLabel = "Safe"
                        , optionDescription = "Use the conservative rollout."
                        , optionPreview = Just "10% → 50% → 100%"
                        }
                    , QuestionnaireOption
                        { optionLabel = "Fast"
                        , optionDescription = "Roll out immediately."
                        , optionPreview = Just "100%"
                        }
                    ]
                , questionMultiSelect = False
                }
            ]
        }

continuing :: QuestionnaireTransition -> QuestionnaireState
continuing = \case
    QuestionnaireContinue state -> state
    QuestionnaireComplete outcome ->
        error ("unexpected completed questionnaire: " <> show outcome)

applyCommands
    :: [QuestionnaireCommand]
    -> QuestionnaireState
    -> QuestionnaireState
applyCommands commands initial =
    foldl
        (\state command -> continuing (stepQuestionnaire command state))
        initial
        commands
