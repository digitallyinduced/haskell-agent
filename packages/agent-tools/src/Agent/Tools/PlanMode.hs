-- | Shared plan-mode state: explore/write only @plan.md@, then present for
-- approve / request-changes / cancel.
--
-- Wire names for Grok follow grok-build (@enter_plan_mode@ / @exit_plan_mode@).
-- Codex presents plans via a @\<proposed_plan\>@ block in assistant text;
-- @update_plan@ stays a separate progress checklist and is blocked while
-- plan mode is active.
module Agent.Tools.PlanMode
    ( PlanModeState(..)
    , PlanDecision(..)
    , PlanCompletion(..)
    , PlanModeEnv(..)
    , PlanModeHooks(..)
    , newPlanModeEnv
    , setPlanModeInputWaitHooks
    , planFileName
    , planFilePath
    , isPlanModeActive
    , activatePlanMode
    , deactivatePlanMode
    , readPlanMarkdown
    , writePlanMarkdown
    , planModeReminder
    , planApprovedContinuation
    , planModeBlockedEditMessage
    , isPlanFileEditTarget
    , enterPlanModeTool
    , enterCodexPlanModeTool
    , writePlanTool
    , exitPlanModeTool
    , askUserQuestionTool
    , answerAskUserQuestionInput
    ) where

import Agent.FileRetry (retryOnFileBusy)
import Agent.Json.Decode (Decoder)
import qualified Agent.Json.Decode as Json
import Agent.OsPath (toText, unsafeToFilePath)
import Agent.ToolArgs (objectArgs, optBool, optList, optText, reqText)
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    )
import Agent.ToolDispatch (typedTool)
import Agent.Tools.Types
    ( AppTool
    , ToolExecutionPolicy(..)
    , jsonTool
    )
import Control.Applicative ((<|>))
import Control.Exception.Safe (bracket_, tryAny)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LazyByteString
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory.OsPath (createDirectoryIfMissing, doesFileExist)
import System.OsPath (OsPath, equalFilePath, takeDirectory, unsafeEncodeUtf, (</>))

data PlanModeState
    = PlanInactive
    | PlanPending
    -- ^ User toggled plan mode; becomes Active on the next prompt.
    | PlanActive
    deriving (Eq, Show)

data PlanCompletion
    = CompleteWithExitTool
    | CompleteWithProposedPlan
    deriving (Eq, Show)

data PlanDecision
    = PlanApprove
    | PlanRequestChanges Text
    | PlanCancel
    deriving (Eq, Show)

-- | Session-scoped plan mode. 'planSessionDir' is the persisted session
-- directory when known; otherwise plans live under the tool cwd.
data PlanModeEnv = PlanModeEnv
    { planStateRef :: !(IORef PlanModeState)
    , planSessionDir :: !(IORef (Maybe OsPath))
    , planFallbackDir :: !OsPath
    , planHooks :: !PlanModeHooks
    , planInputWaitHooks :: !(IORef (IO (), IO ()))
    }

data PlanModeHooks = PlanModeHooks
    { planConfirmEnter :: !(Text -> IO Bool)
    -- ^ Ask the user before agent-initiated enter_plan_mode.
    , planDecideExit :: !(Text -> IO PlanDecision)
    -- ^ Present plan markdown; return approve / changes / cancel.
    , planAskQuestion :: !(Text -> [Text] -> IO (Maybe Text))
    -- ^ Optional multiple-choice style question during planning.
    }

planFileName :: OsPath
planFileName = unsafeEncodeUtf "plan.md"

defaultHooks :: PlanModeHooks
defaultHooks = PlanModeHooks
    { planConfirmEnter = \_ -> pure True
    , planDecideExit = \_ -> pure PlanApprove
    , planAskQuestion = \_ _ -> pure Nothing
    }

newPlanModeEnv :: OsPath -> Maybe PlanModeHooks -> IO PlanModeEnv
newPlanModeEnv fallbackDir hooks = do
    stateRef <- newIORef PlanInactive
    sessionRef <- newIORef Nothing
    inputWaitHooksRef <- newIORef (pure (), pure ())
    let baseHooks = fromMaybe defaultHooks hooks
        withInputWait action = do
            (beginWait, endWait) <- readIORef inputWaitHooksRef
            bracket_ beginWait endWait action
        wrappedHooks = PlanModeHooks
            { planConfirmEnter = \reason ->
                withInputWait (baseHooks.planConfirmEnter reason)
            , planDecideExit = \markdown ->
                withInputWait (baseHooks.planDecideExit markdown)
            , planAskQuestion = \question options ->
                withInputWait (baseHooks.planAskQuestion question options)
            }
    pure PlanModeEnv
        { planStateRef = stateRef
        , planSessionDir = sessionRef
        , planFallbackDir = fallbackDir
        , planHooks = wrappedHooks
        , planInputWaitHooks = inputWaitHooksRef
        }

-- | Configure callbacks which bracket every plan-mode prompt that can wait
-- for the user. The default callbacks are no-ops for non-interactive hosts.
setPlanModeInputWaitHooks :: PlanModeEnv -> IO () -> IO () -> IO ()
setPlanModeInputWaitHooks env beginWait endWait =
    writeIORef env.planInputWaitHooks (beginWait, endWait)

planFilePath :: PlanModeEnv -> IO OsPath
planFilePath env = do
    sessionDir <- readIORef env.planSessionDir
    pure $ case sessionDir of
        Just dir -> dir </> planFileName
        Nothing -> env.planFallbackDir </> planFileName

isPlanModeActive :: PlanModeEnv -> IO Bool
isPlanModeActive env = (== PlanActive) <$> readIORef env.planStateRef

activatePlanMode :: PlanModeEnv -> IO ()
activatePlanMode env = writeIORef env.planStateRef PlanActive

deactivatePlanMode :: PlanModeEnv -> IO ()
deactivatePlanMode env = writeIORef env.planStateRef PlanInactive

readPlanMarkdown :: PlanModeEnv -> IO Text
readPlanMarkdown env = do
    path <- planFilePath env
    exists <- doesFileExist path
    if not exists
        then pure ""
        else either (const "") id
            <$> tryAny (retryOnFileBusy (Text.readFile (unsafeToFilePath path)))

writePlanMarkdown :: PlanModeEnv -> Text -> IO (Either Text ())
writePlanMarkdown env content = do
    path <- planFilePath env
    createDirectoryIfMissing True (takeDirectory path)
    result <- tryAny (retryOnFileBusy (Text.writeFile (unsafeToFilePath path) content))
    pure $ case result of
        Left err -> Left ("failed to write plan file: " <> Text.pack (show err))
        Right () -> Right ()

planModeReminder :: PlanCompletion -> OsPath -> Text
planModeReminder completion path =
    Text.unlines
        [ "Plan mode is active. Do not make any edits or writes to the system except for the plan file."
        , ""
        , "## Plan File"
        , "Write your plan to `" <> toText path <> "`."
        , "Use `write_plan` when it is available; otherwise use the provider's dedicated plan-file edit tool."
        , "That is the only file you may create or modify."
        , "Inspect the repository with dedicated read-only tools such as `read_file`, `grep`, and `list_dir`; shell tools are unavailable in Plan Mode."
        , ""
        , completionInstruction completion
        ]

completionInstruction :: PlanCompletion -> Text
completionInstruction = \case
    CompleteWithExitTool ->
        "When the plan is ready, call `exit_plan_mode` so the user can approve, request changes, or cancel."
    CompleteWithProposedPlan ->
        "When the plan is ready, end your turn with a complete `<proposed_plan>` … `</proposed_plan>` block so the user can approve, request changes, or cancel."

planApprovedContinuation :: Text
planApprovedContinuation =
    "The user approved the plan. Plan mode is now off. "
        <> "Begin implementing the approved plan immediately. "
        <> "Do not wait for another user message."

planModeBlockedEditMessage :: OsPath -> Text
planModeBlockedEditMessage path =
    "Rejected: file edits are not allowed in plan mode - the only editable file is the plan file ("
        <> toText path
        <> ")."

-- | True when @target@ refers to this session's plan.md (absolute or basename).
isPlanFileEditTarget :: OsPath -> OsPath -> Bool
isPlanFileEditTarget planPath target =
    equalFilePath planPath target
        || equalFilePath planFileName target

--------------------------------------------------------------------------------
-- Grok-build tools
--------------------------------------------------------------------------------

data EnterPlanArgs = EnterPlanArgs
    { explanation :: Maybe Text
    }

enterPlanArgsDecoder :: Decoder EnterPlanArgs
enterPlanArgsDecoder = objectArgs \object -> EnterPlanArgs
        <$> optText object "explanation"

enterPlanModeTool :: PlanModeEnv -> AppTool
enterPlanModeTool env =
    enterPlanModeToolWith CompleteWithExitTool env

enterCodexPlanModeTool :: PlanModeEnv -> AppTool
enterCodexPlanModeTool env =
    enterPlanModeToolWith CompleteWithProposedPlan env

enterPlanModeToolWith :: PlanCompletion -> PlanModeEnv -> AppTool
enterPlanModeToolWith completion env = jsonTool "enter_plan_mode"
    (enterPlanDescription completion)
    [ PropertySchema "explanation" PropertyString False $ Just
        "Optional reason this task needs a planning phase before implementation."
    ]
    -- The tool performs its own explicit user confirmation through
    -- planConfirmEnter, so it must not also trigger generic tool approval.
    True
    TurnSequential
    (typedTool "enter_plan_mode" enterPlanArgsDecoder (runEnterPlanMode completion env))

enterPlanDescription :: PlanCompletion -> Text
enterPlanDescription completion =
    "Enter plan mode when a task has genuine architectural ambiguity.\n\
    \Requires user approval. While active, only the session plan.md file may be edited;\n\
    \explore the codebase, write the plan, then "
        <> case completion of
            CompleteWithExitTool -> "call exit_plan_mode for approval."
            CompleteWithProposedPlan ->
                "present it in a complete <proposed_plan> block for approval."

runEnterPlanMode
    :: PlanCompletion
    -> PlanModeEnv
    -> EnterPlanArgs
    -> IO (Either Text Text)
runEnterPlanMode completion env args = do
    active <- isPlanModeActive env
    if active
        then pure $ Right "Plan mode is already active."
        else do
            let reason = fromMaybe "Enter plan mode to design an approach before coding." args.explanation
            ok <- env.planHooks.planConfirmEnter reason
            if not ok
                then pure $ Left "User declined plan mode. Stay in normal mode and continue."
                else do
                    path <- planFilePath env
                    activatePlanMode env
                    pure $ Right $
                        "You have entered plan mode. Explore the codebase and write an implementation plan to "
                            <> toText path
                            <> ". "
                            <> completionInstruction completion

data WritePlanArgs = WritePlanArgs
    { content :: Text
    }

writePlanArgsDecoder :: Decoder WritePlanArgs
writePlanArgsDecoder = objectArgs \object ->
        WritePlanArgs <$> reqText object "content"

writePlanTool :: PlanModeEnv -> AppTool
writePlanTool env = jsonTool "write_plan" writePlanDescription
    [ PropertySchema "content" PropertyString True $ Just
        "Complete Markdown content to store in the session plan.md file."
    ]
    True
    TurnSequential
    (typedTool "write_plan" writePlanArgsDecoder (runWritePlan env))

writePlanDescription :: Text
writePlanDescription =
    "Write the current implementation plan to the session plan.md file.\n\
    \This tool is available only while Plan Mode is active and cannot write any other path."

runWritePlan :: PlanModeEnv -> WritePlanArgs -> IO (Either Text Text)
runWritePlan env args = do
    active <- isPlanModeActive env
    if not active
        then pure (Left "Plan mode is not active.")
        else writePlanMarkdown env args.content >>= \case
            Left err -> pure (Left err)
            Right () -> do
                path <- planFilePath env
                pure $ Right $
                    "Wrote the plan to " <> toText path
                        <> ". Continue planning or present it for approval when ready."

data ExitPlanArgs = ExitPlanArgs
    { summary :: Maybe Text
    }

exitPlanArgsDecoder :: Decoder ExitPlanArgs
exitPlanArgsDecoder = objectArgs \object -> ExitPlanArgs
        <$> optText object "summary"

exitPlanModeTool :: PlanModeEnv -> AppTool
exitPlanModeTool env = jsonTool "exit_plan_mode" exitPlanDescription
    [ PropertySchema "summary" PropertyString False $ Just
        "Optional short summary shown with the plan approval prompt."
    ]
    False
    TurnSequential
    (typedTool "exit_plan_mode" exitPlanArgsDecoder (runExitPlanMode env))

exitPlanDescription :: Text
exitPlanDescription =
    "Present the plan file for user approval and leave plan mode if approved.\n\
    \The plan is read from plan.md on disk (not passed as an argument).\n\
    \The user may approve (start implementing), request changes (stay in plan mode),\n\
    \or cancel (abandon the plan and turn plan mode off)."

runExitPlanMode :: PlanModeEnv -> ExitPlanArgs -> IO (Either Text Text)
runExitPlanMode env args = do
    active <- isPlanModeActive env
    if not active
        then pure $ Left "Plan mode is not active."
        else do
            content <- readPlanMarkdown env
            path <- planFilePath env
            let body
                    | Text.null (Text.strip content) =
                        "No plan written yet.\n\n(expected plan file: " <> toText path <> ")"
                    | otherwise = content
                header = case args.summary of
                    Just s | not (Text.null (Text.strip s)) -> s <> "\n\n"
                    _ -> ""
            decision <- env.planHooks.planDecideExit (header <> body)
            case decision of
                PlanApprove -> do
                    deactivatePlanMode env
                    pure (Right planApprovedContinuation)
                PlanRequestChanges notes ->
                    pure $ Right $
                        "The user requested changes to the plan. Stay in plan mode and revise plan.md.\n"
                            <> "Feedback:\n"
                            <> notes
                PlanCancel -> do
                    deactivatePlanMode env
                    pure $ Right
                        "The user cancelled the plan. Plan mode is off. Do not call exit_plan_mode again unless asked to re-enter plan mode."

data AskUserQuestionOption = AskUserQuestionOption
    { label :: Text
    , description :: Text
    , preview :: Maybe Text
    }

askUserQuestionOptionDecoder :: Decoder AskUserQuestionOption
askUserQuestionOptionDecoder = objectArgs \object ->
        AskUserQuestionOption
            <$> reqText object "label"
            <*> reqText object "description"
            <*> optText object "preview"

data AskUserQuestion = AskUserQuestion
    { question :: Text
    , options :: [AskUserQuestionOption]
    , multiSelect :: Maybe Bool
    }

askUserQuestionDecoder :: Decoder AskUserQuestion
askUserQuestionDecoder = objectArgs \object -> do
        question <- reqText object "question"
        options <- optList askUserQuestionOptionDecoder object "options" "Expected array for key: options"
            >>= maybe (fail "Missing parameter: options") pure
        multiSelectSnake <- optBool object "multi_select"
        multiSelectCamel <- optBool object "multiSelect"
        let multiSelect = multiSelectSnake <|> multiSelectCamel
        pure AskUserQuestion { question, options, multiSelect }

newtype AskUserQuestionArgs = AskUserQuestionArgs
    { questions :: [AskUserQuestion]
    }

data AskQuestionResult
    = AskQuestionAnswered (Text, Text)
    | AskQuestionBack

data AskReviewDecision
    = AskReviewSubmit
    | AskReviewBack

askUserQuestionArgsDecoder :: Decoder AskUserQuestionArgs
askUserQuestionArgsDecoder = objectArgs \object -> do
        modern <- optList askUserQuestionDecoder object "questions" "Expected array for key: questions"
        case modern of
            Just questions -> pure AskUserQuestionArgs { questions }
            Nothing -> do
                -- Compatibility with the original single-question shape.
                question <- reqText object "question"
                rawOptions <- optText object "options"
                let options =
                        [ AskUserQuestionOption
                            { label = choice
                            , description = ""
                            , preview = Nothing
                            }
                        | choice <- parseOptions (fromMaybe "" rawOptions)
                        ]
                pure AskUserQuestionArgs
                    { questions =
                        [ AskUserQuestion
                            { question
                            , options
                            , multiSelect = Nothing
                            }
                        ]
                    }

askUserQuestionTool :: PlanModeEnv -> AppTool
askUserQuestionTool env = jsonTool "ask_user_question" askUserDescription
    [ PropertySchema "questions"
        (PropertyArray (PropertyObject
            [ PropertySchema "question" PropertyString True $ Just
                "The question to ask, phrased as a full question."
            , PropertySchema "options"
                (PropertyArray (PropertyObject
                    [ PropertySchema "label" PropertyString True $ Just
                        "Option text shown to the user. A few words at most."
                    , PropertySchema "description" PropertyString True $ Just
                        "What picking this option means or implies."
                    , PropertySchema "preview" PropertyString False $ Just
                        "Optional content shown while the option is focused, such as a mockup or code snippet. Single-select questions only."
                    ]))
                True
                (Just "The choices for this question.")
            , PropertySchema "multi_select" PropertyBoolean False $ Just
                "Let the user pick more than one option (default false)."
            ]))
        True
        (Just "The questions to ask, each with its own options.")
    ]
    True
    TurnSequential
    (typedTool "ask_user_question" askUserQuestionArgsDecoder (runAskUserQuestion env))

askUserDescription :: Text
askUserDescription =
    "Ask the user one or more multiple-choice questions. "
        <> "This tool works both inside and outside plan mode."

runAskUserQuestion :: PlanModeEnv -> AskUserQuestionArgs -> IO (Either Text Text)
runAskUserQuestion env args
    | null args.questions =
        pure (Right "No questions provided. Continue with the task.")
    | otherwise = do
        answers <- collectAnswers
        pure (formatAnswers <$> answers)
  where
    collectAnswers
        :: IO (Either Text [(Text, Text)])
    collectAnswers = go 0 []
      where
        questionCount = length args.questions

        go index answers
            | index >= questionCount =
                reviewAnswers answers >>= \case
                    Left err -> pure (Left err)
                    Right AskReviewSubmit -> pure (Right answers)
                    Right AskReviewBack ->
                        let previousIndex = questionCount - 1
                        in go previousIndex (take previousIndex answers)
            | otherwise =
                case drop index args.questions of
                    [] -> pure (Right answers)
                    question : _ ->
                        ask (index > 0) question >>= \case
                            Left err -> pure (Left err)
                            Right AskQuestionBack ->
                                let previousIndex = index - 1
                                in go previousIndex (take previousIndex answers)
                            Right (AskQuestionAnswered answer) ->
                                go
                                    (index + 1)
                                    (take index answers <> [answer])

    reviewAnswers
        :: [(Text, Text)]
        -> IO (Either Text AskReviewDecision)
    reviewAnswers answers = do
        answer <- env.planHooks.planAskQuestion
            (formatAnswerReview answers)
            [submitAnswersChoice, backToLastQuestionChoice]
        pure $ case answer of
            Nothing -> Left "No answer from user."
            Just text
                | Text.null (Text.strip text) ->
                    Left "No answer from user."
                | text == submitAnswersChoice ->
                    Right AskReviewSubmit
                | text == backToLastQuestionChoice ->
                    Right AskReviewBack
                | otherwise ->
                    Left "No answer from user."

    ask
        :: Bool
        -> AskUserQuestion
        -> IO (Either Text AskQuestionResult)
    ask allowBack question
        | question.multiSelect == Just True =
            askMultiple allowBack question
        | otherwise = do
            let displayed = map formatOption question.options
                backChoice = optionalBackChoice allowBack displayed
                customChoice =
                    freshControlChoice
                        customReplyChoice
                        (displayed <> [choice | Just choice <- [backChoice]])
                choices =
                    displayed
                        <> [choice | Just choice <- [backChoice]]
                        <> [customChoice]
                labelsByChoice =
                    Map.fromList
                        (zip displayed (map (.label) question.options))
            answer <- env.planHooks.planAskQuestion question.question choices
            case answer of
                Nothing -> pure (Left "No answer from user.")
                Just text | Text.null (Text.strip text) ->
                    pure (Left "No answer from user.")
                Just text | Just text == backChoice ->
                    pure (Right AskQuestionBack)
                Just text | text == customChoice ->
                    askCustomReply env question.question >>= \case
                        Left err -> pure (Left err)
                        Right custom ->
                            pure $ Right $ AskQuestionAnswered
                                (question.question, custom)
                Just text ->
                    pure $ Right $ AskQuestionAnswered
                        ( question.question
                        , fromMaybe text (Map.lookup text labelsByChoice)
                        )

    askMultiple
        :: Bool
        -> AskUserQuestion
        -> IO (Either Text AskQuestionResult)
    askMultiple allowBack question =
        choose [] question.options
      where
        doneChoice = "Done selecting"
        backChoice =
            optionalBackChoice
                allowBack
                (doneChoice : map formatOption question.options)

        choose selected remaining = do
            let displayed = map formatOption remaining
                labelsByDisplayed =
                    Map.fromList (zip displayed (map (.label) remaining))
                customChoice =
                    freshControlChoice
                        customReplyChoice
                        ( displayed
                            <> [doneChoice]
                            <> [choice | Just choice <- [backChoice]]
                        )
                choices =
                    displayed
                        <> [doneChoice]
                        <> [choice | Just choice <- [backChoice]]
                        <> [customChoice]
                prompt
                    | null selected = question.question
                    | otherwise =
                        question.question
                            <> "\nSelected: "
                            <> Text.intercalate ", " (reverse selected)
            answer <- env.planHooks.planAskQuestion prompt choices
            case answer of
                Nothing -> noAnswer selected
                Just raw
                    | Text.null (Text.strip raw) -> noAnswer selected
                    | Just raw == backChoice ->
                        pure (Right AskQuestionBack)
                    | raw == doneChoice ->
                        if null selected
                            then pure (Left "No answer from user.")
                            else pure
                                (answered
                                    (Text.intercalate ", " (reverse selected)))
                    | raw == customChoice ->
                        askCustomReply env question.question >>= \case
                            Left err -> pure (Left err)
                            Right custom ->
                                pure $
                                    answered $
                                        Text.intercalate ", "
                                            (reverse selected <> [custom])
                    | Just label <-
                        Map.lookup raw labelsByDisplayed ->
                            choose
                                (label : selected)
                                [ option
                                | option <- remaining
                                , option.label /= label
                                ]
                    | otherwise ->
                        -- Non-TUI hooks may return a comma-separated answer
                        -- directly rather than one displayed choice at a time.
                        pure (answered (Text.strip raw))

        noAnswer selected
            | null selected = pure (Left "No answer from user.")
            | otherwise =
                pure
                    (answered
                        (Text.intercalate ", " (reverse selected)))

        answered text =
            Right (AskQuestionAnswered (question.question, text))

optionalBackChoice :: Bool -> [Text] -> Maybe Text
optionalBackChoice allowBack unavailable
    | allowBack =
        Just (freshControlChoice backToPreviousQuestionChoice unavailable)
    | otherwise = Nothing

askCustomReply :: PlanModeEnv -> Text -> IO (Either Text Text)
askCustomReply env question = do
    answer <- env.planHooks.planAskQuestion
        (question <> "\nEnter a custom reply:")
        []
    pure $ case Text.strip <$> answer of
        Nothing -> Left "No answer from user."
        Just text
            | Text.null text -> Left "No answer from user."
            | otherwise -> Right text

freshControlChoice :: Text -> [Text] -> Text
freshControlChoice base unavailable = go 1
  where
    go :: Int -> Text
    go suffix =
        let candidate
                | suffix == 1 = base
                | otherwise =
                    base <> " (" <> Text.pack (show suffix) <> ")"
        in if candidate `elem` unavailable
            then go (suffix + 1)
            else candidate

submitAnswersChoice :: Text
submitAnswersChoice = "Submit answers"

backToPreviousQuestionChoice :: Text
backToPreviousQuestionChoice = "← Back to previous question"

backToLastQuestionChoice :: Text
backToLastQuestionChoice = "← Back to last question"

customReplyChoice :: Text
customReplyChoice = "Type a custom reply"

-- | Collect native-provider answers without the host tool's final review
-- prompt. Native callbacks already have their own submit boundary.
collectNativeAskUserQuestionAnswers
    :: PlanModeEnv
    -> [AskUserQuestion]
    -> IO (Either Text [(Text, Text)])
collectNativeAskUserQuestionAnswers _ [] = pure (Right [])
collectNativeAskUserQuestionAnswers env (question : rest) =
    ask question >>= \case
        Left err -> pure (Left err)
        Right answer ->
            collectNativeAskUserQuestionAnswers env rest >>= \case
                Left err -> pure (Left err)
                Right answers -> pure (Right (answer : answers))
  where
    ask :: AskUserQuestion -> IO (Either Text (Text, Text))
    ask question
        | question.multiSelect == Just True =
            askMultiple question >>= \case
                Left err -> pure (Left err)
                Right answer -> pure (Right (question.question, answer))
        | otherwise = do
            let choices = map formatOption question.options
                customChoice =
                    freshControlChoice customReplyChoice choices
                labelsByChoice =
                    Map.fromList (zip choices (map (.label) question.options))
            answer <-
                env.planHooks.planAskQuestion
                    question.question
                    (choices <> [customChoice])
            case answer of
                Nothing -> pure (Left "No answer from user.")
                Just text | Text.null (Text.strip text) ->
                    pure (Left "No answer from user.")
                Just text | text == customChoice ->
                    fmap
                        (fmap \custom -> (question.question, custom))
                        (askCustomReply env question.question)
                Just text ->
                    pure $ Right
                        ( question.question
                        , fromMaybe text (Map.lookup text labelsByChoice)
                        )

    askMultiple :: AskUserQuestion -> IO (Either Text Text)
    askMultiple question =
        choose [] question.options
      where
        doneChoice = "Done selecting"
        choose selected remaining = do
            let displayed = map formatOption remaining
                labelsByDisplayed =
                    Map.fromList (zip displayed (map (.label) remaining))
                customChoice =
                    freshControlChoice
                        customReplyChoice
                        (displayed <> [doneChoice])
                choices = displayed <> [doneChoice, customChoice]
                prompt
                    | null selected = question.question
                    | otherwise =
                        question.question
                            <> "\nSelected: "
                            <> Text.intercalate ", " (reverse selected)
            answer <- env.planHooks.planAskQuestion prompt choices
            case answer of
                Nothing -> noAnswer selected
                Just raw
                    | Text.null (Text.strip raw) -> noAnswer selected
                    | raw == doneChoice ->
                        if null selected
                            then pure (Left "No answer from user.")
                            else pure
                                (Right (Text.intercalate ", " (reverse selected)))
                    | raw == customChoice ->
                        askCustomReply env question.question >>= \case
                            Left err -> pure (Left err)
                            Right custom ->
                                pure $
                                    Right $
                                        Text.intercalate ", "
                                            (reverse selected <> [custom])
                    | Just label <- Map.lookup raw labelsByDisplayed ->
                        choose
                            (label : selected)
                            [ option
                            | option <- remaining
                            , option.label /= label
                            ]
                    | otherwise -> pure (Right (Text.strip raw))
        noAnswer selected
            | null selected = pure (Left "No answer from user.")
            | otherwise =
                pure (Right (Text.intercalate ", " (reverse selected)))

-- | Run the same host question UI used by 'askUserQuestionTool', then add the
-- @answers@ object expected by Claude Code's native AskUserQuestion tool.
answerAskUserQuestionInput
    :: PlanModeEnv
    -> Aeson.Value
    -> IO (Either Text Aeson.Value)
answerAskUserQuestionInput env input =
    case input of
        Aeson.Object object ->
            case
                Json.decodeEither
                    askUserQuestionArgsDecoder
                    (LazyByteString.toStrict (Aeson.encode input))
              of
                Left err ->
                    pure (Left err.jsonErrorMessage)
                Right args ->
                    collectNativeAskUserQuestionAnswers env args.questions >>= \case
                        Left err -> pure (Left err)
                        Right answers ->
                            pure $
                                Right $
                                    Aeson.Object $
                                        KeyMap.insert
                                            "answers"
                                            (Aeson.toJSON
                                                (Map.fromList answers))
                                            object
        _ ->
            pure (Left "AskUserQuestion input must be an object.")

formatOption :: AskUserQuestionOption -> Text
formatOption option =
    Text.intercalate " — " $
        [option.label]
            <> [option.description | nonBlank option.description]
            <> [ "Preview: " <> Text.replace "\n" "\\n" preview
               | Just preview <- [option.preview]
               , nonBlank preview
               ]
  where
    nonBlank = not . Text.null . Text.strip

formatAnswerReview :: [(Text, Text)] -> Text
formatAnswerReview answers =
    Text.intercalate "\n\n" $
        "Review your answers before sending them:"
            : [ Text.pack (show index)
                    <> ". "
                    <> indent question
                    <> "\n   "
                    <> indent answer
              | (index, (question, answer)) <- zip [(1 :: Int)..] answers
              ]
  where
    indent = Text.replace "\n" "\n   "

formatAnswers :: [(Text, Text)] -> Text
formatAnswers answers =
    "User has answered your questions: "
        <> Text.intercalate ", "
            [ "\"" <> question <> "\"=\"" <> answer <> "\""
            | (question, answer) <- answers
            ]
        <> ". You can now continue with the user's answers in mind."

parseOptions :: Text -> [Text]
parseOptions raw =
    filter (not . Text.null)
        [ Text.strip part
        | part <- Text.split (\c -> c == '\n' || c == ',') raw
        ]
