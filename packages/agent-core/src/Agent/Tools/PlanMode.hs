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
    , PlanDigest(..)
    , PlanSnapshot(..)
    , PlanFileError(..)
    , PlanReadResult(..)
    , PlanModeEnv(..)
    , PlanModeHooks(..)
    , newPlanModeEnv
    , planFileName
    , planFilePath
    , readPlanModeState
    , writePlanModeState
    , readPlanSessionDir
    , setPlanSessionDir
    , attachPlanSessionDir
    , readPlanTracker
    , updatePlanTracker
    , restrictPlanTracker
    , isPlanModeActive
    , activatePlanMode
    , deactivatePlanMode
    , readPlanMarkdown
    , readPlanSnapshot
    , writePlanMarkdown
    , writePlanSnapshot
    , ensurePlanMarkdown
    , planModeReminder
    , planApprovedContinuation
    , planModeBlockedEditMessage
    , isPlanFileEditTarget
    , enterPlanModeTool
    , enterCodexPlanModeTool
    , writePlanTool
    , exitPlanModeTool
    , askUserQuestionTool
    ) where

import Agent.Json.Decode (Decoder)
import Agent.OsPath (toText)
import Agent.ToolArgs (objectArgs, optBool, optList, optText, reqText)
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    )
import Agent.ToolDispatch (typedTool)
import Agent.Tools.Types
    ( AppTool
    , PlanModeCapability(..)
    , ToolBatchPhase(..)
    , ToolExecutionPolicy(..)
    , jsonTool
    , withPlanModeCapability
    , withToolBatchPhase
    )
import Agent.Tools.PlanMode.File
    ( PlanDigest(..)
    , PlanFileError(..)
    , PlanReadResult(..)
    , PlanSnapshot(..)
    , ensurePlanFile
    , readPlanFile
    , renderPlanFileError
    , writePlanFile
    )
import Agent.Tools.PlanMode.Persistence
    ( readPlanTrackerState
    , validatePlanTracker
    , writePlanTrackerState
    )
import Agent.Tools.PlanMode.Tracker
    ( PlanTracker(..)
    , PlanTrackerPhase(..)
    , activatePlanTracker
    , deactivatePlanTracker
    , initialPlanTracker
    , normalizePlanTrackerAfterRestart
    , requestPlanActivation
    )
import Control.Applicative ((<|>))
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newMVar
    , readMVar
    )
import Control.Exception.Safe (displayException, throwString, tryIO)
import Control.Monad (when)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory.OsPath (canonicalizePath)
import System.OsPath (OsPath, equalFilePath, unsafeEncodeUtf, (</>))

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
    , planTrackerRuntime :: !(MVar PlanTrackerRuntime)
    }

data PlanTrackerRuntime = PlanTrackerRuntime
    { runtimeTracker :: !PlanTracker
    , runtimeAttachedDir :: !(Maybe OsPath)
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
    trackerRuntime <- newMVar PlanTrackerRuntime
        { runtimeTracker = initialPlanTracker
        , runtimeAttachedDir = Nothing
        }
    pure PlanModeEnv
        { planStateRef = stateRef
        , planSessionDir = sessionRef
        , planFallbackDir = fallbackDir
        , planHooks = fromMaybe defaultHooks hooks
        , planTrackerRuntime = trackerRuntime
        }

planFilePath :: PlanModeEnv -> IO OsPath
planFilePath env = do
    sessionDir <- readPlanSessionDir env
    canonicalDirectory <- canonicalizePath $ case sessionDir of
        Just dir -> dir
        Nothing -> env.planFallbackDir
    pure (canonicalDirectory </> planFileName)

readPlanModeState :: PlanModeEnv -> IO PlanModeState
readPlanModeState = readIORef . (.planStateRef)

-- | Compatibility setter while callers migrate from direct 'IORef' access to
-- the durable tracker. New code should keep state transitions in one owner.
writePlanModeState :: PlanModeEnv -> PlanModeState -> IO ()
writePlanModeState env target = do
    let transition tracker = Right (legacyStateTransition target tracker)
        apply
            | target == PlanActive = restrictPlanTracker
            | otherwise = updatePlanTracker
    apply env transition >>= \case
        Left err -> throwString (Text.unpack err)
        Right _ -> pure ()

readPlanSessionDir :: PlanModeEnv -> IO (Maybe OsPath)
readPlanSessionDir = readIORef . (.planSessionDir)

setPlanSessionDir :: PlanModeEnv -> Maybe OsPath -> IO ()
setPlanSessionDir env = writeIORef env.planSessionDir

-- | Attach durable plan state to a session directory exactly once. A missing
-- sidecar is a legacy inactive session unless an in-memory pre-attach mode
-- transition already occurred, in which case that transition is persisted.
-- Corrupt or unreadable state is returned as a startup error.
attachPlanSessionDir :: PlanModeEnv -> OsPath -> IO (Either Text ())
attachPlanSessionDir env rawDirectory = do
    tryIO (canonicalizePath rawDirectory) >>= \case
        Left err ->
            pure
                (Left
                    ("could not resolve plan-mode session directory: "
                        <> Text.pack (displayException err)))
        Right directory ->
            modifyMVar env.planTrackerRuntime \runtime ->
                case runtime.runtimeAttachedDir of
                    Just attached
                        | equalFilePath attached directory -> do
                            setPlanSessionDir env (Just directory)
                            pure (runtime, Right ())
                        | otherwise ->
                            pure
                                ( runtime
                                , Left
                                    ("plan mode is already attached to "
                                        <> toText attached)
                                )
                    Nothing ->
                        readPlanTrackerState directory >>= \case
                            Left err -> pure (runtime, Left err)
                            Right persisted -> do
                                legacyState <- readIORef env.planStateRef
                                let selected = case persisted of
                                        Just tracker ->
                                            normalizePlanTrackerAfterRestart tracker
                                        Nothing ->
                                            mergeLegacyState
                                                legacyState
                                                runtime.runtimeTracker
                                    shouldWrite =
                                        maybe
                                            (selected /= initialPlanTracker)
                                            (/= selected)
                                            persisted
                                    restricted = trackerRestricts selected
                                when restricted $
                                    mirrorTrackerState env selected
                                persistedResult <-
                                    if shouldWrite
                                        then
                                            writePlanTrackerState
                                                directory
                                                selected
                                        else pure (Right ())
                                case persistedResult of
                                    Left err ->
                                        pure
                                            ( if restricted
                                                then runtime
                                                    { runtimeTracker = selected }
                                                else runtime
                                            , Left err
                                            )
                                    Right () -> do
                                        mirrorTrackerState env selected
                                        setPlanSessionDir env (Just directory)
                                        pure
                                            ( runtime
                                                { runtimeTracker = selected
                                                , runtimeAttachedDir =
                                                    Just directory
                                                }
                                            , Right ()
                                            )

readPlanTracker :: PlanModeEnv -> IO PlanTracker
readPlanTracker env =
    (.runtimeTracker) <$> readMVar env.planTrackerRuntime

-- | Persist a candidate before exposing it. Persistence failure leaves the
-- previous tracker and restrictions intact, which is appropriate for reminder
-- updates, approval resolution, and any transition that might relax policy.
updatePlanTracker
    :: PlanModeEnv
    -> (PlanTracker -> Either Text PlanTracker)
    -> IO (Either Text PlanTracker)
updatePlanTracker = changePlanTracker PersistBeforeExpose

-- | Expose a more restrictive candidate before persistence. If persistence
-- fails, the new restriction remains active in memory and the caller receives
-- the error.
restrictPlanTracker
    :: PlanModeEnv
    -> (PlanTracker -> Either Text PlanTracker)
    -> IO (Either Text PlanTracker)
restrictPlanTracker = changePlanTracker ExposeBeforePersist

data TrackerCommitOrder
    = PersistBeforeExpose
    | ExposeBeforePersist

changePlanTracker
    :: TrackerCommitOrder
    -> PlanModeEnv
    -> (PlanTracker -> Either Text PlanTracker)
    -> IO (Either Text PlanTracker)
changePlanTracker order env transition =
    modifyMVar env.planTrackerRuntime \runtime ->
        case transition runtime.runtimeTracker of
            Left err -> pure (runtime, Left err)
            Right candidate
                | Left err <- validatePlanTracker candidate ->
                    pure (runtime, Left err)
                | otherwise ->
                    case
                        ( effectiveCommitOrder
                            order
                            runtime.runtimeTracker
                            candidate
                        , runtime.runtimeAttachedDir
                        )
                    of
                        (_, Nothing) -> do
                            mirrorTrackerState env candidate
                            pure
                                ( runtime { runtimeTracker = candidate }
                                , Right candidate
                                )
                        (PersistBeforeExpose, Just directory) ->
                            writePlanTrackerState directory candidate >>= \case
                                Left err -> pure (runtime, Left err)
                                Right () -> do
                                    mirrorTrackerState env candidate
                                    pure
                                        ( runtime { runtimeTracker = candidate }
                                        , Right candidate
                                        )
                        (ExposeBeforePersist, Just directory) -> do
                            mirrorTrackerState env candidate
                            writePlanTrackerState directory candidate >>= \case
                                Left err ->
                                    pure
                                        ( runtime { runtimeTracker = candidate }
                                        , Left err
                                        )
                                Right () ->
                                    pure
                                        ( runtime { runtimeTracker = candidate }
                                        , Right candidate
                                        )

effectiveCommitOrder
    :: TrackerCommitOrder
    -> PlanTracker
    -> PlanTracker
    -> TrackerCommitOrder
effectiveCommitOrder requested previous candidate
    | not (trackerRestricts previous) && trackerRestricts candidate =
        requested
    | otherwise = PersistBeforeExpose

isPlanModeActive :: PlanModeEnv -> IO Bool
isPlanModeActive env = (== PlanActive) <$> readPlanModeState env

activatePlanMode :: PlanModeEnv -> IO ()
activatePlanMode env = writePlanModeState env PlanActive

deactivatePlanMode :: PlanModeEnv -> IO ()
deactivatePlanMode env = writePlanModeState env PlanInactive

legacyStateTransition :: PlanModeState -> PlanTracker -> PlanTracker
legacyStateTransition = \case
    PlanInactive -> deactivatePlanTracker False
    PlanPending ->
        requestPlanActivation . deactivatePlanTracker False
    PlanActive -> activatePlanTracker

mergeLegacyState :: PlanModeState -> PlanTracker -> PlanTracker
mergeLegacyState legacy tracker
    | tracker /= initialPlanTracker = tracker
    | otherwise = case legacy of
        PlanInactive -> tracker
        _ -> legacyStateTransition legacy tracker

trackerRestricts :: PlanTracker -> Bool
trackerRestricts tracker =
    tracker.trackerPhase `elem` [TrackerActive, TrackerExitPending]

mirrorTrackerState :: PlanModeEnv -> PlanTracker -> IO ()
mirrorTrackerState env tracker =
    writeIORef env.planStateRef $ case tracker.trackerPhase of
        TrackerInactive -> PlanInactive
        TrackerPending -> PlanPending
        TrackerActive -> PlanActive
        TrackerExitPending -> PlanActive

readPlanMarkdown :: PlanModeEnv -> IO Text
readPlanMarkdown env =
    readPlanSnapshot env >>= \case
        PlanAbsent -> pure ""
        PlanPresent snapshot -> pure snapshot.planSnapshotMarkdown
        PlanUnreadable err ->
            throwString (Text.unpack (renderPlanFileError err))

readPlanSnapshot :: PlanModeEnv -> IO PlanReadResult
readPlanSnapshot env =
    planFilePath env >>= readPlanFile

writePlanMarkdown :: PlanModeEnv -> Text -> IO (Either Text ())
writePlanMarkdown env content =
    fmap (either (Left . renderPlanFileError) (const (Right ())))
        (writePlanSnapshot env content)

writePlanSnapshot
    :: PlanModeEnv
    -> Text
    -> IO (Either PlanFileError PlanSnapshot)
writePlanSnapshot env content = do
    path <- planFilePath env
    writePlanFile path content

ensurePlanMarkdown
    :: PlanModeEnv
    -> IO (Either PlanFileError PlanSnapshot)
ensurePlanMarkdown env =
    planFilePath env >>= ensurePlanFile

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

-- | Compare paths only after the caller has resolved both to canonical paths.
-- A raw @plan.md@ basename is deliberately not authorization.
isPlanFileEditTarget :: OsPath -> OsPath -> Bool
isPlanFileEditTarget = equalFilePath

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
enterPlanModeToolWith completion env =
    withPlanModeCapability PlanModeInteraction $
    withToolBatchPhase ToolBatchModeBarrier $
        jsonTool "enter_plan_mode"
            (enterPlanDescription completion)
            [ PropertySchema "explanation" PropertyString False $ Just
                "Optional reason this task needs a planning phase before implementation."
            ]
            -- The tool performs its own explicit user confirmation through
            -- planConfirmEnter, so it must not also trigger generic tool approval.
            True
            TurnSequential
            (typedTool "enter_plan_mode" enterPlanArgsDecoder
                (runEnterPlanMode completion env))

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
                    ensurePlanFile path >>= \case
                        Left err ->
                            pure (Left (renderPlanFileError err))
                        Right _ -> do
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
writePlanTool env =
    withPlanModeCapability
        (PlanModePlanFileWrite (\_ -> Right <$> planFilePath env)) $
    jsonTool "write_plan" writePlanDescription
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
exitPlanModeTool env =
    withPlanModeCapability PlanModeInteraction $
    withToolBatchPhase ToolBatchTerminal $
        jsonTool "exit_plan_mode" exitPlanDescription
            [ PropertySchema "summary" PropertyString False $ Just
                "Optional short summary shown with the plan approval prompt."
            ]
            False
            TurnSequential
            (typedTool "exit_plan_mode" exitPlanArgsDecoder
                (runExitPlanMode env))

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
            path <- planFilePath env
            readPlanFile path >>= \case
                PlanUnreadable err ->
                    pure (Left (renderPlanFileError err))
                PlanAbsent -> reviewPlanContent path ""
                PlanPresent snapshot ->
                    reviewPlanContent path snapshot.planSnapshotMarkdown
  where
    reviewPlanContent path content = do
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
askUserQuestionTool env =
    withPlanModeCapability PlanModeInteraction $
    jsonTool "ask_user_question" askUserDescription
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
        answers <- collectAnswers args.questions
        pure (formatAnswers <$> answers)
  where
    collectAnswers
        :: [AskUserQuestion]
        -> IO (Either Text [(Text, Text)])
    collectAnswers [] = pure (Right [])
    collectAnswers (question : rest) =
        ask question >>= \case
            Left err -> pure (Left err)
            Right answer ->
                collectAnswers rest >>= \case
                    Left err -> pure (Left err)
                    Right answers -> pure (Right (answer : answers))

    ask :: AskUserQuestion -> IO (Either Text (Text, Text))
    ask question
        | question.multiSelect == Just True =
            askMultiple question >>= \case
                Left err -> pure (Left err)
                Right answer -> pure (Right (question.question, answer))
        | otherwise = do
            let choices = map formatOption question.options
                labelsByChoice =
                    Map.fromList (zip choices (map (.label) question.options))
            answer <- env.planHooks.planAskQuestion question.question choices
            pure $ case answer of
                Nothing -> Left "No answer from user."
                Just text | Text.null (Text.strip text) ->
                    Left "No answer from user."
                Just text ->
                    Right
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
                choices = displayed <> [doneChoice]
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
                        pure (Right (Text.strip raw))

        noAnswer selected
            | null selected = pure (Left "No answer from user.")
            | otherwise =
                pure (Right (Text.intercalate ", " (reverse selected)))

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
