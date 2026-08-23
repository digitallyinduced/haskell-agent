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
    ) where

import Agent.FileRetry (retryOnFileBusy)
import Agent.OsPath (toText, unsafeToFilePath)
import Agent.ToolArgs (objectArgs, optText, reqText)
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
import Control.Exception.Safe (tryAny)
import Data.Aeson (FromJSON(..), withObject)
import Data.IORef
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
    pure PlanModeEnv
        { planStateRef = stateRef
        , planSessionDir = sessionRef
        , planFallbackDir = fallbackDir
        , planHooks = fromMaybe defaultHooks hooks
        }

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

instance FromJSON EnterPlanArgs where
    parseJSON = objectArgs \object -> EnterPlanArgs
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
    (typedTool "enter_plan_mode" (runEnterPlanMode completion env))

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

instance FromJSON WritePlanArgs where
    parseJSON = withObject "write_plan" \object ->
        WritePlanArgs <$> reqText object "content"

writePlanTool :: PlanModeEnv -> AppTool
writePlanTool env = jsonTool "write_plan" writePlanDescription
    [ PropertySchema "content" PropertyString True $ Just
        "Complete Markdown content to store in the session plan.md file."
    ]
    True
    TurnSequential
    (typedTool "write_plan" (runWritePlan env))

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

instance FromJSON ExitPlanArgs where
    parseJSON = objectArgs \object -> ExitPlanArgs
        <$> optText object "summary"

exitPlanModeTool :: PlanModeEnv -> AppTool
exitPlanModeTool env = jsonTool "exit_plan_mode" exitPlanDescription
    [ PropertySchema "summary" PropertyString False $ Just
        "Optional short summary shown with the plan approval prompt."
    ]
    False
    TurnSequential
    (typedTool "exit_plan_mode" (runExitPlanMode env))

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

data AskUserQuestionArgs = AskUserQuestionArgs
    { question :: Text
    , options :: Maybe Text
    }

instance FromJSON AskUserQuestionArgs where
    parseJSON = withObject "ask_user_question" \object -> do
        question <- reqText object "question"
        options <- optText object "options"
        pure AskUserQuestionArgs { question, options }

askUserQuestionTool :: PlanModeEnv -> AppTool
askUserQuestionTool env = jsonTool "ask_user_question" askUserDescription
    [ PropertySchema "question" PropertyString True $ Just
        "Question to ask the user while planning."
    , PropertySchema "options" PropertyString False $ Just
        "Optional newline- or comma-separated choices."
    ]
    True
    TurnSequential
    (typedTool "ask_user_question" (runAskUserQuestion env))

askUserDescription :: Text
askUserDescription =
    "Ask the user a clarifying question during plan mode without leaving plan mode."

runAskUserQuestion :: PlanModeEnv -> AskUserQuestionArgs -> IO (Either Text Text)
runAskUserQuestion env args = do
    let choices = parseOptions (fromMaybe "" args.options)
    answer <- env.planHooks.planAskQuestion args.question choices
    pure $ case answer of
        Nothing -> Left "No answer from user."
        Just text | Text.null (Text.strip text) -> Left "No answer from user."
        Just text -> Right text

parseOptions :: Text -> [Text]
parseOptions raw =
    filter (not . Text.null)
        [ Text.strip part
        | part <- Text.split (\c -> c == '\n' || c == ',') raw
        ]
