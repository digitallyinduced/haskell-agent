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
    , planModeBlockedEditMessage
    , isPlanFileEditTarget
    , enterPlanModeTool
    , exitPlanModeTool
    , askUserQuestionTool
    ) where

import Agent.OsPath (OsPath, fromFilePath, toFilePath, toText)
import Agent.ToolArgs (objectArgs, optText, reqText)
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    )
import Agent.ToolDispatch (ToolHandler, typedTool)
import Agent.Tools.Types
    ( AppTool(..)
    , AppToolKind(..)
    )
import Control.Exception.Safe (tryAny)
import Data.Aeson (FromJSON(..), withObject)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory.OsPath (createDirectoryIfMissing, doesFileExist)
import System.OsPath (equalFilePath, takeDirectory, takeFileName, (</>))

data PlanModeState
    = PlanInactive
    | PlanPending
    -- ^ User toggled plan mode; becomes Active on the next prompt.
    | PlanActive
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
planFileName = fromFilePath "plan.md"

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
        else either (const "") id <$> tryAny (Text.readFile (toFilePath path))

writePlanMarkdown :: PlanModeEnv -> Text -> IO (Either Text ())
writePlanMarkdown env content = do
    path <- planFilePath env
    createDirectoryIfMissing True (takeDirectory path)
    result <- tryAny (Text.writeFile (toFilePath path) content)
    pure $ case result of
        Left err -> Left ("failed to write plan file: " <> Text.pack (show err))
        Right () -> Right ()

planModeReminder :: OsPath -> Text
planModeReminder path =
    Text.unlines
        [ "Plan mode is active. Do not make any edits or writes to the system except for the plan file."
        , ""
        , "## Plan File"
        , "Write your plan to `" <> toText path <> "` using the edit tool."
        , "That is the only file you may create or modify."
        , ""
        , "When the plan is ready, call `exit_plan_mode` (Grok/OpenRouter) or end your turn with a complete `<proposed_plan>` … `</proposed_plan>` block (OpenAI/Codex) so the user can approve, request changes, or cancel."
        ]

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
        || equalFilePath planFileName (takeFileName target)

--------------------------------------------------------------------------------
-- Grok-build tools
--------------------------------------------------------------------------------

jsonTool
    :: Text
    -> Text
    -> [PropertySchema]
    -> Bool
    -> ToolHandler
    -> AppTool
jsonTool name description parameters readOnly handler = AppTool
    { appToolName = name
    , appToolDescription = description
    , appToolParameters = parameters
    , appToolHandler = handler
    , appToolKind = JsonFunction
    , appToolReadOnly = readOnly
    , appToolIsReadOnlyCall = Nothing
    }

data EnterPlanArgs = EnterPlanArgs
    { explanation :: Maybe Text
    }

instance FromJSON EnterPlanArgs where
    parseJSON = objectArgs \object -> EnterPlanArgs
        <$> optText object "explanation"

enterPlanModeTool :: PlanModeEnv -> AppTool
enterPlanModeTool env = jsonTool "enter_plan_mode" enterPlanDescription
    [ PropertySchema "explanation" PropertyString False $ Just
        "Optional reason this task needs a planning phase before implementation."
    ]
    -- The tool performs its own explicit user confirmation through
    -- planConfirmEnter, so it must not also trigger generic tool approval.
    True
    (typedTool "enter_plan_mode" (runEnterPlanMode env))

enterPlanDescription :: Text
enterPlanDescription =
    "Enter plan mode when a task has genuine architectural ambiguity.\n\
    \Requires user approval. While active, only the session plan.md file may be edited;\n\
    \explore the codebase, write the plan, then call exit_plan_mode for approval."

runEnterPlanMode :: PlanModeEnv -> EnterPlanArgs -> IO (Either Text Text)
runEnterPlanMode env args = do
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
                            <> ". Call exit_plan_mode when ready for approval."

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
                    pure $ Right
                        "Your plan has been approved. You can now start coding."
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
