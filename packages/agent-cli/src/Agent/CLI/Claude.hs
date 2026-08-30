-- | Late-bound Claude Code permission integration.
--
-- Provider startup needs the callback before the session runner has built its
-- approval UI. The slot fails closed until the runner installs the complete
-- policy-aware runtime.
module Agent.CLI.Claude
    ( ClaudeSessionRuntime(..)
    , ClaudeSessionRuntimeSlot
    , approveClaudeRegisteredTool
    , handleClaudePermissionRequest
    , installClaudeSessionRuntime
    , nativeClaudeToolReadOnly
    , newClaudeSessionRuntimeSlot
    ) where

import Agent.Claude
    ( ClaudeCodePermissionRequest(..)
    , ClaudeCodePermissionResult(..)
    )
import Agent.JsonText (jsonTextFieldDefault)
import Agent.ToolDispatch
    ( ToolCall(..)
    , canonicalToolName
    , functionToolCall
    )
import Agent.Tools.PlanMode
    ( PlanDecision(..)
    , PlanModeEnv(..)
    , PlanModeHooks(..)
    , activatePlanMode
    , answerAskUserQuestionInput
    , deactivatePlanMode
    , isPlanModeActive
    , readPlanMarkdown
    , writePlanMarkdown
    )
import Agent.Tools.ShellReadOnly (shellCommandIsReadOnly)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Functor ((<&>))
import Data.IORef
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

data ClaudeSessionRuntime = ClaudeSessionRuntime
    { approveNativeTool
        :: !(ToolCall -> Maybe Bool -> IO (Either Text Bool))
    , approveRegisteredTool
        :: !(ToolCall -> IO (Either Text Bool))
    , planMode :: !PlanModeEnv
    }

newtype ClaudeSessionRuntimeSlot =
    ClaudeSessionRuntimeSlot (IORef (Maybe ClaudeSessionRuntime))

newClaudeSessionRuntimeSlot :: IO ClaudeSessionRuntimeSlot
newClaudeSessionRuntimeSlot =
    ClaudeSessionRuntimeSlot <$> newIORef Nothing

installClaudeSessionRuntime
    :: ClaudeSessionRuntimeSlot
    -> ClaudeSessionRuntime
    -> IO ()
installClaudeSessionRuntime (ClaudeSessionRuntimeSlot ref) =
    writeIORef ref . Just

approveClaudeRegisteredTool
    :: ClaudeSessionRuntimeSlot
    -> ToolCall
    -> IO (Either Text Bool)
approveClaudeRegisteredTool (ClaudeSessionRuntimeSlot ref) call =
    readIORef ref >>= \case
        Nothing -> pure (Left "The host approval pipeline is not ready.")
        Just runtime -> runtime.approveRegisteredTool call

handleClaudePermissionRequest
    :: ClaudeSessionRuntimeSlot
    -> ClaudeCodePermissionRequest
    -> IO ClaudeCodePermissionResult
handleClaudePermissionRequest (ClaudeSessionRuntimeSlot runtimeRef) request =
    readIORef runtimeRef >>= \case
        Nothing ->
            pure $
                deny
                    "The host approval pipeline is not ready; the tool was denied."
        Just runtime ->
            -- The synthetic in-process MCP server performs the real host
            -- approval in its @tools/call@ handler. Claude's permission
            -- callback must allow the transport envelope here or every
            -- registered tool would be prompted twice.
            if "mcp__haskell-agent__" `Text.isPrefixOf` request.toolName
                then
                    pure ClaudeCodePermissionAllow
                        { updatedInput = Nothing
                        , updatedPermissions = []
                        }
                else
                    case canonicalToolName request.toolName of
                        "ask_user_question" ->
                            answerAskUserQuestionInput
                                runtime.planMode
                                request.input >>= \case
                                    Left message -> pure (deny message)
                                    Right updated ->
                                        pure $
                                            ClaudeCodePermissionAllow
                                                { updatedInput = Just updated
                                                , updatedPermissions = []
                                                }
                        "enter_plan_mode" ->
                            enterPlanMode runtime.planMode request
                        "exit_plan_mode" ->
                            exitPlanMode runtime.planMode request
                        _ ->
                            approveNative runtime request

approveNative
    :: ClaudeSessionRuntime
    -> ClaudeCodePermissionRequest
    -> IO ClaudeCodePermissionResult
approveNative runtime request = do
    let call =
            functionToolCall
                (fromMaybe
                    ("claude-native:" <> request.toolName)
                    request.toolUseId)
                request.toolName
                (encodeValue request.input)
        readOnly = nativeClaudeToolReadOnly call
    runtime.approveNativeTool call (Just readOnly) >>= \case
        Left message ->
            pure (deny message)
        Right False ->
            pure (deny "Tool call rejected by user.")
        Right True ->
            pure ClaudeCodePermissionAllow
                { updatedInput = Nothing
                , updatedPermissions = []
                }

enterPlanMode
    :: PlanModeEnv
    -> ClaudeCodePermissionRequest
    -> IO ClaudeCodePermissionResult
enterPlanMode plan request = do
    let explanation =
            nonEmpty
                (jsonTextFieldDefault "explanation" (encodeValue request.input))
                `orElse` request.description
                `orElse` request.decisionReason
    approved <-
        plan.planHooks.planConfirmEnter
            (fromMaybe "Claude requested Plan Mode." explanation)
    if approved
        then do
            activatePlanMode plan
            pure ClaudeCodePermissionAllow
                { updatedInput = Nothing
                , updatedPermissions = []
                }
        else
            pure (deny "The user declined Plan Mode.")

exitPlanMode
    :: PlanModeEnv
    -> ClaudeCodePermissionRequest
    -> IO ClaudeCodePermissionResult
exitPlanMode plan request = do
    active <- isPlanModeActive plan
    if not active
        then pure (deny "Plan mode is not active.")
        else do
            let suppliedPlan =
                    firstNonEmpty
                        [ jsonTextFieldDefault field
                            (encodeValue request.input)
                        | field <-
                            [ "plan", "plan_content", "planContent", "markdown" ]
                        ]
            markdownResult <- case suppliedPlan of
                Nothing -> Right <$> readPlanMarkdown plan
                Just content ->
                    writePlanMarkdown plan content
                        <&> fmap (const content)
            case markdownResult of
                Left message -> pure (deny message)
                Right markdown ->
                    plan.planHooks.planDecideExit markdown >>= \case
                        PlanApprove -> do
                            deactivatePlanMode plan
                            pure ClaudeCodePermissionAllow
                                { updatedInput = Nothing
                                , updatedPermissions = []
                                }
                        PlanRequestChanges feedback ->
                            pure $
                                deny
                                    ( "The user requested plan changes: "
                                        <> feedback
                                    )
                        PlanCancel -> do
                            deactivatePlanMode plan
                            pure (deny "The user cancelled the plan.")

-- | Conservative classification of Claude-owned tools. Unknown tools remain
-- mutating. Shell commands reuse the harness's explicit observational
-- allowlist; catastrophic commands are still rejected earlier by Approval.
nativeClaudeToolReadOnly :: ToolCall -> Bool
nativeClaudeToolReadOnly call =
    case canonicalToolName call.name of
        "run_terminal_cmd" ->
            let command = jsonTextFieldDefault "command" call.arguments
            in not (Text.null command) && shellCommandIsReadOnly command
        name ->
            name `elem`
                [ "read_file"
                , "grep"
                , "Glob"
                , "WebFetch"
                , "WebSearch"
                , "NotebookRead"
                , "get_task_output"
                , "BashOutput"
                , "TaskGet"
                , "TaskList"
                , "ToolSearch"
                , "CronList"
                ]

deny :: Text -> ClaudeCodePermissionResult
deny message =
    ClaudeCodePermissionDeny
        { message
        , interrupt = False
        }

encodeValue :: Aeson.Value -> Text
encodeValue =
    TextEncoding.decodeUtf8
        . LazyByteString.toStrict
        . Aeson.encode

nonEmpty :: Text -> Maybe Text
nonEmpty value
    | Text.null (Text.strip value) = Nothing
    | otherwise = Just (Text.strip value)

orElse :: Maybe a -> Maybe a -> Maybe a
orElse left right =
    case left of
        Just value -> Just value
        Nothing -> right

firstNonEmpty :: [Text] -> Maybe Text
firstNonEmpty = foldr (orElse . nonEmpty) Nothing
