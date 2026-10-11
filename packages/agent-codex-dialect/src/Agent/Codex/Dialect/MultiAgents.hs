-- | Codex multi-agent v2 tools (collaboration namespace).
--
-- Wire names and schemas follow openai/codex
-- @codex-rs/core/src/tools/handlers/multi_agents_spec.rs@ (v2). The spawn
-- machinery the tools share with other dialects is "Agent.Subagents.Spawn".
module Agent.Codex.Dialect.MultiAgents
    ( multiAgentTools
    , plainMultiAgentTools
    , multiAgentNamespace
    , multiAgentToolNames
    ) where

import Agent.Json.Decode (Decoder)
import Agent.Subagents
    ( SubagentId(..)
    , RootTurnId
    , SubagentStatus(..)
    , defaultWaitTimeoutMs
    , interruptSubagent
    , listAgents
    , queueMessageFromForTurn
    , resolveAgentTarget
    , sendInputMessageForTurn
    , waitAnyLive
    , waitSubagentsFrom
    )
import Agent.Subagents.Spawn
    ( MultiAgentContext(..)
    , SpawnRequest(..)
    , SpawnWorkspace(..)
    , callMessageContent
    , spawnSubagentFromCall
    )
import Agent.Subagents.TaskPath
    ( TaskPath
    , resolveTaskPath
    , taskPathRoot
    , taskPathText
    )
import Agent.InterAgentMessage
    ( InterAgentMessage(..)
    , InterAgentMessageContent
    , InterAgentMessageType(..)
    )
import Agent.ToolArgs
    ( objectArgs
    , objectArgsLenient
    , optInt
    , optText
    , optTextList
    , rejectField
    , reqText
    )
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    )
import Agent.ToolDispatch (ToolCall(..), typedTool, typedToolWithCall)
import Agent.Tools.Types
    ( AppTool
    , ToolExecutionPolicy(..)
    , jsonTool
    )
import Data.Aeson (object, (.=))
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text

multiAgentNamespace :: Text
multiAgentNamespace = "collaboration"

multiAgentToolNames :: [Text]
multiAgentToolNames =
    [ "spawn_agent"
    , "wait_agent"
    , "send_message"
    , "followup_task"
    , "list_agents"
    , "interrupt_agent"
    ]

-- | How the tools declare their message fields.
data MessageFields
    -- | Encrypted by the provider, as OpenAI's collaboration namespace
    -- declares them.
    = EncryptedMessages
    -- | Plain strings.
    | PlainMessages

multiAgentTools :: MultiAgentContext -> [AppTool]
multiAgentTools = multiAgentToolsWith EncryptedMessages

-- | 'multiAgentTools' with plain-text message fields, for hosts that pass
-- them as ordinary function tools without encrypted arguments.
plainMultiAgentTools :: MultiAgentContext -> [AppTool]
plainMultiAgentTools = multiAgentToolsWith PlainMessages

multiAgentToolsWith :: MessageFields -> MultiAgentContext -> [AppTool]
multiAgentToolsWith fields ctx =
    [spawnAgentTool fields ctx]
    <> maybe [] (const [spawnAgentInWorktreeTool ctx]) ctx.multiCreateWorktree
    <> [ waitAgentTool ctx
    , sendMessageTool fields ctx
    , followupTaskTool fields ctx
    , listAgentsTool ctx
    , interruptAgentTool ctx
    ]

--------------------------------------------------------------------------------
-- spawn_agent
--------------------------------------------------------------------------------

spawnAgentArgsDecoder :: Decoder SpawnRequest
spawnAgentArgsDecoder = objectArgs \object_ -> do
        rejectField object_ "isolation"
            "isolation is no longer supported; use spawn_agent_in_worktree"
        SpawnRequest
            <$> reqText object_ "task_name"
            <*> reqText object_ "message"
            <*> optText object_ "model"
            <*> optText object_ "reasoning_effort"
            <*> optText object_ "fork_turns"

spawnAgentTool :: MessageFields -> MultiAgentContext -> AppTool
spawnAgentTool fields ctx = jsonTool "spawn_agent" spawnAgentDescription
    (spawnAgentParameters ctx fields)
    True
    TurnSequential
    (typedToolWithCall "spawn_agent" spawnAgentArgsDecoder (spawnSubagentFromCall ctx SharedWorkspace))

spawnAgentInWorktreeTool :: MultiAgentContext -> AppTool
spawnAgentInWorktreeTool ctx =
    jsonTool "spawn_agent_in_worktree" spawnAgentInWorktreeDescription
        (spawnAgentParameters ctx PlainMessages)
        True
        TurnSequential
        (typedToolWithCall
            "spawn_agent_in_worktree"
            spawnAgentArgsDecoder
            (spawnSubagentFromCall ctx IsolatedWorktree))

spawnAgentParameters :: MultiAgentContext -> MessageFields -> [PropertySchema]
spawnAgentParameters ctx fields =
    [ PropertySchema "task_name" PropertyString True $ Just
        "Task name for the new agent. Use lowercase letters, digits, and underscores."
    , messageProperty fields "Initial plain-text task for the new agent."
    ]
        -- The host's preparation hook applies overrides and history forks.
        -- Without one, a child starts from its message alone.
        <> if isJust ctx.multiPrepareSpawn then spawnOptionParameters ctx else []

spawnOptionParameters :: MultiAgentContext -> [PropertySchema]
spawnOptionParameters ctx =
    [ PropertySchema "model" PropertyString False $ Just
        (Text.intercalate " " $
            [ "Model override for the new agent. Omit unless an explicit override is needed."
            , "Model or reasoning-effort overrides require `fork_turns` to be `none` or a positive integer."
            ]
                <> maybe [] pure ctx.multiSpawnModelGuidance
                <> allowedModelGuidance ctx.multiAllowedChildModels)
    , PropertySchema "reasoning_effort" PropertyString False $ Just
        "Reasoning effort override for the new agent. Omit to inherit the parent effort. Overrides require `fork_turns` to be `none` or a positive integer."
    , PropertySchema "fork_turns" PropertyString False $ Just
        "Optional number of turns to fork. Defaults to `all`. Full-history forks (`all` or omitted) inherit the parent model and reasoning effort and do not accept overrides. Use `none` or a positive integer string such as `3` when setting `model` or `reasoning_effort`."
    ]

messageProperty :: MessageFields -> Text -> PropertySchema
messageProperty fields description = case fields of
    EncryptedMessages ->
        PropertySchema "message" (encryptedString description) True Nothing
    PlainMessages ->
        PropertySchema "message" PropertyString True (Just description)

spawnAgentDescription :: Text
spawnAgentDescription =
    "Spawns an agent to work on the specified task. If your current task is \
    \/root/task1 and you spawn_agent with task_name \"task_3\" the agent will \
    \have canonical task name /root/task1/task_3. You may refer to this agent \
    \as task_3 or /root/task1/task_3. Returns labeled text with the canonical \
    \task name."

spawnAgentInWorktreeDescription :: Text
spawnAgentInWorktreeDescription =
    "Creates a dedicated git worktree and spawns an agent inside it. The \
    \worktree is owned by the child agent and removed when that agent is \
    \closed. Returns labeled text with the canonical task name and worktree \
    \path."

allowedModelGuidance :: Maybe [Text] -> [Text]
allowedModelGuidance = \case
    Nothing -> []
    Just rawAllowed ->
        let allowed = filter (not . Text.null) (map Text.strip rawAllowed)
            rendered =
                if null allowed
                    then "(none)"
                    else Text.intercalate ", " allowed
        in
            [ "Allowed child model IDs: "
                <> rendered
                <> ". Omit to inherit the parent model."
            ]

--------------------------------------------------------------------------------
-- wait_agent
--------------------------------------------------------------------------------

data WaitAgentArgs = WaitAgentArgs
    { targets :: Maybe [Text]
    , timeoutMs :: Maybe Int
    }

waitAgentArgsDecoder :: Decoder WaitAgentArgs
waitAgentArgsDecoder = objectArgsLenient \object_ -> do
        targets <- optTextList object_ "targets"
        timeoutMs <- optInt object_ "timeout_ms"
        pure WaitAgentArgs { targets, timeoutMs }

waitAgentTool :: MultiAgentContext -> AppTool
waitAgentTool ctx = jsonTool "wait_agent" waitAgentDescription
    -- OpenAI reserves collaboration.wait_agent and requires type "number"
    -- exactly, even though the runtime decodes whole milliseconds as Int.
    [ PropertySchema "timeout_ms" PropertyNumber False $ Just
        "Timeout in milliseconds. Defaults to 30000 ms."
    ]
    True
    TurnSequential
    (typedTool "wait_agent" waitAgentArgsDecoder (runWait ctx))

waitAgentDescription :: Text
waitAgentDescription =
    "Wait for a mailbox update from live agents, including final-status \
    \notifications. Returns labeled text summarizing which agents have \
    \updates, or a timeout notice if no activity arrives before the deadline."

runWait :: MultiAgentContext -> WaitAgentArgs -> IO (Either Text Text)
runWait ctx args = do
    let timeoutMs = fromMaybe defaultWaitTimeoutMs args.timeoutMs
    case args.targets of
        Nothing -> do
            (statuses, timedOut) <-
                waitAnyLive ctx.multiRegistry ctx.multiSelfId timeoutMs
            pure $ Right $ renderWaitResult timedOut statuses False
        Just [] -> pure (Left "targets must be non-empty when provided")
        Just targets -> do
            resolved <- mapM (resolveAgentTarget ctx.multiRegistry ctx.multiTaskPath) targets
            case sequence resolved of
                Left err -> pure (Left err)
                Right ids -> do
                    (statuses, timedOut) <-
                        waitSubagentsFrom
                            ctx.multiRegistry ctx.multiSelfId ids timeoutMs
                    pure $ Right $ renderWaitResult timedOut statuses True

renderWaitResult :: Bool -> Map SubagentId SubagentStatus -> Bool -> Text
renderWaitResult timedOut statuses includeStatus =
    Text.intercalate "\n" $
        waitSummary timedOut statuses
            : if includeStatus && not (Map.null statuses)
                then "" : map renderWaitStatus (Map.toList statuses)
                else []

renderWaitStatus :: (SubagentId, SubagentStatus) -> Text
renderWaitStatus (agentId, status) =
    Text.intercalate "\n" $
        [ agentId.unSubagentId
        , "  Status: " <> agentListStatus status
        ]
            <> agentListDetails status

waitSummary :: Bool -> Map SubagentId SubagentStatus -> Text
waitSummary timedOut statuses
    | timedOut = "timed out waiting for agent updates"
    | otherwise =
        let finals =
                [ agentId.unSubagentId <> "=" <> shortStatus status
                | (agentId, status) <- Map.toList statuses
                ]
        in "agent updates: " <> Text.intercalate ", " finals

shortStatus :: SubagentStatus -> Text
shortStatus = \case
    Completed _ -> "completed"
    Errored _ -> "errored"
    Interrupted -> "interrupted"
    Closed -> "shutdown"
    Running -> "running"
    Pending -> "pending"
    NotFound -> "not_found"

--------------------------------------------------------------------------------
-- send_message / followup_task
--------------------------------------------------------------------------------

data MessageArgs = MessageArgs
    { target :: Text
    , message :: Text
    }

messageArgsDecoder :: Decoder MessageArgs
messageArgsDecoder = objectArgs \object_ -> MessageArgs
        <$> reqText object_ "target"
        <*> reqText object_ "message"

sendMessageTool :: MessageFields -> MultiAgentContext -> AppTool
sendMessageTool fields ctx = jsonTool "send_message" sendMessageDescription
    [ PropertySchema "target" PropertyString True $ Just
        "Relative or canonical task name to message (from a spawn tool)."
    , messageProperty fields "Message text to queue on the target agent."
    ]
    True
    TurnSequential
    (typedToolWithCall "send_message" messageArgsDecoder (runSendMessage ctx))

sendMessageDescription :: Text
sendMessageDescription =
    "Send a message to an existing agent. The message will be delivered \
    \promptly. Does not trigger a new turn."

runSendMessage :: MultiAgentContext -> ToolCall -> MessageArgs -> IO (Either Text Text)
runSendMessage ctx call args
    = case validateMessage "send_message" args.message of
        Left err -> pure (Left err)
        Right () ->
            withMessageTarget ctx args.target \target ->
                case target of
                    RootMessageTarget ->
                        sendToRoot ctx (callMessageContent call args.message)
                    AgentMessageTarget agentId rootTurnId ->
                        queueMessageFromForTurn
                            ctx.multiRegistry rootTurnId
                            ctx.multiTaskPath agentId
                            (callMessageContent call args.message)

followupTaskTool :: MessageFields -> MultiAgentContext -> AppTool
followupTaskTool fields ctx = jsonTool "followup_task" followupDescription
    [ PropertySchema "target" PropertyString True $ Just
        "Agent id or canonical task name to follow up with (from a spawn tool)."
    , messageProperty fields "Message text to send to the target agent."
    ]
    True
    TurnSequential
    (typedToolWithCall "followup_task" messageArgsDecoder (runFollowup ctx))

followupDescription :: Text
followupDescription =
    "Send a follow-up task to an existing non-root target agent and trigger a \
    \turn if it is idle. If the target is already running, deliver the task \
    \promptly at message boundaries."

runFollowup :: MultiAgentContext -> ToolCall -> MessageArgs -> IO (Either Text Text)
runFollowup ctx call args
    = case validateMessage "followup_task" args.message of
        Left err -> pure (Left err)
        Right () ->
            withMessageTarget ctx args.target \target ->
                case target of
                    RootMessageTarget ->
                        pure (Left "followup_task cannot target the root agent; use send_message")
                    AgentMessageTarget agentId rootTurnId ->
                        sendInputMessageForTurn
                            ctx.multiRegistry rootTurnId
                            ctx.multiTaskPath agentId
                            (callMessageContent call args.message) False

validateMessage :: Text -> Text -> Either Text ()
validateMessage toolName message
    | Text.null (Text.strip message) =
        Left (toolName <> " requires a non-empty message")
    | otherwise = Right ()

data MessageTarget
    = RootMessageTarget
    | AgentMessageTarget SubagentId (Maybe RootTurnId)

withMessageTarget
    :: MultiAgentContext
    -> Text
    -> (MessageTarget -> IO (Either Text Text))
    -> IO (Either Text Text)
withMessageTarget ctx target action =
    case resolveTaskPath ctx.multiTaskPath target of
        Right targetPath | targetPath == taskPathRoot ->
            action RootMessageTarget
        _ -> do
            restored <- maybeRestore ctx target
            case restored of
                Left err -> pure (Left err)
                Right () -> do
                    resolved <-
                        resolveAgentTarget
                            ctx.multiRegistry ctx.multiTaskPath target
                    case resolved of
                        Left err -> pure (Left err)
                        Right agentId -> do
                            rootTurnId <- ctx.multiRootTurnId
                            action (AgentMessageTarget agentId rootTurnId)

sendToRoot :: MultiAgentContext -> InterAgentMessageContent -> IO (Either Text Text)
sendToRoot ctx content =
    if ctx.multiTaskPath == taskPathRoot
        then pure (Left "root agent cannot send_message to itself")
        else case ctx.multiSendToRoot of
            Nothing -> pure (Left "root agent mailbox is unavailable")
            Just deliver -> deliver (rootMessage ctx content)

rootMessage :: MultiAgentContext -> InterAgentMessageContent -> InterAgentMessage
rootMessage ctx content = InterAgentMessage
    { messageAuthor = taskPathText ctx.multiTaskPath
    , messageRecipient = taskPathText taskPathRoot
    , messageType = QueuedMessage
    , messageContent = content
    }

maybeRestore :: MultiAgentContext -> Text -> IO (Either Text ())
maybeRestore ctx target = case ctx.multiResumeFromDisk of
    Nothing -> pure (Right ())
    Just restore ->
        resolveAgentTarget ctx.multiRegistry ctx.multiTaskPath target >>= \case
            Left _ ->
                -- Target may only exist on disk as agent id.
                if "agent-" `Text.isPrefixOf` target
                    then restore (SubagentId target)
                    else pure (Right ())
            Right agentId -> restore agentId

--------------------------------------------------------------------------------
-- list_agents
--------------------------------------------------------------------------------

data ListAgentsArgs = ListAgentsArgs
    { pathPrefix :: Maybe Text
    }

listAgentsArgsDecoder :: Decoder ListAgentsArgs
listAgentsArgsDecoder = objectArgsLenient \object_ -> ListAgentsArgs
        <$> optText object_ "path_prefix"

listAgentsTool :: MultiAgentContext -> AppTool
listAgentsTool ctx = jsonTool "list_agents" listAgentsDescription
    [ PropertySchema "path_prefix" PropertyString False $ Just
        "Task-path prefix filter without a trailing slash. Omit to list all live agents."
    ]
    True
    ParallelSafe
    (typedTool "list_agents" listAgentsArgsDecoder (runListAgents ctx))

listAgentsDescription :: Text
listAgentsDescription =
    "List live agents in the current root thread tree. Optionally filter by \
    \task-path prefix. Returns readable labeled text, not JSON."

runListAgents :: MultiAgentContext -> ListAgentsArgs -> IO (Either Text Text)
runListAgents ctx args = do
    agents <- listAgents ctx.multiRegistry args.pathPrefix
    pure $ Right $ renderAgentList agents

renderAgentList :: [(TaskPath, SubagentId, SubagentStatus)] -> Text
renderAgentList agents =
    case sortOn (\(path, _, _) -> taskPathText path) agents of
        [] -> "(no live agents)"
        sorted -> Text.intercalate "\n\n" (map renderAgentListEntry sorted)

renderAgentListEntry :: (TaskPath, SubagentId, SubagentStatus) -> Text
renderAgentListEntry (path, agentId, status) =
    Text.intercalate "\n" $
        [ "Agent: " <> taskPathText path
        , "  ID: " <> agentId.unSubagentId
        , "  Status: " <> agentListStatus status
        ]
            <> agentListDetails status

agentListStatus :: SubagentStatus -> Text
agentListStatus = \case
    Pending -> "pending_init"
    Running -> "running"
    Completed _ -> "completed"
    Errored _ -> "errored"
    Interrupted -> "interrupted"
    Closed -> "shutdown"
    NotFound -> "not_found"

agentListDetails :: SubagentStatus -> [Text]
agentListDetails = \case
    Completed (Just text) -> labeledField "Final" text
    Errored err -> labeledField "Error" err
    _ -> []

labeledField :: Text -> Text -> [Text]
labeledField label value =
    case Text.lines (Text.strip value) of
        [] -> []
        first : rest ->
            ("  " <> label <> ": " <> first) : map ("    " <>) rest

--------------------------------------------------------------------------------
-- interrupt_agent
--------------------------------------------------------------------------------

newtype InterruptAgentArgs = InterruptAgentArgs { target :: Text }

interruptAgentArgsDecoder :: Decoder InterruptAgentArgs
interruptAgentArgsDecoder = objectArgs \object_ -> InterruptAgentArgs <$> reqText object_ "target"

interruptAgentTool :: MultiAgentContext -> AppTool
interruptAgentTool ctx = jsonTool "interrupt_agent" interruptDescription
    [ PropertySchema "target" PropertyString True $ Just
        "Agent id or canonical task name to interrupt (from spawn_agent)."
    ]
    True
    TurnSequential
    (typedTool "interrupt_agent" interruptAgentArgsDecoder (runInterrupt ctx))

interruptDescription :: Text
interruptDescription =
    "Interrupt an agent's current turn, if any, and return its previous status \
    \as labeled text. The agent remains available for messages and follow-up \
    \tasks."

encryptedString :: Text -> PropertyType
encryptedString description = PropertyRaw $ object
    [ "type" .= ("string" :: Text)
    , "description" .= description
    , "encrypted" .= True
    ]

runInterrupt :: MultiAgentContext -> InterruptAgentArgs -> IO (Either Text Text)
runInterrupt ctx args = do
    resolved <- resolveAgentTarget ctx.multiRegistry ctx.multiTaskPath args.target
    case resolved of
        Left err -> pure (Left err)
        Right agentId -> do
            result <- interruptSubagent ctx.multiRegistry agentId
            pure $ case result of
                Left err -> Left err
                Right previous -> Right (renderInterruptStatus previous)

renderInterruptStatus :: SubagentStatus -> Text
renderInterruptStatus previous =
    Text.intercalate "\n" $
        ("Previous status: " <> agentListStatus previous)
            : agentListDetails previous
