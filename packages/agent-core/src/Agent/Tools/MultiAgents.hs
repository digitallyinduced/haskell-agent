-- | Codex multi-agent v2 tools (collaboration namespace).
--
-- Wire names and schemas follow openai/codex
-- @codex-rs/core/src/tools/handlers/multi_agents_spec.rs@ (v2).
module Agent.Tools.MultiAgents
    ( MultiAgentContext(..)
    , CollaborationSpawnOptions(..)
    , SubagentWorktree(..)
    , multiAgentTools
    , multiAgentNamespace
    , multiAgentToolNames
    ) where

import Agent.Subagents
    ( SubagentId(..)
    , SubagentRegistry
    , RootTurnId
    , SubagentStatus(..)
    , defaultWaitTimeoutMs
    , encodeStatus
    , interruptSubagent
    , listAgents
    , queueMessageFromForTurn
    , resolveAgentTarget
    , sendInputMessageForTurn
    , spawnSubagentAtPreparedForTurn
    , waitAnyLive
    , waitSubagentsFrom
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
    , encryptedInterAgentContent
    , plainInterAgentContent
    )
import Agent.OsPath (OsPath)
import Agent.ToolArgs
    ( objectArgs
    , optInt
    , optText
    , reqText
    , reqTextList
    )
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    )
import Agent.ToolDispatch (ToolCall(..), ToolHandler, typedTool, typedToolWithCall)
import Agent.Tools.Types
    ( AppTool
    , ApprovalRule(..)
    , jsonAppTool
    )
import Data.Aeson (FromJSON(..), Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

data SubagentWorktree = SubagentWorktree
    { subagentWorktreePath :: !OsPath
    , subagentWorktreeCleanup :: !(IO (Either Text ()))
    }

data CollaborationSpawnOptions = CollaborationSpawnOptions
    { collaborationModel :: !(Maybe Text)
    , collaborationReasoningEffort :: !(Maybe Text)
    , collaborationForkTurns :: !(Maybe Text)
    } deriving (Eq, Show)

-- | Per-agent identity for nesting depth / parent linkage / task path.
data MultiAgentContext = MultiAgentContext
    { multiRegistry :: !SubagentRegistry
    , multiSelfId :: !(Maybe SubagentId)
    , multiDepth :: !Int
    , multiTaskPath :: !TaskPath
    , multiRootTurnId :: !(IO (Maybe RootTurnId))
      -- | Optional host hook to rehydrate a closed/missing agent from disk
      -- before follow-ups. 'Nothing' means in-memory only.
    , multiResumeFromDisk :: !(Maybe (SubagentId -> IO (Either Text ())))
      -- | Optional host hook for Grok-style isolated worktree children.
    , multiCreateWorktree :: !(Maybe (OsPath -> IO (Either Text SubagentWorktree)))
      -- | Record model/effort overrides and seed the child transcript before
      -- its worker starts.
    , multiPrepareSpawn
        :: !(Maybe (SubagentId -> CollaborationSpawnOptions -> IO ()))
      -- | Deliver a child message to the root agent's next model turn.
    , multiSendToRoot :: !(Maybe (InterAgentMessage -> IO (Either Text Text)))
    }

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

multiAgentTools :: MultiAgentContext -> [AppTool]
multiAgentTools ctx =
    [ spawnAgentTool ctx
    , waitAgentTool ctx
    , sendMessageTool ctx
    , followupTaskTool ctx
    , listAgentsTool ctx
    , interruptAgentTool ctx
    ]

jsonTool
    :: Text
    -> Text
    -> [PropertySchema]
    -> Bool
    -> ToolHandler
    -> AppTool
jsonTool name description parameters readOnly =
    jsonAppTool name description parameters
        (if readOnly then AlwaysReadOnly else AlwaysPrompt)

--------------------------------------------------------------------------------
-- spawn_agent
--------------------------------------------------------------------------------

data SpawnAgentArgs = SpawnAgentArgs
    { taskName :: Text
    , message :: Text
    , model :: Maybe Text
    , reasoningEffort :: Maybe Text
    , forkTurns :: Maybe Text
    }

instance FromJSON SpawnAgentArgs where
    parseJSON = objectArgs \object_ -> SpawnAgentArgs
        <$> reqText object_ "task_name"
        <*> reqText object_ "message"
        <*> optText object_ "model"
        <*> optText object_ "reasoning_effort"
        <*> optText object_ "fork_turns"

spawnAgentTool :: MultiAgentContext -> AppTool
spawnAgentTool ctx = jsonTool "spawn_agent" spawnAgentDescription
    [ PropertySchema "task_name" PropertyString True $ Just
        "Task name for the new agent. Use lowercase letters, digits, and underscores."
    , PropertySchema "message"
        (encryptedString "Initial plain-text task for the new agent.") True Nothing
    , PropertySchema "model" PropertyString False $ Just
        "Model override for the new agent. Omit unless an explicit override is needed."
    , PropertySchema "reasoning_effort" PropertyString False $ Just
        "Reasoning effort override for the new agent. Omit to inherit the parent effort."
    , PropertySchema "fork_turns" PropertyString False $ Just
        "Optional number of turns to fork. Defaults to `all`. Use `none`, `all`, or a positive integer string such as `3` to fork only the most recent turns."
    ]
    True
    (typedToolWithCall "spawn_agent" (runSpawn ctx))

spawnAgentDescription :: Text
spawnAgentDescription =
    "Spawns an agent to work on the specified task. If your current task is \
    \/root/task1 and you spawn_agent with task_name \"task_3\" the agent will \
    \have canonical task name /root/task1/task_3. You may refer to this agent \
    \as task_3 or /root/task1/task_3. Returns the canonical task_name."

runSpawn :: MultiAgentContext -> ToolCall -> SpawnAgentArgs -> IO (Either Text Text)
runSpawn ctx call args
    | Text.null (Text.strip args.message) =
        pure (Left "spawn_agent requires a non-empty message")
    | not (validForkTurns args.forkTurns) =
        pure (Left "fork_turns must be none, all, or a positive integer string")
    | otherwise = do
        rootTurnId <- ctx.multiRootTurnId
        let spawnOptions = CollaborationSpawnOptions
                { collaborationModel = sanitizeOverride args.model
                , collaborationReasoningEffort =
                    sanitizeOverride args.reasoningEffort
                , collaborationForkTurns = normalizeForkTurns args.forkTurns
                }
            prepare agentId =
                maybe (pure ()) (\hook -> hook agentId spawnOptions)
                    ctx.multiPrepareSpawn
        result <- spawnSubagentAtPreparedForTurn
            ctx.multiRegistry
            rootTurnId
            prepare
            ctx.multiSelfId
            ctx.multiTaskPath
            ctx.multiDepth
            args.taskName
            (messageContent call args.message)
            Nothing
        pure $ case result of
            Left err -> Left err
            Right (_agentId, path) ->
                Right $ encodeJson $ object
                    [ "task_name" .= taskPathText path
                    , "nickname" .= Aeson.Null
                    ]

validForkTurns :: Maybe Text -> Bool
validForkTurns = \case
    Nothing -> True
    Just turns ->
        let stripped = Text.toLower (Text.strip turns)
        in stripped `elem` ["none", "all", ""]
            || case reads (Text.unpack stripped) of
                [(turns :: Int, "")] -> turns > 0
                _ -> False

sanitizeOverride :: Maybe Text -> Maybe Text
sanitizeOverride value = value >>= \raw ->
    let stripped = Text.strip raw
    in if Text.null stripped then Nothing else Just stripped

normalizeForkTurns :: Maybe Text -> Maybe Text
normalizeForkTurns value =
    case Text.toLower . Text.strip <$> value of
        Nothing -> Just "all"
        Just "" -> Just "all"
        normalized -> normalized

--------------------------------------------------------------------------------
-- wait_agent
--------------------------------------------------------------------------------

data WaitAgentArgs = WaitAgentArgs
    { targets :: Maybe [Text]
    , timeoutMs :: Maybe Int
    }

instance FromJSON WaitAgentArgs where
    parseJSON = objectArgs \object_ -> do
        targets <- case KeyMap.lookup (Key.fromText "targets") object_ of
            Nothing -> pure Nothing
            Just _ -> Just <$> reqTextList object_ "targets"
        timeoutMs <- optInt object_ "timeout_ms"
        pure WaitAgentArgs { targets, timeoutMs }

waitAgentTool :: MultiAgentContext -> AppTool
waitAgentTool ctx = jsonTool "wait_agent" waitAgentDescription
    [ PropertySchema "timeout_ms" PropertyNumber False $ Just
        "Timeout in milliseconds. Defaults to 30000 ms."
    ]
    True
    (typedTool "wait_agent" (runWait ctx))

waitAgentDescription :: Text
waitAgentDescription =
    "Wait for a mailbox update from live agents, including final-status \
    \notifications. Returns a summary of which agents have updates, or a \
    \timeout summary if no activity arrives before the deadline."

runWait :: MultiAgentContext -> WaitAgentArgs -> IO (Either Text Text)
runWait ctx args = do
    let timeoutMs = fromMaybe defaultWaitTimeoutMs args.timeoutMs
    case args.targets of
        Nothing -> do
            (statuses, timedOut) <-
                waitAnyLive ctx.multiRegistry ctx.multiSelfId timeoutMs
            pure $ Right $ encodeJson $ object
                [ "message" .= waitSummary timedOut statuses
                , "timed_out" .= timedOut
                ]
        Just [] -> pure (Left "targets must be non-empty when provided")
        Just targets -> do
            resolved <- mapM (resolveAgentTarget ctx.multiRegistry ctx.multiTaskPath) targets
            case sequence resolved of
                Left err -> pure (Left err)
                Right ids -> do
                    (statuses, timedOut) <-
                        waitSubagentsFrom
                            ctx.multiRegistry ctx.multiSelfId ids timeoutMs
                    pure $ Right $ encodeJson $ object
                        [ "message" .= waitSummary timedOut statuses
                        , "timed_out" .= timedOut
                        , "status" .= statusObject statuses
                        ]

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

statusObject :: Map SubagentId SubagentStatus -> Value
statusObject =
    Object
        . KeyMap.fromList
        . map (\(SubagentId tid, status) -> (Key.fromText tid, encodeStatus status))
        . Map.toList

--------------------------------------------------------------------------------
-- send_message / followup_task
--------------------------------------------------------------------------------

data MessageArgs = MessageArgs
    { target :: Text
    , message :: Text
    }

instance FromJSON MessageArgs where
    parseJSON = objectArgs \object_ -> MessageArgs
        <$> reqText object_ "target"
        <*> reqText object_ "message"

sendMessageTool :: MultiAgentContext -> AppTool
sendMessageTool ctx = jsonTool "send_message" sendMessageDescription
    [ PropertySchema "target" PropertyString True $ Just
        "Relative or canonical task name to message (from spawn_agent)."
    , PropertySchema "message"
        (encryptedString "Message text to queue on the target agent.") True Nothing
    ]
    True
    (typedToolWithCall "send_message" (runSendMessage ctx))

sendMessageDescription :: Text
sendMessageDescription =
    "Send a message to an existing agent. The message will be delivered \
    \promptly. Does not trigger a new turn."

runSendMessage :: MultiAgentContext -> ToolCall -> MessageArgs -> IO (Either Text Text)
runSendMessage ctx call args
    | Text.null (Text.strip args.message) =
        pure (Left "send_message requires a non-empty message")
    | otherwise = do
        case resolveTaskPath ctx.multiTaskPath args.target of
            Right targetPath | targetPath == taskPathRoot ->
                sendToRoot ctx (messageContent call args.message)
            _ -> do
                restored <- maybeRestore ctx args.target
                case restored of
                    Left err -> pure (Left err)
                    Right () -> do
                        resolved <-
                            resolveAgentTarget
                                ctx.multiRegistry ctx.multiTaskPath args.target
                        case resolved of
                            Left err -> pure (Left err)
                            Right agentId -> do
                                rootTurnId <- ctx.multiRootTurnId
                                queueMessageFromForTurn
                                    ctx.multiRegistry rootTurnId
                                    ctx.multiTaskPath agentId
                                    (messageContent call args.message)

followupTaskTool :: MultiAgentContext -> AppTool
followupTaskTool ctx = jsonTool "followup_task" followupDescription
    [ PropertySchema "target" PropertyString True $ Just
        "Agent id or canonical task name to send a follow-up task to (from spawn_agent)."
    , PropertySchema "message"
        (encryptedString "Message text to send to the target agent.") True Nothing
    ]
    True
    (typedToolWithCall "followup_task" (runFollowup ctx))

followupDescription :: Text
followupDescription =
    "Send a follow-up task to an existing non-root target agent and trigger a \
    \turn if it is idle. If the target is already running, deliver the task \
    \promptly at message boundaries."

runFollowup :: MultiAgentContext -> ToolCall -> MessageArgs -> IO (Either Text Text)
runFollowup ctx call args
    | Text.null (Text.strip args.message) =
        pure (Left "followup_task requires a non-empty message")
    | otherwise = do
        case resolveTaskPath ctx.multiTaskPath args.target of
            Right targetPath | targetPath == taskPathRoot ->
                pure (Left "followup_task cannot target the root agent; use send_message")
            _ -> do
                restored <- maybeRestore ctx args.target
                case restored of
                    Left err -> pure (Left err)
                    Right () -> do
                        resolved <-
                            resolveAgentTarget
                                ctx.multiRegistry ctx.multiTaskPath args.target
                        case resolved of
                            Left err -> pure (Left err)
                            Right agentId -> do
                                rootTurnId <- ctx.multiRootTurnId
                                sendInputMessageForTurn
                                    ctx.multiRegistry rootTurnId
                                    ctx.multiTaskPath agentId
                                    (messageContent call args.message) False

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

messageContent :: ToolCall -> Text -> InterAgentMessageContent
messageContent call
    | call.argumentsEncrypted = encryptedInterAgentContent
    | otherwise = plainInterAgentContent

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

instance FromJSON ListAgentsArgs where
    parseJSON = objectArgs \object_ -> ListAgentsArgs
        <$> optText object_ "path_prefix"

listAgentsTool :: MultiAgentContext -> AppTool
listAgentsTool ctx = jsonTool "list_agents" listAgentsDescription
    [ PropertySchema "path_prefix" PropertyString False $ Just
        "Task-path prefix filter without a trailing slash. Omit to list all live agents."
    ]
    True
    (typedTool "list_agents" (runListAgents ctx))

listAgentsDescription :: Text
listAgentsDescription =
    "List live agents in the current root thread tree. Optionally filter by \
    \task-path prefix."

runListAgents :: MultiAgentContext -> ListAgentsArgs -> IO (Either Text Text)
runListAgents ctx args = do
    agents <- listAgents ctx.multiRegistry args.pathPrefix
    let payload =
            [ object
                [ "agent_name" .= taskPathText path
                , "agent_id" .= agentId.unSubagentId
                , "agent_status" .= encodeStatus status
                ]
            | (path, agentId, status) <- agents
            ]
    pure $ Right $ encodeJson $ object [ "agents" .= payload ]

--------------------------------------------------------------------------------
-- interrupt_agent
--------------------------------------------------------------------------------

newtype InterruptAgentArgs = InterruptAgentArgs { target :: Text }

instance FromJSON InterruptAgentArgs where
    parseJSON = objectArgs \object_ -> InterruptAgentArgs <$> reqText object_ "target"

interruptAgentTool :: MultiAgentContext -> AppTool
interruptAgentTool ctx = jsonTool "interrupt_agent" interruptDescription
    [ PropertySchema "target" PropertyString True $ Just
        "Agent id or canonical task name to interrupt (from spawn_agent)."
    ]
    True
    (typedTool "interrupt_agent" (runInterrupt ctx))

interruptDescription :: Text
interruptDescription =
    "Interrupt an agent's current turn, if any, and return its previous status. \
    \The agent remains available for messages and follow-up tasks."

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
                Right previous ->
                    Right $ encodeJson $ object
                        [ "previous_status" .= encodeStatus previous
                        ]

encodeJson :: Value -> Text
encodeJson = Text.decodeUtf8 . LBS.toStrict . Aeson.encode
