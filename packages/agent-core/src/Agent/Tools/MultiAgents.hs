-- | Codex multi-agent v1 tools (spawn_agent, wait_agent, …).
--
-- Wire names and schemas follow openai/codex
-- @codex-rs/core/src/tools/handlers/multi_agents_spec.rs@ (v1 namespace).
module Agent.Tools.MultiAgents
    ( MultiAgentContext(..)
    , multiAgentTools
    , multiAgentNamespace
    , multiAgentToolNames
    ) where

import Agent.Subagents
    ( SubagentId(..)
    , SubagentRegistry
    , SubagentStatus
    , closeSubagent
    , defaultWaitTimeoutMs
    , encodeStatus
    , maxWaitTimeoutMs
    , minWaitTimeoutMs
    , resumeSubagent
    , sendInput
    , spawnSubagent
    , waitSubagents
    )
import Agent.ToolArgs
    ( objectArgs
    , optBool
    , optInt
    , optText
    , reqText
    , reqTextList
    )
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    )
import Agent.ToolDispatch (ToolHandler, typedTool)
import Agent.Tools.Types
    ( AppTool(..)
    , AppToolKind(..)
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

-- | Per-agent identity for nesting depth / parent linkage.
data MultiAgentContext = MultiAgentContext
    { multiRegistry :: !SubagentRegistry
    , multiSelfId :: !(Maybe SubagentId)
    , multiDepth :: !Int
      -- | Optional host hook to rehydrate a closed/missing agent from disk
      -- before 'resume_agent' / follow-ups. 'Nothing' means in-memory only.
    , multiResumeFromDisk :: !(Maybe (SubagentId -> IO (Either Text ())))
    }

multiAgentNamespace :: Text
multiAgentNamespace = "multi_agent_v1"

multiAgentToolNames :: [Text]
multiAgentToolNames =
    [ "spawn_agent"
    , "wait_agent"
    , "send_input"
    , "close_agent"
    , "resume_agent"
    ]

multiAgentTools :: MultiAgentContext -> [AppTool]
multiAgentTools ctx =
    [ spawnAgentTool ctx
    , waitAgentTool ctx
    , sendInputTool ctx
    , closeAgentTool ctx
    , resumeAgentTool ctx
    ]

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

--------------------------------------------------------------------------------
-- spawn_agent
--------------------------------------------------------------------------------

data SpawnAgentArgs = SpawnAgentArgs
    { message :: Maybe Text
    , agentType :: Maybe Text
    , model :: Maybe Text
    , reasoningEffort :: Maybe Text
    , forkContext :: Bool
    }

instance FromJSON SpawnAgentArgs where
    parseJSON = objectArgs \object -> SpawnAgentArgs
        <$> optText object "message"
        <*> optText object "agent_type"
        <*> optText object "model"
        <*> optText object "reasoning_effort"
        <*> (fromMaybe False <$> optBool object "fork_context")

spawnAgentTool :: MultiAgentContext -> AppTool
spawnAgentTool ctx = jsonTool "spawn_agent" spawnAgentDescription
    [ PropertySchema "message" PropertyString False $ Just
        "Initial plain-text task for the new agent."
    , PropertySchema "agent_type" PropertyString False $ Just
        "Optional agent type override. Omit to use the default worker."
    , PropertySchema "model" PropertyString False $ Just
        "Model override for the new agent. Omit unless an explicit override is needed."
    , PropertySchema "reasoning_effort" PropertyString False $ Just
        "Reasoning effort override for the new agent. Omit to inherit the parent effort."
    , PropertySchema "fork_context" PropertyBoolean False $ Just
        "True forks the current thread history into the new agent; false or omitted starts with only the initial prompt. Full-history fork is not supported yet."
    ]
    False
    (typedTool "spawn_agent" (runSpawn ctx))

spawnAgentDescription :: Text
spawnAgentDescription =
    "Spawn a sub-agent for a well-scoped task. Returns the spawned agent id \
    \plus the user-facing nickname when available. Spawned agents inherit your \
    \current model by default. Do not spawn sub-agents unless the user or \
    \applicable AGENTS.md instructions explicitly ask for sub-agents, \
    \delegation, or parallel agent work. After spawning, prefer useful local \
    \work over busy-waiting; call wait_agent only when blocked on the result."

runSpawn :: MultiAgentContext -> SpawnAgentArgs -> IO (Either Text Text)
runSpawn ctx args = case args.message of
    Nothing -> pure (Left "spawn_agent requires message")
    Just message
        | Text.null (Text.strip message) ->
            pure (Left "spawn_agent requires a non-empty message")
        | args.forkContext ->
            pure (Left "fork_context is not supported yet; spawn with a fresh prompt.")
        | otherwise -> do
            -- model / agent_type / reasoning_effort accepted but unused in v1.
            _ <- pure (args.agentType, args.model, args.reasoningEffort)
            result <- spawnSubagent
                ctx.multiRegistry
                ctx.multiSelfId
                ctx.multiDepth
                message
                Nothing
            pure $ case result of
                Left err -> Left err
                Right agentId ->
                    Right $ encodeJson $ object
                        [ "agent_id" .= agentId.unSubagentId
                        , "nickname" .= Aeson.Null
                        ]

--------------------------------------------------------------------------------
-- wait_agent
--------------------------------------------------------------------------------

data WaitAgentArgs = WaitAgentArgs
    { targets :: [Text]
    , timeoutMs :: Maybe Int
    }

instance FromJSON WaitAgentArgs where
    parseJSON = objectArgs \object_ -> WaitAgentArgs
        <$> reqTextList object_ "targets"
        <*> optInt object_ "timeout_ms"

waitAgentTool :: MultiAgentContext -> AppTool
waitAgentTool ctx = jsonTool "wait_agent" waitAgentDescription
    [ PropertySchema "targets" (PropertyArray PropertyString) True $ Just
        "Agent ids to wait on. Pass multiple ids to wait for whichever finishes first."
    , PropertySchema "timeout_ms" PropertyInteger False $ Just $
        "Timeout in milliseconds. Defaults to "
            <> Text.pack (show defaultWaitTimeoutMs)
            <> ", min "
            <> Text.pack (show minWaitTimeoutMs)
            <> ", max "
            <> Text.pack (show maxWaitTimeoutMs)
            <> ". Prefer longer waits (minutes) to avoid busy polling."
    ]
    True
    (typedTool "wait_agent" (runWait ctx))

waitAgentDescription :: Text
waitAgentDescription =
    "Wait for agents to reach a final status. Completed statuses may include \
    \the agent's final message. Returns empty status when timed out. Once the \
    \agent reaches a final status, a notification message will be received \
    \containing the same completed status."

runWait :: MultiAgentContext -> WaitAgentArgs -> IO (Either Text Text)
runWait ctx args
    | null args.targets = pure (Left "agent ids must be non-empty")
    | otherwise = do
        let ids = map SubagentId args.targets
            timeoutMs = fromMaybe defaultWaitTimeoutMs args.timeoutMs
        (statuses, timedOut) <- waitSubagents ctx.multiRegistry ids timeoutMs
        pure $ Right $ encodeJson $ object
            [ "status" .= statusObject statuses
            , "timed_out" .= timedOut
            ]

statusObject :: Map SubagentId SubagentStatus -> Value
statusObject =
    Object
        . KeyMap.fromList
        . map (\(SubagentId tid, status) -> (Key.fromText tid, encodeStatus status))
        . Map.toList

--------------------------------------------------------------------------------
-- send_input
--------------------------------------------------------------------------------

data SendInputArgs = SendInputArgs
    { target :: Text
    , message :: Maybe Text
    , interrupt :: Bool
    }

instance FromJSON SendInputArgs where
    parseJSON = objectArgs \object_ -> SendInputArgs
        <$> reqText object_ "target"
        <*> optText object_ "message"
        <*> (fromMaybe False <$> optBool object_ "interrupt")

sendInputTool :: MultiAgentContext -> AppTool
sendInputTool ctx = jsonTool "send_input" sendInputDescription
    [ PropertySchema "target" PropertyString True $ Just
        "Agent id to message (from spawn_agent)."
    , PropertySchema "message" PropertyString False $ Just
        "Plain-text message to send to the agent."
    , PropertySchema "interrupt" PropertyBoolean False $ Just
        "True interrupts the current task and handles this message immediately; false or omitted queues it."
    ]
    False
    (typedTool "send_input" (runSendInput ctx))

sendInputDescription :: Text
sendInputDescription =
    "Send a message to an existing agent. Use interrupt=true to redirect work \
    \immediately. You should reuse the agent by send_input if you believe your \
    \assigned task is highly dependent on the context of a previous task."

runSendInput :: MultiAgentContext -> SendInputArgs -> IO (Either Text Text)
runSendInput ctx args = case args.message of
    Nothing -> pure (Left "send_input requires message")
    Just message
        | Text.null (Text.strip message) ->
            pure (Left "send_input requires a non-empty message")
        | otherwise -> do
            result <- sendInput
                ctx.multiRegistry
                (SubagentId args.target)
                message
                args.interrupt
            pure $ case result of
                Left err -> Left err
                Right _ ->
                    Right $ encodeJson $ object
                        [ "submission_id" .= ("queued" :: Text)
                        ]

--------------------------------------------------------------------------------
-- close_agent
--------------------------------------------------------------------------------

newtype CloseAgentArgs = CloseAgentArgs { target :: Text }

instance FromJSON CloseAgentArgs where
    parseJSON = objectArgs \object_ -> CloseAgentArgs <$> reqText object_ "target"

closeAgentTool :: MultiAgentContext -> AppTool
closeAgentTool ctx = jsonTool "close_agent" closeAgentDescription
    [ PropertySchema "target" PropertyString True $ Just
        "Agent id to close (from spawn_agent)."
    ]
    False
    (typedTool "close_agent" (runClose ctx))

closeAgentDescription :: Text
closeAgentDescription =
    "Close an agent and any open descendants when they are no longer needed, \
    \and return the target agent's previous status before shutdown was \
    \requested. Completed agents remain open and count toward the concurrency \
    \limit until closed. Don't keep agents open for too long if they are not \
    \needed anymore."

runClose :: MultiAgentContext -> CloseAgentArgs -> IO (Either Text Text)
runClose ctx args = do
    result <- closeSubagent ctx.multiRegistry (SubagentId args.target)
    pure $ case result of
        Left err -> Left err
        Right previous ->
            Right $ encodeJson $ object
                [ "previous_status" .= encodeStatus previous
                ]

--------------------------------------------------------------------------------
-- resume_agent
--------------------------------------------------------------------------------

newtype ResumeAgentArgs = ResumeAgentArgs { resumeId :: Text }

instance FromJSON ResumeAgentArgs where
    parseJSON = objectArgs \object_ -> ResumeAgentArgs <$> reqText object_ "id"

resumeAgentTool :: MultiAgentContext -> AppTool
resumeAgentTool ctx = jsonTool "resume_agent" resumeAgentDescription
    [ PropertySchema "id" PropertyString True $ Just
        "Agent id to resume."
    ]
    False
    (typedTool "resume_agent" (runResume ctx))

resumeAgentDescription :: Text
resumeAgentDescription =
    "Resume a previously closed agent by id so it can receive send_input and \
    \wait_agent calls."

runResume :: MultiAgentContext -> ResumeAgentArgs -> IO (Either Text Text)
runResume ctx args = do
    let agentId = SubagentId args.resumeId
    _ <- case ctx.multiResumeFromDisk of
        Just restore -> restore agentId
        Nothing -> pure (Right ())
    result <- resumeSubagent ctx.multiRegistry agentId
    pure $ case result of
        Left err -> Left err
        Right status ->
            Right $ encodeJson $ object
                [ "status" .= encodeStatus status
                ]

encodeJson :: Value -> Text
encodeJson = Text.decodeUtf8 . LBS.toStrict . Aeson.encode
