-- | Independent agent turns use the same durable scheduler as terminal turns.
module Agent.Server.AgentSessions (serverAgentSessionActions) where

import Agent.Runtime.AgentSessions (AgentSessionActions(..))
import Agent.Runtime.Session (SessionHandle(..), SessionMeta(..))
import Agent.Runtime.Session.Settings (SessionSettings(..), SessionOperation(..))
import Agent.Runtime.Session.Inbox (deliverSessionInboxMessage, inboxUnavailableError)
import Agent.Server.Application (createTurn)
import Agent.Server.Backend (Backend)
import Agent.Server.Identifier (newUUIDv7Text)
import Agent.Server.Supervisor
import Agent.Server.Types
import Control.Concurrent (threadDelay)
import Data.List (find)
import Data.Text qualified as Text

serverAgentSessionActions :: Backend -> Supervisor -> AccessBoundary -> AgentSessionActions
serverAgentSessionActions backend supervisor boundary = AgentSessionActions
    { agentLaunch = \settings handle message ->
        createTurn backend supervisor boundary handle.sessionMeta.metaId
            CreateTurnRequest
                { createTurnClientRequestId = Nothing, createTurnInput = message
                , createTurnImages = [], createTurnFiles = [], createTurnMessageClock = Nothing
                , createTurnLocalOptions = Just settings{sessionOperation = ConversationTurn}
                } >>= pure . either (Left . (.apiErrorMessage))
                    (\_ -> Right ("started session " <> handle.sessionMeta.metaId))
    , agentStatus = \identifier -> listTurns supervisor boundary (Just identifier) >>= pure . status
    , agentPrepareWait = \identifier -> do
        turns <- listTurns supervisor boundary (Just identifier)
        pure $ case find (active . (.turnRecordStatus)) turns of
            Nothing -> pure (status turns)
            Just turn -> wait turn.turnRecordId
    , agentDeliver = \identifier message -> do
        turns <- listTurns supervisor boundary (Just identifier)
        case find (active . (.turnRecordStatus)) turns of
            Nothing -> deliverSessionInboxMessage identifier message >>= \case
                Left err | err == inboxUnavailableError -> pure Nothing
                other -> pure (Just other)
            Just turn -> do
                request <- newUUIDv7Text
                result <- steerTurn supervisor boundary turn.turnRecordId request message
                pure (Just (either (Left . Text.pack . show) (const (Right ())) result))
    }
  where
    active value = value `elem` [TurnQueued, TurnRunning, TurnWaitingForInput]
    status [] = "idle"
    status (turn : _) = turnStatusText turn.turnRecordStatus
    wait identifier = lookupTurn supervisor boundary identifier >>= \case
        Just turn | active turn.turnRecordStatus -> threadDelay 100000 >> wait identifier
        Just turn -> pure (turnStatusText turn.turnRecordStatus)
        Nothing -> pure "idle"
