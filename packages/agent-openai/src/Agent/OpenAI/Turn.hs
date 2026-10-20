-- | Complete non-streaming model turns with automatic waiting, bounded retries
-- and connection replacement. Callers execute tools only after this returns.
-- Streaming hosts that commit partial output use 'Agent.OpenAI.LoopBackend'.
module Agent.OpenAI.Turn
    ( TurnConnection
    , withTurnConnection
    , acquireTurnConnection
    , closeTurnConnection
    , sendTurn
    , sendTurnWithOptions
    ) where

import Agent.Error (ApiError)
import Agent.History (replayFromLatestCheckpoint)
import Agent.Provider (Credential)
import Agent.ResourceScope (ResourceScope, withResourceScope)
import Agent.Responses.Types
import qualified Agent.Responses.Types as Responses (ResponseCreateParams(..))
import Agent.Transport.SessionResource
import Agent.Turn (runTurn)
import qualified Agent.OpenAI.WebSocketClient as WebSocket
import Control.Monad (forM_)
import Data.IORef
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)

data TurnConnection = TurnConnection
    { credential :: !Credential
    , resource :: !(SessionResource WebSocket.CodexConn Credential ApiError)
    , transcript :: !(IORef (Maybe Transcript))
    }

data Transcript = Transcript
    { responseId :: !Text
    , items :: ![ResponseItem]
    }

-- | Acquire one credential's conversation. Failed submissions release the
-- physical socket before the retry wait; successful turns retain it. The
-- logical turn state and conversation cache key survive socket replacement.
-- Credential failover remains with the enclosing token provider.
withTurnConnection
    :: Maybe Text
    -> Credential
    -> (TurnConnection -> Credential -> IO (Either ApiError result))
    -> IO (Either ApiError result)
withTurnConnection sessionKey selected action = withResourceScope \scope -> do
    acquired <- acquireTurnConnection scope sessionKey selected
    case acquired of
        Left err -> pure (Left err)
        Right connection -> action connection selected

-- | Attach a conversation to an existing owner, such as a retained chat session.
acquireTurnConnection
    :: ResourceScope
    -> Maybe Text
    -> Credential
    -> IO (Either ApiError TurnConnection)
acquireTurnConnection scope sessionKey selected = do
    turnState <- WebSocket.newCodexTurnState
    let acquire
            :: ResourceScope -> Credential
            -> IO (Either ApiError (WebSocket.CodexConn, Credential))
        acquire connectionScope current = do
            acquired <- WebSocket.acquireCodexWsCredentialUsingTurnStateForSession
                connectionScope (fromMaybe "" sessionKey) current turnState
            pure (fmap (\connection -> (connection, current)) acquired)
    resource <- newSessionResource scope acquire
    ready <- runSessionResource resource selected (\_ active _ -> pure (Right active))
    case ready of
        Left err -> pure (Left err)
        Right credential -> do
            transcript <- newIORef Nothing
            pure (Right TurnConnection { credential, resource, transcript })

closeTurnConnection :: TurnConnection -> IO ()
closeTurnConnection connection = resetSessionResource connection.resource

sendTurn
    :: TurnConnection -> ResponseCreateParams -> Maybe Text
    -> IO (Either ApiError Response)
sendTurn = sendTurnWithOptions WebSocket.defaultCodexWsOptions

sendTurnWithOptions
    :: WebSocket.CodexWsOptions -> TurnConnection -> ResponseCreateParams -> Maybe Text
    -> IO (Either ApiError Response)
sendTurnWithOptions options connection request previous = runTurn $
    runSessionResource connection.resource connection.credential \socket _ fresh -> do
        known <- readIORef connection.transcript
        let input = requestItems request
            prior = case known of
                Just recorded | previous == Just recorded.responseId -> Just recorded.items
                _ -> Nothing
            replay = fresh && isJust previous && isJust prior
            history = maybe input (<> input) prior
            outgoing = if replay
                then request { Responses.input = Just (ResponseInputItems history) }
                else request
        result <- WebSocket.sendWsRequestOnceWithOptions options socket outgoing
            (if replay then Nothing else previous)
        forM_ result \response ->
            -- Stored continuations from before this handle remain opaque.
            -- A complete request must establish history before local replay.
            if isJust previous && isNothing prior
                then writeIORef connection.transcript Nothing
                else writeIORef connection.transcript (Just Transcript
                    { responseId = response.responseId
                    , items = replayFromLatestCheckpoint isCheckpoint (history <> response.output)
                    })
        pure result

requestItems :: ResponseCreateParams -> [ResponseItem]
requestItems request = case request.input of
    Just (ResponseInputItems items) -> items
    Just (ResponseInputText value) ->
        [MessageItem ResponseMessage
            { messageId = Nothing, role = RoleUser
            , content = MessageContentParts [InputTextPart value Nothing]
            , status = Nothing, phase = Nothing, passthrough = Nothing
            }]
    Nothing -> []

isCheckpoint :: ResponseItem -> Bool
isCheckpoint = \case
    CompactionItemValue _ -> True
    ContextCompactionItemValue item -> isJust item.encryptedContent
    KnownResponseItem ItemCompaction _ -> True
    KnownResponseItem ItemContextCompaction _ -> True
    _ -> False
