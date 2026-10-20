{-# LANGUAGE RankNTypes #-}
-- | Run a native conversation across bounded connection replacements and
-- credential failover. Hosts retain ownership of successful response effects.
module Agent.OpenAI.Conversation
    ( ConversationRecovery(..)
    , defaultConversationRecovery
    , runConversation
    , runConversationWith
    ) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.History (replayFromLatestCheckpoint)
import Agent.Loop.Conversation
import qualified Agent.Loop.Recovery as Recovery
import Agent.OpenAI.Error (isPreviousResponseIdError)
import Agent.Provider (TokenProvider, accountFailureFromApiError, runWithTokenProvider)
import Agent.Responses.Types
import Agent.Transport.WebSocket (transientWsConnectFailureLabel, transientWsMidRunFailureLabel)
import Agent.Turn (requestRetryDelayMicros)
import qualified Agent.OpenAI.Turn as Turn
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import qualified Control.Exception.Safe as Exception
import Control.Monad (forM_)
import Data.IORef
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text

data ConversationRecovery = ConversationRecovery
    { replacementLimit :: !Int
    -- | Called after the old connection closes, before acquiring its replacement.
    -- The default waits using the shared exponential backoff.
    , beforeReplacement :: Int -> ApiError -> IO ()
    -- | May only classify failures safe to replay on another account.
    , accountFailure :: ApiError -> Bool
    -- | Prepare complete replay input, for example repairing interrupted tools
    -- from durable history. Runs only when a full transcript is available.
    , prepareReplay :: [ResponseItem] -> [ResponseItem]
    }

defaultConversationRecovery :: ConversationRecovery
defaultConversationRecovery = ConversationRecovery
    { replacementLimit = 3
    , beforeReplacement = \attempt err -> threadDelay (requestRetryDelayMicros err attempt)
    , accountFailure = isJust . accountFailureFromApiError
    , prepareReplay = replayFromLatestCheckpoint isCheckpoint
    }

-- | Production connector for a native Responses conversation. Send hooks use
-- the transport's shared turn retries; overload never opens a second budget.
runConversation
    :: ConversationRecovery
    -> Text
    -> TokenProvider
    -> (Turn.TurnConnection -> AgentLoopHooks Response result)
    -> AgentLoopState
    -> IO (Either ApiError result)
runConversation config sessionKey provider =
    runConversationWith config id \attempt ->
        runWithTokenProvider provider \credential ->
            Turn.withTurnConnection (Just sessionKey) credential \connection _ -> attempt connection

-- | Use a host connector and retain an opaque response wrapper verbatim. The
-- projection reads native history without rebuilding audit or delivery data.
-- The connector owns credential failover and may re-enter on account failure.
-- Build hooks must preserve 'agentLoopInput' as the request input.
runConversationWith
    :: ConversationRecovery
    -> (response -> Response)
    -> (forall value. (connection -> IO (Either ApiError value)) -> IO (Either ApiError value))
    -> (connection -> AgentLoopHooks response result)
    -> AgentLoopState
    -> IO (Either ApiError result)
runConversationWith config nativeResponse withConnection mkHooks initialState = do
    replay <- Recovery.newReplayState initialState
    iterationRef <- newIORef initialState.agentLoopIteration
    let ownsConversation = isNothing initialState.agentLoopPreviousResponseId
        resumeStateFor request previous fullInput iteration = AgentLoopState
            { agentLoopIteration = iteration
            , agentLoopInput = if ownsConversation then config.prepareReplay fullInput else requestItems request
            , agentLoopPreviousResponseId = if ownsConversation then Nothing else previous
            }
        wrap available connection = AgentLoopHooks
            { agentLoopMaxIterations = inner.agentLoopMaxIterations
            , agentLoopBeforeIteration = \state -> fmap Finished <$> inner.agentLoopBeforeIteration state
            , agentLoopOnMaxIterations = fmap Finished . inner.agentLoopOnMaxIterations
            , agentLoopBuildRequest = inner.agentLoopBuildRequest
            , agentLoopOnRequest = \state request -> do
                writeIORef iterationRef state.agentLoopIteration
                inner.agentLoopOnRequest state request
            , agentLoopSendRequest = \request previous -> do
                iteration <- readIORef iterationRef
                fullInput <- Recovery.prepareReplayAttempt replay (isJust previous) (requestItems request)
                    (\items -> resumeStateFor request previous items iteration)
                result <- Exception.catchAny (inner.agentLoopSendRequest request previous) \exception ->
                    maybe (Exception.throwIO exception) (pure . Left) (transportFailure exception)
                forM_ result \response -> Recovery.commitReplayResponse replay
                    (replayFromLatestCheckpoint isCheckpoint) fullInput (nativeResponse response).output
                pure result
            , agentLoopOnError = \state request err ->
                if ownsConversation && config.accountFailure err
                    then pure (AgentLoopFinish (AccountFailed err))
                    else if replaceable ownsConversation err
                        then do
                            permitted <- available
                            if permitted
                                then pure (AgentLoopFinish (Reconnect err))
                                else delegate state request err
                        else delegate state request err
            , agentLoopOnResponse = \state request response ->
                mapDecision <$> inner.agentLoopOnResponse state request response
            }
          where
            inner = mkHooks connection
            delegate state request err = mapDecision <$> inner.agentLoopOnError state request err
        mapDecision = \case
            AgentLoopContinue input previous -> AgentLoopContinue input previous
            AgentLoopFinish result -> AgentLoopFinish (Finished result)
        attempt available connection = do
            resume <- Recovery.readResumeState replay
            -- Only acquisition and submission failures permit replay. Keep
            -- exceptions from host effects as data until the connection closes,
            -- so an IOException in a tool cannot restart its successful response.
            Exception.tryAny (runAgenticLoop (wrap available connection) resume) >>= \case
                Left exception -> pure (Right (Recovery.ConnectionFinished (Left exception)))
                Right (Finished result) -> pure (Right (Recovery.ConnectionFinished (Right result)))
                Right (AccountFailed err) -> pure (Left err)
                Right (Reconnect err) -> pure (Right (Recovery.ReplaceConnection err))
    result <- Recovery.runRecoveringConnections Recovery.ConnectionRecovery
        { replacementLimit = config.replacementLimit
        , classifyException = transportFailure
        , exhaustedError = id
        , beforeReplacement = config.beforeReplacement
        } withConnection attempt
    traverse (either Exception.throwIO pure) result

data Outcome result = Finished result | Reconnect ApiError | AccountFailed ApiError

transportFailure :: Exception.SomeException -> Maybe ApiError
transportFailure exception = ConnectionError <$>
    (transientWsConnectFailureLabel exception <|> transientWsMidRunFailureLabel exception)

-- Overload has already spent the turn retry budget. An opaque stored
-- continuation cannot be moved to another account or rebuilt locally.
replaceable :: Bool -> ApiError -> Bool
replaceable ownsConversation = \case
    ConnectionError _ -> True
    ProviderError WebSocketConnectionLimitReached _ _ -> True
    err -> ownsConversation && isPreviousResponseIdError err

requestItems :: ResponseCreateParams -> [ResponseItem]
requestItems request = case request.input of
    Just (ResponseInputItems items) -> items
    Just (ResponseInputText text) ->
        [MessageItem ResponseMessage
            { messageId = Nothing, role = RoleUser, content = MessageContentText text
            , status = Nothing, phase = Nothing, passthrough = Nothing
            }]
    Nothing -> []

isCheckpoint :: ResponseItem -> Bool
isCheckpoint = \case
    CompactionItemValue _ -> True
    ContextCompactionItemValue item -> isJust item.encryptedContent
    KnownResponseItem _ item -> checkpointTag item.tag
    UnknownResponseItem item -> checkpointTag item.tag
    _ -> False
  where
    checkpointTag tag = maybe tag id (Text.stripPrefix "raw:" tag)
        `elem` ["compaction", "compaction_summary", "context_compaction"]
