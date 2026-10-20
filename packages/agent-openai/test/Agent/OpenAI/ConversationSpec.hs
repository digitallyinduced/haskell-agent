{-# LANGUAGE RankNTypes #-}
module Agent.OpenAI.ConversationSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import qualified Agent.Json as Json
import Agent.Loop.Conversation
import Agent.OpenAI.Conversation
import qualified Agent.Responses.Codec as Codec
import Agent.Responses.Types hiding (CustomToolCall(..))
import Agent.Turn (runTurnWithPolicy, requestRetryLimit)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, cancel, waitCatch)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception.Safe (bracket_, throwIO)
import Control.Retry (constantDelay, limitRetries)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Network.WebSockets as WS
import System.Timeout (timeout)
import Test.Hspec

-- Metadata outside the provider schema must survive recovery unchanged.
data Envelope = Envelope { native :: Response, auditToken :: Text }

data Sent = Sent Int Int (Maybe Text) [ResponseItem] deriving (Eq, Show)

spec :: Spec
spec = describe "recovering native conversations" do
    it "replays committed history and pending tool output without repeating effects" do
        (result, sent, effects) <- scripted 3 ownedStart
            [Right (response "tool" [toolCall]), Left (ConnectionError "closed"), Right completed] id
        result `shouldBe` Right "done"
        sent `shouldBe`
            [ Sent 1 0 Nothing [userMessage "start"]
            , Sent 1 1 (Just "tool") [toolOutput]
            , Sent 2 1 Nothing [userMessage "start", toolCall, toolOutput]
            ]
        effects `shouldBe` [("tool", "audit"), ("done", "audit")]

    it "does not multiply the turn retry budget across new connections" do
        let err = ProviderError OverloadedError "busy" Nothing
        (result, sent, effects) <- scripted 3 ownedStart (replicate 4 (Left err)) id
        result `shouldBe` Right "error"
        length sent `shouldBe` (1 + requestRetryLimit)
        [connection | Sent connection _ _ _ <- sent] `shouldBe` replicate 4 1
        effects `shouldBe` []

    it "loads authoritative durable input only after replacement without duplicating pending output" do
        loads <- newIORef (0 :: Int)
        let persisted = [userMessage "start", toolCall, toolOutput, userMessage "durable observation"]
            recovery = immediateRecovery
                { loadReplay = Just (modifyIORef' loads (+ 1) >> pure persisted) }
        (result, sent, effects) <- scriptedWithRecovery recovery ownedStart
            [Right (response "tool" [toolCall]), Left (ConnectionError "closed"), Right completed] id
        result `shouldBe` Right "done"
        last sent `shouldBe` Sent 2 1 Nothing persisted
        readIORef loads `shouldReturn` 1
        effects `shouldBe` [("tool", "audit"), ("done", "audit")]

    it "uses durable history to move a stored continuation to another account" do
        let persisted = [userMessage "complete durable transcript"]
            start = ownedStart { agentLoopPreviousResponseId = Just "stored" }
            recovery = immediateRecovery { loadReplay = Just (pure persisted) }
        (result, sent, _) <- scriptedWithRecovery recovery start
            [Left (ProviderError UsageLimitReached "quota" Nothing), Right completed] id
        result `shouldBe` Right "done"
        sent `shouldBe` [Sent 1 0 (Just "stored") [userMessage "start"], Sent 2 0 Nothing persisted]

    it "propagates a history loader failure instead of treating it as a model disconnect" do
        loads <- newIORef (0 :: Int)
        let recovery = immediateRecovery
                { loadReplay = Just (modifyIORef' loads (+ 1) >> throwIO WS.ConnectionClosed) }
        scriptedWithRecovery recovery ownedStart [Left (ConnectionError "closed")] id
            `shouldThrow` (== WS.ConnectionClosed)
        readIORef loads `shouldReturn` 1

    it "replays on account failover while retaining the pending iteration" do
        (result, sent, _) <- scripted 3 ownedStart
            [ Right (response "tool" [toolCall])
            , Left (ProviderError UsageLimitReached "quota" Nothing)
            , Right completed
            ] id
        result `shouldBe` Right "done"
        [ (connection, iteration, previous) | Sent connection iteration previous _ <- sent ]
            `shouldBe` [(1, 0, Nothing), (1, 1, Just "tool"), (2, 1, Nothing)]

    it "retains an opaque stored continuation and delegates its account failure" do
        let start = ownedStart { agentLoopPreviousResponseId = Just "stored" }
        (result, sent, _) <- scripted 3 start
            [Left (ConnectionError "closed"), Left (ProviderError UsageLimitReached "quota" Nothing)] id
        result `shouldBe` Right "error"
        [previous | Sent _ _ previous _ <- sent] `shouldBe` [Just "stored", Just "stored"]

    it "delegates a persistent connection failure after the replacement limit" do
        (result, sent, _) <- scripted 2 ownedStart (replicate 5 (Left (ConnectionError "closed"))) id
        result `shouldBe` Right "error"
        [connection | Sent connection _ _ _ <- sent] `shouldBe` [1, 2, 3]

    it "cuts replay at the latest provider checkpoint" do
        let checkpoint = CompactionItemValue (CompactionItem Nothing (Just "encrypted"))
        (_, sent, _) <- scripted 3 ownedStart
            [ Right (response "tool" [checkpoint, toolCall])
            , Left (ConnectionError "closed")
            , Right completed
            ] id
        last sent `shouldBe` Sent 2 1 Nothing [checkpoint, toolCall, toolOutput]

    it "recovers a raw socket exception from a model submission" do
        attempts <- newIORef (0 :: Int)
        let customize hooks = hooks
                { agentLoopSendRequest = \request previous -> do
                    count <- atomicModifyIORef' attempts (\n -> (n + 1, n))
                    if count == 0 then throwIO WS.ConnectionClosed
                        else hooks.agentLoopSendRequest request previous
                }
        (result, sent, _) <- scripted 3 ownedStart [Right completed] customize
        result `shouldBe` Right "done"
        [connection | Sent connection _ _ _ <- sent] `shouldBe` [2]

    it "never replays a successful response after a socket exception in a host effect" do
        effects <- newIORef (0 :: Int)
        let customize hooks = hooks
                { agentLoopOnResponse = \_ _ _ -> modifyIORef' effects (+ 1) >> throwIO WS.ConnectionClosed }
        scripted 3 ownedStart [Right completed] customize `shouldThrow` (== WS.ConnectionClosed)
        readIORef effects `shouldReturn` 1

    it "closes the old connection before waiting for its replacement" do
        active <- newIORef False
        let source :: forall value. (() -> IO (Either ApiError value)) -> IO (Either ApiError value)
            source action = bracket_ (writeIORef active True) (writeIORef active False) (action ())
            config = immediateRecovery
                { replacementLimit = 1
                , beforeReplacement = \_ _ -> readIORef active `shouldReturn` False
                }
        result <- runConversationWith config id source
            (const (baseHooks { agentLoopSendRequest = \_ _ -> pure (Left (ConnectionError "closed")) }))
            ownedStart
        result `shouldBe` Right "error"

    it "cancels a replacement wait without acquiring another connection" do
        connections <- newIORef (0 :: Int)
        waiting <- newEmptyMVar
        let source :: forall value. (() -> IO (Either ApiError value)) -> IO (Either ApiError value)
            source action = modifyIORef' connections (+ 1) >> action ()
            config = immediateRecovery
                { beforeReplacement = \_ _ -> putMVar waiting () >> threadDelay 10_000_000 }
            action = runConversationWith config id source
                (const (baseHooks { agentLoopSendRequest = \_ _ -> pure (Left (ConnectionError "closed")) }))
                ownedStart
        -- Structured ownership cancels and joins the interrupted conversation.
        outcome <- timeout 1_000_000 $ withAsync action \worker ->
            takeMVar waiting >> cancel worker >> waitCatch worker
        outcome `shouldSatisfy` \case Just (Left _) -> True; _ -> False
        readIORef connections `shouldReturn` 1

scripted
    :: Int -> AgentLoopState -> [Either ApiError Response]
    -> (AgentLoopHooks Envelope Text -> AgentLoopHooks Envelope Text)
    -> IO (Either ApiError Text, [Sent], [(Text, Text)])
scripted replacements start script customize =
    scriptedWithRecovery (immediateRecovery { replacementLimit = replacements }) start script customize

scriptedWithRecovery
    :: ConversationRecovery -> AgentLoopState -> [Either ApiError Response]
    -> (AgentLoopHooks Envelope Text -> AgentLoopHooks Envelope Text)
    -> IO (Either ApiError Text, [Sent], [(Text, Text)])
scriptedWithRecovery recovery start script customize = do
    remaining <- newIORef script
    sent <- newIORef []
    effects <- newIORef []
    connections <- newIORef (0 :: Int)
    iteration <- newIORef start.agentLoopIteration
    let source :: forall value. (Int -> IO (Either ApiError value)) -> IO (Either ApiError value)
        source action = do
            connection <- atomicModifyIORef' connections (\n -> (n + 1, n + 1))
            result <- action connection
            case result of
                Left (ProviderError UsageLimitReached _ _) | connection < 5 -> source action
                _ -> pure result
        hooks :: Int -> AgentLoopHooks Envelope Text
        hooks connection = customize AgentLoopHooks
            { agentLoopMaxIterations = 10
            , agentLoopBeforeIteration = const (pure Nothing)
            , agentLoopOnMaxIterations = const (pure "limit")
            , agentLoopBuildRequest = \state -> pure defaultResponseCreateParams
                { input = Just (ResponseInputItems state.agentLoopInput) }
            , agentLoopOnRequest = \state _ -> writeIORef iteration state.agentLoopIteration
            , agentLoopSendRequest = \request previous -> runTurnWithPolicy
                (constantDelay 0 <> limitRetries requestRetryLimit) do
                current <- readIORef iteration
                let items = case request.input of Just (ResponseInputItems values) -> values; _ -> []
                modifyIORef' sent (<> [Sent connection current previous items])
                next <- atomicModifyIORef' remaining \case
                    step : rest -> (rest, step)
                    [] -> ([], Left (ConnectionError "script exhausted"))
                pure (fmap (\native -> Envelope native "audit") next)
            , agentLoopOnError = \_ _ _ -> pure (AgentLoopFinish "error")
            , agentLoopOnResponse = \_ _ envelope -> do
                modifyIORef' effects (<> [(envelope.native.responseId, envelope.auditToken)])
                pure if any isCall envelope.native.output
                    then AgentLoopContinue [toolOutput] (Just envelope.native.responseId)
                    else AgentLoopFinish "done"
            }
    result <- runConversationWith recovery (.native) source hooks start
    (result,,) <$> readIORef sent <*> readIORef effects
  where
    isCall FunctionCallItem{} = True
    isCall _ = False

immediateRecovery :: ConversationRecovery
immediateRecovery = defaultConversationRecovery { beforeReplacement = \_ _ -> pure () }

baseHooks :: AgentLoopHooks Response Text
baseHooks = AgentLoopHooks
    { agentLoopMaxIterations = 5
    , agentLoopBeforeIteration = const (pure Nothing)
    , agentLoopOnMaxIterations = const (pure "limit")
    , agentLoopBuildRequest = \state -> pure defaultResponseCreateParams
        { input = Just (ResponseInputItems state.agentLoopInput) }
    , agentLoopOnRequest = \_ _ -> pure ()
    , agentLoopSendRequest = \_ _ -> pure (Right completed)
    , agentLoopOnError = \_ _ _ -> pure (AgentLoopFinish "error")
    , agentLoopOnResponse = \_ _ _ -> pure (AgentLoopFinish "done")
    }

ownedStart :: AgentLoopState
ownedStart = emptyAgentLoopState { agentLoopInput = [userMessage "start"] }

completed :: Response
completed = response "done" []

-- Fixture decoding exercises the native schema, including opaque item fields.
response :: Text -> [ResponseItem] -> Response
response identifier items = either (error . Text.unpack) id $ Codec.decodeResponse $ LBS.toStrict $ Aeson.encode $
    Aeson.object ["id" Aeson..= identifier, "model" Aeson..= ("test" :: Text), "output" Aeson..= items]

userMessage :: Text -> ResponseItem
userMessage text = MessageItem ResponseMessage
    { messageId = Nothing, role = RoleUser, content = MessageContentText text
    , status = Nothing, phase = Nothing, passthrough = Nothing }

toolCall :: ResponseItem
toolCall = FunctionCallItem FunctionCall
    { itemId = Nothing, callId = "call", name = "work", arguments = "{}"
    , namespace = Nothing, provider = Nothing, encryptedFunctionArgs = Nothing
    , status = Nothing, async = Nothing }

toolOutput :: ResponseItem
toolOutput = FunctionCallOutputItem FunctionCallOutput
    { itemId = Nothing, callId = "call", name = Nothing, namespace = Nothing, provider = Nothing
    , output = Json.rawJsonFromEncoding (Aeson.toEncoding ("done" :: Text))
    , status = Nothing, async = Nothing, localOutcome = Nothing }
