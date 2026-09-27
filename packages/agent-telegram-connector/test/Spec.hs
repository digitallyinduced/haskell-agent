module Main where

import Agent.Telegram.Connector
import Agent.Telegram.Client (TelegramRequestError(..))
import Agent.Telegram.Connector.Controls
import Agent.Telegram.Connector.Media
import Agent.Telegram.Types.Wire
import qualified Agent.Telegram.Types.State
import qualified Agent.Json.Decode as Json
import Control.Exception (AsyncException(ThreadKilled), throwIO)
import Data.Aeson (Value, object, (.=), encode)
import qualified Data.ByteString.Lazy as ByteString
import Data.IORef
import Data.Text (Text)
import Data.Time
import Test.Hspec
import qualified ServerSpec
import qualified InputSpec

main :: IO ()
main = coreTests

coreTests :: IO ()
coreTests = hspec do
    ServerSpec.spec
    InputSpec.spec
    describe "Single-attempt delivery failures" do
        it "retains definite rate-limit rejections and their retry deadline" do
            classifyDeliveryFailure (TelegramRequestError "limited" (Just 429) (Just 37) True)
                `shouldBe` DeliveryRetryAfter 37
            classifyDeliveryFailure (TelegramRequestError "limited" (Just 429) Nothing True)
                `shouldBe` DeliveryRetryAfter 1
            classifyDeliveryFailure (TelegramRequestError "limited" (Just 429) (Just (-1)) True)
                `shouldBe` DeliveryRetryAfter 1
        it "never retries uncertain transport/server errors or permanent rejections" do
            classifyDeliveryFailure (TelegramRequestError "lost" Nothing (Just 10) True)
                `shouldBe` DeliveryUncertain
            classifyDeliveryFailure (TelegramRequestError "server" (Just 500) (Just 10) True)
                `shouldBe` DeliveryUncertain
            classifyDeliveryFailure (TelegramRequestError "forbidden" (Just 403) Nothing False)
                `shouldBe` DeliveryRejected
    describe "Scoped one-use controls" do
        it "rejects a different binding, actor, message or expired token" do
            let now = UTCTime (fromGregorian 2026 1 1) 0
                scope = ControlScope "owner" 42 "revision" "session" "request" "turn"
                invocation = ControlInvocation "opaque" scope 42 99
                pending = PendingControl scope 99 (addUTCTime 30 now) CancelSession
            validateControl now invocation pending `shouldBe` True
            validateControl now invocation { invocationActor = 43 } pending `shouldBe` False
            validateControl now invocation { invocationMessage = 100 } pending `shouldBe` False
            validateControl now invocation { invocationScope = scope { controlBindingRevision = "old" } } pending `shouldBe` False
            validateControl (addUTCTime 30 now) invocation pending `shouldBe` False
        it "consumes before dispatch and never replays an uncertain cancellation" do
            consumed <- newIORef False
            calls <- newIORef (0 :: Int)
            let now = UTCTime (fromGregorian 2026 1 1) 0
                scope = ControlScope "owner" 42 "revision" "session" "request" "turn"
                invocation = ControlInvocation "opaque" scope 42 99
                pending = PendingControl scope 99 (addUTCTime 30 now) CancelSession
                store = ControlStore \_ valid -> atomicModifyIORef' consumed \used ->
                    if not used && valid pending then (True, Just pending) else (used, Nothing)
                backend = fixtureBackend
                    { cancelExecution = \_ _ -> do
                        readIORef consumed `shouldReturn` True
                        modifyIORef' calls (+ 1)
                        pure (Left SessionUnavailable)
                    }
            handleSessionControl store backend now invocation
                `shouldReturn` ControlBackendFailure SessionUnavailable
            handleSessionControl store backend now invocation `shouldReturn` ControlInvalid
            readIORef calls `shouldReturn` 1
    describe "Session execution protocol" do
        it "submits only before a remote identifier has been persisted" do
            calls <- newIORef ([] :: [Text])
            let backend = fixtureBackend
                    { submitExecution = \_ -> modifyIORef' calls (<> ["submit"]) >> pure (Right running)
                    , inspectExecution = \_ _ -> modifyIORef' calls (<> ["inspect"]) >> pure (Right running)
                    }
            advanceSessionExecution backend execution `shouldReturn` ExecutionRunning "turn"
            advanceSessionExecution backend execution { executionTurnId = Just "turn" }
                `shouldReturn` ExecutionRunning "turn"
            readIORef calls `shouldReturn` ["submit", "inspect"]
        it "rejects unrelated session, request, and persisted turn identifiers" do
            map (validateSessionSnapshot execution)
                [running { snapshotSessionId = "other" }, running { snapshotRequestId = "other" }]
                `shouldSatisfy` all failed
            validateSessionSnapshot execution { executionTurnId = Just "other" } running
                `shouldSatisfy` failed
        it "does not erase a backend outage or invent a completion" do
            advanceSessionExecution fixtureBackend
                { submitExecution = const (pure (Left SessionUnavailable)) } execution
                `shouldReturn` ExecutionRetry
        it "preserves waiting human requests and completed content" do
            let request = HumanRequest "approval" "approval" "Proceed?" ["yes", "no"]
            validateSessionSnapshot execution running { snapshotStatus = SessionWaiting [request] }
                `shouldBe` ExecutionWaiting "turn" [request]
            validateSessionSnapshot execution running { snapshotStatus = SessionCompleted "answer" }
                `shouldBe` ExecutionCompleted "turn" "answer"
        it "rejects invalid output but permits intentional silence" do
            validateSessionSnapshot execution running { snapshotStatus = SessionCompleted "bad\0text" }
                `shouldSatisfy` failed
            validateSessionSnapshot execution running { snapshotStatus = SessionCompleted "" }
                `shouldBe` ExecutionCompleted "turn" ""
    describe "Human request routing" do
        it "responds only to the same still-pending request" do
            calls <- newIORef (0 :: Int)
            let request = HumanRequest "approval" "approval" "Proceed?" ["yes", "no"]
                backend = fixtureBackend
                    { inspectExecution = \_ _ -> pure (Right running { snapshotStatus = SessionWaiting [request] })
                    , respondToRequest = \_ _ _ _ -> modifyIORef' calls (+ 1) >> pure (Right ())
                    }
                submitted = execution { executionTurnId = Just "turn" }
            respondToSessionRequest backend submitted request (object []) `shouldReturn` Right ()
            respondToSessionRequest backend submitted request { humanRequestId = "stale" } (object [])
                `shouldSatisfyIO` isLeft
            readIORef calls `shouldReturn` 1
        it "does not cancel an unrelated or completed turn" do
            calls <- newIORef (0 :: Int)
            let backend = fixtureBackend
                    { inspectExecution = \_ _ -> pure (Right running { snapshotStatus = SessionCompleted "done" })
                    , cancelExecution = \_ _ -> modifyIORef' calls (+ 1) >> pure (Right ())
                    }
            cancelSessionExecution backend execution { executionTurnId = Just "turn" }
                `shouldSatisfyIO` isLeft
            readIORef calls `shouldReturn` 0
    describe "Private conversation admission" do
        it "bounds message identifiers for messages, edits, reactions and callbacks" do
            case classify (messagePayload 17 17 False "private") of
                Just (_, _, ConversationMessage message) -> do
                    let actor = TelegramUser 17 False Nothing Nothing Nothing
                        updates identifier =
                            let changed = message { messageId = identifier }
                                empty = TelegramUpdate 1 Nothing Nothing Nothing Nothing Nothing
                            in [ empty { updateMessage = Just changed }
                               , empty { updateEditedMessage = Just changed }
                               , empty { updateMessageReaction = Just (TelegramMessageReaction message.messageChat identifier (Just actor) [] []) }
                               , empty { updateCallbackQuery = Just (TelegramCallbackQuery "callback" actor (Just changed) (Just "opaque")) }
                               ]
                    mapM_ (\identifier -> mapM_ (\update ->
                        classifyPrivateConversationUpdate update `shouldBe` Nothing)
                        (updates identifier)) [0, -1, 9223372036854775808]
                    mapM_ (\identifier -> mapM_ (\update ->
                        classifyPrivateConversationUpdate update `shouldSatisfy` present)
                        (updates identifier)) [1, 9223372036854775807]
                _ -> expectationFailure "Expected fixture message"
        it "shares message content recognition with the standalone connector" do
            case classify (messagePayload 17 17 False "private") of
                Just (_, _, ConversationMessage message) -> do
                    messageContentText message `shouldBe` Just "hello"
                    messageMediaAttachments message `shouldBe` []
                _ -> expectationFailure "Expected private message"
        it "accepts a private human and rejects spoofed or bot senders" do
            classify (messagePayload 17 17 False "private") `shouldSatisfy` present
            classify (messagePayload 17 18 False "private") `shouldBe` Nothing
            classify (messagePayload 17 17 True "private") `shouldBe` Nothing
            classify (messagePayload 17 17 False "group") `shouldBe` Nothing
        it "keeps edited messages distinct from new submissions" do
            let payload = object ["update_id" .= (1 :: Int), "edited_message" .= messageValue 17 17 False "private"]
            case classify payload of
                Just (_, _, ConversationEdit _) -> pure ()
                _ -> expectationFailure "Edit lost its distinct event kind"
        it "never routes an unlinked reaction into account linking" do
            recorded <- newIORef False
            let payload = object
                    [ "update_id" .= (1 :: Int)
                    , "message_reaction" .= object
                        [ "chat" .= object ["id" .= (17 :: Int), "type" .= ("private" :: Text)]
                        , "user" .= object ["id" .= (17 :: Int), "is_bot" .= False]
                        , "message_id" .= (3 :: Int), "date" .= (1 :: Int)
                        , "old_reaction" .= ([] :: [Value]), "new_reaction" .= ([] :: [Value])
                        ]
                    ]
                store = InboxStore (pure 0) (\_ _ -> pure ()) (\action -> action () payload >> pure True)
                application = ConversationApplication (\_ _ -> pure (Nothing :: Maybe ()))
                    (\_ -> writeIORef recorded True) (\_ _ _ -> writeIORef recorded True)
            processConnectorUpdate store (const application) `shouldReturn` True
            readIORef recorded `shouldReturn` False
        it "constructs publication capabilities from the claimed transaction context" do
            recorded <- newIORef (0 :: Int)
            let payload = object ["update_id" .= (1 :: Int), "message" .= messageValue 17 17 False "private"]
                store = InboxStore (pure 0) (\_ _ -> pure ())
                    (\action -> action (42 :: Int) payload >> pure True)
                application context = ConversationApplication (\_ _ -> pure (Just ()))
                    (\_ -> expectationFailure "linked message")
                    (\_ _ _ -> writeIORef recorded context)
            processConnectorUpdate store application `shouldReturn` True
            readIORef recorded `shouldReturn` 42
    describe "Persisted delivery presentation" do
        it "preserves prepared wire fields without formatting them again" do
            let payload = object ["chat_id" .= (42 :: Int), "text" .= ("*already\\_escaped*" :: Text), "parse_mode" .= ("MarkdownV2" :: Text)]
            prepareDeliveryPayload (PreparedDeliveryAttempt payload) `shouldBe` payload
    describe "Conversation queue" do
        it "advances in order only after each action completes" do
            remaining <- newIORef [1 :: Int, 2]
            recorded <- newIORef []
            let queue = ConversationQueue
                    { nextConversationAction = readIORef remaining >>= \case
                        [] -> pure Nothing
                        value : _ -> pure (Just value)
                    , waitConversationAction = const (pure ())
                    , performConversationAction = \value -> do
                        modifyIORef' recorded (<> [value])
                        modifyIORef' remaining (drop 1)
                    , failConversationAction = \_ _ -> expectationFailure "Unexpected failure" >> pure Nothing
                    }
            runConversationQueue queue
            readIORef recorded `shouldReturn` [1,2]
        it "propagates cancellation instead of recording a retry" do
            let queue = ConversationQueue (pure (Just ())) (const (pure ()))
                    (\_ -> throwIO ThreadKilled)
                    (\_ _ -> expectationFailure "Cancellation swallowed" >> pure Nothing)
            runConversationQueue queue `shouldThrow` (== ThreadKilled)
        it "continues after a failed action is durably dispositioned" do
            remaining <- newIORef [1 :: Int, 2]
            recorded <- newIORef []
            let queue = ConversationQueue
                    { nextConversationAction = readIORef remaining >>= \case
                        [] -> pure Nothing
                        value : _ -> pure (Just value)
                    , waitConversationAction = const (pure ())
                    , performConversationAction = \value ->
                        if value == 1 then ioError (userError "fixture failure")
                        else modifyIORef' recorded (<> [value]) >> modifyIORef' remaining (drop 1)
                    , failConversationAction = \value _ -> do
                        value `shouldBe` 1
                        modifyIORef' remaining (drop 1)
                        pure Nothing
                    }
            runConversationQueue queue
            readIORef recorded `shouldReturn` [2]

execution :: SessionExecution
execution = SessionExecution "request" "session" "prompt" Nothing

running :: SessionSnapshot
running = SessionSnapshot "session" "request" "turn" SessionRunning

fixtureBackend :: SessionBackend
fixtureBackend = SessionBackend (const (pure (Right running)))
    (\_ _ -> pure (Right running)) (\_ _ -> pure (Right ())) (\_ _ _ _ -> pure (Right ()))

failed :: ExecutionTransition -> Bool
failed (ExecutionFailed _) = True
failed _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

shouldSatisfyIO :: Show a => IO a -> (a -> Bool) -> Expectation
shouldSatisfyIO action predicate = action >>= (`shouldSatisfy` predicate)

present :: Maybe a -> Bool
present (Just _) = True
present Nothing = False

classify :: Value -> Maybe (Agent.Telegram.Types.State.TelegramChatKey, Integer, ConversationEvent)
classify payload = case Json.decodeEither telegramUpdateDecoder (ByteString.toStrict (encode payload)) of
    Left _ -> Nothing
    Right update -> classifyPrivateConversationUpdate update

messagePayload :: Int -> Int -> Bool -> Text -> Value
messagePayload chat actor bot kind =
    object ["update_id" .= (1 :: Int), "message" .= messageValue chat actor bot kind]

messageValue :: Int -> Int -> Bool -> Text -> Value
messageValue chat actor bot kind = object
    [ "message_id" .= (1 :: Int)
    , "from" .= object ["id" .= actor, "is_bot" .= bot]
    , "chat" .= object ["id" .= chat, "type" .= kind]
    , "text" .= ("hello" :: Text)
    ]
