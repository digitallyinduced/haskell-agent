module Agent.Loop.NativeSpec (spec) where

import qualified Agent.Loop as Agent
import Agent.Loop.Native
import Agent.Loop.OutputDelivery (processOrderedOutput)
import qualified Agent.Subagents.Native as Child
import qualified Agent.ToolDispatch as Dispatch
import qualified Agent.Tools.Types as Tools
import Control.Exception.Safe (Exception, throwIO)
import Control.Monad (void, forM_)
import Data.IORef
import Data.Text (Text)
import Test.Hspec
import System.Timeout (timeout)

data HostFailure = HostFailure deriving (Eq, Show)
instance Exception HostFailure

-- Deliberately not a Responses item: hosts need not adopt a provider schema.
data Item = Opaque Text | Message Text | Call Text deriving (Eq, Show)
data Response = Response [Item] Bool deriving (Eq, Show)

spec :: Spec
spec = describe "native host loop" do
    it "submits opaque initial and steering items without conversion" do
        seen <- newIORef []
        inbox <- newIORef [Opaque "encrypted-input"]
        config <- fixtureConfig
        let configured = config
                { submit = \_ _ items -> do
                    modifyIORef' seen (<> [items])
                    pure (Response [Message "done"] False)
                , readSteering = readIORef inbox
                , acknowledgeSteering = \count -> modifyIORef' inbox (drop count)
                }
        outcome <- runNativeLoop configured Nothing [Opaque "file-reference"]
        case outcome of
            NativeLoopFinished _ -> pure ()
            _ -> expectationFailure "expected completed response"
        submitted <- readIORef seen
        concat submitted `shouldBe` [Opaque "file-reference", Opaque "encrypted-input"]
        readIORef inbox `shouldReturn` []

    it "continues an application turn after a completed transport response" do
        seen <- newIORef []
        config <- fixtureConfig
        let configured = config
                { submit = \iteration _ items -> do
                    modifyIORef' seen (<> [items])
                    pure (Response [Message "answer"] (iteration == 0))
                }
        _ <- runNativeLoop configured Nothing []
        readIORef seen `shouldReturn` [[], []]

    it "preserves the original typed persistence failure" do
        config <- fixtureConfig
        runNativeLoop (config { commitResponse = \_ -> throwIO HostFailure }) Nothing []
            `shouldThrow` (== HostFailure)

    it "returns unacknowledged native steering at the turn limit" do
        inbox <- newIORef []
        config <- fixtureConfig
        let configured = config
                { maxTurns = 1
                , submit = \_ _ _ -> do
                    writeIORef inbox [Opaque "pending-file"]
                    pure (Response [Message "answer"] False)
                , readSteering = readIORef inbox
                , acknowledgeSteering = \count -> modifyIORef' inbox (drop count)
                }
        outcome <- runNativeLoop configured Nothing []
        case outcome of
            NativeLoopReachedLimit limit -> do
                limit.pendingItems `shouldBe` [Opaque "pending-file"]
                readIORef inbox `shouldReturn` [Opaque "pending-file"]
                limit.acknowledgePendingInput
                readIORef inbox `shouldReturn` []
            _ -> expectationFailure "expected pending input at turn limit"

    forM_ [False, True] \beforeAnswer ->
      it ("acknowledges pending child inputs at the turn limit (before answer: " <> show beforeAnswer <> ")") do
        inbox <- newIORef []
        children <- newIORef []
        acknowledgements <- newIORef []
        submitted <- newIORef []
        config <- fixtureConfig
        let childHooks = Agent.LoopSubagents
                { subagentsBeginTurn = pure ()
                , subagentsReadInputs = if beforeAnswer then pure [] else readIORef children
                , subagentsCommitInputs = \count -> do
                    modifyIORef' acknowledgements (<> [count])
                    modifyIORef' children (drop count)
                , subagentsBeforeAnswer = readIORef children
                , subagentsEndTurn = const (pure ())
                }
            configured = config
                { maxTurns = 1
                , submit = \_ _ _ -> do
                    writeIORef inbox (if beforeAnswer then [] else [Opaque "host-input"])
                    writeIORef children [Agent.UserMessage "child-message", Agent.UserMessage "child-complete"]
                    pure (Response [Message "answer"] False)
                , readSteering = readIORef inbox
                , acknowledgeSteering = \count -> modifyIORef' inbox (drop count)
                , subagents = Just childHooks
                }
        outcome <- runNativeLoop configured Nothing []
        case outcome of
            NativeLoopReachedLimit limit -> do
                limit.pendingItems `shouldBe`
                    (if beforeAnswer then [] else [Opaque "host-input"])
                        <> [Message "child-message", Message "child-complete"]
                readIORef children `shouldReturn`
                    [Agent.UserMessage "child-message", Agent.UserMessage "child-complete"]
                limit.acknowledgePendingInput
                readIORef inbox `shouldReturn` []
                readIORef children `shouldReturn` []
                counts <- readIORef acknowledgements
                last counts `shouldBe` 2
            _ -> expectationFailure "expected child input at turn limit"
        _ <- runNativeLoop (configured
            { submit = \_ _ items -> do
                modifyIORef' submitted (<> [items])
                pure (Response [Message "done"] False)
            }) (Just "response") []
        readIORef submitted `shouldReturn` [[]]

    it "delivers text before the following call batch and after the preceding one" do
        events <- newIORef []
        let record text = modifyIORef' events (<> [text])
        result <- processOrderedOutput messageText isCall
            (\_ text -> record text)
            (const (pure ()))
            (\items -> record "execute" >> pure items)
            [Message "before", Call "a", Call "b", Message "after"]
        result `shouldBe` [Call "a", Call "b"]
        readIORef events `shouldReturn` ["before", "execute", "after"]

    it "retains separate native histories for each child" do
        conversations <- Child.newNativeConversations
        first <- Child.nativeConversation conversations ("first" :: Text)
        second <- Child.nativeConversation conversations "second"
        writeIORef first [Opaque "private"]
        again <- Child.nativeConversation conversations "first"
        readIORef again `shouldReturn` [Opaque "private"]
        readIORef second `shouldReturn` []

    it "releases delivery barriers after denied approval without running the handler" do
        events <- newIORef []
        executed <- newIORef False
        config <- fixtureConfig
        let record text = modifyIORef' events (<> [text])
            guarded = Tools.jsonAppTool "guarded" "" [] Tools.AlwaysReadOnly
                (Dispatch.noArgsTool "guarded" (writeIORef executed True >> pure (Right "unexpected")))
        registry <- either (fail . show) pure (Tools.mkToolRegistry [guarded])
        let configured = config
                { tools = registry
                , codec = fixtureCodec
                    { responseTurnOutput = \(Response items _) ->
                        Agent.emptyTurnOutput "response"
                            [Dispatch.functionToolCall identifier "guarded" "{}" | Call identifier <- items]
                            Nothing
                    }
                , approveTool = \_ -> pure (Agent.ToolApprovalDenied "denied by host")
                , submit = \iteration _ _ -> pure $ Response
                    (if iteration == 0 then [Message "before", Call "guarded-call", Message "after"] else [])
                    False
                , persistToolOutput = \_ -> record "persist-denial"
                , deliverOutput = \awaitTool items -> void $ processOrderedOutput messageText isCall
                    (\_ text -> record text)
                    (const (pure ()))
                    (\calls -> do
                        forM_ calls \case Call identifier -> awaitTool identifier; _ -> pure ()
                        pure [])
                    items
                }
        result <- timeout 1000000 (runNativeLoop configured Nothing [])
        case result of
            Just NativeLoopFinished{} -> pure ()
            _ -> expectationFailure "denied call left delivery blocked"
        readIORef executed `shouldReturn` False
        readIORef events `shouldReturn` ["before", "persist-denial", "after"]

    it "replays child history only without a continuation and appends only new items" do
        history <- newIORef [Opaque "past"]
        seen <- newIORef []
        let backend = Child.nativeTranscriptBackend Child.NativeBackendConfig
                { codec = fixtureCodec
                , continuationNamespace = "test"
                , prepareReplay = id
                , submit = \_ items -> do
                    modifyIORef' seen (<> [items])
                    pure (Right (Response [Message "answer"] False))
                , recordResponse = const (pure ())
                } history
        _ <- backend.submitTurn Agent.emptyBackendSnapshot Nothing
            [Agent.UserMessage "first"] (const (pure ()))
        _ <- backend.submitTurn Agent.emptyBackendSnapshot (Just "response")
            [Agent.UserMessage "second"] (const (pure ()))
        readIORef seen `shouldReturn`
            [[Opaque "past", Message "first"], [Message "second"]]
        readIORef history `shouldReturn`
            [Opaque "past", Message "first", Message "answer", Message "second", Message "answer"]

fixtureConfig :: IO (NativeLoopConfig Item Response)
fixtureConfig = do
    registry <- either (fail . show) pure (Tools.mkToolRegistry [])
    pure NativeLoopConfig
        { maxTurns = 5
        , continuationNamespace = "test"
        , codec = fixtureCodec
        , tools = registry
        , approveTool = \_ -> fail "no tool may be approved in this fixture"
        , submit = \_ _ _ -> pure (Response [Message "done"] False)
        , commitResponse = const (pure ())
        , deliverOutput = \_ _ -> pure ()
        , persistToolOutput = const (pure ())
        , readSteering = pure []
        , acknowledgeSteering = const (pure ())
        , awaitNewSteering = noLiveSteering
        , readAdditionalInput = const (pure [])
        , subagents = Nothing
        }

fixtureCodec :: NativeCodec Item Response
fixtureCodec = NativeCodec
    { responseItems = \(Response items _) -> items
    , responseIdentifier = const "response"
    , responseCompleted = const True
    , responseContinues = \(Response _ continues) -> continues
    , responseTurnOutput = const (Agent.emptyTurnOutput "response" [] (Just "done"))
    , responseToolItems = const []
    , itemCallIdentifier = \case Call identifier -> Just identifier; _ -> Nothing
    , itemIsAssistantMessage = \case Message _ -> True; _ -> False
    , completedToolItem = \result -> Opaque (result :: Dispatch.ToolCallResult).output
    , finalizedToolItem = \_ output -> Opaque output
    , auxiliaryInputItems = \inputs -> [Message text | Agent.UserMessage text <- inputs, text /= ""]
    }

messageText :: Item -> Maybe Text
messageText (Message text) = Just text
messageText _ = Nothing

isCall :: Item -> Bool
isCall Call{} = True
isCall _ = False
