module Agent.DurableTranscriptSpec (spec) where

import qualified Agent.Loop as Agent
import Agent.Loop.Native (NativeCodec (..))
import Agent.Subagents.DurableTranscript
import qualified Agent.ToolDispatch as Dispatch
import Data.IORef
import Data.Text (Text)
import Test.Hspec

data Item = Checkpoint Text | Message Text | Output Text Text deriving (Eq, Show)

spec :: Spec
spec = describe "durable native transcript" do
    it "persists additional native input before a pause and replays it after restart" do
        history <- newIORef []
        (_, record, appendInput) <- newDurableTranscriptBackendWithInput (configuration history)
        record (Agent.ToolFinished result)
        appendInput [Message "image attachment"]
        readIORef history `shouldReturn` [Output "call" "suspended", Message "image attachment"]
        seen <- newIORef []
        (backend, _) <- newDurableTranscriptBackend
            ((configuration history) { submit = \_ items -> writeIORef seen items >> pure (Right []) })
        _ <- backend.submitTurn Agent.emptyBackendSnapshot Nothing [] (const (pure ()))
        readIORef seen `shouldReturn` [Output "call" "suspended", Message "image attachment"]

    it "includes added input once on the next live continuation" do
        history <- newIORef []
        seen <- newIORef []
        (backend, _, appendInput) <- newDurableTranscriptBackendWithInput
            ((configuration history) { submit = \_ items -> modifyIORef' seen (<> [items]) >> pure (Right []) })
        appendInput [Message "attachment"]
        _ <- backend.submitTurn Agent.emptyBackendSnapshot (Just "first") [] (const (pure ()))
        _ <- backend.submitTurn Agent.emptyBackendSnapshot (Just "second") [] (const (pure ()))
        readIORef seen `shouldReturn` [[Message "attachment"], []]
        readIORef history `shouldReturn` [Message "attachment"]

    it "fails before acknowledging an unsaved tool result" do
        history <- newIORef []
        (_, record) <- newDurableTranscriptBackend
            ((configuration history) { saveTranscript = \_ -> fail "storage unavailable" })
        record (Agent.ToolFinished result) `shouldThrow` anyIOException

    it "deduplicates event persistence against the next provider input" do
        history <- newIORef []
        seen <- newIORef []
        (backend, record) <- newDurableTranscriptBackend
            ((configuration history) { submit = \_ items -> writeIORef seen items >> pure (Right []) })
        record (Agent.ToolFinished result)
        _ <- backend.submitTurn Agent.emptyBackendSnapshot Nothing
            [Agent.CompletedTool result] (const (pure ()))
        readIORef history `shouldReturn` [Output "call" "suspended"]
        readIORef seen `shouldReturn` [Output "call" "suspended"]

    it "replays the latest checkpoint on a fresh backend" do
        history <- newIORef [Checkpoint "old", Message "discard", Checkpoint "latest"]
        (_, record) <- newDurableTranscriptBackend (configuration history)
        record (Agent.ToolFinished result)
        seen <- newIORef []
        (backend, _) <- newDurableTranscriptBackend
            ((configuration history) { submit = \_ items -> writeIORef seen items >> pure (Right []) })
        _ <- backend.submitTurn Agent.emptyBackendSnapshot Nothing [] (const (pure ()))
        readIORef seen `shouldReturn` [Checkpoint "latest", Output "call" "suspended"]

    it "sends only new input on the same provider continuation" do
        history <- newIORef [Message "past"]
        seen <- newIORef []
        (backend, _) <- newDurableTranscriptBackend
            ((configuration history) { submit = \_ items -> writeIORef seen items >> pure (Right []) })
        _ <- backend.submitTurn Agent.emptyBackendSnapshot (Just "response")
            [Agent.UserMessage "new"] (const (pure ()))
        readIORef seen `shouldReturn` [Message "new"]
        readIORef history `shouldReturn` [Message "past", Message "new"]

configuration :: IORef [Item] -> DurableTranscriptConfig Item [Item]
configuration history = DurableTranscriptConfig
    { codec = NativeCodec
        { responseItems = id
        , responseIdentifier = const "response"
        , responseCompleted = const True
        , responseContinues = const False
        , responseTurnOutput = const (Agent.emptyTurnOutput "response" [] (Just "done"))
        , responseToolItems = const []
        , itemCallIdentifier = const Nothing
        , itemIsAssistantMessage = const False
        , completedToolItem = \output -> Output output.callId output.output
        , finalizedToolItem = \call output -> Output call.callId output
        , auxiliaryInputItems = \inputs -> [Message text | Agent.UserMessage text <- inputs]
        }
    , continuationNamespace = "test"
    , loadTranscript = readIORef history
    , saveTranscript = writeIORef history
    , retainHistory = foldl (\previous item -> case item of
        Checkpoint _ -> [item]
        _ -> previous <> [item]) []
    , prepareReplay = id
    , toolOutputIdentifier = \case Output identifier _ -> Just identifier; _ -> Nothing
    , submit = \_ _ -> pure (Right [])
    , recordResponse = const (pure ())
    }

result :: Dispatch.ToolCallResult
result = Dispatch.ToolCallResult
    { callId = "call"
    , output = "suspended"
    , callKind = Dispatch.FunctionCallKind
    , toolResultMode = Dispatch.BlockingToolCall
    , toolResultImages = []
    , toolResultOutcome = Nothing
    }
