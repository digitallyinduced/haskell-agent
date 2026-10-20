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
    it "includes durable guidance arriving after backend creation exactly once" do
        history <- newIORef [Message "original task"]
        inbox <- newIORef []
        seen <- newIORef []
        let save items = do
                incoming <- atomicModifyIORef' inbox (\pending -> ([], pending))
                writeIORef history (items <> incoming)
                pure incoming
        (backend, _, _) <- newDurableTranscriptBackendWithInbox
            ((configuration history) { submit = \_ items -> modifyIORef' seen (<> [items]) >> pure (Right []) }) save
        writeIORef inbox [Message "only the corrected month"]
        _ <- backend.submitTurn Agent.emptyBackendSnapshot (Just "first") [] (const (pure ()))
        _ <- backend.submitTurn Agent.emptyBackendSnapshot (Just "response") [] (const (pure ()))
        readIORef seen `shouldReturn` [[Message "only the corrected month"], []]
        readIORef history `shouldReturn` [Message "original task", Message "only the corrected month"]

    it "retains inbox input received while a response is being persisted for the next continuation" do
        history <- newIORef []
        seen <- newIORef []
        inbox <- newIORef []
        let save items = do
                incoming <- atomicModifyIORef' inbox (\pending -> ([], pending))
                writeIORef history (items <> incoming)
                pure incoming
            config = (configuration history)
                { submit = \_ items -> modifyIORef' seen (<> [items]) >> pure (Right [])
                , recordResponse = \_ -> writeIORef inbox [Message "new guidance"]
                }
        (backend, _, _) <- newDurableTranscriptBackendWithInbox config save
        _ <- backend.submitTurn Agent.emptyBackendSnapshot (Just "first") [] (const (pure ()))
        readIORef inbox `shouldReturn` [Message "new guidance"]
        _ <- backend.submitTurn Agent.emptyBackendSnapshot (Just "response") [] (const (pure ()))
        readIORef seen `shouldReturn` [[], [Message "new guidance"]]

    it "replays guidance and the durable response when the loop still has an older continuation" do
        history <- newIORef []
        seen <- newIORef []
        let config = (configuration history)
                { submit = \previous items -> do
                    modifyIORef' seen (<> [(previous, items)])
                    pure (Right [Message "saved response"])
                }
        (backend, _, appendInput) <- newDurableTranscriptBackendWithInput config
        appendInput [Message "corrected task"]
        _ <- backend.submitTurn Agent.emptyBackendSnapshot (Just "older") [] (const (pure ()))
        _ <- backend.submitTurn Agent.emptyBackendSnapshot (Just "older") [] (const (pure ()))
        readIORef seen `shouldReturn`
            [(Just "older", [Message "corrected task"])
            ,(Nothing, [Message "corrected task", Message "saved response"])]

    it "requires reloading after an ambiguous inbox commit rather than overwriting accepted guidance" do
        history <- newIORef []
        let save items = do
                writeIORef history (items <> [Message "accepted guidance"])
                fail "commit acknowledgement lost"
        (backend, _, _) <- newDurableTranscriptBackendWithInbox (configuration history) save
        backend.submitTurn Agent.emptyBackendSnapshot Nothing [] (const (pure ())) `shouldThrow` anyIOException
        backend.submitTurn Agent.emptyBackendSnapshot Nothing [] (const (pure ())) `shouldThrow` anyIOException
        readIORef history `shouldReturn` [Message "accepted guidance"]
        seen <- newIORef []
        (restored, _) <- newDurableTranscriptBackend
            ((configuration history) { submit = \_ items -> writeIORef seen items >> pure (Right []) })
        _ <- restored.submitTurn Agent.emptyBackendSnapshot Nothing [] (const (pure ()))
        readIORef seen `shouldReturn` [Message "accepted guidance"]

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
        _ <- backend.submitTurn Agent.emptyBackendSnapshot (Just "response") [] (const (pure ()))
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
