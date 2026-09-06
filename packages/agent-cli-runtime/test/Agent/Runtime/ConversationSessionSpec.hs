module Agent.Runtime.ConversationSessionSpec (spec) where

import Agent.Cancel (newCancelFlag)
import Agent.Error (ApiError(..))
import Agent.Loop
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Responses.Types (ResponseItem)
import Agent.Runtime.Compaction (AutomaticCompactionBoundary(..))
import Agent.Runtime.ConversationStore
import Agent.Runtime.SessionState qualified as Session
import Agent.Runtime.TurnEngine qualified as Engine
import Agent.Runtime.TurnExecution qualified as Execution
import Agent.Runtime.TurnState
import Agent.Tools.Types (mkToolRegistry)
import Data.IORef
import Data.Text qualified as Text
import Test.Hspec

spec :: Spec
spec = describe "runtime conversation state" do
    it "allocates independent state without a frontend" do
        first <- Session.newSessionState
        second <- Session.newSessionState
        Session.commitConversationPatch first keepPatch
            { patchTranscript = SetField history
            , patchLastAssistant = SetField (Just "answer")
            }
        readTranscript first `shouldReturn` history
        readTranscript second `shouldReturn` []
        readIORef second.stateLastAssistant `shouldReturn` Nothing
        readIORef second.stateUsage `shouldReturn` emptyTokenUsage

    it "borrows durable references while resetting only per-host context" do
        original <- Session.newSessionState
        Session.commitConversationPatch original keepPatch
            { patchTranscript = SetField history
            , patchUsageDelta = TokenUsage 10 4 1
            , patchLastAssistant = SetField (Just "old answer")
            }
        writeIORef original.stateStartupContext (Just "pending")
        rebuilt <- Session.newSessionStateWith
            original.stateConversation original.stateStartupContext
            original.stateUsage original.stateAutomaticCompaction
            (Just "new framing")
        readTranscript rebuilt `shouldReturn` history
        readIORef rebuilt.stateUsage `shouldReturn` TokenUsage 10 4 1
        Session.takeStartupContext rebuilt `shouldReturn` Just "pending"
        readIORef original.stateStartupContext `shouldReturn` Nothing
        readIORef rebuilt.stateLastAssistant `shouldReturn` Nothing
        readIORef original.stateLastAssistant `shouldReturn` Just "old answer"
        readIORef rebuilt.stateGrokFirstTurnContext `shouldReturn` Just "new framing"

    it "preserves transcript-write continuation invalidation and adds newer usage" do
        state <- Session.newSessionState
        store <- readIORef state.stateConversation
        writeConversationPreviousResponseId store (Just "old")
        writeIORef state.stateUsage (TokenUsage 20 8 2)
        Session.commitConversationPatch state keepPatch
            { patchPreviousResponseId = SetField (Just "new")
            , patchTranscript = SetField history
            , patchUsageDelta = TokenUsage 10 4 1
            , patchLastAssistant = SetField (Just "answer")
            }
        readConversationPreviousResponseId store `shouldReturn` Nothing
        readTranscript state `shouldReturn` history
        readIORef state.stateUsage `shouldReturn` TokenUsage 30 12 3
        readIORef state.stateLastAssistant `shouldReturn` Just "answer"

    it "consumes startup once and restores without losing refreshed context" do
        state <- Session.newSessionState
        writeIORef state.stateStartupContext (Just "original startup")
        Session.takeStartupContext state `shouldReturn` Just "original startup"
        Session.takeStartupContext state `shouldReturn` Nothing
        writeIORef state.stateStartupContext (Just "refreshed skills")
        writeIORef state.stateGrokFirstTurnContext (Just "new framing")
        Session.restoreConsumedPromptContext state
            (Just "original startup") (Just "old framing")
        readIORef state.stateStartupContext `shouldReturn`
            restoreStartupContext "original startup" (Just "refreshed skills")
        readIORef state.stateGrokFirstTurnContext `shouldReturn` Just "new framing"

    it "restores missing context through the shared patch operation" do
        state <- Session.newSessionState
        Session.commitConversationPatch state keepPatch
            { patchStartupContext = RestoreStartup "startup"
            , patchGrokFirstTurnContext = RestoreGrokContext "framing"
            }
        readIORef state.stateStartupContext `shouldReturn` Just "startup"
        readIORef state.stateGrokFirstTurnContext `shouldReturn` Just "framing"

    it "commits failed-loop model inputs without committing partial display text" do
        state <- Session.newSessionState
        Session.commitConversationPatch state keepPatch
            { patchTranscript = SetField history }
        config <- configFor state (Backend \_ _ _ emit -> do
            emit (TextDelta "partial answer")
            pure (Left (ConnectionError "offline")))
        executed <- execute state config
        let final = Engine.finalizeTurn
                (Engine.TurnPolicy (const False) Text.strip)
                Nothing Nothing prepared executed.executedLoop
        Session.commitConversationPatch state final.finalizedPatch
        Engine.displayItems final.finalizedDisplayItems
            `shouldSatisfy` (not . null)
        readTranscript state `shouldReturn` (history <> inputOnlyTurnItems prepared)
        readIORef state.stateLastAssistant `shouldReturn` Nothing

    it "applies exceptional rollback against the installed compaction boundary" do
        state <- Session.newSessionState
        let summary = turnInputsToItems [UserMessage "summary"]
            boundary = AutomaticCompactionBoundary summary [UserMessage "continue"]
        base <- configFor state (Backend \_ _ _ _ ->
            pure (Left (ConnectionError "offline")))
        let config = base
                { loopBackendState = base.loopBackendState
                    { readBackendState = do
                        Session.commitConversationPatch state keepPatch
                            { patchTranscript = SetField summary }
                        writeIORef state.stateAutomaticCompaction (Just boundary)
                        ioError (userError "interrupted")
                    }
                }
        execute state config `shouldThrow` anyIOException
        readTranscript state `shouldReturn` summary
        readIORef state.stateStartupContext `shouldReturn` Nothing
        readIORef state.stateGrokFirstTurnContext `shouldReturn` Nothing

keepPatch :: ConversationPatch
keepPatch = ConversationPatch
    { patchPreviousResponseId = KeepField
    , patchTranscript = KeepField
    , patchStartupContext = KeepStartup
    , patchGrokFirstTurnContext = KeepGrokContext
    , patchUsageDelta = emptyTokenUsage
    , patchLastAssistant = KeepField
    }

history :: [ResponseItem]
history = turnInputsToItems [UserMessage "earlier"]

prepared :: PreparedTurn
prepared = PreparedTurn history (Just "startup") (Just "framing")
    [UserMessage "fix it"]

readTranscript :: Session.SessionState -> IO [ResponseItem]
readTranscript state = readIORef state.stateConversation >>= \store ->
    withConversationTranscript store pure

execute :: Session.SessionState -> LoopConfig -> IO Execution.ExecutedTurn
execute state config = Execution.executePreparedTurn
    (Execution.PreparedExecution config Nothing prepared)
    (readIORef state.stateAutomaticCompaction)
    (Session.commitConversationPatch state . (.exceptionalPatch))

configFor :: Session.SessionState -> Backend -> IO LoopConfig
configFor state backend = do
    cancel <- newCancelFlag
    tools <- either (fail . Text.unpack) pure (mkToolRegistry [])
    let readStore = readIORef state.stateConversation
    pure LoopConfig
        { loopBackend = backend
        , loopBackendState = BackendStateStore
            { readBackendState = readStore >>= \store ->
                withConversationBackendState store pure
            , commitBackendState = \snapshot -> readStore >>= \store ->
                commitConversationBackendState store snapshot
            }
        , loopTools = tools
        , loopDispatch = defaultLoopDispatch
        , loopMaxTurns = 2
        , loopOnEvent = const (pure ())
        , loopApprove = const (pure (Right False))
        , loopReadSteering = pure []
        , loopCommitSteering = const (pure ())
        , loopInterrupt = pure ()
        , loopCancel = cancel
        }
