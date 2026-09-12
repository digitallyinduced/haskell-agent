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
import Control.Concurrent.Async (concurrently_)
import Control.Monad (replicateM_)
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
        Session.readLastAssistant second `shouldReturn` Nothing
        Session.readSessionUsage second `shouldReturn` emptyTokenUsage

    it "borrows durable references while resetting only per-host context" do
        original <- Session.newSessionState
        Session.commitConversationPatch original keepPatch
            { patchTranscript = SetField history
            , patchUsageDelta = TokenUsage 10 4 1
            , patchLastAssistant = SetField (Just "old answer")
            }
        Session.restoreConsumedPromptContext original (Just "pending") Nothing
        rebuilt <- Session.restartSessionState original (Just "new framing")
        readTranscript rebuilt `shouldReturn` history
        Session.readSessionUsage rebuilt `shouldReturn` TokenUsage 10 4 1
        Session.takeStartupContext rebuilt `shouldReturn` Just "pending"
        Session.takeStartupContext original `shouldReturn` Nothing
        Session.readLastAssistant rebuilt `shouldReturn` Nothing
        Session.readLastAssistant original `shouldReturn` Just "old answer"
        Session.takeGrokContext rebuilt `shouldReturn` Just "new framing"

    it "preserves transcript-write continuation invalidation and adds newer usage" do
        state <- Session.newSessionState
        store <- readIORef (Session.borrowConversationRef state)
        writeConversationPreviousResponseId store (Just "old")
        Session.addSessionUsage state (TokenUsage 20 8 2)
        Session.commitConversationPatch state keepPatch
            { patchPreviousResponseId = SetField (Just "new")
            , patchTranscript = SetField history
            , patchUsageDelta = TokenUsage 10 4 1
            , patchLastAssistant = SetField (Just "answer")
            }
        readConversationPreviousResponseId store `shouldReturn` Nothing
        readTranscript state `shouldReturn` history
        Session.readSessionUsage state `shouldReturn` TokenUsage 30 12 3
        Session.readLastAssistant state `shouldReturn` Just "answer"

    it "consumes startup once and restores without losing refreshed context" do
        conversation <- newIORef =<< newConversationStore Nothing [] []
        startup <- newIORef (Just "original startup")
        usage <- newIORef emptyTokenUsage
        compaction <- newIORef Nothing
        state <- Session.newSessionStateWith
            conversation startup usage compaction Nothing
        Session.takeStartupContext state `shouldReturn` Just "original startup"
        Session.takeStartupContext state `shouldReturn` Nothing
        -- The host retains ownership of generated startup context while the
        -- runtime owns how consumed context is restored.
        writeIORef startup (Just "refreshed skills")
        Session.restoreConsumedPromptContext state Nothing (Just "new framing")
        Session.restoreConsumedPromptContext state
            (Just "original startup") (Just "old framing")
        Session.takeStartupContext state `shouldReturn`
            restoreStartupContext "original startup" (Just "refreshed skills")
        Session.takeGrokContext state `shouldReturn` Just "new framing"

    it "restores missing context through the shared patch operation" do
        state <- Session.newSessionState
        Session.commitConversationPatch state keepPatch
            { patchStartupContext = RestoreStartup "startup"
            , patchGrokFirstTurnContext = RestoreGrokContext "framing"
            }
        Session.takeStartupContext state `shouldReturn` Just "startup"
        Session.takeGrokContext state `shouldReturn` Just "framing"

    it "adds concurrent usage deltas without exposing the usage reference" do
        state <- Session.newSessionState
        let add = replicateM_ 1000 (Session.addSessionUsage state (TokenUsage 1 2 3))
        concurrently_ add add
        Session.readSessionUsage state `shouldReturn` TokenUsage 2000 4000 6000

    it "retains compaction across a host rebuild and clears it explicitly" do
        original <- Session.newSessionState
        let boundary = AutomaticCompactionBoundary history [UserMessage "continue"]
        Session.installAutomaticCompaction original boundary
        rebuilt <- Session.restartSessionState original Nothing
        Session.readAutomaticCompaction rebuilt `shouldReturn` Just boundary
        Session.clearAutomaticCompaction rebuilt
        Session.readAutomaticCompaction original `shouldReturn` Nothing

    it "clears per-run answer and framing without resetting the transcript" do
        state <- Session.newSessionState
        Session.commitConversationPatch state keepPatch
            { patchTranscript = SetField history
            , patchLastAssistant = SetField (Just "answer")
            , patchGrokFirstTurnContext = RestoreGrokContext "framing"
            }
        Session.clearLastAssistant state
        Session.clearGrokContext state
        Session.readLastAssistant state `shouldReturn` Nothing
        Session.takeGrokContext state `shouldReturn` Nothing
        readTranscript state `shouldReturn` history

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
        Session.readLastAssistant state `shouldReturn` Nothing

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
                        Session.installAutomaticCompaction state boundary
                        ioError (userError "interrupted")
                    }
                }
        execute state config `shouldThrow` anyIOException
        readTranscript state `shouldReturn` summary
        Session.takeStartupContext state `shouldReturn` Nothing
        Session.takeGrokContext state `shouldReturn` Nothing

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
readTranscript = Session.readSessionTranscript

execute :: Session.SessionState -> LoopConfig -> IO Execution.ExecutedTurn
execute state config = Execution.executePreparedTurn
    (Execution.PreparedExecution config Nothing prepared)
    (Session.readAutomaticCompaction state)
    (Session.commitConversationPatch state . (.exceptionalPatch))

configFor :: Session.SessionState -> Backend -> IO LoopConfig
configFor state backend = do
    cancel <- newCancelFlag
    tools <- either (fail . Text.unpack) pure (mkToolRegistry [])
    let readStore = readIORef (Session.borrowConversationRef state)
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
        , loopApprove = const (pure ToolApprovalRejected)
        , loopReadSteering = pure []
        , loopCommitSteering = const (pure ())
        , loopInterrupt = pure ()
        , loopCancel = cancel
        }
