module Main (main) where

import qualified Agent.Runtime.RequestSpec as Request
import qualified Agent.Runtime.Tools.ResourcesSpec as ToolResources
import qualified Agent.Runtime.Tools.DialectsSpec as ToolDialects
import qualified Agent.Runtime.Tools.StartupSpec as ToolStartup
import qualified Agent.Runtime.Tools.LocalStartupSpec as LocalTools
import qualified Agent.Runtime.Tools.WebLspSpec as WebLsp
import qualified Agent.Runtime.Mcp.StartupSpec as McpStartup
import qualified Agent.Runtime.CollaborationSpec as Collaboration
import qualified Agent.Runtime.ProviderRuntimeSpec as ProviderRuntime
import qualified Agent.Runtime.CompactionSpec as Compaction
import qualified Agent.Runtime.StartupPolicySpec as StartupPolicy
import qualified Agent.Runtime.Startup.ContextSpec as StartupContext
import qualified Agent.Runtime.Startup.ModelSpec as StartupModel
import qualified Agent.Runtime.Startup.PolicySpec as StartupApproval
import qualified Agent.Runtime.Startup.GatewaySpec as StartupGateway
import qualified Agent.Runtime.ConversationStoreSpec as ConversationStore
import qualified Agent.Runtime.ConversationSessionSpec as ConversationSession
import qualified Agent.Runtime.TurnEngineSpec as TurnEngine
import qualified Agent.Runtime.TurnExecutionSpec as TurnExecution
import qualified Agent.Runtime.SessionOwnerSpec as SessionOwner
import qualified Agent.Runtime.SessionActivitySpec as SessionActivitySpec
import qualified Agent.Runtime.SessionObservationSpec as SessionObservationSpec
import qualified Agent.Runtime.SessionInboxSpec as SessionInboxSpec
import qualified Agent.Runtime.SessionRequestSpec as SessionRequestSpec
import qualified Agent.Runtime.TurnRecordSpec as TurnRecordSpec
import qualified Agent.Runtime.SessionThreadsSpec as SessionThreadsSpec
import Agent.Runtime.Session.TitlePolicy (titleRefreshIndex)
import Agent.Runtime.Session.PullRequest (advanceSessionPullRequestIndex)
import Data.IORef
import qualified Agent.Runtime.EnvironmentSpec as EnvironmentSpec
import qualified Agent.Runtime.ErrorSpec as ErrorSpec
import qualified Agent.Runtime.GatewayBoundarySpec as GatewayBoundarySpec
import qualified Agent.Runtime.GatewayBridgeSpec as GatewayBridgeSpec
import qualified Agent.Runtime.GatewayClientSpec as GatewayClientSpec
import qualified Agent.Runtime.ManagedTurnSpec as ManagedTurnSpec
import qualified Agent.Runtime.ModelConfigSpec as ModelConfigSpec
import qualified Agent.Runtime.ModelsSpec as ModelsSpec
import qualified Agent.Runtime.NativeProcessSpec as NativeProcessSpec
import qualified Agent.Runtime.SessionSpec as SessionSpec
import qualified Agent.Runtime.Session.ResourcesSpec as SessionResources
import qualified Agent.Runtime.Session.PreparationSpec as SessionPreparation
import qualified Agent.Runtime.TurnStateSpec as TurnStateSpec
import Test.Hspec

main :: IO ()
main = hspec do
    ToolDialects.spec
    ToolResources.spec
    ToolStartup.spec
    LocalTools.spec
    WebLsp.spec
    McpStartup.spec
    Collaboration.spec
    StartupContext.spec
    Request.spec
    ProviderRuntime.spec
    Compaction.spec
    StartupPolicy.spec
    StartupModel.spec
    StartupApproval.spec
    StartupGateway.spec
    ConversationStore.spec
    ConversationSession.spec
    TurnEngine.spec
    TurnExecution.spec
    SessionOwner.spec
    SessionActivitySpec.spec
    SessionObservationSpec.spec
    SessionInboxSpec.spec
    SessionRequestSpec.spec
    TurnRecordSpec.spec
    SessionThreadsSpec.spec
    describe "pull request cache indexing" do
        it "persists a long history once and performs no reads for a current cache" do
            reads <- newIORef (0 :: Int)
            saved <- newIORef Nothing
            let older = "https://github.com/o/repo/pull/1"
                newer = "https://github.com/o/repo/pull/2"
                loadPage start end = do
                    modifyIORef' reads (+ 1)
                    pure (Right [(index, if index == 0 then [newer, older] else [])
                        | index <- [start .. min (end - 1) (start + 31)]])
                savePage cursor urls = writeIORef saved (Just (cursor, urls)) >> pure (Right ())
            advanceSessionPullRequestIndex loadPage savePage 4096 Nothing
                `shouldReturn` Right [newer, older]
            readIORef reads `shouldReturn` 128
            cached <- readIORef saved
            cached `shouldBe` Just (4096, [newer, older])
            advanceSessionPullRequestIndex loadPage savePage 4096 cached
                `shouldReturn` Right [newer, older]
            readIORef reads `shouldReturn` 128
        it "reads only appended turns and prioritizes newly associated pull requests" do
            let older = "https://github.com/o/repo/pull/1"
                newer = "https://github.com/o/repo/pull/2"
                loadPage start end = do
                    (start, end) `shouldBe` (4096, 4097)
                    pure (Right [(4096, [newer])])
                savePage cursor urls = do
                    (cursor, urls) `shouldBe` (4097, [newer, older])
                    pure (Right ())
            advanceSessionPullRequestIndex loadPage savePage 4097 (Just (4096, [older]))
                `shouldReturn` Right [newer, older]
        it "does not advance the cursor when persistence fails" do
            advanceSessionPullRequestIndex
                (\_ _ -> pure (Right [(0, [])]))
                (\_ _ -> pure (Left "write failed"))
                64 Nothing `shouldReturn` Left "write failed"
        it "rejects incomplete pages instead of marking missing history indexed" do
            advanceSessionPullRequestIndex
                (\_ _ -> pure (Right [(1, [])]))
                (\_ _ -> expectationFailure "must not persist incomplete history" >> pure (Right ()))
                2 Nothing `shouldReturn` Left "invalid PR association history page"
    describe "titleRefreshIndex" do
        it "advances only at the persisted title milestones" do
            map titleRefreshIndex [0, 1, 2, 3, 5, 6, 10]
                `shouldBe` [0, 0, 0, 1, 1, 2, 2]
    EnvironmentSpec.spec
    ErrorSpec.spec
    GatewayBoundarySpec.spec
    GatewayBridgeSpec.spec
    GatewayClientSpec.spec
    ManagedTurnSpec.spec
    ModelConfigSpec.spec
    ModelsSpec.spec
    NativeProcessSpec.spec
    SessionSpec.spec
    SessionResources.spec
    SessionPreparation.spec
    TurnStateSpec.spec
