module Main (main) where

import qualified Agent.CLI.SessionActivitySpec as SessionActivitySpec
import qualified Agent.CLI.SessionObservationSpec as SessionObservationSpec
import qualified Agent.CLI.SessionRequestSpec as SessionRequestSpec
import qualified Agent.CLI.SessionThreadsSpec as SessionThreadsSpec
import Agent.CLI.Session.TitlePolicy (titleRefreshIndex)
import Agent.CLI.Session.PullRequest (advanceSessionPullRequestIndex)
import Data.IORef
import qualified Agent.CLI.CredentialStoreSpec as CredentialStoreSpec
import qualified Agent.CLI.EnvironmentSpec as EnvironmentSpec
import qualified Agent.CLI.ErrorSpec as ErrorSpec
import qualified Agent.CLI.GatewayBoundarySpec as GatewayBoundarySpec
import qualified Agent.CLI.GatewayBridgeSpec as GatewayBridgeSpec
import qualified Agent.CLI.GatewayClientSpec as GatewayClientSpec
import qualified Agent.CLI.ManagedTurnSpec as ManagedTurnSpec
import qualified Agent.CLI.ModelConfigSpec as ModelConfigSpec
import qualified Agent.CLI.ModelsSpec as ModelsSpec
import qualified Agent.CLI.NativeProcessSpec as NativeProcessSpec
import qualified Agent.CLI.SessionSpec as SessionSpec
import qualified Agent.Runtime.RequestSpec as RuntimeRequestSpec
import qualified Agent.Runtime.StartupPolicySpec as StartupPolicySpec
import qualified Agent.Runtime.ConversationStoreSpec as ConversationStoreSpec
import qualified Agent.Runtime.ConversationSessionSpec as ConversationSessionSpec
import qualified Agent.Runtime.TurnEngineSpec as TurnEngineSpec
import qualified Agent.Runtime.TurnExecutionSpec as TurnExecutionSpec
import qualified Agent.Runtime.TurnStateSpec as TurnStateSpec
import Test.Hspec

main :: IO ()
main = hspec do
    SessionActivitySpec.spec
    SessionObservationSpec.spec
    SessionRequestSpec.spec
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
    CredentialStoreSpec.spec
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
    RuntimeRequestSpec.spec
    StartupPolicySpec.spec
    ConversationStoreSpec.spec
    ConversationSessionSpec.spec
    TurnEngineSpec.spec
    TurnExecutionSpec.spec
    TurnStateSpec.spec
