module Agent.Runtime.Tools.StartupSpec (spec) where

import Agent.ResourceScope (allocateAcquire)
import Agent.Runtime.Tools.Resources
import Agent.Runtime.Tools.Startup
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.MVar
    ( MVar, newEmptyMVar, putMVar, readMVar, takeMVar )
import Control.Exception.Safe (finally, throwIO, tryAny)
import Control.Monad (forM_, void)
import Data.Acquire (Acquire, mkAcquire)
import Data.Either (isLeft)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "shared tool startup" do
    it "acquires all domains concurrently and retains their typed results until scope exit" do
        released <- newIORef []
        mcp <- newEmptyMVar
        coding <- newEmptyMVar
        web <- newEmptyMVar
        lsp <- newEmptyMVar
        computer <- newEmptyMVar
        let started = [mcp, coding, web, lsp, computer]
        proceed <- newEmptyMVar
        let acquisition :: MVar () -> Text -> a -> Acquire a
            acquisition gate name value = mkAcquire
                (putMVar gate () >> readMVar proceed >> pure value)
                (const (record released name))
        result <- timeout 2000000 $
            withSessionResourceScopes \scopes -> do
                resources <- acquireToolStartup scopes ToolAcquisitions
                    { acquireMcp = acquisition mcp "mcp" (42 :: Int)
                    , acquireCoding = acquisition coding "coding" ("coding result" :: Text)
                    , acquireWebFetch = acquisition web "web" True
                    , acquireLsp = acquisition lsp "lsp" (Just (7 :: Int))
                    , acquireComputerUse = acquisition computer "computer" [1, 2 :: Int]
                    , preloadContext = do
                        -- A serialized startup deadlocks here: every tool
                        -- must have started before any one can complete.
                        forM_ started takeMVar
                        putMVar proceed ()
                        pure ("context" :: Text)
                    }
                resources.startupMcp `shouldBe` 42
                resources.startupLocalTools `shouldBe` "coding result"
                resources.startupWebFetch `shouldBe` True
                resources.startupLsp `shouldBe` Just 7
                resources.startupComputerUse `shouldBe` [1, 2]
                resources.startupInitialContext `shouldBe` "context"
                readIORef released `shouldReturn` []
        result `shouldBe` Just ()
        readIORef released `shouldReturn` ["computer", "lsp", "web", "mcp", "coding"]

    it "joins a blocked sibling before releasing completed tools after context failure" do
        released <- newIORef []
        codingReady <- newEmptyMVar
        mcpStarted <- newEmptyMVar
        blocked <- newEmptyMVar
        result <- timeout 2000000 $ tryAny $
            withSessionResourceScopes \scopes -> do
                retainScratch scopes released
                void $ acquireToolStartup scopes ToolAcquisitions
                    { acquireMcp = mkAcquire
                        ((putMVar mcpStarted () >> takeMVar blocked :: IO ())
                            `finally` record released "mcp acquisition stopped")
                        (const (record released "mcp"))
                    , acquireCoding = mkAcquire
                        (putMVar codingReady ())
                        (const (record released "coding"))
                    , acquireWebFetch = pure ()
                    , acquireLsp = pure ()
                    , acquireComputerUse = pure ()
                    , preloadContext = do
                        takeMVar codingReady
                        takeMVar mcpStarted
                        throwIO (userError "context failed") :: IO ()
                    }
        result `shouldSatisfy` maybe False isLeft
        readIORef released
            `shouldReturn` ["mcp acquisition stopped", "coding", "scratch"]

    it "joins startup workers and releases completed tools when its owner is cancelled" do
        released <- newIORef []
        codingReady <- newEmptyMVar
        mcpStarted <- newEmptyMVar
        contextStarted <- newEmptyMVar
        blocked <- newEmptyMVar
        let startup = withSessionResourceScopes \scopes -> do
                retainScratch scopes released
                void $ acquireToolStartup scopes ToolAcquisitions
                    { acquireMcp = mkAcquire
                        ((putMVar mcpStarted () >> readMVar blocked :: IO ())
                            `finally` record released "mcp acquisition stopped")
                        (const (record released "mcp"))
                    , acquireCoding = mkAcquire
                        (putMVar codingReady ())
                        (const (record released "coding"))
                    , acquireWebFetch = pure ()
                    , acquireLsp = pure ()
                    , acquireComputerUse = pure ()
                    , preloadContext =
                        (do
                            takeMVar codingReady
                            takeMVar mcpStarted
                            putMVar contextStarted ()
                            readMVar blocked)
                            `finally` record released "context stopped"
                    }
        result <- timeout 2000000 $
            withAsync startup \worker -> do
                takeMVar contextStarted
                cancel worker
        result `shouldBe` Just ()
        events <- readIORef released
        -- Siblings may stop in either order, but both must be joined before
        -- resource teardown starts.
        take 2 events `shouldMatchList` ["mcp acquisition stopped", "context stopped"]
        drop 2 events `shouldBe` ["coding", "scratch"]

retainScratch :: SessionResourceScopes -> IORef [Text] -> IO ()
retainScratch scopes released =
    void $ allocateAcquire scopes.scratchResources $
        mkAcquire (pure ()) (const (record released "scratch"))

record :: IORef [Text] -> Text -> IO ()
record events event =
    atomicModifyIORef' events \previous -> (previous <> [event], ())
