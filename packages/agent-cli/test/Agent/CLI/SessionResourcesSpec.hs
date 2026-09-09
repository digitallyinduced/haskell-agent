module Agent.CLI.SessionResourcesSpec (spec) where

import Agent.CLI.Runtime.Orchestration.Tools.Resources
import Agent.ResourceScope
    ( ResourceScope, allocateAcquire, registerResource, releaseResource )
import Control.Concurrent.Async (Concurrently(..), cancel, race, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception.Safe (finally, throwIO, tryAny)
import Control.Monad (forM_, void)
import Data.Acquire (mkAcquire)
import Data.Either (isLeft)
import Data.IORef
    ( IORef, atomicModifyIORef', newIORef, readIORef, writeIORef )
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "session resource ownership" do
    it "tears down domains in dependency order rather than registration order" do
        released <- newIORef []
        withSessionResourceScopes \scopes -> do
            -- Deliberately register in shutdown order. A single shared scope
            -- would reverse this order and release scratch storage first.
            forM_ (shutdownDomains scopes) \(name, scope) ->
                retain scope released name
        readIORef released `shouldReturn` shutdownNames

    it "preserves dependency teardown for either concurrent completion order" do
        forM_ [False, True] \mcpCompletesFirst -> do
            completed <- newIORef []
            released <- newIORef []
            firstCompleted <- newEmptyMVar
            secondStarted <- newEmptyMVar
            result <- timeout 2000000 $
                withSessionResourceScopes \scopes -> do
                    let (firstName, firstScope, secondName, secondScope)
                            | mcpCompletesFirst =
                                ("mcp", scopes.mcpResources,
                                 "coding", scopes.codingResources)
                            | otherwise =
                                ("coding", scopes.codingResources,
                                 "mcp", scopes.mcpResources)
                    runConcurrently $
                        Concurrently (do
                            -- Both acquisitions have begun before either
                            -- completes; this also detects serialized startup.
                            void $ allocateAcquire firstScope $
                                mkAcquire
                                    (takeMVar secondStarted)
                                    (const (record released firstName))
                            record completed firstName
                            putMVar firstCompleted ())
                        *> Concurrently (do
                            void $ allocateAcquire secondScope $
                                mkAcquire
                                    (putMVar secondStarted ()
                                        >> takeMVar firstCompleted)
                                    (const (record released secondName))
                            record completed secondName)
                    readIORef completed
                        `shouldReturn` [firstName, secondName]
            result `shouldBe` Just ()
            readIORef released `shouldReturn` ["mcp", "coding"]

    it "joins cancelled applicative acquisitions before releasing owned resources" do
        released <- newIORef []
        codingAcquired <- newEmptyMVar
        mcpStarted <- newEmptyMVar
        blocked <- newEmptyMVar
        result <- timeout 2000000 $ tryAny $
            withSessionResourceScopes \scopes ->
                runConcurrently $
                    (,,)
                        <$> Concurrently (do
                            retain scopes.codingResources released "coding"
                            putMVar codingAcquired ())
                        <*> Concurrently
                            (void $ allocateAcquire scopes.mcpResources $
                                mkAcquire
                                    ((putMVar mcpStarted () >> takeMVar blocked :: IO ())
                                        `finally` record released "acquisition stopped")
                                    (const (record released "mcp")))
                        <*> Concurrently (do
                            takeMVar codingAcquired
                            takeMVar mcpStarted
                            throwIO (userError "initial context failed") :: IO ())
        result `shouldSatisfy` maybe False isLeft
        readIORef released `shouldReturn` ["acquisition stopped", "coding"]

    it "releases completed startup resources when another acquisition is cancelled" do
        released <- newIORef []
        acquisitionStarted <- newEmptyMVar
        blocked <- newEmptyMVar
        result <- timeout 2000000 $
            race
                (withSessionResourceScopes \scopes -> do
                    retain scopes.scratchResources released "scratch"
                    retain scopes.codingResources released "coding"
                    void $ allocateAcquire scopes.mcpResources $
                        mkAcquire
                            (putMVar acquisitionStarted ()
                                >> takeMVar blocked :: IO ())
                            (const (record released "mcp")))
                (takeMVar acquisitionStarted)
        result `shouldBe` Just (Right ())
        readIORef released `shouldReturn` ["coding", "scratch"]

    it "continues other domain finalizers before reporting a cleanup exception" do
        released <- newIORef []
        result <- tryAny $
            withSessionResourceScopes \scopes -> do
                retain scopes.scratchResources released "scratch"
                retain scopes.codingResources released "coding"
                void $ allocateAcquire scopes.mcpResources $
                    mkAcquire (pure ()) \() -> do
                        record released "mcp"
                        throwIO (userError "MCP cleanup failed")
                retain scopes.activityResources released "activities"
        readIORef released
            `shouldReturn` ["activities", "mcp", "coding", "scratch"]
        result `shouldSatisfy` isLeft

    it "does not repeat an early domain-resource release at session exit" do
        released <- newIORef []
        withSessionResourceScopes \scopes -> do
            (key, _) <- allocateAcquire scopes.codeModeResources $
                mkAcquire (pure ()) (const (record released "code mode"))
            releaseResource key
            releaseResource key
        readIORef released `shouldReturn` ["code mode"]

    it "joins an owned activity before closing its tool resources" do
        activityStarted <- newEmptyMVar
        blocked <- newEmptyMVar
        activityFinished <- newIORef False
        observedAtToolClose <- newIORef False
        released <- newIORef []
        let activity =
                (putMVar activityStarted () >> takeMVar blocked :: IO ())
                    `finally` do
                        record released "activity"
                        writeIORef activityFinished True
        result <- timeout 2000000 $
            withAsync activity \worker ->
                withSessionResourceScopes \scopes -> do
                    takeMVar activityStarted
                    void $ allocateAcquire scopes.codingResources $
                        mkAcquire (pure ()) \() -> do
                            readIORef activityFinished
                                >>= writeIORef observedAtToolClose
                            record released "coding"
                    -- The worker is lexically protected by withAsync even
                    -- if setup fails; session closure is its normal owner.
                    void $ registerResource scopes.activityResources
                        (cancel worker)
        result `shouldBe` Just ()
        readIORef observedAtToolClose `shouldReturn` True
        readIORef released `shouldReturn` ["activity", "coding"]

shutdownDomains :: SessionResourceScopes -> [(String, ResourceScope)]
shutdownDomains scopes =
    [ ("activities", scopes.activityResources)
    , ("code mode", scopes.codeModeResources)
    , ("session lock", scopes.sessionLockResources)
    , ("computer use", scopes.computerUseResources)
    , ("LSP", scopes.lspResources)
    , ("web fetch", scopes.webFetchResources)
    , ("mcp", scopes.mcpResources)
    , ("coding", scopes.codingResources)
    , ("scratch", scopes.scratchResources)
    ]

shutdownNames :: [String]
shutdownNames =
    [ "activities", "code mode", "session lock", "computer use", "LSP"
    , "web fetch", "mcp", "coding", "scratch"
    ]

retain :: ResourceScope -> IORef [String] -> String -> IO ()
retain scope released name =
    void $ allocateAcquire scope $
        mkAcquire (pure ()) (const (record released name))

record :: IORef [a] -> a -> IO ()
record values value =
    atomicModifyIORef' values \previous -> (previous <> [value], ())
