module Agent.CLI.MacOS.BridgeSpec (spec) where

import Agent.CLI.MacOS.Bridge
    ( TurnStart(..)
    , emitBoundaryChecked
    , invokeGatewayCallbackOnce
    , nativeExceptionMessage
    , nativeRequestRequiresGatewayLock
    , nativeSessionRouteMatchesBoundary
    , nativeTurnRouteMatchesBoundary
    , nativeTurnArguments
    )
import Agent.CLI.MacOS.NativeSupervisor
    ( launchIntegrationWorkerWith
    , launchLocalIntegrationWorkerWith
    , completeBoundaryChecked
    , newIntegrationWorkerRegistry
    , shutdownIntegrationWorkers
    , shutdownOrganizationIntegrationWorkers
    )
import Agent.Runtime.GatewayClient
    ( GatewayCredential(..)
    , registerGatewayCredentialInvalidatorAt
    , saveGatewayCredentialAt
    )
import Agent.CLI.NativeRuntime (StartupFailure(..), exitFailedTurn)
import Agent.CLI.MacOS.TurnEvents (turnFailedEvent)
import Control.Concurrent
    ( newEmptyMVar
    , newMVar
    , putMVar
    , takeMVar
    , threadDelay
    , tryReadMVar
    , withMVar
    )
import Control.Concurrent.Async (poll, wait, withAsync)
import Control.Exception.Safe
    ( bracket
    , bracket_
    , displayException
    , throwString
    , toException
    , tryAny
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.IORef
    ( atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef )
import Data.Maybe (isNothing)
import Data.Either (isLeft)
import System.Directory
    ( createDirectory, getTemporaryDirectory, removeFile, removePathForcibly )
import System.IO (hClose, openTempFile)
import System.OsPath (unsafeEncodeUtf)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
    boundaryCheckedEmissionSpec
    gatewayTransitionCallbackSpec
    computerUseSpec
    nativeRequestBoundarySpec
    nativeSessionBoundarySpec
    nativeTurnBoundarySpec
    nativeFailureSpec
    integrationWorkerSpec

integrationWorkerSpec :: Spec
integrationWorkerSpec =
    describe "native integration worker supervision" do
        it "preserves local OAuth work through a real credential invalidation and joins it on shutdown" do
            temporaryRoot <- getTemporaryDirectory
            let acquireHome = do
                    (path, handle) <- openTempFile temporaryRoot "native-mail-workers"
                    hClose handle
                    removeFile path
                    createDirectory path
                    pure path
            bracket acquireHome removePathForcibly \path ->
                bracket newIntegrationWorkerRegistry shutdownIntegrationWorkers \registry -> do
                    let home = unsafeEncodeUtf path
                        credential = GatewayCredential
                            "https://gateway.example" "wss://gateway.example/ws" "first"
                    saveGatewayCredentialAt home credential `shouldReturn` Right ()
                    bracket
                        (registerGatewayCredentialInvalidatorAt home
                            (shutdownOrganizationIntegrationWorkers registry))
                        id \_ -> do
                            localStarted <- newEmptyMVar
                            organizationStarted <- newEmptyMVar
                            resumePoll <- newEmptyMVar
                            organizationBlock <- newEmptyMVar
                            localResult <- newEmptyMVar
                            organizationResult <- newEmptyMVar
                            launchLocalIntegrationWorkerWith registry (putMVar localResult) do
                                putMVar localStarted ()
                                takeMVar resumePoll
                                pure (Right ())
                            launchIntegrationWorkerWith registry (putMVar organizationResult) do
                                putMVar organizationStarted ()
                                takeMVar organizationBlock
                                pure (Right ())
                            takeMVar localStarted
                            takeMVar organizationStarted
                            timeout 1000000 (saveGatewayCredentialAt home
                                credential {gatewayAccessToken = "second"})
                                `shouldReturn` Just (Right ())
                            takeMVar organizationResult >>= (`shouldSatisfy` isLeft)
                            tryReadMVar localResult `shouldReturn` Nothing
                            putMVar resumePoll ()
                            timeout 1000000 (takeMVar localResult)
                                `shouldReturn` Just (Right ())
                            -- A second pending local operation must still be
                            -- cancelled/joined when the engine itself closes.
                            shutdownStarted <- newEmptyMVar
                            shutdownBlock <- newEmptyMVar
                            shutdownResult <- newEmptyMVar
                            launchLocalIntegrationWorkerWith registry (putMVar shutdownResult) do
                                putMVar shutdownStarted ()
                                takeMVar shutdownBlock
                                pure (Right ())
                            takeMVar shutdownStarted
                            timeout 1000000 (shutdownIntegrationWorkers registry)
                                `shouldReturn` Just ()
                            takeMVar shutdownResult >>= (`shouldSatisfy` isLeft)

        it "completes once when cancelled while waiting for the output boundary" do
            registry <- newIntegrationWorkerRegistry
            waiting <- newEmptyMVar
            blocked <- newEmptyMVar
            callbacks <- newIORef ([] :: [String])
            let record value = atomicModifyIORef' callbacks \values ->
                    (values <> [value], ())
            launchIntegrationWorkerWith registry
                (\_ -> completeBoundaryChecked
                    (\_ -> putMVar waiting () >> takeMVar blocked)
                    (record "success")
                    (record "cancelled"))
                (pure (Right ()))
            takeMVar waiting
            timeout 1000000 (shutdownIntegrationWorkers registry)
                `shouldReturn` Just ()
            readIORef callbacks `shouldReturn` ["cancelled"]

        it "does not retry a callback after boundary delivery has begun" do
            callbacks <- newIORef ([] :: [String])
            let record value = modifyIORef' callbacks (<> [value])
            completeBoundaryChecked
                (\emit -> emit >> pure (Left "changed"))
                (record "success")
                (record "cancelled")
            readIORef callbacks `shouldReturn` ["success"]

        it "keeps the mailbox launch nonblocking and completes once on shutdown" do
            registry <- newIntegrationWorkerRegistry
            started <- newEmptyMVar
            callbacks <- newIORef ([] :: [String])
            launched <- timeout 1000000 $
                launchIntegrationWorkerWith
                    registry
                    (\result ->
                        atomicModifyIORef' callbacks \current ->
                            ( current
                                <> [either show (const "success") result]
                            , ()
                            ))
                    do
                        putMVar started ()
                        threadDelay maxBound
                        pure (Left "unexpected")
            launched `shouldBe` Just ()
            takeMVar started
            timeout 1000000 (shutdownIntegrationWorkers registry)
                `shouldReturn` Just ()
            readIORef callbacks
                `shouldReturn`
                    [ "\"engine stopped before integration operation completed\""
                    ]

        it "turns operation exceptions into one terminal failure" do
            registry <- newIntegrationWorkerRegistry
            callback <- newEmptyMVar
            launchIntegrationWorkerWith
                registry
                (putMVar callback . either id (const "success"))
                (throwString "boom")
            timeout 1000000 (takeMVar callback)
                `shouldReturn` Just "integration admin operation failed"
            shutdownIntegrationWorkers registry

computerUseSpec :: Spec
computerUseSpec = describe "native computer-use turn propagation" do
    it "defaults omitted computerUse to disabled" do
        case decodeStart
            "{\"turnId\":\"t\",\"prompt\":\"p\",\"cwd\":\"/tmp\"}" of
            Left err -> expectationFailure err
            Right start -> do
                start.turnStartComputerUse `shouldBe` False
                nativeTurnArguments start
                    `shouldContain` ["--no-computer-use"]
                nativeTurnArguments start
                    `shouldNotContain` ["--computer-use"]

    it "passes an explicit computerUse opt-in to the native runtime" do
        case decodeStart
            "{\"turnId\":\"t\",\"prompt\":\"p\",\"cwd\":\"/tmp\",\"computerUse\":true}" of
            Left err -> expectationFailure err
            Right start -> do
                start.turnStartComputerUse `shouldBe` True
                nativeTurnArguments start
                    `shouldContain` ["--computer-use"]
                nativeTurnArguments start
                    `shouldNotContain` ["--no-computer-use"]

boundaryCheckedEmissionSpec :: Spec
boundaryCheckedEmissionSpec =
    describe "native boundary-checked emission" do
        it "stops before the next item when credentials change" do
            checks <- newIORef
                [Right (), Left "gateway credentials changed"]
            emitted <- newIORef ([] :: [Int])
            terminal <- newIORef False
            let check =
                    atomicModifyIORef' checks \case
                        [] -> ([], Left "missing boundary check")
                        result : remaining -> (remaining, result)
            result <- emitBoundaryChecked
                id
                check
                (\item -> modifyIORef' emitted (<> [item]))
                (writeIORef terminal True)
                [1, 2]
            result `shouldBe` Left "gateway credentials changed"
            readIORef emitted `shouldReturn` [1]
            readIORef terminal `shouldReturn` False

        it "checks again before the terminal callback" do
            checks <- newIORef [Right (), Right ()]
            emitted <- newIORef ([] :: [Int])
            terminal <- newIORef False
            let check =
                    atomicModifyIORef' checks \case
                        [] -> ([], Left "missing boundary check")
                        result : remaining -> (remaining, result)
            emitBoundaryChecked
                id
                check
                (\item -> modifyIORef' emitted (<> [item]))
                (writeIORef terminal True)
                [1]
                `shouldReturn` Right ()
            readIORef emitted `shouldReturn` [1]
            readIORef terminal `shouldReturn` True

        it "holds one critical section across each check and callback" do
            nextSection <- newIORef (0 :: Int)
            activeSection <- newIORef (0 :: Int)
            observedSections <- newIORef ([] :: [Int])
            let critical action = do
                    section <- atomicModifyIORef' nextSection \current ->
                        let next = current + 1
                        in (next, next)
                    bracket_
                        (writeIORef activeSection section)
                        (writeIORef activeSection 0)
                        action
                observe =
                    readIORef activeSection >>= \section ->
                        modifyIORef' observedSections (<> [section])
                check = observe >> pure (Right ())
            emitBoundaryChecked
                critical
                check
                (const observe)
                observe
                [1 :: Int]
                `shouldReturn` Right ()
            readIORef observedSections `shouldReturn` [1, 1, 2, 2]
            readIORef activeSection `shouldReturn` 0

        it "blocks a boundary switch between a check and its callback" do
            boundaryLock <- newMVar ()
            checked <- newEmptyMVar
            releaseCallback <- newEmptyMVar
            switchStarted <- newEmptyMVar
            callbackRan <- newIORef False
            let critical action =
                    withMVar boundaryLock (const action)
                check = putMVar checked () >> pure (Right ())
                emit _ = do
                    takeMVar releaseCallback
                    writeIORef callbackRan True
            withAsync
                (emitBoundaryChecked
                    critical
                    check
                    emit
                    (pure ())
                    [1 :: Int])
                \emission -> do
                    takeMVar checked
                    withAsync
                        (putMVar switchStarted ()
                            >> withMVar boundaryLock (const (pure ())))
                        \switch -> do
                            takeMVar switchStarted
                            threadDelay 100000
                            switchState <- poll switch
                            switchState `shouldSatisfy` isNothing
                            readIORef callbackRan `shouldReturn` False
                            putMVar releaseCallback ()
                            wait emission `shouldReturn` Right ()
                            wait switch
            readIORef callbackRan `shouldReturn` True

gatewayTransitionCallbackSpec :: Spec
gatewayTransitionCallbackSpec =
    describe "native gateway transition callbacks" do
        it "contains a host exception after one terminal callback attempt" do
            attempts <- newIORef (0 :: Int)
            invokeGatewayCallbackOnce do
                modifyIORef' attempts (+ 1)
                throwString "host callback failed"
            readIORef attempts `shouldReturn` 1

nativeRequestBoundarySpec :: Spec
nativeRequestBoundarySpec =
    describe "native request gateway callback boundary" do
        it "serializes every request returning session or model data" do
            map nativeRequestRequiresGatewayLock
                ["sessions.list", "sessions.show", "models.list"]
                `shouldBe` [True, True, True]
            map nativeRequestRequiresGatewayLock
                ["ping", "turn.agents", "unknown"]
                `shouldBe` [False, False, False]

nativeTurnBoundarySpec :: Spec
nativeTurnBoundarySpec =
    describe "native turn gateway boundary" do
        it "routes a queued turn only under its captured credential identity" do
            nativeTurnRouteMatchesBoundary
                (Just "gateway-organization-a")
                (Just "gateway-organization-a")
                `shouldBe` True
            nativeTurnRouteMatchesBoundary
                (Just "gateway-organization-a")
                (Just "gateway-organization-b")
                `shouldBe` False

        it "does not cross between direct and organization routes" do
            nativeTurnRouteMatchesBoundary Nothing Nothing
                `shouldBe` True
            nativeTurnRouteMatchesBoundary
                Nothing
                (Just "gateway-organization")
                `shouldBe` False
            nativeTurnRouteMatchesBoundary
                (Just "gateway-organization")
                Nothing
                `shouldBe` False

nativeFailureSpec :: Spec
nativeFailureSpec =
    describe "native turn failures" do
        it "delivers background turn failure guidance in the terminal event" do
            let message =
                    "The request is too large.\n"
                        <> "Remove large attachments or run /compact, then retry."
            result <- tryAny (exitFailedTurn True message :: IO ())
            case result of
                Right () -> expectationFailure "expected a turn failure"
                Left exception ->
                    turnFailedEvent "failed-turn" (nativeExceptionMessage exception)
                        `shouldBe` Aeson.object
                            [ "event" Aeson..= ("turn.failed" :: String)
                            , "turnId" Aeson..= ("failed-turn" :: String)
                            , "error" Aeson..= message
                            ]

        it "displays a startup failure without its constructor wrapper" do
            displayException
                (StartupFailure
                    "Provider rejected the request.\nRetry the message.")
                `shouldBe`
                    "Provider rejected the request.\nRetry the message."

        it "returns the detailed message without an exception wrapper" do
            nativeExceptionMessage
                (toException
                    (StartupFailure
                        "Provider rejected the request.\nRetry the message."))
                `shouldBe`
                    "Provider rejected the request.\nRetry the message."

nativeSessionBoundarySpec :: Spec
nativeSessionBoundarySpec =
    describe "native session gateway boundary" do
        it "allows persisted sessions while disconnected" do
            nativeSessionRouteMatchesBoundary Nothing "openai" Nothing
                `shouldBe` True
            nativeSessionRouteMatchesBoundary
                Nothing
                "organization-gateway"
                (Just "gateway-a")
                `shouldBe` True

        it "allows persisted sessions while a gateway is connected" do
            nativeSessionRouteMatchesBoundary
                (Just "gateway-a")
                "organization-gateway"
                (Just "gateway-a")
                `shouldBe` True
            nativeSessionRouteMatchesBoundary
                (Just "gateway-a")
                "organization-gateway"
                (Just "gateway-b")
                `shouldBe` True
            nativeSessionRouteMatchesBoundary
                (Just "gateway-a")
                "organization-gateway"
                Nothing
                `shouldBe` True
            nativeSessionRouteMatchesBoundary
                (Just "gateway-a")
                "openai"
                Nothing
                `shouldBe` True

decodeStart :: String -> Either String TurnStart
decodeStart = Aeson.eitherDecode . LBS8.pack
