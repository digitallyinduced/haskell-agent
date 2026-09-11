{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Agent.CLI.MacOS.BridgeFFISpec (spec) where

#ifdef darwin_HOST_OS
import Agent.CLI.MacOS.Bridge
    ( RepositoryCheckHandle(..)
    , ha_repository_check_destroy
    , NativeInteractionResolution(..)
    , PendingInteraction(..)
    , cancelPendingInteractions
    , discardStagedTurn
    , discardStagedTurnById
    , integrationABISynchronousValidationSmoke
    , resolvePendingInteraction
    , turnStartCleanupId
    )
import Agent.CLI.MacOS.RepositoryWorkers (withRepositoryCallbackThread)
import Agent.CLI.MacOS.EngineEvents (EventCallback)
import Agent.CLI.MacOS.InteractionState (InteractionRuntime(..), setTurnInteractionMode)
import Agent.CLI.MacOS.NativeInteraction (requestFreshApproval, resolveApproval)
import Agent.CLI.MacOS.NativeRequest (BridgeRequest(..))
import Agent.CLI.MacOS.TurnState (TurnControl(..), newTurnControl)
import Agent.CLI.Permission (PermissionChoice(..))
import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.CLI.NativeRuntime (NativeInteractionMode(..), applyNativeInteractionMode)
import Agent.Tools.PlanMode (newPlanModeEnv, isPlanModeActive)
import qualified System.OsPath as OsPath
import Agent.ToolDispatch (ToolCall(..), ToolCallKind(..))
import Control.Concurrent.Async (concurrently, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar, readMVar, modifyMVar_)
import Control.Concurrent.STM
    ( atomically
    , newEmptyTMVarIO
    , newTVarIO
    , readTMVar
    , readTVarIO
    , isEmptyTMVar
    , writeTVar
    )
import Control.Exception.Safe (bracket, finally, tryAny)
import Data.Either (isRight)
import Data.IORef (newIORef, modifyIORef', readIORef, writeIORef)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Foreign.Ptr (FunPtr, castPtr, freeHaskellFunPtr, nullPtr)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.StablePtr (castStablePtrToPtr, newStablePtr)
import System.Directory
    ( createDirectory
    , getTemporaryDirectory
    , removeFile
    , removePathForcibly
    )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (hClose, openTempFile)
import System.Timeout (timeout)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldReturn
    , shouldSatisfy
    )

foreign import ccall "ha_image_attachment_stage_smoke"
    imageAttachmentStageSmoke :: IO CInt

foreign import ccall "ha_repository_review_abi_smoke"
    repositoryReviewAbiSmoke :: IO CInt

foreign import ccall "ha_task_supervisor_abi_smoke"
    taskSupervisorAbiSmoke :: IO CInt

foreign import ccall "ha_learned_skill_admin_validation_smoke"
    learnedSkillAdminValidationSmoke :: IO CInt

foreign import ccall "ha_native_turn_options_stage_smoke"
    nativeTurnOptionsStageSmoke :: IO CInt

foreign import ccall "ha_turn_staging_discard_smoke"
    turnStagingDiscardSmoke :: IO CInt

foreign import ccall "wrapper"
    makeApprovalEventCallback :: EventCallback -> IO (FunPtr EventCallback)

withinApprovalDeadline :: IO () -> IO ()
withinApprovalDeadline action =
    timeout 5000000 action `shouldReturn` Just ()
#else
import Test.Hspec (Spec, describe, it, pendingWith, shouldReturn)
#endif

spec :: Spec
spec = describe "native bridge FFI" do
#ifdef darwin_HOST_OS
    it "changes the turn-local approval and planning policies in both directions" do
        policy <- newIORef PromptMutating
        directory <- OsPath.encodeUtf "."
        plan <- newPlanModeEnv directory Nothing
        let check mode expectedPolicy expectedPlan = do
                applyNativeInteractionMode policy plan mode
                readIORef policy `shouldReturn` expectedPolicy
                isPlanModeActive plan `shouldReturn` expectedPlan
        check NativeYolo ApproveAll False
        check NativeAsk PromptMutating False

        check NativePlan PromptMutating True
        check NativeYolo ApproveAll False
        check NativePlan PromptMutating True
        check NativeAsk PromptMutating False

    it "targets only the registered turn and leaves pending approvals unanswered" do
        calls <- newIORef []
        waiter <- newEmptyTMVarIO
        pending <- newTVarIO $ Map.singleton ("running", "approval") (PendingInteraction 2 waiter)
        setters <- newMVar $ Map.singleton "running" (\mode -> modifyIORef' calls (<> [mode]))
        runtime <- InteractionRuntime <$> newTVarIO Nothing <*> newMVar () <*> pure pending <*> pure setters
        setTurnInteractionMode runtime "other" NativeYolo `shouldReturn` False
        readIORef calls `shouldReturn` []
        setTurnInteractionMode runtime "running" NativeYolo `shouldReturn` True
        setTurnInteractionMode runtime "running" NativeAsk `shouldReturn` True
        readIORef calls `shouldReturn` [NativeYolo, NativeAsk]
        atomically (isEmptyTMVar waiter) `shouldReturn` True
        Map.size <$> readTVarIO pending `shouldReturn` 1
        modifyMVar_ setters (pure . Map.delete "running")
        setTurnInteractionMode runtime "running" NativePlan `shouldReturn` False

    it "requests fresh approvals despite remembered tool grants and rejects broad decisions" $ withinApprovalDeadline do
        interactions <- InteractionRuntime
            <$> newTVarIO Nothing <*> newMVar () <*> newTVarIO Map.empty
            <*> newMVar Map.empty
        control <- newTurnControl "fresh-approval-test" Nothing Nothing interactions
        atomically $
            writeTVar control.turnControlAllowedTools (Set.singleton "shell_command")
        events <- newIORef []
        resolutions <- newIORef []
        decision <- newIORef "allow_once"
        let call = ToolCall
                { callId = "fresh-call"
                , name = "shell_command"
                , arguments = "{\"command\":\"pwd\",\"sandbox_permissions\":\"require_escalated\",\"justification\":\"Inspect directory\"}"
                , callKind = FunctionCallKind
                , argumentsEncrypted = False
                }
            callback _ pointer length = do
                bytes <- BS.packCStringLen (castPtr pointer, fromIntegral length)
                case Aeson.decodeStrict' bytes of
                    Just event@(Aeson.Object object)
                        | Just (Aeson.Object approval) <- KeyMap.lookup "approval" object
                        , Just identifier <- KeyMap.lookup "id" approval -> do
                            modifyIORef' events (<> [event])
                            let resolution selectedDecision marker = BridgeRequest
                                    "resolve" "approval.resolve"
                                    (Aeson.object
                                        ([ "approvalId" Aeson..= identifier
                                         , "decision" Aeson..= (selectedDecision :: String)
                                         ] <> maybe [] (\value -> ["onceOnly" Aeson..= (value :: Bool)]) marker))
                            broad <- resolveApproval control (resolution "allow_tool" (Just True))
                            legacy <- resolveApproval control (resolution "allow_once" Nothing)
                            unsupported <- resolveApproval control (resolution "allow_once" (Just False))
                            selected <- readIORef decision
                            exact <- resolveApproval control
                                (resolution selected (if selected == "deny" then Nothing else Just True))
                            stale <- resolveApproval control (resolution "allow_once" (Just True))
                            modifyIORef' resolutions (<> [broad, legacy, unsupported, exact, stale])
                    _ -> pure ()
        bracket (makeApprovalEventCallback callback) freeHaskellFunPtr \handler -> do
            requestFreshApproval handler nullPtr control call
                `shouldReturn` Just PermissionAllowOnce
            requestFreshApproval handler nullPtr control call
                `shouldReturn` Just PermissionAllowOnce
            writeIORef decision "deny"
            requestFreshApproval handler nullPtr control call
                `shouldReturn` Just PermissionDeny
        recorded <- readIORef events
        length recorded `shouldBe` 3
        let approvalField key (Aeson.Object object)
                | Just (Aeson.Object approval) <- KeyMap.lookup "approval" object =
                    KeyMap.lookup key approval
            approvalField _ _ = Nothing
        map (approvalField "onceOnly") recorded `shouldBe` replicate 3 (Just (Aeson.Bool True))
        map (approvalField "arguments") recorded `shouldBe` replicate 3 (Just (Aeson.String call.arguments))
        results <- readIORef resolutions
        let resultEvent (Aeson.Object object) = KeyMap.lookup "ok" object
            resultEvent _ = Nothing
        map resultEvent results `shouldBe`
            concat (replicate 3
                [Just (Aeson.Bool False), Just (Aeson.Bool False), Just (Aeson.Bool False)
                , Just (Aeson.Bool True), Just (Aeson.Bool False)])
        readTVarIO control.turnControlAllowedTools
            `shouldReturn` Set.singleton "shell_command"
        Map.null <$> readTVarIO control.turnControlApprovals `shouldReturn` True
#endif
    it "stages copied images and completes restart before destroy returns" do
#ifdef darwin_HOST_OS
        imageAttachmentStageSmoke `shouldReturn` 0
#else
        pendingWith "the native bridge smoke test only links on macOS"
#endif
    it "does not self-deadlock when a callback destroys its check" do
#ifdef darwin_HOST_OS
        repositoryCheckDestroyReentrancySmoke `shouldReturn` True
#else
        pendingWith "the native bridge smoke test only links on macOS"
#endif

    it "rejects invalid integration ABI calls before launching workers" do
#ifdef darwin_HOST_OS
        integrationABISynchronousValidationSmoke `shouldReturn` True
#else
        pendingWith "the native bridge smoke test only links on macOS"
#endif

    it "validates the typed repository-review ABI from native code" do
#ifdef darwin_HOST_OS
        repositoryReviewAbiSmoke `shouldReturn` 0
#else
        pendingWith "the native bridge smoke test only links on macOS"
#endif

    it "discards staged turns through the C ABI and destroys cleanly" do
#ifdef darwin_HOST_OS
        turnStagingDiscardSmoke `shouldReturn` 0
#else
        pendingWith "the native bridge smoke test only links on macOS"
#endif

    it "controls and snapshots native tasks through the exported bridge" do
#ifdef darwin_HOST_OS
        withIsolatedHome $ taskSupervisorAbiSmoke `shouldReturn` 0
#else
        pendingWith "the native bridge smoke test only links on macOS"
#endif

    it "rejects invalid learned resource inputs through exported symbols" do
#ifdef darwin_HOST_OS
        learnedSkillAdminValidationSmoke `shouldReturn` 0
#else
        pendingWith "the native bridge smoke test only links on macOS"
#endif

#ifdef darwin_HOST_OS
    it "cleans malformed turn.start staging by turnId, not request id" do
        let malformed = Aeson.object
                [ "turnId" Aeson..= ("turn-1" :: String)
                , "prompt" Aeson..= (17 :: Int)
                ]
        turnStartCleanupId "request-1" malformed `shouldBe` "turn-1"
        turnStartCleanupId "request-1" (Aeson.object [])
            `shouldBe` "request-1"
        stagedImages <- newTVarIO $ Map.fromList
            [("turn-1", 1 :: Int), ("request-1", 2)]
        stagedOptions <- newTVarIO $ Map.fromList
            [("turn-1", 1 :: Int), ("request-1", 2)]
        atomically $ discardStagedTurn
            "request-1"
            malformed
            stagedImages
            stagedOptions
        readTVarIO stagedImages
            `shouldReturn` Map.singleton "request-1" 2
        readTVarIO stagedOptions
            `shouldReturn` Map.singleton "request-1" 2

    it "atomically discards options-only, images-only, both, and repeatedly" do
        stagedImages <- newTVarIO $ Map.fromList
            [("images-only", 1 :: Int), ("both", 2)]
        stagedOptions <- newTVarIO $ Map.fromList
            [("options-only", 1 :: Int), ("both", 2)]
        atomically $ discardStagedTurnById
            "options-only" stagedImages stagedOptions
        readTVarIO stagedImages `shouldReturn` Map.fromList
            [("images-only", 1), ("both", 2)]
        readTVarIO stagedOptions `shouldReturn` Map.singleton "both" 2
        atomically $ discardStagedTurnById
            "images-only" stagedImages stagedOptions
        readTVarIO stagedImages `shouldReturn` Map.singleton "both" 2
        readTVarIO stagedOptions `shouldReturn` Map.singleton "both" 2
        atomically $ discardStagedTurnById
            "both" stagedImages stagedOptions
        atomically $ discardStagedTurnById
            "both" stagedImages stagedOptions
        (Map.null <$> readTVarIO stagedImages) `shouldReturn` True
        (Map.null <$> readTVarIO stagedOptions) `shouldReturn` True

    it "resolves a pending callback answer exactly once" do
        waiter <- newEmptyTMVarIO
        pending <- newTVarIO $ Map.singleton
            ("turn", "question")
            PendingInteraction
                { pendingInteractionOptionCount = 2
                , pendingInteractionWaiter = waiter
                }
        let resolution = NativeInteractionResolution
                { interactionSelectedIndex = 1
                , interactionCustomText = Just "notes"
                }
        atomically (resolvePendingInteraction
            pending
            ("turn", "question")
            resolution) `shouldReturn` True
        atomically (readTMVar waiter) `shouldReturn` resolution
        atomically (resolvePendingInteraction
            pending
            ("turn", "question")
            resolution) `shouldReturn` False
        (Map.null <$> readTVarIO pending) `shouldReturn` True

    it "cancels callback replacement races without stranding a waiter" do
        waiter <- newEmptyTMVarIO
        pending <- newTVarIO $ Map.singleton
            ("turn", "question")
            PendingInteraction
                { pendingInteractionOptionCount = 1
                , pendingInteractionWaiter = waiter
                }
        let resolution = NativeInteractionResolution
                { interactionSelectedIndex = 0
                , interactionCustomText = Nothing
                }
        _ <- concurrently
            (atomically $ resolvePendingInteraction
                pending
                ("turn", "question")
                resolution)
            (atomically $ cancelPendingInteractions pending)
        result <- atomically (readTMVar waiter)
        result `shouldSatisfy` (`elem`
            [ resolution
            , NativeInteractionResolution (-1) Nothing
            ])
        (Map.null <$> readTVarIO pending) `shouldReturn` True
#endif

    it "stages typed turn options and validates interaction resolution" do
#ifdef darwin_HOST_OS
        nativeTurnOptionsStageSmoke `shouldReturn` 0
#else
        pendingWith "the native bridge smoke test only links on macOS"
#endif

#ifdef darwin_HOST_OS
withIsolatedHome :: IO a -> IO a
withIsolatedHome action =
    bracket create removePathForcibly \home ->
        bracket
            (do
                old <- lookupEnv "HOME"
                setEnv "HOME" home
                pure old)
            (\case
                Just old -> setEnv "HOME" old
                Nothing -> unsetEnv "HOME")
            (\_ -> action)
  where
    create = do
        temporary <- getTemporaryDirectory
        (path, handle) <- openTempFile temporary "agent-native-bridge-home"
        hClose handle
        removeFile path
        createDirectory path
        pure path

repositoryCheckDestroyReentrancySmoke :: IO Bool
repositoryCheckDestroyReentrancySmoke = do
    gate <- newEmptyMVar
    withAsync (readMVar gate) \owner -> do
        value <- newIORef Nothing
        cancelled <- newIORef False
        stable <- newStablePtr RepositoryCheckHandle
            { repositoryCheckValue = value
            , repositoryCheckCancelRequested = cancelled
            , repositoryCheckOwner = owner
            }
        let pointer = castStablePtrToPtr stable
        result <- tryAny
            (withRepositoryCallbackThread
                (ha_repository_check_destroy pointer))
            `finally` putMVar gate ()
        ha_repository_check_destroy pointer
        pure (isRight result)
#endif
