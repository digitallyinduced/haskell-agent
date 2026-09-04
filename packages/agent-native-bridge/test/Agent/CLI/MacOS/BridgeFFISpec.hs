{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Agent.CLI.MacOS.BridgeFFISpec (spec) where

import qualified Agent.CLI.MacOS.Bridge as Bridge
#ifdef darwin_HOST_OS
import Agent.CLI.MacOS.Bridge
    ( NativeInteractionResolution(..)
    , PendingInteraction(..)
    , cancelPendingInteractions
    , discardStagedTurn
    , discardStagedTurnById
    , resolvePendingInteraction
    , turnStartCleanupId
    )
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM
    ( atomically
    , newEmptyTMVarIO
    , newTVarIO
    , readTMVar
    , readTVarIO
    )
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Foreign.C.Types (CInt(..))
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
#else
import Test.Hspec (Spec, describe, it, pendingWith, shouldReturn)
#endif

spec :: Spec
spec = describe "native bridge FFI" do
    it "stages copied images and completes restart before destroy returns" do
#ifdef darwin_HOST_OS
        imageAttachmentStageSmoke `shouldReturn` 0
#else
        pendingWith "the native bridge smoke test only links on macOS"
#endif
    it "blocks new repository workers until cancel-all returns" do
        Bridge.repositoryCancelAllAdmissionSmoke `shouldReturn` True

    it "does not self-deadlock on callback lifecycle calls" do
        Bridge.repositoryCancelAllReentrancySmoke `shouldReturn` True
        Bridge.repositoryCheckDestroyReentrancySmoke `shouldReturn` True

    it "classifies async cancellation as cancelled, not failure" do
        Bridge.repositoryCancelClassificationSmoke `shouldReturn` True

    it "does not retry a terminal callback that throws" do
        Bridge.repositoryTerminalThrowSmoke `shouldReturn` True

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
        taskSupervisorAbiSmoke `shouldReturn` 0
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
