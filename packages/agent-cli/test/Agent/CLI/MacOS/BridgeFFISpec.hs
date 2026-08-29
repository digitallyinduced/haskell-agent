{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Agent.CLI.MacOS.BridgeFFISpec (spec) where

#ifdef darwin_HOST_OS
import Agent.CLI.MacOS.Bridge
    ( NativeInteractionResolution(..)
    , PendingInteraction(..)
    , cancelPendingInteractions
    , discardStagedTurn
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

foreign import ccall "ha_native_turn_options_stage_smoke"
    nativeTurnOptionsStageSmoke :: IO CInt
#else
import Test.Hspec (Spec, describe, it, pendingWith)
#endif

spec :: Spec
spec = describe "native bridge FFI" do
    it "accepts copied image buffers through the exported bridge" do
#ifdef darwin_HOST_OS
        imageAttachmentStageSmoke `shouldReturn` 0
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
