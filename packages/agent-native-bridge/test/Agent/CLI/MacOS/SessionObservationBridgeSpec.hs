{-# LANGUAGE ForeignFunctionInterface #-}

module Agent.CLI.MacOS.SessionObservationBridgeSpec (spec) where

import Agent.CLI.MacOS.Marshalling (decodeInput)
import Agent.CLI.MacOS.SessionObservationBridge
    ( SessionObservationCallback, deliverUpdate )
import Agent.CLI.Session.Observation
import Control.Exception.Safe (bracket)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Foreign (FunPtr, freeHaskellFunPtr, nullPtr)
import Foreign.C.Types (CInt(..), CSize(..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

foreign import ccall "ha_session_observation_validation_smoke"
    sessionObservationValidationSmoke :: IO CInt

foreign import ccall "wrapper"
    wrapObservationCallback
        :: SessionObservationCallback -> IO (FunPtr SessionObservationCallback)

spec :: Spec
spec = describe "typed session observation delivery" do
    it "validates native inputs and joins cancellation before context release" do
        sessionObservationValidationSmoke `shouldReturn` 0
    it "copies Unicode catch-up fields and commits one ordered frame" do
        received <- newIORef []
        let callback _ kind owner ownerLength turn turnLength sequence generation boundary
                text textLength _ _ _ _ _ _ flags = do
                ownerID <- decodeInput owner (fromIntegral ownerLength)
                turnID <- decodeInput turn (fromIntegral turnLength)
                value <- decodeInput text (fromIntegral textLength)
                modifyIORef' received (<> [(kind, ownerID, turnID, sequence, generation, boundary, value, flags)])
            frame = SessionObservationFrame
                { ownerID = "owner-α"
                , turnID = "turn-β"
                , sequence = 42
                , generationStart = 12
                , durableTurnCount = 17
                , state = ObservationRunning
                , reset = True
                , truncated = False
                , userText = "Überprüfen"
                , events =
                    [ SessionObservationEvent
                        { kind = ObservationReasoning
                        , identifier = "", name = "", text = "Prüfung"
                        , arguments = "", isError = False
                        , summary = "", argumentsEncrypted = False, isAsync = False, isTruncated = False
                        }
                    , SessionObservationEvent
                        { kind = ObservationText
                        , identifier = "", name = "", text = "Antwort"
                        , arguments = "", isError = False
                        , summary = "", argumentsEncrypted = False, isAsync = False, isTruncated = False
                        }
                    ]
                }
        bracket (wrapObservationCallback callback) freeHaskellFunPtr \pointer ->
            deliverUpdate pointer nullPtr (ObservationFrame frame)
        readIORef received >>= (`shouldBe`
            [ (1, "owner-α", "turn-β", 42, 12, 17, "Überprüfen", 0)
            , (4, "owner-α", "turn-β", 42, 12, 17, "Prüfung", 0)
            , (3, "owner-α", "turn-β", 42, 12, 17, "Antwort", 0)
            , (2, "owner-α", "turn-β", 42, 12, 17, "", 0)
            ])
    it "delivers persisted boundary before the completed frame commit" do
        received <- newIORef []
        let callback _ kind _ _ _ _ _ _ boundary _ _ _ _ _ _ _ _ flags =
                modifyIORef' received (<> [(kind, boundary, flags)])
            frame = SessionObservationFrame
                { ownerID = "owner", turnID = "turn", sequence = 8
                , generationStart = 5
                , durableTurnCount = 9, state = ObservationCompleted
                , reset = False, truncated = True, userText = "", events = []
                }
        bracket (wrapObservationCallback callback) freeHaskellFunPtr \pointer ->
            deliverUpdate pointer nullPtr (ObservationFrame frame)
        readIORef received >>= (`shouldBe` [(8, 9, 258), (2, 9, 258)])
