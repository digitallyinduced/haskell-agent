module Agent.CLI.MacOS.BridgeSpec (spec) where

import Agent.CLI.MacOS.Bridge
    ( TurnStart(..)
    , emitBoundaryChecked
    , nativeExceptionMessage
    , nativeSessionRouteMatchesBoundary
    , nativeTurnArguments
    )
import Agent.CLI.NativeRuntime (StartupFailure(..))
import Control.Exception.Safe (displayException, toException)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.IORef
    ( atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef )
import Test.Hspec

spec :: Spec
spec = do
    boundaryCheckedEmissionSpec
    computerUseSpec
    nativeSessionBoundarySpec
    nativeFailureSpec

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
                check
                (\item -> modifyIORef' emitted (<> [item]))
                (writeIORef terminal True)
                [1]
                `shouldReturn` Right ()
            readIORef emitted `shouldReturn` [1]
            readIORef terminal `shouldReturn` True

nativeFailureSpec :: Spec
nativeFailureSpec =
    describe "native turn failures" do
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
        it "allows only direct sessions while disconnected" do
            nativeSessionRouteMatchesBoundary Nothing "openai" Nothing
                `shouldBe` True
            nativeSessionRouteMatchesBoundary
                Nothing
                "organization-gateway"
                (Just "gateway-a")
                `shouldBe` False

        it "allows only the exact connected gateway identity" do
            nativeSessionRouteMatchesBoundary
                (Just "gateway-a")
                "organization-gateway"
                (Just "gateway-a")
                `shouldBe` True
            nativeSessionRouteMatchesBoundary
                (Just "gateway-a")
                "organization-gateway"
                (Just "gateway-b")
                `shouldBe` False
            nativeSessionRouteMatchesBoundary
                (Just "gateway-a")
                "openai"
                Nothing
                `shouldBe` False

decodeStart :: String -> Either String TurnStart
decodeStart = Aeson.eitherDecode . LBS8.pack
