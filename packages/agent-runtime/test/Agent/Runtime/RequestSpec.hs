module Agent.Runtime.RequestSpec (spec) where

import Agent.Runtime.Request
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = describe "validateNativeTurnRequest" do
    it "accepts new and resumed turns without CLI options" do
        validateNativeTurnRequest request `shouldBe` Right ()
        validateNativeTurnRequest
            request { nativeTurnSession = NativeResumeSession "session-1" }
            `shouldBe` Right ()

    it "accepts every interaction and shell mode" do
        mapM_ (\mode ->
            mapM_ (\shell ->
                validateNativeTurnRequest request
                    { nativeTurnInteractionMode = mode
                    , nativeTurnShellMode = shell
                    }
                    `shouldBe` Right ())
                [NativeShellNone, NativeShellBash, NativeShellGhci, NativeShellBoth])
            [NativeAsk, NativePlan, NativeYolo]

    it "rejects empty and whitespace-only resume identifiers" do
        mapM_ (\sessionId ->
            validateNativeTurnRequest
                request { nativeTurnSession = NativeResumeSession sessionId }
                `shouldBe` Left "native resume session id must not be empty")
            ["", " \n\t"]

    it "rejects invalid resume requests in auto-approval mode" do
        validateNativeTurnRequest request
            { nativeTurnInteractionMode = NativeYolo
            , nativeTurnSession = NativeResumeSession ""
            }
            `shouldBe` Left "native resume session id must not be empty"

request :: NativeTurnRequest
request = NativeTurnRequest
    { nativeTurnPrompt = "fix the tests"
    , nativeTurnImages = []
    , nativeTurnSession = NativeNewSession
    , nativeTurnProvider = Nothing
    , nativeTurnModel = Nothing
    , nativeTurnCwd = unsafeEncodeUtf "/workspace"
    , nativeTurnEffort = Nothing
    , nativeTurnInteractionMode = NativeAsk
    , nativeTurnShellMode = NativeShellNone
    , nativeTurnMessageClock = Nothing
    }
