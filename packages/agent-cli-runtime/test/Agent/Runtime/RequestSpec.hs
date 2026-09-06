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

    it "accepts ask and plan interaction with every shell mode" do
        mapM_ (\mode ->
            mapM_ (\shell ->
                validateNativeTurnRequest request
                    { nativeTurnInteractionMode = mode
                    , nativeTurnShellMode = shell
                    }
                    `shouldBe` Right ())
                [NativeShellNone, NativeShellBash, NativeShellGhci, NativeShellBoth])
            [NativeAsk, NativePlan]

    it "rejects auto-approval before execution" do
        validateNativeTurnRequest
            request { nativeTurnInteractionMode = NativeYolo }
            `shouldBe` Left "typed native turns do not support auto-approval"

    it "rejects empty and whitespace-only resume identifiers" do
        mapM_ (\sessionId ->
            validateNativeTurnRequest
                request { nativeTurnSession = NativeResumeSession sessionId }
                `shouldBe` Left "native resume session id must not be empty")
            ["", " \n\t"]

    it "preserves auto-approval error precedence for invalid resume requests" do
        validateNativeTurnRequest request
            { nativeTurnInteractionMode = NativeYolo
            , nativeTurnSession = NativeResumeSession ""
            }
            `shouldBe` Left "typed native turns do not support auto-approval"

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
    }
