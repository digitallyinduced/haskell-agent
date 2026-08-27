module Agent.CLI.RecapSpec (spec) where

import Agent.CLI.Recap
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , TurnInput(..)
    , emptyTurnOutput
    )
import Agent.Responses.Types
import Agent.ToolDispatch (functionToolCall)
import Data.IORef
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.Recap" do
    describe "cleanRecapText" do
        it "collapses whitespace and strips stray labels" do
            cleanRecapText "Recap:  We  fixed\nauth"
                `shouldBe` "We fixed auth"
            cleanRecapText "\"We merged the branch\""
                `shouldBe` "We merged the branch"

        it "caps runaway recaps" do
            let out = cleanRecapText (Text.replicate (recapMaxChars + 20) "a")
            Text.length out `shouldBe` recapMaxChars + 1
            Text.last out `shouldBe` '\8230'

    describe "recapGate" do
        it "allows a manual recap after the first user turn" do
            recapGate 1 0 RecapManual True `shouldBe` Right ()
            recapGate 0 0 RecapManual True `shouldBe` Left RecapNoMainTurns

        it "requires idle time, three turns, and a new turn for auto recap" do
            recapGate 3 2 RecapAuto True `shouldBe` Right ()
            recapGate 2 0 RecapAuto True `shouldBe` Left RecapTooFewTurns
            recapGate 3 3 RecapAuto True `shouldBe` Left RecapNoNewMainTurn
            recapGate 3 2 RecapAuto False `shouldBe` Left RecapIdleThresholdUnmet

    describe "shouldSuppressAutoRecapDisplay" do
        it "hides long-tail automatic recaps but always shows manual ones" do
            shouldSuppressAutoRecapDisplay
                RecapAuto
                (Text.replicate (recapAutoRawDisplayMax + 1) "x")
                "short"
                `shouldBe` True
            shouldSuppressAutoRecapDisplay RecapManual
                (Text.replicate (recapAutoRawDisplayMax + 1) "x")
                "short"
                `shouldBe` False

    describe "runRecapWithCancel" do
        it "appends the recap instruction without mutating parent state" do
            let original =
                    [ MessageItem ResponseMessage
                        { messageId = Just "u1"
                        , content = MessageContentText "fix auth"
                        , role = RoleUser
                        , status = Just ItemCompleted
                        , phase = Nothing
                        , passthrough = Nothing
                        }
                    ]
            params <- newIORef defaultResponseCreateParams
            transcript <- newIORef original
            seenInputs <- newIORef []
            let factory _ = Backend \state _ inputs _ -> do
                    writeIORef seenInputs inputs
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "recap-1" []
                                (Just "We fixed auth retries in billing/retry.rs.")
                        , backendState = state
                        }
            result <-
                runRecapWithCancel (\_ action -> action)
                    factory params transcript RecapManual
            result
                `shouldBe`
                    Right
                        (RecapShown
                            "We fixed auth retries in billing/retry.rs.")
            readIORef transcript `shouldReturn` original
            seen <- readIORef seenInputs
            seen `shouldSatisfy` \case
                [UserMessage text] ->
                    "Write ONE sentence recap body" `Text.isInfixOf` text
                _ -> False

        it "rejects tool calls" do
            params <- newIORef defaultResponseCreateParams
            transcript <- newIORef []
            let factory _ = Backend \state _ _ _ ->
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "recap-1"
                                [functionToolCall "call-1" "shell_command" "{}"]
                                Nothing
                        , backendState = state
                        }
            runRecapWithCancel (\_ action -> action)
                factory params transcript RecapManual
                >>= (`shouldBe` Left RecapUnexpectedToolCall)
