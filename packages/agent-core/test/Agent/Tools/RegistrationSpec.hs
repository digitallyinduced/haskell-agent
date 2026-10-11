module Agent.Tools.RegistrationSpec (spec) where

import Agent.Tools.Registration
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Text (Text)
import Test.Hspec

type Handler = Text -> IO (Either Text Text)
type Registration = RegisteredTool Text Text Handler

spec :: Spec
spec = describe "native tool registration" do
    it "rejects missing and duplicate handlers before selecting advertisements" do
        fmap (map registeredName) (selectAdvertised ["missing"] [])
            `shouldBe` Left (MissingHandlers ["missing"])
        let tool = registration "duplicate" (\input -> pure (Right (input, [])))
        fmap (map registeredName) (selectAdvertised [] [tool, tool])
            `shouldBe` Left (DuplicateHandlers ["duplicate"])

    it "exposes only the advertised handlers" do
        let first = registration "first" (\input -> pure (Right (input, [])))
            second = registration "second" (\input -> pure (Right (input, [])))
        fmap (map registeredName) (selectAdvertised ["second"] [first, second])
            `shouldBe` Right ["second"]

    it "derives the dispatch identity from the schema" do
        let tool = registerTool id "canonical" True [] \name _ _ -> pure (Right name)
        dispatchRegistered dispatch [tool] "input" `shouldReturn` (Right "canonical", [] :: [Text])

    it "keeps declarations available before invocation but publishes no effects on failure" do
        let tool = registerTool id "send" True ["delivery"] \_ effects input ->
                recordToolEffects effects (pure (Left input :: Either Text (Text, [Text])))
        registeredEffects tool `shouldBe` ["delivery"]
        dispatchRegistered dispatch [tool] "failed" `shouldReturn` (Left "failed", [])

    it "isolates runtime effects between concurrent invocations" do
        started <- newEmptyMVar
        release <- newEmptyMVar
        let tool = registration "deliver" \input -> do
                if input == "first"
                    then putMVar started () >> takeMVar release
                    else takeMVar started >> putMVar release ()
                pure (Right (input, [input]))
        concurrently
            (dispatchRegistered dispatch [tool] "first")
            (dispatchRegistered dispatch [tool] "second")
            `shouldReturn` ((Right "first", ["first"]), (Right "second", ["second"]))

registration :: Text -> (Text -> IO (Either Text (Text, [Text]))) -> Registration
registration name action = registerTool id name True [] \_ effects input ->
    recordToolEffects effects (action input)

dispatch :: [Handler] -> Text -> IO (Either Text Text)
dispatch [handler] input = handler input
dispatch _ _ = pure (Left "invalid registration count")
