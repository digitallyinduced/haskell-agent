module Agent.Tools.CodeMode.IsolatedHostSpec (spec) where

import Agent.Tools.CodeMode.IsolatedHost
import Agent.Tools.CodeMode.Protocol.Value
import Data.Aeson (Value(..), object, (.=))
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import System.Environment (getExecutablePath)
import Test.Hspec

spec :: Spec
spec = do
    describe "isolated code-mode containment" do
        it "does not bypass a rejected launcher during availability checks" do
            executable <- getExecutablePath
            attempts <- newIORef (0 :: Int)
            let prepare _ _ = modifyIORef' attempts (+ 1) >> pure (Left "containment denied")
                config = (defaultCodeModeConfig prepare executable (\_ _ -> pure (Right Null)))
                    { bunExecutable = executable }
            checked <- checkCodeModeAvailability config
            checked `shouldSatisfy` either (Text.isInfixOf "containment denied") (const False)
            readIORef attempts `shouldReturn` 1
        it "does not bypass containment after a failed prewarm" do
            executable <- getExecutablePath
            attempts <- newIORef (0 :: Int)
            let prepare _ _ = modifyIORef' attempts (+ 1) >> pure (Left "containment denied")
                config = (defaultCodeModeConfig prepare executable (\_ _ -> pure (Right Null)))
                    { bunExecutable = executable }
            withCodeModeHost config \host -> do
                result <- execCodeCell host "text(1)" [] 10
                result `shouldSatisfy` \case
                    Left (CodeModeStartupError message) -> "containment denied" `Text.isInfixOf` message
                    _ -> False
                readRunningCodeCells host `shouldReturn` []
            readIORef attempts `shouldReturn` 2
    describe "Aeson-valued shared protocol" do
        it "preserves a successful JSON null result" $
            decodeProtocolMessage "{\"jsonrpc\":\"2.0\",\"id\":\"exec\",\"result\":null}"
                `shouldBe` Right (WorkerExecSucceeded "exec" Null Map.empty)
        it "preserves structured tool arguments" $
            decodeProtocolMessage "{\"jsonrpc\":\"2.0\",\"id\":\"1\",\"method\":\"tool/call\",\"params\":{\"name\":\"echo\",\"arguments\":null}}"
                `shouldBe` Right (WorkerToolInvocation (ToolInvocation "1" "echo" Null))
        it "rejects an ambiguous success and error response" $
            decodeProtocolMessage "{\"jsonrpc\":\"2.0\",\"id\":\"exec\",\"result\":null,\"error\":{\"code\":1,\"message\":\"failed\"}}"
                `shouldSatisfy` either (const True) (const False)
        it "defaults an explicitly null partial result to empty content" $
            decodeProtocolMessage "{\"jsonrpc\":\"2.0\",\"id\":\"exec\",\"error\":{\"code\":1,\"message\":\"failed\"},\"partial_result\":null}"
                `shouldBe` Right (WorkerExecFailed "exec" "failed"
                    (object ["content" .= ([] :: [Value])]) Map.empty)
        it "defaults explicitly null stored-value writes to an empty map" $
            decodeProtocolMessage "{\"jsonrpc\":\"2.0\",\"id\":\"exec\",\"result\":null,\"stored_value_writes\":null}"
                `shouldBe` Right (WorkerExecSucceeded "exec" Null Map.empty)
