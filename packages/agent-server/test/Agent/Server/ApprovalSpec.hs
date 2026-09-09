module Agent.Server.ApprovalSpec (spec) where

import Agent.CLI.Permission.Types (PermissionChoice(..))
import Agent.Server.Runtime (requestFreshToolApproval)
import Agent.Server.Types
import Agent.ToolDispatch (ToolCall(..), ToolCallKind(..))
import Control.Monad (forM_)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text qualified as Text
import Test.Hspec

spec :: Spec
spec = describe "server fresh tool approval" do
    it "prompts for every exact invocation and offers only once or deny" do
        requests <- newIORef []
        let requestInput request = do
                modifyIORef' requests (<> [request])
                pure (Right (HumanResponse "allow_once" Nothing))
        requestFreshToolApproval requestInput call
            `shouldReturn` Just PermissionAllowOnce
        requestFreshToolApproval requestInput call
            `shouldReturn` Just PermissionAllowOnce
        captured <- readIORef requests
        length captured `shouldBe` 2
        forM_ captured \request -> do
            request.humanRequestSpecKind `shouldBe` ToolApprovalRequest
            request.humanRequestSpecOptions `shouldBe` ["allow_once", "deny"]
            request.humanRequestSpecPrompt `shouldSatisfy` Text.isInfixOf call.arguments
            request.humanRequestSpecPrompt `shouldSatisfy` Text.isInfixOf call.callId
            request.humanRequestSpecPrompt `shouldSatisfy` Text.isInfixOf call.name

    it "accepts denial and fails closed for broad or missing decisions" do
        requestFreshToolApproval (const (pure (Right (HumanResponse "deny" Nothing)))) call
            `shouldReturn` Just PermissionDeny
        forM_ ["allow_tool", "allow_all", "unknown"] \decision ->
            requestFreshToolApproval (const (pure (Right (HumanResponse decision Nothing)))) call
                `shouldReturn` Nothing
        requestFreshToolApproval (const (pure (Left "cancelled"))) call
            `shouldReturn` Nothing

call :: ToolCall
call = ToolCall
    { callId = "fresh-call"
    , name = "shell_command"
    , arguments = "{\"command\":\"pwd\",\"sandbox_permissions\":\"require_escalated\",\"justification\":\"Inspect directory\"}"
    , callKind = FunctionCallKind
    , argumentsEncrypted = False
    }
