module Agent.Runtime.Startup.PolicySpec (spec) where

import Agent.Runtime.Options (ApprovalPolicy(..))
import Agent.Runtime.Startup.Policy
import Test.Hspec

spec :: Spec
spec = describe "startup approval policy" do
    mapM_ (\(label, inputs, expected) ->
        it label $ resolveApproval inputs `shouldBe` expected)
        [ ("prompts in an interactive session", defaults, PromptMutating)
        , ("inherits project approval interactively",
            defaults{approvalProjectAutoApprove = True}, ApproveAll)
        , ("explicit opt-out overrides project approval",
            defaults{approvalYolo = Just False, approvalProjectAutoApprove = True}, PromptMutating)
        , ("denies an unattended interactive session",
            defaults{approvalInteractive = False, approvalProjectAutoApprove = True}, DenyMutating)
        , ("auto-approves unattended one-shot work",
            defaults{approvalInteractive = False, approvalOneShot = True}, ApproveAll)
        , ("explicit opt-out denies unattended one-shot work",
            defaults{approvalYolo = Just False, approvalInteractive = False, approvalOneShot = True}, DenyMutating)
        , ("managed prompt policy uses the host instead of stdin",
            defaults{approvalYolo = Just False, approvalManagedTurn = True, approvalInteractive = False}, PromptMutating)
        , ("managed deny wins over the managed prompt exception",
            defaults{approvalYolo = Just False, approvalManagedTurn = True, approvalDenyMutations = True}, DenyMutating)
        , ("explicit yolo retains precedence over managed deny",
            defaults{approvalYolo = Just True, approvalDenyMutations = True}, ApproveAll)
        ]
    mapM_ (\(mode, expected) -> it ("resolves native " <> show mode) do
        resolveNativeApproval mode `shouldBe` expected
        claudeBypassEnabled (Just (mode, True)) (Just True) True `shouldBe` False
        claudeBypassEnabled (Just (mode, False)) (Just False) False
            `shouldBe` (mode == NativeApprovalYolo))
        [(NativeApprovalYolo, ApproveAll), (NativeApprovalAsk, PromptMutating), (NativeApprovalPlan, PromptMutating)]
    it "only bypasses Claude approval when inherited or explicit yolo allows it" do
        map (\(override, project) -> claudeBypassEnabled Nothing override project)
            [(Nothing, False), (Nothing, True), (Just False, True), (Just True, False)]
            `shouldBe` [False, True, False, True]

defaults :: ApprovalInputs
defaults = ApprovalInputs
    { approvalYolo = Nothing
    , approvalDenyMutations = False
    , approvalManagedTurn = False
    , approvalOneShot = False
    , approvalInteractive = True
    , approvalProjectAutoApprove = False
    }
