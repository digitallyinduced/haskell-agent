module Agent.Runtime.StartupPolicySpec (spec) where

import Agent.Runtime.StartupPolicy
import Test.Hspec

spec :: Spec
spec = describe "native startup permissions" do
    it "requires both context and execution restrictions for the restricted preset" do
        restrictedNativeStartupPolicy
            `shouldBe` NativeStartupPolicy SuppliedContextOnly TurnScopedFacilities

    it "retains both legacy host permissions for the host preset" do
        hostNativeStartupPolicy
            `shouldBe` NativeStartupPolicy WorkspaceContextAllowed HostStartupFacilities
