module Main (main) where

import qualified Agent.CLI.SessionActivitySpec as SessionActivitySpec
import Agent.CLI.Session.TitlePolicy (titleRefreshIndex)
import qualified Agent.CLI.CredentialStoreSpec as CredentialStoreSpec
import qualified Agent.CLI.EnvironmentSpec as EnvironmentSpec
import qualified Agent.CLI.ErrorSpec as ErrorSpec
import qualified Agent.CLI.GatewayBoundarySpec as GatewayBoundarySpec
import qualified Agent.CLI.GatewayBridgeSpec as GatewayBridgeSpec
import qualified Agent.CLI.GatewayClientSpec as GatewayClientSpec
import qualified Agent.CLI.ManagedTurnSpec as ManagedTurnSpec
import qualified Agent.CLI.ModelConfigSpec as ModelConfigSpec
import qualified Agent.CLI.ModelsSpec as ModelsSpec
import qualified Agent.CLI.SessionSpec as SessionSpec
import Test.Hspec

main :: IO ()
main = hspec do
    SessionActivitySpec.spec
    describe "titleRefreshIndex" do
        it "advances only at the persisted title milestones" do
            map titleRefreshIndex [0, 1, 2, 3, 5, 6, 10]
                `shouldBe` [0, 0, 0, 1, 1, 2, 2]
    CredentialStoreSpec.spec
    EnvironmentSpec.spec
    ErrorSpec.spec
    GatewayBoundarySpec.spec
    GatewayBridgeSpec.spec
    GatewayClientSpec.spec
    ManagedTurnSpec.spec
    ModelConfigSpec.spec
    ModelsSpec.spec
    SessionSpec.spec
