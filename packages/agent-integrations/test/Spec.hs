module Main (main) where

import qualified Agent.Integrations.AdminSpec as AdminSpec
import qualified Agent.Integrations.RuntimeSpec as RuntimeSpec
import qualified Agent.Integrations.Email.GatewaySpec as GatewaySpec
import qualified Agent.Integrations.Email.MimeSpec as MimeSpec
import qualified Agent.Integrations.Email.OAuthSpec as OAuthSpec
import qualified Agent.Integrations.Email.StoreSpec as StoreSpec
import qualified Agent.Integrations.Email.ToolsSpec as ToolsSpec
import qualified Agent.Integrations.Email.TransportSpec as TransportSpec
import qualified Agent.Integrations.ServerSpec as ServerSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
    AdminSpec.spec
    RuntimeSpec.spec
    StoreSpec.spec
    MimeSpec.spec
    GatewaySpec.spec
    OAuthSpec.spec
    ToolsSpec.spec
    TransportSpec.spec
    ServerSpec.spec
