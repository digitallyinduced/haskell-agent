module Main (main) where

import Agent.Server.Client.GatewayIdentitySpec qualified as GatewayIdentitySpec
import Agent.Server.Client.ProtocolSpec qualified as ProtocolSpec
import Agent.Server.ClientSpec qualified as ClientSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
    ClientSpec.spec
    GatewayIdentitySpec.spec
    ProtocolSpec.spec
