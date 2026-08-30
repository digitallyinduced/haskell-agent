module Main (main) where

import qualified Agent.MCPSpec as MCPSpec
import qualified Agent.MCP.InProcessSpec as InProcessSpec
import qualified Agent.MCP.OAuthSpec as MCPOAuthSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
    MCPSpec.spec
    InProcessSpec.spec
    MCPOAuthSpec.spec
