module Main (main) where

import Test.Hspec (hspec)
import Agent.Server.ApplicationSpec qualified
import Agent.Server.AuthSpec qualified
import Agent.Server.ConfigSpec qualified
import Agent.Server.EventSpec qualified
import Agent.Server.SandboxSpec qualified
import Agent.Server.SupervisorSpec qualified
import Agent.Server.TenantSpec qualified
import System.Environment (getArgs)

main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        "serve" : _ ->
            Agent.Server.SandboxSpec.fakeSandboxRunner arguments
        _ ->
            hspec do
                Agent.Server.AuthSpec.spec
                Agent.Server.ConfigSpec.spec
                Agent.Server.EventSpec.spec
                Agent.Server.SupervisorSpec.spec
                Agent.Server.TenantSpec.spec
                Agent.Server.SandboxSpec.spec
                Agent.Server.ApplicationSpec.spec
