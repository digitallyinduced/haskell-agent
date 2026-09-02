module Main (main) where

import Test.Hspec (hspec)
import Agent.Server.ApplicationSpec qualified
import Agent.Server.AuthSpec qualified
import Agent.Server.ConfigSpec qualified
import Agent.Server.EventSpec qualified
import Agent.Server.SupervisorSpec qualified

main :: IO ()
main = hspec do
    Agent.Server.AuthSpec.spec
    Agent.Server.ConfigSpec.spec
    Agent.Server.EventSpec.spec
    Agent.Server.SupervisorSpec.spec
    Agent.Server.ApplicationSpec.spec
