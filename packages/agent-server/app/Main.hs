module Main (main) where

import Agent.Server (runServer)
import Agent.Runtime.Portable (configurePortableBundle)

main :: IO ()
main = configurePortableBundle >> runServer
