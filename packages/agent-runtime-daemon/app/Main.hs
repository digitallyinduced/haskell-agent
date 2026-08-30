module Main (main) where

import Agent.Runtime.Daemon

main :: IO ()
main = do
    config <- defaultDaemonConfig
    runner <- processTaskRunner
    runTaskDaemon config runner
