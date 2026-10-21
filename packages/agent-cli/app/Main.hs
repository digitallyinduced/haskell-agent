module Main (main) where

import Agent.CLI (run)
import Agent.Runtime.Portable (configurePortableBundle)
import HeapDiagnostics (withHeapDiagnostics)

main :: IO ()
main = configurePortableBundle >> withHeapDiagnostics run
