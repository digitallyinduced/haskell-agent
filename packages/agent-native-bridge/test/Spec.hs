{-# LANGUAGE CPP #-}

module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.CLI.BrowserToolsSpec as BrowserToolsSpec
import qualified Agent.CLI.MacOS.EngineMailboxSpec as EngineMailboxSpec
import qualified Agent.CLI.MacOS.NativeLoopEventSpec as NativeLoopEventSpec
import qualified Agent.CLI.MacOS.RepositoryWorkersSpec as RepositoryWorkersSpec
import qualified Agent.CLI.MacOS.RepositoryInputSpec as RepositoryInputSpec
import qualified Agent.CLI.MacOS.TaskSchedulerSpec as TaskSchedulerSpec
import qualified Agent.CLI.McpAdminSpec as McpAdminSpec
import qualified Agent.CLI.ResourceAdminSpec as ResourceAdminSpec
#ifdef darwin_HOST_OS
import qualified Agent.CLI.MacOS.AccountConnectionSpec as AccountConnectionSpec
import qualified Agent.CLI.MacOS.BridgeFFISpec as BridgeFFISpec
import qualified Agent.CLI.MacOS.BridgeHeaderSpec as BridgeHeaderSpec
import qualified Agent.CLI.MacOS.BridgeSpec as BridgeSpec
import qualified Agent.CLI.MacOS.BrowserBridgeFFISpec as BrowserBridgeFFISpec
import qualified Agent.CLI.MacOS.ComputerBridgeSpec as ComputerBridgeSpec
#endif

main :: IO ()
main = hspec do
    BrowserToolsSpec.spec
    EngineMailboxSpec.spec
    McpAdminSpec.spec
    NativeLoopEventSpec.spec
    ResourceAdminSpec.spec
    TaskSchedulerSpec.spec
    RepositoryWorkersSpec.spec
    RepositoryInputSpec.spec
#ifdef darwin_HOST_OS
    AccountConnectionSpec.spec
    BrowserBridgeFFISpec.spec
    BridgeFFISpec.spec
    BridgeHeaderSpec.spec
    BridgeSpec.spec
    ComputerBridgeSpec.spec
#endif
