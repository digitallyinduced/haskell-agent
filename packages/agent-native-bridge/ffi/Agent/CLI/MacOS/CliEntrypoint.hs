{-# LANGUAGE ForeignFunctionInterface #-}

-- | Private connection between the process-owning C entrypoint and the CLI.
module Agent.CLI.MacOS.CliEntrypoint () where

import qualified Agent.CLI as CLI
import Foreign.StablePtr (StablePtr, newStablePtr)

-- The C owner submits this action with rts_evalStableIOMain, which installs
-- GHC's ordinary main-thread exception and signal handlers. An ordinary
-- foreign-export invocation of CLI.run would not establish those handlers.
foreign export ccall "haskell_agent_cli_main_action"
    cliMainAction :: IO (StablePtr (IO ()))

cliMainAction :: IO (StablePtr (IO ()))
cliMainAction = newStablePtr CLI.run
