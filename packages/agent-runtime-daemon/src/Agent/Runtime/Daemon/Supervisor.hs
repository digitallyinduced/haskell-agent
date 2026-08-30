module Agent.Runtime.Daemon.Supervisor
    ( Supervisor (..)
    , unavailableSupervisor
    ) where

import Data.Aeson (Value)
import Data.Text (Text)

import Agent.Runtime.Daemon.Protocol (CommandId)

-- | Integration seam for the task supervisor. The daemon foundation owns
-- transport and durability; a supervisor owns task execution and commands.
newtype Supervisor = Supervisor
    { handleCommand :: CommandId -> Value -> IO (Either Text Value)
    }

unavailableSupervisor :: Supervisor
unavailableSupervisor =
    Supervisor
        { handleCommand = \_ _ -> pure (Left "task supervisor is not configured")
        }
