-- | Provider-neutral result of a mutating-tool permission request.
module Agent.CLI.Permission.Types
    ( PermissionChoice(..)
    ) where

data PermissionChoice
    = PermissionAllowOnce
    | PermissionAllowAll
    | PermissionAllowTool
    | PermissionDeny
    deriving (Eq, Show)
