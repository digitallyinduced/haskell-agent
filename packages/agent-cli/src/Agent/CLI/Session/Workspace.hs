-- | Immutable path identity carried from startup into the session.
--
-- These paths have different roles: the working directory can be a nested
-- directory or worktree, the project root scopes project configuration, and
-- the user's home locates user configuration and session storage. Keep them
-- together when handing a session to another runtime rather than resolving
-- them again from the process's mutable working directory.
module Agent.CLI.Session.Workspace
    ( WorkspaceContext(..)
    ) where

import System.OsPath (OsPath)

data WorkspaceContext = WorkspaceContext
    { projectRoot :: !OsPath
    , cwd :: !OsPath
    , home :: !OsPath
    }
    deriving (Eq, Show)
