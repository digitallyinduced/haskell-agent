-- | Execution language selection, independent of provider tool projection.
module Agent.Tools.CodeMode.Backend
    ( CodeModeBackend(..)
    ) where

data CodeModeBackend
    = JavaScriptBackend
    | HaskellBackend
    deriving (Eq, Show)
