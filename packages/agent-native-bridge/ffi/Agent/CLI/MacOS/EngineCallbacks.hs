{-# LANGUAGE ForeignFunctionInterface #-}

-- | Callback signatures shared by engine commands and their workers.
module Agent.CLI.MacOS.EngineCallbacks
    ( SessionResultCallback
    , invokeSessionResultCallback
    , SearchCallback
    , invokeSearchCallback
    , TaskSnapshotCallback
    , invokeTaskSnapshotCallback
    , IntegrationResultCallback
    , invokeIntegrationResultCallback
    ) where

import Data.Int (Int64)
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CSize(..), CDouble(..))
import Foreign.Ptr (FunPtr, Ptr)

type SessionResultCallback =
    Ptr () -> CInt -> CString -> CSize -> IO ()

-- Status is 0 for a JSON result and -1 for a failure. Every pointer is scoped
-- to the callback invocation.
type IntegrationResultCallback =
    Ptr () -> CInt
    -> CString -> CSize
    -> CString -> CSize
    -> IO ()

-- Status is 0 for a result, 1 for completion, and -1 for failure. Every
-- pointer is callback-scoped UTF-8. A turn index of -1 denotes a metadata hit;
-- role is 0 (metadata), 1 (user), or 2 (assistant).
type SearchCallback =
    Ptr () -> CInt
    -> Ptr Word8 -> CSize -- session id
    -> Ptr Word8 -> CSize -- title
    -> Ptr Word8 -> CSize -- cwd
    -> Ptr Word8 -> CSize -- provider
    -> Ptr Word8 -> CSize -- model
    -> Int64 -> CInt -> Int64 -> Int64 -> CInt
    -> Ptr Word8 -> CSize -- user
    -> Ptr Word8 -> CSize -- assistant
    -> CDouble
    -> Ptr Word8 -> CSize -- error
    -> IO ()

-- Status is 0 for an active task, 1 for completion, and -1 for failure.
-- State is 0 for queued and 1 for running. Every pointer is callback-scoped.
type TaskSnapshotCallback =
    Ptr () -> CInt
    -> Ptr Word8 -> CSize -- task id
    -> Ptr Word8 -> CSize -- session id, optional
    -> CInt
    -> Ptr Word8 -> CSize -- error
    -> IO ()

foreign import ccall "dynamic"
    invokeSessionResultCallback
        :: FunPtr SessionResultCallback -> SessionResultCallback

foreign import ccall "dynamic"
    invokeSearchCallback :: FunPtr SearchCallback -> SearchCallback

foreign import ccall "dynamic"
    invokeTaskSnapshotCallback
        :: FunPtr TaskSnapshotCallback -> TaskSnapshotCallback

foreign import ccall "dynamic"
    invokeIntegrationResultCallback
        :: FunPtr IntegrationResultCallback -> IntegrationResultCallback
