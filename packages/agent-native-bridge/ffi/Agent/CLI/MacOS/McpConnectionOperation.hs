{-# LANGUAGE ForeignFunctionInterface #-}

-- | Foreign ownership for one bounded MCP connection operation. The owned
-- worker races a scoped operation against cancellation; destruction joins it.
module Agent.CLI.MacOS.McpConnectionOperation
    ( startMcpConnectionOperation
    , ha_mcp_connection_operation_cancel
    , ha_mcp_connection_operation_destroy
    ) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, readMVar, tryPutMVar)
import Control.Concurrent.Async (Async, asyncWithUnmask, cancel, race, waitCatch)
import Control.Exception.Safe (mask, onException, tryAny)
import Control.Monad (void, when)
import Foreign
    ( Ptr, castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr
    , freeStablePtr, newStablePtr, nullPtr, poke
    )
import Foreign.C.Types (CInt(..))

data McpConnectionOperation = McpConnectionOperation
    { cancellation :: !(MVar ())
    , worker :: !(Async ())
    }

-- | Completion receives 'Nothing' for cancellation or 'Just' for the result.
-- Exceptions are translated by the supplied fixed-message failure handler;
-- exception text must not cross this boundary because HTTP exceptions may
-- include authorization headers and URLs. Completion runs only after the
-- scoped operation and any cancellation cleanup have finished.
startMcpConnectionOperation
    :: Ptr (Ptr ())
    -> IO a
    -> (Maybe a -> IO ())
    -> IO ()
    -> IO CInt
startMcpConnectionOperation output action complete failed = do
    started <- tryAny $
        -- Narrow foreign-ownership transfer. The action itself is unmasked
        -- inside race; publication must register the worker before callbacks.
        mask \_ -> do
            gate <- newEmptyMVar
            cancellation <- newEmptyMVar
            worker <- asyncWithUnmask \unmask -> do
                readMVar gate
                outcome <- tryAny (unmask (race (readMVar cancellation) action))
                case outcome of
                    Left _ -> failed
                    Right (Left ()) -> complete Nothing
                    Right (Right result) -> complete (Just result)
            let operation = McpConnectionOperation{cancellation, worker}
            stable <- newStablePtr operation `onException` cancel worker
            (poke output (castStablePtrToPtr stable) >> putMVar gate ())
                `onException` (cancel worker >> freeStablePtr stable)
    pure (either (const 3) (const 0) started)

foreign export ccall ha_mcp_connection_operation_cancel :: Ptr () -> IO ()
foreign export ccall ha_mcp_connection_operation_destroy :: Ptr () -> IO ()

ha_mcp_connection_operation_cancel :: Ptr () -> IO ()
ha_mcp_connection_operation_cancel pointer =
    when (pointer /= nullPtr) do
        operation <- deRefStablePtr (castPtrToStablePtr pointer)
        void (tryPutMVar (operation :: McpConnectionOperation).cancellation ())

ha_mcp_connection_operation_destroy :: Ptr () -> IO ()
ha_mcp_connection_operation_destroy pointer =
    when (pointer /= nullPtr) do
        let stable = castPtrToStablePtr pointer
        operation <- deRefStablePtr stable
        void (tryPutMVar (operation :: McpConnectionOperation).cancellation ())
        void (waitCatch operation.worker)
        freeStablePtr stable
