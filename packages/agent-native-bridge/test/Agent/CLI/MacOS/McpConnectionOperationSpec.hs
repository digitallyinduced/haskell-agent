module Agent.CLI.MacOS.McpConnectionOperationSpec (spec) where

import Agent.CLI.MacOS.McpConnectionOperation
import Control.Concurrent (newEmptyMVar, putMVar, readMVar)
import Control.Exception.Safe (finally)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Foreign (alloca, nullPtr, peek, poke)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

spec :: Spec
spec = describe "MCP connection operation ownership" do
    it "joins cancellation cleanup before completing and releasing ownership" do
        entered <- newEmptyMVar
        blocked <- newEmptyMVar
        cleaned <- newIORef False
        delivered <- newIORef ([] :: [(Maybe (), Bool)])
        alloca \output -> do
            poke output nullPtr
            status <- startMcpConnectionOperation output
                ((putMVar entered () >> readMVar blocked)
                    `finally` writeIORef cleaned True)
                (\result -> do
                    cleanupFinished <- readIORef cleaned
                    modifyIORef' delivered (<> [(result, cleanupFinished)]))
                (error "unexpected operation failure")
            status `shouldBe` 0
            readMVar entered
            operation <- peek output
            ha_mcp_connection_operation_cancel operation
            ha_mcp_connection_operation_cancel operation
            ha_mcp_connection_operation_destroy operation
        readIORef delivered `shouldReturn` [(Nothing, True)]
    it "delivers successful results exactly once before destruction returns" do
        completed <- newEmptyMVar
        delivered <- newIORef ([] :: [Maybe Int])
        alloca \output -> do
            poke output nullPtr
            startMcpConnectionOperation output (pure 42)
                (\result -> modifyIORef' delivered (<> [result]) >> putMVar completed ())
                (error "unexpected operation failure")
                `shouldReturn` 0
            readMVar completed
            peek output >>= ha_mcp_connection_operation_destroy
        readIORef delivered `shouldReturn` [Just 42]
    it "does not expose exception text through its failure callback" do
        completed <- newEmptyMVar
        delivered <- newIORef (0 :: Int)
        alloca \output -> do
            poke output nullPtr
            startMcpConnectionOperation output (ioError (userError "secret-token"))
                (\(_ :: Maybe ()) -> error "unexpected successful completion")
                (modifyIORef' delivered (+ 1) >> putMVar completed ())
                `shouldReturn` 0
            readMVar completed
            peek output >>= ha_mcp_connection_operation_destroy
        readIORef delivered `shouldReturn` 1
