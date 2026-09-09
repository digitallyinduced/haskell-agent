{-# LANGUAGE ForeignFunctionInterface #-}

module Agent.CLI.MacOS.McpConnectionBridgeSpec (spec) where

import Agent.CLI.MacOS.McpConnectionBridge
import Agent.CLI.MacOS.Marshalling (decodeInput)
import Agent.CLI.McpConnection (McpConnection(..))
import Control.Exception.Safe (bracket)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Foreign (FunPtr, castPtr, freeHaskellFunPtr, nullPtr)
import Foreign.C.Types (CInt(..), CSize(..))
import Test.Hspec (Spec, describe, it, shouldReturn)

foreign import ccall "ha_mcp_connection_validation_smoke"
    connectionValidationSmoke :: IO CInt
foreign import ccall "wrapper"
    wrapConnectionCallback :: McpConnectionCallback -> IO (FunPtr McpConnectionCallback)

spec :: Spec
spec = describe "typed MCP connection operations" do
    it "rejects malformed inputs across every native surface without callbacks" do
        connectionValidationSmoke `shouldReturn` 0
    it "copies Unicode identity fields and distinguishes configured from ready" do
        delivered <- newIORef []
        let callback _ status revision identifier identifierLength label labelLength
                endpoint endpointLength enabled state _ _ = do
                    connectionId <- decodeInput (castPtr identifier) (fromIntegral identifierLength)
                    displayName <- decodeInput (castPtr label) (fromIntegral labelLength)
                    url <- decodeInput (castPtr endpoint) (fromIntegral endpointLength)
                    modifyIORef' delivered (<> [(status, revision, connectionId, displayName, url, enabled, state)])
            connection = McpConnection
                { connectionId = "immutable-account"
                , connectionDisplayName = "Büro — Zürich"
                , connectionUrl = "https://example.invalid/mcp"
                , connectionEnabled = True
                , connectionGeneration = Just "callback-generation"
                }
        bracket (wrapConnectionCallback callback) freeHaskellFunPtr \pointer -> do
            emitConnection pointer nullPtr 17 0 connection
            emitConnection pointer nullPtr 17 3 connection
            emitConnectionTerminal pointer nullPtr 1 17 ""
        readIORef delivered `shouldReturn`
            [ (0, 17, "immutable-account", "Büro — Zürich", "https://example.invalid/mcp", 1, 0)
            , (0, 17, "immutable-account", "Büro — Zürich", "https://example.invalid/mcp", 1, 3)
            , (1, 17, "", "", "", 0, 0)
            ]
    it "preserves observed readiness on reload without inferring it from configuration" do
        let connection = statusConnection "reload"
        observedConnectionState 0 connection `shouldReturn` 0
        observedConnectionState 3 connection `shouldReturn` 3
        observedConnectionState 0 connection `shouldReturn` 3
        observedConnectionState 0 (connection { connectionDisplayName = "Renamed" })
            `shouldReturn` 3
        observedConnectionState 0 (connection { connectionEnabled = False })
            `shouldReturn` 0
        observedConnectionState 0 (connection { connectionGeneration = Just "replacement" })
            `shouldReturn` 0
        observedConnectionState 0 (connection { connectionId = "other-account" })
            `shouldReturn` 0
    it "keeps generation observations independent of completion order" do
        let older = statusConnection "concurrent"
            newer = older { connectionGeneration = Just "newer-generation" }
            failed = newer { connectionGeneration = Just "failed-generation" }
        observedConnectionState 3 newer `shouldReturn` 3
        observedConnectionState 3 older `shouldReturn` 3
        observedConnectionState 0 newer `shouldReturn` 3
        observedConnectionState 0 failed `shouldReturn` 0
    it "does not record readiness without a generation or for a disabled connection" do
        let connection = statusConnection "disabled"
        observedConnectionState 3 (connection { connectionEnabled = False })
            `shouldReturn` 0
        observedConnectionState 0 connection `shouldReturn` 0
        observedConnectionState 3 (connection { connectionGeneration = Nothing })
            `shouldReturn` 0

statusConnection :: Text -> McpConnection
statusConnection identifier = McpConnection
    { connectionId = identifier
    , connectionDisplayName = "Example"
    , connectionUrl = "https://example.invalid/mcp"
    , connectionEnabled = True
    , connectionGeneration = Just "initial-generation"
    }
