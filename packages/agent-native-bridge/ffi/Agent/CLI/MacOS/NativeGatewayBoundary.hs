-- | Native session visibility and callback publication at the gateway boundary.
-- The credential critical sections belong here, not to individual FFI clients.
module Agent.CLI.MacOS.NativeGatewayBoundary
    ( loadNativeGatewayIdentity
    , withNativeGatewayBoundary
    , withNativeGatewayCredentialBoundary
    , ensureNativeGatewayIdentity
    , nativeTurnRouteMatchesBoundary
    , emitForNativeGatewayBoundary
    , emitBoundaryChecked
    , validateNativeSessionBoundary
    , withNativeSessionBoundary
    , nativeSessionMatchesBoundary
    , nativeSessionRouteMatchesBoundary
    ) where

import qualified Agent.CLI.GatewayBoundary as GatewayBoundary
import Agent.CLI.GatewayClient (GatewayCredential)
import Agent.CLI.Session (SessionMeta(..), loadSessionMeta)
import Agent.Store.Postgres.Connection (StorePool)
import Data.Bifunctor (first)
import Data.Either (isRight)
import Data.Text (Text)
import System.OsPath (OsPath)

loadNativeGatewayIdentity :: IO (Either Text (Maybe Text))
loadNativeGatewayIdentity =
    GatewayBoundary.loadGatewayBoundary >>= \case
        Left err ->
            pure (Left (GatewayBoundary.renderGatewayBoundaryError err))
        Right boundary ->
            pure (Right boundary.gatewayBoundaryIdentity)

withNativeGatewayBoundary
    :: (Maybe Text -> IO (Either Text a))
    -> IO (Either Text a)
withNativeGatewayBoundary action =
    withNativeGatewayCredentialBoundary
        (\_ gatewayIdentity -> action gatewayIdentity)

withNativeGatewayCredentialBoundary
    :: (Maybe GatewayCredential -> Maybe Text -> IO (Either Text a))
    -> IO (Either Text a)
withNativeGatewayCredentialBoundary action =
    GatewayBoundary.withCurrentGatewayCredentialBoundary
        (\snapshot ->
            action
                snapshot.gatewayBoundaryCredential
                snapshot.gatewayBoundary.gatewayBoundaryIdentity)
        >>= \case
            Left err ->
                pure
                    (Left
                        (GatewayBoundary.renderGatewayBoundaryError err))
            Right result -> pure result

ensureNativeGatewayIdentity :: Maybe Text -> IO (Either Text ())
ensureNativeGatewayIdentity expected =
    GatewayBoundary.loadGatewayBoundary >>= \case
        Left err ->
            pure (Left (GatewayBoundary.renderGatewayBoundaryError err))
        Right current ->
            pure $
                case
                    GatewayBoundary.validateGatewayBoundary
                        (GatewayBoundary.GatewayBoundary expected)
                        current
                of
                    Left err ->
                        Left (GatewayBoundary.renderGatewayBoundaryError err)
                    Right () -> Right ()

-- | A queued or running native turn belongs to the exact gateway credential
-- identity captured when the turn was accepted. Direct and gateway routes are
-- distinct, as are two successive credentials for the same gateway.
nativeTurnRouteMatchesBoundary :: Maybe Text -> Maybe Text -> Bool
nativeTurnRouteMatchesBoundary expected current =
    GatewayBoundary.gatewayBoundariesMatch
        (GatewayBoundary.GatewayBoundary expected)
        (GatewayBoundary.GatewayBoundary current)

emitForNativeGatewayBoundary
    :: Maybe Text
    -> IO ()
    -> IO (Either Text ())
emitForNativeGatewayBoundary gatewayIdentity emit =
    GatewayBoundary.withExpectedGatewayBoundary
        (GatewayBoundary.GatewayBoundary gatewayIdentity)
        emit >>= \case
            Left err ->
                pure (Left (GatewayBoundary.renderGatewayBoundaryError err))
            Right () -> pure (Right ())

-- | Revalidate immediately before every asynchronous item and terminal
-- callback. If the boundary changes, no later item is emitted.
emitBoundaryChecked
    :: (IO (Either Text ()) -> IO (Either Text ()))
    -> IO (Either Text ())
    -> (item -> IO ())
    -> IO ()
    -> [item]
    -> IO (Either Text ())
emitBoundaryChecked critical check emit terminal = go
  where
    go [] =
        critical $
            check >>= \case
                Left err -> pure (Left err)
                Right () -> terminal >> pure (Right ())
    go (item : remaining) =
        critical
            (check >>= \case
                Left err -> pure (Left err)
                Right () -> emit item >> pure (Right ())) >>= \case
                    Left err -> pure (Left err)
                    Right () -> go remaining

validateNativeSessionBoundary
    :: StorePool
    -> OsPath
    -> Maybe Text
    -> Text
    -> IO (Either Text SessionMeta)
validateNativeSessionBoundary pool root gatewayIdentity sessionId =
    loadSessionMeta pool root sessionId >>= \loaded ->
        pure do
            meta <- loaded
            first GatewayBoundary.renderGatewayBoundaryError $
                GatewayBoundary.validateGatewaySessionBoundary
                    (GatewayBoundary.GatewayBoundary gatewayIdentity)
                    meta.metaConnection
                    meta.metaGatewayIdentity
            pure meta

withNativeSessionBoundary
    :: StorePool
    -> OsPath
    -> Text
    -> (Maybe Text -> SessionMeta -> IO (Either Text a))
    -> IO (Either Text a)
withNativeSessionBoundary pool root sessionId action =
    withNativeGatewayBoundary \gatewayIdentity ->
        validateNativeSessionBoundary
            pool root gatewayIdentity sessionId >>= \case
                Left err -> pure (Left err)
                Right meta -> action gatewayIdentity meta

nativeSessionMatchesBoundary :: Maybe Text -> SessionMeta -> Bool
nativeSessionMatchesBoundary gatewayIdentity meta =
    nativeSessionRouteMatchesBoundary
        gatewayIdentity
        meta.metaConnection
        meta.metaGatewayIdentity

nativeSessionRouteMatchesBoundary
    :: Maybe Text
    -> Text
    -> Maybe Text
    -> Bool
nativeSessionRouteMatchesBoundary gatewayIdentity connection persistedIdentity =
    isRight
        (GatewayBoundary.validateGatewaySessionBoundary
            (GatewayBoundary.GatewayBoundary gatewayIdentity)
            connection
            persistedIdentity)
