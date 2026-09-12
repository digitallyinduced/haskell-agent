-- | Read-only JSON requests. Boundary scopes include the complete snapshot read.
module Agent.CLI.MacOS.NativeRequestHandler
    ( nativeRequestRequiresGatewayLock, handleRequest ) where

import Agent.CLI.MacOS.EngineEvents
import Agent.CLI.MacOS.EngineStore
import Agent.CLI.MacOS.NativeGatewayBoundary
import Agent.CLI.MacOS.NativeModelCatalog
import Agent.CLI.MacOS.NativeRequest
import Agent.Runtime.Session (SessionMeta(..), listSessions, listArchivedSessionIds)
import Agent.CLI.SessionAdmin (loadSessionPageJSON, sessionSummaryWithStatusJSON)
import Agent.Store.Postgres (ManagedPostgresConfig, Store, trustedPool)
import Control.Concurrent.MVar (MVar)
import Control.Exception.Safe (tryAny)
import qualified Data.Aeson as Aeson
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (OsPath)

-- These methods return gateway-scoped session or model data in one event.
-- Keep the final event callback in the same credential critical section as
-- the snapshot query so a completed connect/disconnect cannot overtake it.
nativeRequestRequiresGatewayLock :: Text -> Bool
nativeRequestRequiresGatewayLock = \case
    "sessions.list" -> True
    "sessions.show" -> True
    "models.list" -> True
    _ -> False

handleRequest
    :: ManagedPostgresConfig
    -> MVar (Maybe Store)
    -> OsPath
    -> BridgeRequest
    -> IO Aeson.Value
handleRequest config store root request = do
    result <- tryAny (handleMethod request)
    pure $ either
        (failureEvent request.requestId . Text.pack . show)
        id
        result
  where
    handleMethod current =
        case current.requestMethod of
            "ping" ->
                pure $ successEvent current.requestId $
                    Aeson.object
                        [ "runtime" Aeson..= ("haskell" :: Text)
                        , "protocol" Aeson..= (4 :: Int)
                        ]
            "sessions.list" -> do
                activeStore <- acquireStore config store
                let pool = trustedPool activeStore
                visible <- withNativeGatewayBoundary \gatewayIdentity -> do
                    (sessions, _warnings) <- listSessions pool root
                    archivedIds <- listArchivedSessionIds pool
                    case archivedIds of
                        Left err -> pure (Left err)
                        Right identifiers -> do
                            let archived = Set.fromList identifiers
                                allowed =
                                    filter
                                        (nativeSessionMatchesBoundary
                                            gatewayIdentity)
                                        sessions
                            summaries <- mapM
                                (\session -> sessionSummaryWithStatusJSON
                                    root
                                    (Set.member session.metaId archived)
                                    session)
                                allowed
                            pure (Right summaries)
                pure $ either
                    (failureEvent current.requestId)
                    (successEvent current.requestId)
                    visible
            "sessions.show" ->
                case (parseParams current
                    :: Either Text SessionPageRequest) of
                    Left err ->
                        pure (failureEvent current.requestId err)
                    Right page -> do
                        activeStore <- acquireStore config store
                        let pool = trustedPool activeStore
                        snapshot <- withNativeGatewayBoundary
                            \gatewayIdentity ->
                                validateNativeSessionBoundary
                                    pool
                                    root
                                    gatewayIdentity
                                    page.sessionPageId >>= \case
                                        Left err -> pure (Left err)
                                        Right _ ->
                                            loadSessionPageJSON
                                                pool
                                                root
                                                page.sessionPageId
                                                page.sessionPageBefore
                                                (max 1
                                                    (min 200
                                                        (maybe
                                                            50
                                                            id
                                                            page.sessionPageLimit)))
                        pure $ either
                            (failureEvent current.requestId)
                            (successEvent current.requestId)
                            snapshot
            "turn.agents" ->
                pure $ successEvent current.requestId ([] :: [Aeson.Value])
            "models.list" ->
                case (parseParams current
                    :: Either Text ModelsListRequest) of
                    Left err ->
                        pure (failureEvent current.requestId err)
                    Right parameters -> do
                        activeStore <- acquireStore config store
                        catalogResult <-
                            withNativeGatewayCredentialBoundary
                            \credential gatewayIdentity ->
                                loadNativeModelCatalog
                                    activeStore
                                    root
                                    credential
                                    gatewayIdentity
                                    parameters
                        pure $ either
                            (failureEvent current.requestId)
                            (successEvent current.requestId)
                            catalogResult
            method ->
                pure $ failureEvent current.requestId
                    ("unknown method: " <> method)
