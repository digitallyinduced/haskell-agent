-- | Frontend-neutral creation and retargeting of session persistence.
module Agent.Runtime.Session.Preparation
    ( PersistenceRequest(..)
    , prepareSessionPersistence
    ) where

import Agent.Runtime.Models (ModelTarget(..))
import Agent.Runtime.Session
    ( Persistence(..), SessionCreate(..), SessionHandle(..), SessionMeta(..)
    , newActivePersistence, newPendingPersistence, sessionLegacySubagentTarget
    , sessionTempDirForId, sessionTitleFromPrompt, writeSessionMeta )
import Agent.OsPath (fromText)
import Agent.Store.Postgres.Connection (StorePool)
import Control.Monad (when)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))

data PersistenceRequest = PersistenceRequest
    { persistencePool :: StorePool
    , persistenceRoot :: OsPath
    , persistenceTarget :: ModelTarget
    , persistenceGatewayIdentity :: Maybe Text
    , persistenceRetargetResumed :: Bool
    , persistenceCwd :: OsPath
    , persistenceEffort :: Text
    , persistencePrompt :: Maybe Text
    , persistenceResumed :: Maybe SessionMeta
    , persistenceEnabled :: Bool
    }

prepareSessionPersistence :: PersistenceRequest -> IO Persistence
prepareSessionPersistence PersistenceRequest
    { persistencePool = sessionPool
    , persistenceRoot = root
    , persistenceTarget = target
    , persistenceGatewayIdentity = gatewayIdentity
    , persistenceRetargetResumed = retargetResumed
    , persistenceCwd = cwd
    , persistenceEffort = effort
    , persistencePrompt = prompt
    , persistenceResumed = resumed
    , persistenceEnabled = enabled
    } =
    case resumed of
        Just meta -> do
            now <- getCurrentTime
            let targetChanged =
                    retargetResumed
                        && ( target.targetProvider /= meta.metaProvider
                            || target.targetConnectionId /= meta.metaConnection
                            || target.targetModelId /= meta.metaModel
                            || maybe
                                False
                                (/= target.targetWireModelId)
                                meta.metaTransportModel
                            || target.targetDialect /= meta.metaDialect
                           )
                metadataChanged =
                    retargetResumed
                        && ( targetChanged
                            || meta.metaGatewayIdentity /= gatewayIdentity
                            || meta.metaTransportModel
                                /= Just target.targetWireModelId
                            || isNothing meta.metaLegacySubagentTarget
                           )
                activeMeta
                    | metadataChanged =
                        meta
                            { metaProvider = target.targetProvider
                            , metaConnection = target.targetConnectionId
                            , metaGatewayIdentity = gatewayIdentity
                            , metaModel = target.targetModelId
                            , metaTransportModel =
                                Just target.targetWireModelId
                            , metaDialect = target.targetDialect
                            , metaLegacySubagentTarget =
                                Just (sessionLegacySubagentTarget meta)
                            , metaLastResponseId =
                                if targetChanged
                                    then Nothing
                                    else meta.metaLastResponseId
                            , metaUpdatedAt = now
                            }
                    | otherwise = meta
            let handle = SessionHandle
                    { sessionPool = sessionPool
                    , sessionDir = root </> fromText activeMeta.metaId
                    , sessionTempDir =
                        either
                            (error . Text.unpack)
                            id
                            (sessionTempDirForId root activeMeta.metaId)
                    , sessionMetaPath =
                        root
                            </> fromText activeMeta.metaId
                            </> unsafeEncodeUtf "meta.json"
                    , sessionTranscriptPath =
                        root
                            </> fromText activeMeta.metaId
                            </> unsafeEncodeUtf "transcript.jsonl"
                    , sessionMeta = activeMeta
                    }
            when metadataChanged $
                writeSessionMeta
                    handle.sessionPool
                    handle.sessionMetaPath
                    activeMeta
            newActivePersistence handle
        Nothing
            | enabled ->
                -- Defer directory creation until the first successful turn so
                -- an abandoned REPL does not leave empty session folders.
                newPendingPersistence SessionCreate
                    { createPool = sessionPool
                    , createRoot = root
                    , createTarget = target
                    , createGatewayIdentity = gatewayIdentity
                    , createCwd = cwd
                    , createEffort = effort
                    , createTitleHint = sessionTitleFromPrompt <$> prompt
                    , createTitleIsManual = False
                    }
            | otherwise -> pure PersistenceDisabled
