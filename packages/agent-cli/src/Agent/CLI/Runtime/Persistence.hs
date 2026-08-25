module Agent.CLI.Runtime.Persistence
    ( preparePersistence
    , shouldPersist
    ) where

import Agent.CLI.Models (ModelTarget(..))
import Agent.CLI.Options (CliOptions(..), isOneShot)
import Agent.CLI.Render (putTextLn)
import Agent.CLI.Session
    ( Persistence(..)
    , SessionCreate(..)
    , SessionHandle(..)
    , SessionMeta(..)
    , SessionTurn
    , newActivePersistence
    , newPendingPersistence
    , sessionLegacySubagentTarget
    , sessionTempDirForId
    , sessionTitleFromPrompt
    , writeSessionMeta
    )
import Agent.CLI.Session.Runtime.Types (StartupRuntime(..))
import Agent.CLI.Style (glyphSession, roleMuted)
import Agent.CLI.Terminal (resolveColor)
import Agent.CLI.TUI.App
    ( emitUiEvent
    )
import Agent.OsPath (fromText)
import Agent.Store.Postgres.Connection (StorePool)
import Agent.TUI.Model (UiEvent(..))
import Control.Monad (when)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import System.OsPath
    ( OsPath
    , unsafeEncodeUtf
    , (</>)
    )

preparePersistence
    :: StorePool
    -> StartupRuntime
    -> CliOptions
    -> OsPath
    -> ModelTarget
    -> Bool
    -> OsPath
    -> Text
    -> Maybe Text
    -> Maybe (SessionMeta, [SessionTurn])
    -> IO Persistence
preparePersistence
        sessionPool startup options root target
        retargetResumed cwd effort prompt resumed =
    case resumed of
        Just (meta, _) -> do
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
                            || meta.metaTransportModel
                                /= Just target.targetWireModelId
                            || isNothing meta.metaLegacySubagentTarget
                           )
                activeMeta
                    | metadataChanged =
                        meta
                            { metaProvider = target.targetProvider
                            , metaConnection = target.targetConnectionId
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
            let message = "session: " <> activeMeta.metaId <> " (resumed)"
            case startup.startupFullscreen of
                Nothing -> do
                    color <- resolveColor startup.startupStderr
                    putTextLn startup.startupStderr
                        (roleMuted color (glyphSession <> message))
                Just runtime ->
                    emitUiEvent runtime (UiSystemMessage message)
            newActivePersistence handle
        Nothing
            | shouldPersist options ->
                -- Defer directory creation until the first successful turn so
                -- an abandoned REPL does not leave empty session folders.
                newPendingPersistence SessionCreate
                    { createPool = sessionPool
                    , createRoot = root
                    , createTarget = target
                    , createCwd = cwd
                    , createEffort = effort
                    , createTitleHint = sessionTitleFromPrompt <$> prompt
                    , createTitleIsManual = False
                    }
            | otherwise -> pure PersistenceDisabled

shouldPersist :: CliOptions -> Bool
shouldPersist options = not (isOneShot options) || options.optSaveSession
