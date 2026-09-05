module Agent.CLI.SessionSpec.Fixtures
    ( fromFilePath
    , toFilePath
    , sampleTaskPlan
    , sampleStoredTaskPlanItems
    , sampleStoredTaskPlan
    , testCreate
    , testMeta
    , testPromptSnapshot
    , promptFunctionTool
    , asyncPromptFunctionTool
    , fixedTime
    , sampleTurnTelemetry
    , modeOf
    , withTempSessionRoot
    , addSessionTemp
    , withTempStore
    ) where

import Agent.CLI.Session
import Agent.CLI.Models (ModelTarget(..))
import Agent.Dialect (DialectId(..))
import Agent.Provider (Provider(..))
import Agent.Responses.Types
import Agent.Telemetry (TurnTelemetry(..))
import Agent.Store.Postgres
    ( Store
    , closeStore
    , defaultManagedPostgresConfig
    , openStore
    , storeConfig
    )
import Agent.Store.Postgres.Managed (stopManagedPostgres)
import Agent.Store.Postgres.Connection (StorePool)
import qualified Agent.Store.Postgres.Session as Store
import Agent.Store.Types (renderStoreError)
import Agent.Tools.TaskPlan (TaskPlan(..), TaskPlanItem(..), TaskPlanStatus(..))
import Control.Exception.Safe (bracket)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf, (</>))
import System.Posix.Files (fileMode, getFileStatus)
import System.Posix.Temp (mkdtemp)

fromFilePath :: FilePath -> OsPath
fromFilePath = unsafeEncodeUtf

toFilePath :: OsPath -> FilePath
toFilePath path = either (error . show) id (decodeUtf path)

sampleTaskPlan :: TaskPlan
sampleTaskPlan = TaskPlan
    { taskPlanExplanation = Just "Keep durable progress."
    , taskPlanItems =
        [ TaskPlanItem "persist the plan" TaskPlanCompleted
        , TaskPlanItem "resume the plan" TaskPlanInProgress
        ]
    }

sampleStoredTaskPlanItems :: [Store.SessionTaskPlanItem]
sampleStoredTaskPlanItems =
    [ Store.SessionTaskPlanItem "persist the plan" Store.SessionTaskPlanCompleted
    , Store.SessionTaskPlanItem "resume the plan" Store.SessionTaskPlanInProgress
    ]

sampleStoredTaskPlan :: Store.SessionTaskPlan
sampleStoredTaskPlan = Store.SessionTaskPlan
    { Store.sessionTaskPlanRevision = 1
    , Store.sessionTaskPlanExplanation = Just "Keep durable progress."
    , Store.sessionTaskPlanItems = sampleStoredTaskPlanItems
    }

testCreate :: StorePool -> OsPath -> SessionCreate
testCreate pool root = SessionCreate
    { createPool = pool
    , createRoot = root
    , createTarget = ModelTarget
        { targetProvider = XAIProvider
        , targetConnectionId = "xai"
        , targetModelId = "grok-4"
        , targetWireModelId = "grok-4"
        , targetDialect = GrokBuildDialect
        }
    , createGatewayIdentity = Nothing
    , createCwd = fromFilePath "/tmp/work"
    , createEffort = "low"
    , createTitleHint = Nothing
    , createTitleIsManual = False
    }

testMeta :: Text.Text -> SessionMeta
testMeta sessionId = SessionMeta
    { metaVersion = 1
    , metaId = sessionId
    , metaCreatedAt = fixedTime
    , metaUpdatedAt = fixedTime
    , metaProvider = XAIProvider
    , metaConnection = "xai"
    , metaGatewayIdentity = Nothing
    , metaModel = "grok-4"
    , metaTransportModel = Just "grok-4"
    , metaDialect = GrokBuildDialect
    , metaLegacySubagentTarget = Just LegacySubagentTarget
        { legacyTargetProvider = XAIProvider
        , legacyTargetConnection = "xai"
        , legacyTargetEffectiveModel = "grok-4"
        , legacyTargetDialect = GrokBuildDialect
        }
    , metaCwd = fromFilePath "/tmp/work"
    , metaEffort = "low"
    , metaTitle = "legacy"
    , metaTitleIsManual = False
    , metaTitleRefreshIndex = 0
    , metaTitleUserTurns = 0
    , metaLastResponseId = Nothing
    , metaInputTokens = 0
    , metaOutputTokens = 0
    , metaCachedTokens = 0
    , metaLastRecap = Nothing
    , metaLastTurnSummary = Nothing
    , metaLastRecapMainTurns = 0
    , metaPromptSnapshot = Nothing
    }

testPromptSnapshot :: Text.Text -> SessionPromptSnapshot
testPromptSnapshot sessionId = SessionPromptSnapshot
    { promptSnapshotVersion = 1
    , promptSnapshotCreatedAt = fixedTime
    , promptSnapshotProvider = XAIProvider
    , promptSnapshotConnection = "xai"
    , promptSnapshotModel = "grok-4"
    , promptSnapshotDialect = GrokBuildDialect
    , promptSnapshotCwd = fromFilePath "/tmp/work"
    , promptSnapshotInstructions = "persisted instructions"
    , promptSnapshotTools = []
    , promptSnapshotGeneratedContext = Just "project and skill context"
    , promptSnapshotGrokContext = Just "grok first-turn context"
    , promptSnapshotCacheKey = sessionId
    }

promptFunctionTool :: Text.Text -> Text.Text -> ResponseTool
promptFunctionTool toolName documentation =
    FunctionToolValue FunctionTool
        { name = toolName
        , description = Just documentation
        , parameters = Nothing
        , strict = Just True
        , async = Nothing
        }

asyncPromptFunctionTool :: Text.Text -> Text.Text -> ResponseTool
asyncPromptFunctionTool toolName documentation =
    FunctionToolValue FunctionTool
        { name = toolName
        , description = Just documentation
        , parameters = Nothing
        , strict = Just True
        , async = Just True
        }

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2026 8 19) (secondsToDiffTime 0)

sampleTurnTelemetry :: TurnTelemetry
sampleTurnTelemetry = TurnTelemetry
    { telemetryDurationMs = Just 1250
    , telemetryApiDurationMs = Just 1100
    , telemetryCostUsd = Just 0.0125
    , telemetryStopReason = Just "end_turn"
    , telemetryProviderTurns = Just 2
    , telemetryModels = mempty
    , telemetryStructuredOutput = Nothing
    }

modeOf :: OsPath -> IO Integer
modeOf path = do
    status <- getFileStatus (toFilePath path)
    pure (fromIntegral (fileMode status `mod` 0o1000))

withTempSessionRoot :: (OsPath -> IO a) -> IO a
withTempSessionRoot action = do
    tmp <- Directory.getTemporaryDirectory
    bracket
        (mkdtemp (tmp FilePath.</> "agent-session-temp-XXXXXX"))
        Directory.removeDirectoryRecursive
        \basePath -> do
            let root =
                    fromFilePath
                        (basePath
                            FilePath.</> ".haskell-agent"
                            FilePath.</> "sessions")
            Directory.createDirectoryIfMissing True (toFilePath root)
            action root

addSessionTemp :: OsPath -> String -> IO OsPath
addSessionTemp root sessionId = do
    let path =
            sessionTempsRoot root
                </> fromFilePath sessionId
    Directory.createDirectoryIfMissing True (toFilePath path)
    pure path

withTempStore :: (Store -> OsPath -> IO a) -> IO a
withTempStore action = do
    tmp <- Directory.getTemporaryDirectory
    bracket
        (mkdtemp (tmp FilePath.</> "hs"))
        Directory.removeDirectoryRecursive
        \basePath -> do
            let
                stateDirectory = basePath FilePath.</> ".haskell-agent"
                sessionsDirectory =
                    stateDirectory FilePath.</> "sessions"
                config = defaultManagedPostgresConfig stateDirectory ""
            Directory.createDirectoryIfMissing True sessionsDirectory
            bracket
                (openStore config >>= either
                    (fail . Text.unpack . renderStoreError)
                    pure)
                (\store -> do
                    closeStore store
                    _ <- stopManagedPostgres (storeConfig store)
                    pure ())
                (\store -> action store (fromFilePath sessionsDirectory))
