-- | Production PostgreSQL and in-process native-runtime adapter.
module Agent.Server.Runtime
    ( ServerRuntime
    , openServerRuntime
    , closeServerRuntime
    , serverRuntimeBackend
    ) where

import Agent.CLI.GatewayBoundary
    ( GatewayBoundaryError(..)
    , GatewayBoundarySnapshot(..)
    , loadGatewayBoundarySnapshotAt
    , renderGatewayBoundaryError
    , validateGatewayBoundary
    , withCurrentGatewayBoundaryAt
    , withExpectedGatewayBoundaryAt
    , withGatewayTurnBoundaryAt
    )
import Agent.CLI.GatewayModels
    ( loadGatewayModelOptionsWithCredentialAt )
import Agent.CLI.ModelConfig
    ( ModelCatalog
    , organizationGatewayConnectionId
    )
import Agent.CLI.Models
    ( ModelOption(..)
    , ModelTarget(..)
    , defaultModelOptionFor
    , modelCatalog
    )
import Agent.CLI.NativeRuntime
    ( NativeInteractionMode(..)
    , NativeProcessRuntime
    , NativeRunHooks(..)
    , NativeSessionTarget(..)
    , NativeShellMode(..)
    , NativeTurnRequest(..)
    , closeNativeProcessRuntime
    , newNativeProcessRuntime
    , runNativeTurn
    )
import Agent.CLI.Permission.Types (PermissionChoice(..))
import Agent.CLI.Runtime.Options (defaultEffortFor)
import Agent.CLI.Session
    ( SessionCreate(..)
    , SessionHandle(..)
    , SessionMeta(..)
    , SessionTurnPage(..)
    , createSession
    , deleteSession
    , forkSessionAt
    , forkSessionAtTurn
    , isValidSessionId
    , loadRecentSessionHistoryTurns
    , loadSessionHandle
    , loadSessionHistoryTurnsBefore
    , renameSession
    , sessionsRoot
    , setSessionArchived
    )
import Agent.CLI.Session.Codec (fromStoredMetadata)
import Agent.Dialect (dialectSlug)
import Agent.OsPath (unsafeToFilePath)
import Agent.Provider
    ( Provider(..)
    , providerSlug
    )
import Agent.ReasoningEffort
    ( ReasoningEffort
    , parseReasoningEffort
    , reasoningEffortText
    )
import Agent.Server.Backend (Backend(..))
import Agent.Server.Config
    ( ResolvedServerConfig(..)
    , resolveWorkspacePath
    )
import Agent.Server.Event
    ( boundedPublicText
    , projectAgentEntries
    , projectLoopEvent
    , projectPublicValue
    )
import Agent.Server.Supervisor (TurnControl(..))
import Agent.Server.Types
import Agent.Store.Postgres
    ( Store
    , closeStore
    , managedPostgresConfigFromEnv
    , openStore
    , trustedPool
    )
import Agent.Store.Postgres.Session qualified as StoreSession
import Agent.Store.Types
    ( StoreError
    , renderStoreError
    )
import Agent.ToolDispatch (ToolCall(..))
import Agent.Tools.PlanMode
    ( PlanDecision(..)
    , PlanModeHooks(..)
    )
import Control.Applicative ((<|>))
import Control.Exception.Safe
    ( finally
    , onException
    , tryAny
    )
import Data.Aeson
    ( Value
    , object
    , toJSON
    , (.=)
    )
import Data.Bifunctor (first)
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format
    ( defaultTimeLocale
    , formatTime
    , parseTimeM
    )
import System.IO
    ( IOMode(WriteMode)
    , withFile
    )
import System.OsPath (OsPath, unsafeEncodeUtf)

data ServerRuntime = ServerRuntime
    { runtimeStore :: !Store
    , runtimeNative :: !NativeProcessRuntime
    , runtimeBackend :: !Backend
    }

serverRuntimeBackend :: ServerRuntime -> Backend
serverRuntimeBackend = (.runtimeBackend)

openServerRuntime
    :: ResolvedServerConfig
    -> IO (Either Text ServerRuntime)
openServerRuntime config = do
    postgresConfig <-
        managedPostgresConfigFromEnv config.resolvedStateDirectory
    openStore postgresConfig >>= \case
        Left err -> pure (Left (renderStoreError err))
        Right store -> do
            let root = sessionsRoot (unsafeEncodeUtf config.resolvedHome)
            nativeResult <-
                tryAny (newNativeProcessRuntime root)
                    `onException` closeStore store
            case nativeResult of
                Left _ -> do
                    closeStore store
                    pure (Left "could not initialize the native agent runtime")
                Right native -> do
                    let environment = RuntimeEnvironment
                            { environmentConfig = config
                            , environmentStore = store
                            , environmentNative = native
                            , environmentRoot = root
                            }
                        backend = productionBackend environment
                    pure $
                        Right ServerRuntime
                            { runtimeStore = store
                            , runtimeNative = native
                            , runtimeBackend = backend
                            }

closeServerRuntime :: ServerRuntime -> IO ()
closeServerRuntime runtime =
    closeNativeProcessRuntime runtime.runtimeNative
        `finally` closeStore runtime.runtimeStore

data RuntimeEnvironment = RuntimeEnvironment
    { environmentConfig :: !ResolvedServerConfig
    , environmentStore :: !Store
    , environmentNative :: !NativeProcessRuntime
    , environmentRoot :: !OsPath
    }

productionBackend :: RuntimeEnvironment -> Backend
productionBackend environment = Backend
    { backendAdmitBoundary = \action ->
        first gatewayApiError
            <$> withCurrentGatewayBoundaryAt
                home
                action
    , backendContinueBoundary = \boundary action ->
        first gatewayApiError
            <$> withExpectedGatewayBoundaryAt
                home
                boundary
                action
    , backendTurnBoundaryGuard = \boundary action ->
        first gatewayTurnError
            <$> withGatewayTurnBoundaryAt home boundary action
    , backendCheckReady =
        fmap (first storeApiError . fmap (const ())) $
            StoreSession.loadSessionMetadata
                (trustedPool environment.environmentStore)
                "__agent_server_readiness__"
    , backendListModels = listModels environment
    , backendListSessions = listSessions environment
    , backendCreateSession = createSessionForBoundary environment
    , backendGetSession = getSessionForBoundary environment
    , backendPatchSession = patchSessionForBoundary environment
    , backendDeleteSession = deleteSessionForBoundary environment
    , backendSessionHistory = sessionHistoryForBoundary environment
    , backendForkSession = forkSessionForBoundary environment
    , backendRunTurn = runTurn environment
    }
  where
    home = unsafeEncodeUtf environment.environmentConfig.resolvedHome

gatewayApiError :: GatewayBoundaryError -> ApiError
gatewayApiError err =
    case err of
        GatewayBoundaryCredentialLoadFailed _ ->
            ApiError
                { apiErrorStatus = 503
                , apiErrorCode = "gateway_unavailable"
                , apiErrorMessage =
                    "gateway credentials are unavailable"
                , apiErrorDetails = Nothing
                }
        GatewayBoundaryChanged ->
            ApiError
                { apiErrorStatus = 409
                , apiErrorCode = "gateway_boundary_changed"
                , apiErrorMessage = renderGatewayBoundaryError err
                , apiErrorDetails = Nothing
                }
        GatewayBoundarySessionRejected _ ->
            ApiError
                { apiErrorStatus = 404
                , apiErrorCode = "session_not_found"
                , apiErrorMessage = "session not found"
                , apiErrorDetails = Nothing
                }

gatewayTurnError :: GatewayBoundaryError -> Text
gatewayTurnError = \case
    GatewayBoundaryCredentialLoadFailed _ ->
        "gateway credentials are unavailable"
    GatewayBoundaryChanged ->
        "gateway credentials changed before the turn started"
    GatewayBoundarySessionRejected _ ->
        "session does not belong to the current gateway boundary"

listModels
    :: RuntimeEnvironment
    -> GatewayBoundary
    -> IO (Either ApiError Value)
listModels environment boundary =
    loadModelOptions environment boundary
        environment.environmentConfig.resolvedDefaultCwd
        >>= pure . fmap
            (\(_, options) ->
                object
                    [ "data" .= map modelOptionValue options
                    ])

listSessions
    :: RuntimeEnvironment
    -> GatewayBoundary
    -> SessionArchiveFilter
    -> Maybe Text
    -> Int
    -> IO (Either ApiError Value)
listSessions environment boundary archiveFilter rawCursor limit =
    case traverse decodeCursor rawCursor of
        Left err -> pure (Left err)
        Right cursor ->
            StoreSession.listSessionMetadataForBoundary
                pool
                organizationGatewayConnectionId
                boundary.gatewayBoundaryIdentity
                (storeArchiveFilter archiveFilter)
                cursor
                limit >>= \case
                    Left err -> pure (Left (storeApiError err))
                    Right page ->
                        case traverse
                            (\entry ->
                                (, entry.sessionListEntryArchived)
                                    <$> fromStoredMetadata
                                        entry.sessionListEntryMetadata)
                            page.sessionListPageSessions of
                            Left err ->
                                pure
                                    (Left
                                        (internalApiError
                                            ("could not decode session metadata: "
                                                <> err)))
                            Right sessions ->
                                pure $
                                    Right $
                                        object
                                            [ "data" .=
                                                map
                                                    (\(meta, archived) ->
                                                        sessionValue
                                                            archived
                                                            meta)
                                                    sessions
                                            , "nextCursor"
                                                .= fmap encodeCursor
                                                    page.sessionListPageNextCursor
                                            ]
  where
    pool = trustedPool environment.environmentStore

createSessionForBoundary
    :: RuntimeEnvironment
    -> GatewayBoundary
    -> CreateSessionRequest
    -> IO (Either ApiError Value)
createSessionForBoundary environment boundary request =
    resolveWorkspacePath
        environment.environmentConfig
        request.createSessionCwd >>= \case
            Left err -> pure (Left err)
            Right cwd ->
                loadModelOptions environment boundary cwd >>= \case
                    Left err -> pure (Left err)
                    Right (catalog, options) ->
                        case selectModel
                            boundary
                            catalog
                            options
                            request.createSessionModel of
                                Left err -> pure (Left err)
                                Right option ->
                                    case resolveEffort
                                        option.modelTarget.targetProvider
                                        request.createSessionEffort of
                                            Left err -> pure (Left err)
                                            Right effort -> do
                                                created <- tryAny $
                                                    createSession SessionCreate
                                                        { createPool =
                                                            trustedPool
                                                                environment.environmentStore
                                                        , createRoot =
                                                            environment.environmentRoot
                                                        , createTarget =
                                                            option.modelTarget
                                                        , createGatewayIdentity =
                                                            boundary.gatewayBoundaryIdentity
                                                        , createCwd =
                                                            unsafeEncodeUtf cwd
                                                        , createEffort =
                                                            reasoningEffortText effort
                                                        , createTitleHint =
                                                            request.createSessionTitle
                                                        , createTitleIsManual =
                                                            maybe
                                                                False
                                                                (const True)
                                                                request.createSessionTitle
                                                        }
                                                pure case created of
                                                    Left _ ->
                                                        Left
                                                            (internalApiError
                                                                "could not create the session")
                                                    Right handle ->
                                                        Right
                                                            (sessionValue
                                                                False
                                                                handle.sessionMeta)

getSessionForBoundary
    :: RuntimeEnvironment
    -> GatewayBoundary
    -> Text
    -> IO (Either ApiError Value)
getSessionForBoundary environment boundary sessionId =
    fmap
        (fmap
            (\(meta, archived) ->
                sessionValue archived meta)) $
        loadAuthorizedSession environment boundary sessionId

patchSessionForBoundary
    :: RuntimeEnvironment
    -> GatewayBoundary
    -> Text
    -> PatchSessionRequest
    -> IO (Either ApiError Value)
patchSessionForBoundary environment boundary sessionId request =
    loadAuthorizedMeta environment boundary sessionId >>= \case
        Left err -> pure (Left err)
        Right _ -> do
            titleResult <- case request.patchSessionTitle of
                Nothing -> pure (Right ())
                Just title ->
                    fmap (fmap (const ())) $
                        renameSession pool root sessionId title
            case titleResult of
                Left err -> pure (Left (sessionOperationError err))
                Right () -> do
                    archiveResult <- case request.patchSessionArchived of
                        Nothing -> pure (Right ())
                        Just archived ->
                            setSessionArchived
                                pool
                                root
                                sessionId
                                archived
                    case archiveResult of
                        Left err ->
                            pure (Left (sessionOperationError err))
                        Right () ->
                            getSessionForBoundary
                                environment
                                boundary
                                sessionId
  where
    pool = trustedPool environment.environmentStore
    root = environment.environmentRoot

deleteSessionForBoundary
    :: RuntimeEnvironment
    -> GatewayBoundary
    -> Text
    -> IO (Either ApiError ())
deleteSessionForBoundary environment boundary sessionId =
    loadAuthorizedMeta environment boundary sessionId >>= \case
        Left err -> pure (Left err)
        Right _ ->
            first sessionOperationError
                <$> deleteSession
                    (trustedPool environment.environmentStore)
                    environment.environmentRoot
                    sessionId

sessionHistoryForBoundary
    :: RuntimeEnvironment
    -> GatewayBoundary
    -> Text
    -> Maybe Integer
    -> Int
    -> IO (Either ApiError Value)
sessionHistoryForBoundary environment boundary sessionId before limit =
    loadAuthorizedSession environment boundary sessionId >>= \case
        Left err -> pure (Left err)
        Right (meta, archived) ->
            case traverse integerToInt64 before of
                Left err -> pure (Left err)
                Right Nothing -> do
                    page <-
                        loadRecentSessionHistoryTurns
                            pool root sessionId limit
                    pure $
                        first sessionOperationError
                            (historyValue meta archived <$> page)
                Right (Just cursor) -> do
                    page <-
                        loadSessionHistoryTurnsBefore
                            pool root sessionId cursor limit
                    pure $
                        first sessionOperationError
                            (historyValue meta archived <$> page)
  where
    pool = trustedPool environment.environmentStore
    root = environment.environmentRoot

forkSessionForBoundary
    :: RuntimeEnvironment
    -> GatewayBoundary
    -> Text
    -> ForkSessionRequest
    -> IO (Either ApiError Value)
forkSessionForBoundary environment boundary sessionId request =
    loadAuthorizedMeta environment boundary sessionId >>= \case
        Left err -> pure (Left err)
        Right sourceMeta ->
            resolveWorkspacePath
                environment.environmentConfig
                (request.forkSessionCwd
                    <|> Just (unsafeToFilePath sourceMeta.metaCwd)) >>= \case
                    Left err -> pure (Left err)
                    Right cwd ->
                        case request.forkSessionThroughTurn of
                            Just _
                                | request.forkSessionCwd /= Nothing
                                    || request.forkSessionTitle /= Nothing ->
                                    pure $
                                        Left ApiError
                                            { apiErrorStatus = 422
                                            , apiErrorCode =
                                                "unsupported_fork_shape"
                                            , apiErrorMessage =
                                                "title and cwd cannot be changed when forking through a specific turn"
                                            , apiErrorDetails = Nothing
                                            }
                            Just through ->
                                forkThroughTurn through
                            Nothing ->
                                forkAllTurns cwd
  where
    pool = trustedPool environment.environmentStore
    root = environment.environmentRoot

    forkThroughTurn through =
        case integerToInt64 through of
            Left err -> pure (Left err)
            Right index ->
                forkSessionAtTurn pool root sessionId index >>= \case
                    Left err -> pure (Left (sessionOperationError err))
                    Right forkedId ->
                        getSessionForBoundary
                            environment
                            boundary
                            forkedId

    forkAllTurns cwd =
        loadSessionHandle pool root sessionId >>= \case
            Left err -> pure (Left (sessionOperationError err))
            Right (handle, turns) ->
                case validateLoadedMeta boundary handle.sessionMeta of
                    Left err -> pure (Left err)
                    Right () ->
                        forkSessionAt
                            root
                            handle
                            turns
                            request.forkSessionTitle
                            (unsafeEncodeUtf cwd) >>= \case
                                Left err ->
                                    pure (Left (sessionOperationError err))
                                Right forked ->
                                    pure $
                                        Right
                                            (sessionValue
                                                False
                                                forked.sessionMeta)

runTurn
    :: RuntimeEnvironment
    -> TurnControl
    -> TurnSpec
    -> IO (Either Text ())
runTurn environment control spec =
    loadAuthorizedMeta
        environment
        spec.turnSpecBoundary
        spec.turnSpecSessionId >>= \case
            Left err -> pure (Left err.apiErrorMessage)
            Right meta ->
                resolveWorkspacePath
                    environment.environmentConfig
                    (Just (unsafeToFilePath meta.metaCwd)) >>= \case
                        Left err -> pure (Left err.apiErrorMessage)
                        Right cwd ->
                            case parseReasoningEffort meta.metaEffort of
                                Left err -> pure (Left err)
                                Right effort ->
                                    withFile "/dev/null" WriteMode \output ->
                                        runNativeTurn
                                            environment.environmentNative
                                            output
                                            (nativeHooks environment control)
                                            NativeTurnRequest
                                                { nativeTurnPrompt =
                                                    spec.turnSpecPrompt
                                                , nativeTurnSession =
                                                    NativeResumeSession
                                                        spec.turnSpecSessionId
                                                , nativeTurnProvider = Nothing
                                                , nativeTurnModel = Nothing
                                                , nativeTurnCwd =
                                                    unsafeEncodeUtf cwd
                                                , nativeTurnEffort =
                                                    Just effort
                                                , nativeTurnInteractionMode =
                                                    NativeAsk
                                                , nativeTurnShellMode =
                                                    NativeShellBoth
                                                }

nativeHooks :: RuntimeEnvironment -> TurnControl -> NativeRunHooks
nativeHooks environment control = NativeRunHooks
    { nativeOnLoopEvent = \event ->
        let (eventType, value) = projectLoopEvent event
        in control.turnControlEmit eventType value
    , nativeOnSessionId = \_ -> pure ()
    , nativeRegisterCancel = control.turnControlRegisterCancel
    , nativeRegisterAgentSnapshot = \snapshot ->
        control.turnControlSetAgents
            (projectAgentEntries <$> snapshot)
    , nativeRequestApproval = requestToolApproval control
    , nativeRequestRootAccess =
        requestRootAccess
            environment.environmentConfig
            control
    , nativeTools = []
    , nativePlanHooks = planHooks control
    , nativeInteractionMode = NativeAsk
    , nativeShellMode = NativeShellBoth
    }

requestToolApproval
    :: TurnControl
    -> ToolCall
    -> IO (Maybe PermissionChoice)
requestToolApproval control call =
    control.turnControlRequestInput HumanRequestSpec
        { humanRequestSpecKind = ToolApprovalRequest
        , humanRequestSpecPrompt =
            "Allow mutating tool "
                <> fst (boundedPublicText call.name)
                <> " (call "
                <> fst (boundedPublicText call.callId)
                <> ")?\nArguments: "
                <> if call.argumentsEncrypted
                    then "<encrypted>"
                    else fst (boundedPublicText call.arguments)
        , humanRequestSpecOptions =
            [ "allow_once"
            , "allow_tool"
            , "allow_all"
            , "deny"
            ]
        } >>= \case
            Left _ -> pure Nothing
            Right response ->
                pure case response.humanResponseDecision of
                    "allow_once" -> Just PermissionAllowOnce
                    "allow_tool" -> Just PermissionAllowTool
                    "allow_all" -> Just PermissionAllowAll
                    "deny" -> Just PermissionDeny
                    _ -> Nothing

requestRootAccess
    :: ResolvedServerConfig
    -> TurnControl
    -> OsPath
    -> IO Bool
requestRootAccess config control root =
    resolveWorkspacePath config (Just (unsafeToFilePath root)) >>= \case
        Left _ -> pure False
        Right canonical ->
            control.turnControlRequestInput HumanRequestSpec
                { humanRequestSpecKind = RootAccessRequest
                , humanRequestSpecPrompt =
                    "Allow filesystem access to " <> Text.pack canonical
                , humanRequestSpecOptions = ["allow", "deny"]
                } >>= \case
                    Right response ->
                        pure (response.humanResponseDecision == "allow")
                    Left _ -> pure False

planHooks :: TurnControl -> PlanModeHooks
planHooks control = PlanModeHooks
    { planConfirmEnter = \reason ->
        control.turnControlRequestInput HumanRequestSpec
            { humanRequestSpecKind = PlanEnterRequest
            , humanRequestSpecPrompt = reason
            , humanRequestSpecOptions = ["enter", "stay"]
            } >>= \case
                Right response ->
                    pure (response.humanResponseDecision == "enter")
                Left _ -> pure False
    , planDecideExit = \planBody ->
        control.turnControlRequestInput HumanRequestSpec
            { humanRequestSpecKind = PlanExitRequest
            , humanRequestSpecPrompt =
                fst (boundedPublicText planBody)
            , humanRequestSpecOptions =
                ["approve", "request_changes", "cancel"]
            } >>= \case
                Right response ->
                    pure case response.humanResponseDecision of
                        "approve" -> PlanApprove
                        "request_changes" ->
                            PlanRequestChanges
                                (fromMaybe
                                    "(no changes supplied)"
                                    response.humanResponseValue)
                        _ -> PlanCancel
                Left _ -> pure PlanCancel
    , planAskQuestion = \question options ->
        control.turnControlRequestInput HumanRequestSpec
            { humanRequestSpecKind = PlanQuestionRequest
            , humanRequestSpecPrompt = question
            , humanRequestSpecOptions = options <> ["custom"]
            } >>= \case
                Left _ -> pure Nothing
                Right response
                    | response.humanResponseDecision == "custom" ->
                        pure response.humanResponseValue
                    | otherwise ->
                        pure (Just response.humanResponseDecision)
    }

loadAuthorizedMeta
    :: RuntimeEnvironment
    -> GatewayBoundary
    -> Text
    -> IO (Either ApiError SessionMeta)
loadAuthorizedMeta environment boundary sessionId =
    fmap (fmap fst) $
        loadAuthorizedSession environment boundary sessionId

loadAuthorizedSession
    :: RuntimeEnvironment
    -> GatewayBoundary
    -> Text
    -> IO (Either ApiError (SessionMeta, Bool))
loadAuthorizedSession environment boundary sessionId
    | not (isValidSessionId sessionId) =
        pure (Left notFoundApiError)
    | otherwise =
        StoreSession.loadSessionMetadataForBoundary
            (trustedPool environment.environmentStore)
            organizationGatewayConnectionId
            boundary.gatewayBoundaryIdentity
            sessionId >>= \case
                Left err -> pure (Left (storeApiError err))
                Right Nothing -> pure (Left notFoundApiError)
                Right (Just entry) ->
                    pure $
                        (, entry.sessionListEntryArchived)
                            <$> first
                                (internalApiError
                                    . ("could not decode session metadata: " <>))
                                (fromStoredMetadata
                                    entry.sessionListEntryMetadata)

validateLoadedMeta
    :: GatewayBoundary
    -> SessionMeta
    -> Either ApiError ()
validateLoadedMeta boundary meta
    | meta.metaGatewayIdentity == boundary.gatewayBoundaryIdentity
        && case boundary.gatewayBoundaryIdentity of
            Nothing ->
                meta.metaConnection /= organizationGatewayConnectionId
            Just _ ->
                meta.metaConnection == organizationGatewayConnectionId =
            Right ()
    | otherwise = Left notFoundApiError

loadModelOptions
    :: RuntimeEnvironment
    -> GatewayBoundary
    -> FilePath
    -> IO (Either ApiError (ModelCatalog, [ModelOption]))
loadModelOptions environment expected cwd = do
    let home = unsafeEncodeUtf environment.environmentConfig.resolvedHome
        cwdPath = unsafeEncodeUtf cwd
    loadGatewayBoundarySnapshotAt home >>= \case
        Left err -> pure (Left (gatewayApiError err))
        Right snapshot ->
            case validateGatewayBoundary expected snapshot.gatewayBoundary of
                Left err -> pure (Left (gatewayApiError err))
                Right () ->
                    loadGatewayModelOptionsWithCredentialAt
                        home
                        cwdPath
                        snapshot.gatewayBoundaryCredential >>= \case
                            Left err ->
                                pure
                                    (Left ApiError
                                        { apiErrorStatus = 503
                                        , apiErrorCode =
                                            "model_catalog_unavailable"
                                        , apiErrorMessage = err
                                        , apiErrorDetails = Nothing
                                        })
                            Right (catalog, maybeGatewayOptions) ->
                                pure $
                                    Right
                                        ( catalog
                                        , fromMaybe
                                            (modelCatalog catalog)
                                            maybeGatewayOptions
                                        )

selectModel
    :: GatewayBoundary
    -> ModelCatalog
    -> [ModelOption]
    -> Maybe Text
    -> Either ApiError ModelOption
selectModel boundary catalog options requested =
    case requested of
        Just modelId ->
            maybe
                (Left ApiError
                    { apiErrorStatus = 422
                    , apiErrorCode = "model_not_available"
                    , apiErrorMessage =
                        "the requested model is not available in the current gateway boundary"
                    , apiErrorDetails = Nothing
                    })
                Right
                (find
                    ((== modelId) . (.modelTarget.targetModelId))
                    options)
        Nothing ->
            maybe
                (Left ApiError
                    { apiErrorStatus = 503
                    , apiErrorCode = "model_catalog_empty"
                    , apiErrorMessage =
                        "no model is available in the current gateway boundary"
                    , apiErrorDetails = Nothing
                    })
                Right
                defaultOption
  where
    defaultOption = case boundary.gatewayBoundaryIdentity of
        Just _ -> listToMaybe options
        Nothing ->
            defaultModelOptionFor catalog OpenAIProvider
                <|> listToMaybe options

resolveEffort
    :: Provider
    -> Maybe Text
    -> Either ApiError ReasoningEffort
resolveEffort provider requested =
    case requested of
        Nothing -> Right (defaultEffortFor provider)
        Just effort ->
            first
                (\message ->
                    ApiError
                        { apiErrorStatus = 422
                        , apiErrorCode = "invalid_effort"
                        , apiErrorMessage = message
                        , apiErrorDetails = Nothing
                        })
                (parseReasoningEffort effort)

sessionValue :: Bool -> SessionMeta -> Value
sessionValue archived meta = object
    [ "id" .= meta.metaId
    , "createdAt" .= meta.metaCreatedAt
    , "updatedAt" .= meta.metaUpdatedAt
    , "provider" .= providerSlug meta.metaProvider
    , "connection" .= meta.metaConnection
    , "model" .= meta.metaModel
    , "transportModel" .= meta.metaTransportModel
    , "dialect" .= dialectSlug meta.metaDialect
    , "cwd" .= unsafeToFilePath meta.metaCwd
    , "effort" .= meta.metaEffort
    , "title" .= meta.metaTitle
    , "titleIsManual" .= meta.metaTitleIsManual
    , "archived" .= archived
    , "usage" .= object
        [ "input" .= meta.metaInputTokens
        , "output" .= meta.metaOutputTokens
        , "cached" .= meta.metaCachedTokens
        ]
    ]

modelOptionValue :: ModelOption -> Value
modelOptionValue option = object
    [ "id" .= option.modelTarget.targetModelId
    , "provider" .= providerSlug option.modelTarget.targetProvider
    , "connection" .= option.modelTarget.targetConnectionId
    , "transportModel" .= option.modelTarget.targetWireModelId
    , "dialect" .= dialectSlug option.modelTarget.targetDialect
    , "label" .= option.modelLabel
    , "contextWindow" .= option.modelContextWindow
    ]

historyValue :: SessionMeta -> Bool -> SessionTurnPage -> Value
historyValue meta archived page = object
    [ "session" .= sessionValue archived meta
    , "data" .=
        [ object
            [ "index" .= index
            , "turn" .= projectPublicValue (toJSON turn)
            ]
        | (index, turn) <- page.pageTurns
        ]
    , "generationStart" .= page.pageGenerationStart
    , "total" .= page.pageTotalTurns
    , "hasOlder" .= page.pageHasOlder
    , "hasNewer" .= page.pageHasNewer
    , "nextCursor" .=
        if page.pageHasOlder
            then fst <$> listToMaybe page.pageTurns
            else Nothing
    ]

storeArchiveFilter
    :: SessionArchiveFilter
    -> StoreSession.SessionArchiveFilter
storeArchiveFilter = \case
    ActiveSessions -> StoreSession.SessionActive
    ArchivedSessions -> StoreSession.SessionArchived
    AllSessions -> StoreSession.SessionAll

encodeCursor :: StoreSession.SessionListCursor -> Text
encodeCursor cursor =
    Text.pack
        (formatTime
            defaultTimeLocale
            cursorTimestampFormat
            cursor.sessionListCursorUpdatedAt)
        <> "|"
        <> cursor.sessionListCursorKey

decodeCursor
    :: Text
    -> Either ApiError StoreSession.SessionListCursor
decodeCursor raw =
    let (timestamp, separatorAndKey) = Text.breakOn "|" raw
        key = Text.drop 1 separatorAndKey
        parsed =
            parseTimeM
                True
                defaultTimeLocale
                cursorTimestampFormat
                (Text.unpack timestamp)
                :: Maybe UTCTime
    in case parsed of
        Just updatedAt
            | not (Text.null separatorAndKey)
            , not (Text.null key) ->
                Right StoreSession.SessionListCursor
                    { sessionListCursorUpdatedAt = updatedAt
                    , sessionListCursorKey = key
                    }
        _ ->
            Left ApiError
                { apiErrorStatus = 400
                , apiErrorCode = "invalid_cursor"
                , apiErrorMessage = "the session cursor is invalid"
                , apiErrorDetails = Nothing
                }

cursorTimestampFormat :: String
cursorTimestampFormat = "%Y-%m-%dT%H:%M:%S%QZ"

integerToInt64 :: Integer -> Either ApiError Int64
integerToInt64 value
    | value < 0
        || value > toInteger (maxBound :: Int64) =
        Left ApiError
            { apiErrorStatus = 400
            , apiErrorCode = "invalid_turn_cursor"
            , apiErrorMessage =
                "the turn cursor must be a non-negative 64-bit integer"
            , apiErrorDetails = Nothing
            }
    | otherwise = Right (fromInteger value)

storeApiError :: StoreError -> ApiError
storeApiError err =
    ApiError
        { apiErrorStatus = 503
        , apiErrorCode = "store_unavailable"
        , apiErrorMessage = renderStoreError err
        , apiErrorDetails = Nothing
        }

sessionOperationError :: Text -> ApiError
sessionOperationError message
    | "not found" `Text.isInfixOf` Text.toLower message =
        notFoundApiError
    | "running session" `Text.isInfixOf` Text.toLower message =
        ApiError
            { apiErrorStatus = 409
            , apiErrorCode = "session_busy"
            , apiErrorMessage = message
            , apiErrorDetails = Nothing
            }
    | otherwise =
        ApiError
            { apiErrorStatus = 422
            , apiErrorCode = "session_operation_failed"
            , apiErrorMessage = message
            , apiErrorDetails = Nothing
            }

notFoundApiError :: ApiError
notFoundApiError = ApiError
    { apiErrorStatus = 404
    , apiErrorCode = "session_not_found"
    , apiErrorMessage = "session not found"
    , apiErrorDetails = Nothing
    }

internalApiError :: Text -> ApiError
internalApiError message = ApiError
    { apiErrorStatus = 500
    , apiErrorCode = "internal_error"
    , apiErrorMessage = message
    , apiErrorDetails = Nothing
    }
