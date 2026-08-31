{-# LANGUAGE ForeignFunctionInterface #-}

module Agent.CLI.MacOS.Bridge
    ( BrowserCallback
    , BrowserHost(..)
    , BrowserRegistration(..)
    , browserCommandABI
    , browserOutputCapacity
    , browserStatusMessage
    , browserToolsWhenEnabled
    , invokeBrowserCommand
    , repositoryCancelAllAdmissionSmoke
    , repositoryCancelClassificationSmoke
    , repositoryCancelAllReentrancySmoke
    , repositoryCheckDestroyReentrancySmoke
    , repositoryTerminalThrowSmoke
    , TurnStart(..)
    , nativeExceptionMessage
    , nativeTurnArguments
    ) where

import qualified Agent.CLI.AgentViewport as Viewport
import Agent.CLI.BrowserTools
    ( BrowserCommand(..)
    , browserTools
    )
import Agent.CLI.NativeRuntime
    ( NativeProcessRuntime
    , NativeRunHooks(..)
    , StartupFailure(..)
    , closeNativeProcessRuntime
    , newNativeProcessRuntime
    , restartNativeMcpRuntime
    , runNativeAgent
    )
import Agent.CLI.McpAdmin
    ( McpAdminError(..)
    , McpAdminServer(..)
    , McpAdminServerInput(..)
    , McpAdminSnapshot(..)
    , addMcpAdminServer
    , editMcpAdminServer
    , listMcpAdminServers
    , readMcpAdminServer
    , removeMcpAdminServer
    , restartMcpAdminServer
    , setMcpAdminServerEnabled
    )
import Agent.Store.Postgres.Session
    ( NativeConversationSearchResult(..)
    , searchNativeConversations
    )
import Agent.CLI.ManagedTurn
    ( ManagedTurnMedia(..)
    , managedTurnRequestWithImages
    , renderManagedTurnPrompt
    )
import Data.Bifunctor (first)
import Agent.Store.Postgres.Scope (Scope(..), scopeKindText)
import Agent.Store.Postgres.Skill
    ( LearnedSkill(..)
    , learnedSkillActivationText
    , learnedSkillStatusText
    , listAllLearnedSkills
    )
import Agent.CLI.MacOS.NativeLoopEvent
    ( encodeNativeLoopEvent
    )
import Agent.CLI.MacOS.EngineMailbox
    ( EngineMailbox
    , acceptEngineCommand
    , closeEngineMailbox
    , drainEngineCommands
    , newEngineMailboxIO
    , readEngineCommand
    )
import Agent.Runtime.Daemon.TaskScheduler
    ( TaskIdentity(..)
    , selectRunnableTasks
    )
import Agent.CLI.Environment (lookupNonEmpty)
import Agent.CLI.Login
    ( AccountBilling(..)
    , AccountUsage(..)
    , LoginAccount(..)
    , UsageState(..)
    , UsageWindow(..)
    , discoverLoginAccounts
    , loginAccountSelectionId
    , refreshLoginAccount
    , storeConnectedCredential
    )
import Agent.CLI.CredentialStore
    ( ManagedAuthKind(..)
    , deleteManagedCredential
    , setManagedCredentialEnabled
    )
import Agent.CLI.Auth
    ( GrokAuthState(..)
    , grokAuthStateToJson
    , openAIOAuthClientId
    , openaiAuthStateFromJson
    , xaiOAuthClientId
    )
import Agent.CLI.GatewayClient
    ( GatewayCredential(..)
    , GatewayDeviceAuthorization(..)
    , GatewayPollResult(..)
    , exchangeGatewayAuthorizationCode
    , loadGatewayCredential
    , pollGatewayAuthorizationAndSave
    , removeGatewayCredential
    , startGatewayAuthorization
    )
import qualified Agent.OpenAI.Login as OpenAILogin
import qualified Agent.OpenAI.Auth as OpenAIAuth
import qualified Agent.OpenAI.Auth.Types as OpenAIAuthTypes
import qualified Agent.XAI.Auth as XAIAuth
import qualified Agent.OpenRouter.Usage as OpenRouter
import Agent.CLI.ModelConfig
    ( CatalogModel(..)
    , ModelCatalog
    , catalogModelById
    )
import Agent.CLI.GatewayModels (loadGatewayModelCatalogAt)
import Agent.CLI.Database (DatabaseScope(..))
import Agent.CLI.Database.Store
    ( DatabaseBrowsePage(..)
    , DatabaseScopes
    , applicableDatabaseScopes
    , deriveDatabaseScopes
    , listDatabaseObjects
    , loadDatabaseRows
    )
import Agent.CLI.Models
    ( ModelOption(..)
    , ModelTarget(..)
    , PickerState(..)
    , defaultModelOptionFor
    , initialPickerStateResolved
    , modelCatalog
    , resolveConfiguredModel
    , resolveModelOptionDialect
    , selectedOption
    )
import Agent.CLI.Permission (PermissionChoice(..))
import Agent.CLI.Project
    ( ProjectModel(..)
    , ProjectSettings(..)
    , loadProjectSettings
    , resolveProjectRoot
    )
import Agent.CLI.Render
    ( summarizeToolCall )
import qualified Agent.CLI.RepositoryDelivery as RepositoryDelivery
import qualified Agent.CLI.RepositoryReview as RepositoryReview
import Agent.CLI.Options (parseEffort)
import Agent.CLI.Session
    ( SessionMeta(..)
    , SessionTurn(..)
    , SessionTurnPage(..)
    , SessionTransferEnvelope
    , TranscriptEffect(..)
    , deleteSession
    , forkSessionAtTurn
    , importSessionTransferRemapped
    , listArchivedSessionIds
    , listSessions
    , loadSessionHistoryTurnsAround
    , loadSessionMeta
    , renameSession
    , setSessionArchived
    , sessionsRoot
    , streamSessionTransfer
    )
import Agent.CLI.SessionAdmin
    ( loadSessionPageJSON
    , managedPostgresConfigForHome
    , sessionSummaryWithStatusJSON
    )
import Agent.Loop (ImageAttachment(..), LoopEvent(..), TokenUsage(..))
import Agent.Dialect (dialectSlug)
import Agent.Provider (Provider(..), providerSlug, parseProvider, BillingMode(..))
import Agent.Store.Postgres
    ( ManagedPostgresConfig
    , Store
    , closeStore
    , openStore
    , trustedPool
    )
import Agent.Store.Postgres.Connection (StorePool)
import Agent.Store.Postgres.Custom
    ( CatalogColumn(..)
    , CatalogDefinition(..)
    , CatalogObject(..)
    )
import Agent.Store.Types (renderStoreError)
import Agent.ToolDispatch
    ( ToolCall(..)
    , isComputerToolCallKind
    )
import Agent.Tools.Types (AppTool)
import Control.Concurrent
    ( ThreadId
    , forkFinally
    , forkIO
    , myThreadId
    , threadDelay
    )
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , mapConcurrently
    , waitCatch
    , withAsync
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newEmptyMVar
    , newMVar
    , putMVar
    , readMVar
    , withMVar
    , tryReadMVar
    , takeMVar
    )
import Control.Concurrent.STM
    ( TMVar
    , TVar
    , atomically
    , modifyTVar'
    , newEmptyTMVarIO
    , newTVarIO
    , readTVar
    , readTVarIO
    , takeTMVar
    , tryPutTMVar
    , writeTVar
    )
import Control.Applicative ((<|>))
import Control.Exception.Safe
    ( SomeAsyncException
    , SomeException
    , bracket
    , catchAsync
    , finally
    , fromException
    , isAsyncException
    , mask
    , onException
    , throwIO
    , throwString
    , tryAny
    , uninterruptibleMask_
    )
import Control.Monad
    ( foldM
    , filterM
    , forM_
    , void
    , when
    )
import qualified Data.Aeson as Aeson
import Data.Aeson
    ( (.:)
    , (.:?)
    )
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (ord)
import Data.Either (isRight)
import Data.IORef
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    )
import System.IO.Unsafe (unsafePerformIO)
import Data.Int (Int64)
import Data.Foldable (toList)
import Data.List (partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word8, Word64)
import Foreign
    ( FunPtr
    , Ptr
    , StablePtr
    , castPtr
    , castPtrToStablePtr
    , castStablePtrToPtr
    , deRefStablePtr
    , freeStablePtr
    , newStablePtr
    , nullFunPtr
    , nullPtr
    , alloca
    , peek
    , poke
    , plusPtr
    , peekByteOff
    , sizeOf
    )
import Foreign.C.String (CString)
import Foreign.C.Types (CDouble(..), CInt(..), CLLong(..), CSize(..))
import Foreign.Marshal.Alloc (allocaBytes)
import System.Directory
    ( getTemporaryDirectory
    , removeFile
    )
import System.Directory.OsPath (getHomeDirectory)
import qualified System.Exit
import System.IO
    ( IOMode(WriteMode)
    , hClose
    , openBinaryTempFile
    , withFile
    )
import System.OsPath
    ( OsPath
    , decodeFS
    , takeDirectory
    , unsafeEncodeUtf
    )

decodeInput :: Ptr Word8 -> Word64 -> IO Text
decodeInput pointer length
    | pointer == nullPtr || length == 0 = pure ""
    | otherwise = TextEncoding.decodeUtf8 <$> BS.packCStringLen
        (castPtr pointer, fromIntegral length)

decodeUtf8Input :: Ptr Word8 -> Word64 -> IO (Either () Text)
decodeUtf8Input pointer length =
    first (const ()) . TextEncoding.decodeUtf8'
        <$> BS.packCStringLen (castPtr pointer, fromIntegral length)

nonEmptyText :: Text -> Maybe Text
nonEmptyText value
    | Text.null value = Nothing
    | otherwise = Just value

withText :: Text -> (CString -> CSize -> IO a) -> IO a
withText value action = BS.useAsCStringLen (TextEncoding.encodeUtf8 value) \(pointer, length) ->
    action pointer (fromIntegral length)

withOptionalText :: Maybe Text -> (CString -> CSize -> IO a) -> IO a
withOptionalText value action = withText (fromMaybe "" value) action

withNullableText :: Maybe Text -> (CString -> CSize -> IO a) -> IO a
withNullableText value action = case value of
    Nothing -> action nullPtr 0
    Just text -> withText text action

type EventCallback = Ptr () -> Ptr Word8 -> CSize -> IO ()

type AccountListCallback =
    Ptr () -> CInt -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize -> CString -> CSize
    -> CInt -> CInt -> CString -> CSize -> IO ()

type AccountUsageWindowCallback =
    Ptr () -> CString -> CSize -> CString -> CSize
    -> CInt -> CLLong -> CLLong -> IO ()

type AccountResultCallback =
    Ptr () -> CInt -> CString -> CSize -> CString -> CSize -> IO ()

type SessionResultCallback =
    Ptr () -> CInt -> CString -> CSize -> IO ()

type AccountOAuthStartCallback =
    Ptr () -> CInt -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize -> CInt -> CInt
    -> CString -> CSize -> IO ()

type GatewayStatusCallback =
    Ptr () -> CInt
    -> CString -> CSize
    -> CString -> CSize
    -> IO ()

type GatewayConnectStartCallback =
    Ptr () -> CInt
    -> CString -> CSize
    -> CString -> CSize
    -> CString -> CSize
    -> CString -> CSize
    -> CInt -> CInt
    -> CString -> CSize
    -> IO ()

type GatewayPollCallback =
    Ptr () -> CInt -> CInt -> CString -> CSize -> IO ()

type GatewayResultCallback =
    Ptr () -> CInt -> CString -> CSize -> IO ()

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

type LearnedSkillsListCallback =
    Ptr () -> CInt
    -> CString -> CSize -> CString -> CSize -> CLLong
    -> CString -> CSize -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize -> CString -> CSize
    -> CInt -> CString -> CSize -> CString -> CSize -> IO ()

type DataCatalogCallback =
    Ptr () -> CInt -> CInt -> CInt
    -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize -> CInt
    -> CString -> CSize -> CString -> CSize -> IO ()

type DataRowsCallback =
    Ptr () -> CInt -> Int64 -> Int64 -> CInt -> CInt
    -> CString -> CSize -> Int64 -> CInt
    -> CString -> CSize -> IO ()

type BrowserCallback =
    Ptr () -> CInt
    -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize
    -> CDouble -> CDouble
    -> CInt
    -> Ptr Word8 -> CSize -> Ptr CSize
    -> IO CInt

type McpServerCallback =
    Ptr () -> CInt -> Word64
    -> CString -> CSize -> CInt -> CString -> CSize -> CString -> CSize
    -> CInt -> CInt -> CSize -> CSize -> CString -> CSize -> IO ()

-- kind 0 is an argument and kind 1 is an environment key. Environment values
-- never cross the bridge.
type McpServerFieldCallback =
    Ptr () -> CString -> CSize -> CInt -> CSize
    -> CString -> CSize -> IO ()

type McpResultCallback =
    Ptr () -> CInt -> Word64 -> CString -> CSize -> IO ()
type RepositorySnapshotCallback =
    Ptr ()
    -> CString -> CSize -- snapshot id
    -> CString -> CSize -- repository root
    -> CString -> CSize -- HEAD, empty for unborn
    -> CString -> CSize -- index fingerprint
    -> CString -> CSize -- worktree fingerprint
    -> IO ()

type RepositoryFileCallback =
    Ptr ()
    -> CString -> CSize -- path
    -> CString -> CSize -- original path, null when absent
    -> CInt -- index status byte
    -> CInt -- worktree status byte
    -> IO ()

type RepositoryDiffCallback =
    Ptr () -> Ptr Word8 -> CSize -> CInt -> IO ()

type RepositoryHunkCallback =
    Ptr () -> CLLong -> CLLong -> CLLong -> CLLong
    -> CString -> CSize -> IO ()

type RepositoryResultCallback =
    Ptr () -> CInt
    -> CString -> CSize -- current snapshot id on success/stale
    -> CString -> CSize -- error
    -> IO ()

type RepositoryDeliveryStatusCallback =
    Ptr () -> CInt
    -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize
    -> CLLong -> CLLong -> CString -> CSize -> IO ()

type RepositoryPushPreviewCallback =
    Ptr () -> CInt -> CString -> CSize -> CLLong
    -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize
    -> CLLong -> CLLong -> CString -> CSize -> IO ()

type RepositoryPushResultCallback =
    Ptr () -> CInt -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> IO ()

type RepositoryPullRequestPreviewCallback =
    Ptr () -> CInt -> CString -> CSize -> CLLong
    -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> IO ()

type RepositoryPullRequestResultCallback =
    Ptr () -> CInt -> CString -> CSize -> CString -> CSize -> IO ()

type RepositoryCheckOutputCallback =
    Ptr () -> CInt -> Ptr Word8 -> CSize -> IO ()

type RepositoryCheckExitCallback =
    Ptr () -> CInt -> CInt -> CString -> CSize -> IO ()

-- Status is 0 for an active task, 1 for completion, and -1 for failure.
-- State is 0 for queued and 1 for running. Every pointer is callback-scoped.
type TaskSnapshotCallback =
    Ptr () -> CInt
    -> Ptr Word8 -> CSize -- task id
    -> Ptr Word8 -> CSize -- session id, optional
    -> CInt
    -> Ptr Word8 -> CSize -- error
    -> IO ()

-- Session page status is 0 for a turn, 1 for completion, and -1 for failure.
-- Text buffers are callback-scoped UTF-8.
type SessionTurnCallback =
    Ptr () -> CInt -> Int64
    -> CString -> CSize -- occurred at
    -> CString -> CSize -- user
    -> CString -> CSize -- assistant
    -> CString -> CSize -- turn error
    -> CString -> CSize -- response id
    -> CString -> CSize -- transcript effect
    -> CString -> CSize -- provider-extensible response items JSON
    -> CLLong -> CLLong -> CLLong -- usage; -1 means absent
    -> CInt -> CInt -- has older/newer on completion
    -> CString -> CSize -- error
    -> IO ()

-- Transfer result status is 0 for success and -1 for failure.
type SessionTransferResultCallback =
    Ptr () -> CInt -> CString -> CSize -> CString -> CSize -> IO ()

-- Export status is 0 for a chunk, 1 for completion, and -1 for failure.
type SessionExportCallback =
    Ptr () -> CInt -> Ptr Word8 -> CSize -> CString -> CSize -> IO ()

foreign import ccall "dynamic"
    invokeEventCallback :: FunPtr EventCallback -> EventCallback

foreign import ccall "dynamic"
    invokeAccountListCallback
        :: FunPtr AccountListCallback -> AccountListCallback

foreign import ccall "dynamic"
    invokeAccountUsageWindowCallback
        :: FunPtr AccountUsageWindowCallback -> AccountUsageWindowCallback

foreign import ccall "dynamic"
    invokeAccountResultCallback
        :: FunPtr AccountResultCallback -> AccountResultCallback

foreign import ccall "dynamic"
    invokeSessionTransferResultCallback
        :: FunPtr SessionTransferResultCallback -> SessionTransferResultCallback

foreign import ccall "dynamic"
    invokeAccountOAuthStartCallback
        :: FunPtr AccountOAuthStartCallback -> AccountOAuthStartCallback

foreign import ccall "dynamic"
    invokeGatewayStatusCallback
        :: FunPtr GatewayStatusCallback -> GatewayStatusCallback

foreign import ccall "dynamic"
    invokeGatewayConnectStartCallback
        :: FunPtr GatewayConnectStartCallback -> GatewayConnectStartCallback

foreign import ccall "dynamic"
    invokeGatewayPollCallback
        :: FunPtr GatewayPollCallback -> GatewayPollCallback

foreign import ccall "dynamic"
    invokeGatewayResultCallback
        :: FunPtr GatewayResultCallback -> GatewayResultCallback

foreign import ccall "dynamic"
    invokeSearchCallback :: FunPtr SearchCallback -> SearchCallback

foreign import ccall "dynamic"
    invokeLearnedSkillsListCallback
        :: FunPtr LearnedSkillsListCallback -> LearnedSkillsListCallback

foreign import ccall "dynamic"
    invokeDataCatalogCallback
        :: FunPtr DataCatalogCallback -> DataCatalogCallback

foreign import ccall "dynamic"
    invokeDataRowsCallback
        :: FunPtr DataRowsCallback -> DataRowsCallback

foreign import ccall "dynamic"
    invokeBrowserCallback :: FunPtr BrowserCallback -> BrowserCallback

foreign import ccall "dynamic"
    invokeMcpServerCallback
        :: FunPtr McpServerCallback -> McpServerCallback

foreign import ccall "dynamic"
    invokeMcpServerFieldCallback
        :: FunPtr McpServerFieldCallback -> McpServerFieldCallback

foreign import ccall "dynamic"
    invokeMcpResultCallback
        :: FunPtr McpResultCallback -> McpResultCallback

foreign import ccall "dynamic"
    invokeRepositorySnapshotCallback
        :: FunPtr RepositorySnapshotCallback -> RepositorySnapshotCallback

foreign import ccall "dynamic"
    invokeRepositoryFileCallback
        :: FunPtr RepositoryFileCallback -> RepositoryFileCallback

foreign import ccall "dynamic"
    invokeRepositoryDiffCallback
        :: FunPtr RepositoryDiffCallback -> RepositoryDiffCallback

foreign import ccall "dynamic"
    invokeRepositoryHunkCallback
        :: FunPtr RepositoryHunkCallback -> RepositoryHunkCallback

foreign import ccall "dynamic"
    invokeRepositoryResultCallback
        :: FunPtr RepositoryResultCallback -> RepositoryResultCallback

foreign import ccall "dynamic"
    invokeRepositoryDeliveryStatusCallback
        :: FunPtr RepositoryDeliveryStatusCallback
        -> RepositoryDeliveryStatusCallback

foreign import ccall "dynamic"
    invokeRepositoryPushPreviewCallback
        :: FunPtr RepositoryPushPreviewCallback
        -> RepositoryPushPreviewCallback

foreign import ccall "dynamic"
    invokeRepositoryPushResultCallback
        :: FunPtr RepositoryPushResultCallback
        -> RepositoryPushResultCallback

foreign import ccall "dynamic"
    invokeRepositoryPullRequestPreviewCallback
        :: FunPtr RepositoryPullRequestPreviewCallback
        -> RepositoryPullRequestPreviewCallback

foreign import ccall "dynamic"
    invokeRepositoryPullRequestResultCallback
        :: FunPtr RepositoryPullRequestResultCallback
        -> RepositoryPullRequestResultCallback

foreign import ccall "dynamic"
    invokeRepositoryCheckOutputCallback
        :: FunPtr RepositoryCheckOutputCallback -> RepositoryCheckOutputCallback

foreign import ccall "dynamic"
    invokeRepositoryCheckExitCallback
        :: FunPtr RepositoryCheckExitCallback -> RepositoryCheckExitCallback

foreign import ccall "dynamic"
    invokeTaskSnapshotCallback
        :: FunPtr TaskSnapshotCallback -> TaskSnapshotCallback

foreign import ccall "dynamic"
    invokeSessionTurnCallback
        :: FunPtr SessionTurnCallback -> SessionTurnCallback

foreign import ccall "dynamic"
    invokeSessionResultCallback
        :: FunPtr SessionResultCallback -> SessionResultCallback

foreign import ccall "dynamic"
    invokeSessionExportCallback
        :: FunPtr SessionExportCallback -> SessionExportCallback

data BridgeRequest = BridgeRequest
    { requestId :: !Text
    , requestMethod :: !Text
    , requestParams :: !Aeson.Value
    }

instance Aeson.FromJSON BridgeRequest where
    parseJSON = Aeson.withObject "BridgeRequest" \object ->
        BridgeRequest
            <$> object .: "id"
            <*> object .: "method"
            <*> (object .:? "params" Aeson..!= Aeson.object [])

data TurnStart = TurnStart
    { turnStartId :: !Text
    , turnStartPrompt :: !Text
    , turnStartSessionId :: !(Maybe Text)
    , turnStartCwd :: !FilePath
    , turnStartProvider :: !(Maybe Text)
    , turnStartModel :: !(Maybe Text)
    , turnStartEffort :: !(Maybe Text)
    , turnStartWorktree :: !Bool
    , turnStartComputerUse :: !Bool
    }

instance Aeson.FromJSON TurnStart where
    parseJSON = Aeson.withObject "TurnStart" \object -> do
        turnStartId <- object .: "turnId"
        turnStartPrompt <- object .: "prompt"
        turnStartSessionId <- object .:? "sessionId"
        turnStartCwd <- object .: "cwd"
        turnStartProvider <- object .:? "provider"
        turnStartModel <- object .:? "model"
        rawEffort <- object .:? "effort"
        turnStartEffort <- traverse
            (either fail pure . parseEffort)
            rawEffort
        turnStartWorktree <- object .:? "worktree" Aeson..!= False
        turnStartComputerUse <-
            object .:? "computerUse" Aeson..!= False
        let start = TurnStart
                { turnStartId
                , turnStartPrompt
                , turnStartSessionId
                , turnStartCwd
                , turnStartProvider
                , turnStartModel
                , turnStartEffort
                , turnStartWorktree
                , turnStartComputerUse
                }
        case
            ( turnStartSessionId
            , turnStartWorktree
            , turnStartProvider
            , turnStartModel
            )
          of
            (Just _, True, _, _) ->
                fail "a worktree can only be created for a new session"
            (_, _, Nothing, Nothing) -> pure start
            (_, _, Just _, Just _) -> pure start
            _ -> fail "provider and model must be supplied together"

data TurnReference = TurnReference
    { turnReferenceId :: !Text
    }

instance Aeson.FromJSON TurnReference where
    parseJSON = Aeson.withObject "TurnReference" \object ->
        TurnReference <$> object .: "turnId"

data ApprovalResolution = ApprovalResolution
    { approvalResolutionId :: !Text
    , approvalResolutionDecision :: !Text
    }

instance Aeson.FromJSON ApprovalResolution where
    parseJSON = Aeson.withObject "ApprovalResolution" \object ->
        ApprovalResolution
            <$> object .: "approvalId"
            <*> object .: "decision"

data SessionPageRequest = SessionPageRequest
    { sessionPageId :: !Text
    , sessionPageBefore :: !(Maybe Int64)
    , sessionPageLimit :: !(Maybe Int)
    }

instance Aeson.FromJSON SessionPageRequest where
    parseJSON = Aeson.withObject "SessionPageRequest" \object ->
        SessionPageRequest
            <$> object .: "id"
            <*> object .:? "before"
            <*> object .:? "limit"

data SessionReference = SessionReference
    { sessionReferenceId :: !Text
    }

instance Aeson.FromJSON SessionReference where
    parseJSON = Aeson.withObject "SessionReference" \object ->
        SessionReference <$> object .: "id"

data ModelsListRequest = ModelsListRequest
    { modelsListCwd :: !FilePath
    , modelsListSessionId :: !(Maybe Text)
    }

instance Aeson.FromJSON ModelsListRequest where
    parseJSON = Aeson.withObject "ModelsListRequest" \object ->
        ModelsListRequest
            <$> object .: "cwd"
            <*> object .:? "sessionId"

data AccountProviderRequest = AccountProviderRequest
    { accountProvider :: !Text
    }

data AccountOAuthPollRequest = AccountOAuthPollRequest
    { oauthPollProvider :: !Text
    , oauthPollVerificationUrl :: !(Maybe Text)
    , oauthPollUserCode :: !(Maybe Text)
    , oauthPollDeviceAuthId :: !(Maybe Text)
    , oauthPollDeviceCode :: !(Maybe Text)
    , oauthPollIntervalSeconds :: !(Maybe Int)
    , oauthPollExpiresInSeconds :: !(Maybe Int)
    }

data AccountAPIKeyRequest = AccountAPIKeyRequest
    { accountAPIKeyProvider :: !Text
    , accountAPIKey :: !Text
    }

data EngineCommand
    = EngineRequest !BridgeRequest
    | EngineSearch !Text !Int !(FunPtr SearchCallback) !(Ptr ())
    | EngineSessionMutation
        !SessionMutation !(FunPtr SessionResultCallback) !(Ptr ())
    | EngineMcpRestart !Word64 !Text !(FunPtr McpResultCallback) !(Ptr ())
    | EngineCancelTask !Text
    | EngineTaskSnapshot !(FunPtr TaskSnapshotCallback) !(Ptr ())
    | EngineSetTaskLimit !Int
    | EngineTaskSession !Text !Text
    | EngineTaskFinished !Text !TaskResult
    | EngineStop

data SessionMutation
    = SessionRename !Text !Text
    | SessionDelete !Text
    | SessionArchive !Text !Bool

data Engine = Engine
    { engineCommands :: !(EngineMailbox EngineCommand)
    , engineDone :: !(MVar ())
    , engineStagedImages :: !(TVar (Map Text [ImageAttachment]))
    , engineBrowser :: !BrowserHost
    }

data BrowserRegistration = BrowserRegistration
    { browserCallback :: !(FunPtr BrowserCallback)
    , browserContext :: !(Ptr ())
    }

newtype BrowserHost = BrowserHost
    { browserRegistration :: MVar (Maybe BrowserRegistration)
    }

data TurnControl = TurnControl
    { turnControlId :: !Text
    , turnControlSessionId :: !(TVar (Maybe Text))
    , turnControlCancelled :: !(TVar Bool)
    , turnControlCancel :: !(TVar (IO ()))
    , turnControlApprovals
        :: !(TVar (Map Text (TMVar PermissionChoice)))
    , turnControlApprovalCounter :: !(TVar Int)
    , turnControlAllowedTools :: !(TVar (Set.Set Text))
    , turnControlAgentSnapshot :: !(TVar (IO [Viewport.AgentEntry]))
    }

data TurnOutcome = TurnOutcome
    { turnOutcomeSessionId :: !(Maybe Text)
    , turnOutcomeError :: !(Maybe Text)
    }

data TaskResult
    = TaskOutcome !TurnOutcome
    | TaskFailure !Text

data PendingTurn = PendingTurn
    { pendingTurnStart :: !TurnStart
    , pendingTurnImages :: ![ImageAttachment]
    }

data RunningTurn = RunningTurn
    { runningTurnControl :: !TurnControl
    , runningTurnWorker :: !(Async ())
    }

data TaskSupervisor = TaskSupervisor
    { supervisorLimit :: !Int
    , supervisorPending :: !(Seq PendingTurn)
    , supervisorRunning :: !(Map Text RunningTurn)
    , supervisorKnownTaskIds :: !(Set.Set Text)
    }

defaultTaskLimit :: Int
defaultTaskLimit = 3

foreign export ccall ha_engine_create
    :: FunPtr EventCallback -> Ptr () -> IO (Ptr ())

foreign export ccall ha_engine_send_json
    :: Ptr () -> Ptr Word8 -> CSize -> IO CInt

foreign export ccall ha_engine_stage_turn_images
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr () -> CSize -> IO CInt

foreign export ccall ha_engine_set_browser_callback
    :: Ptr () -> FunPtr BrowserCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_cancel_task
    :: Ptr () -> Ptr Word8 -> CSize -> IO CInt

foreign export ccall ha_engine_list_tasks
    :: Ptr () -> FunPtr TaskSnapshotCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_set_task_limit
    :: Ptr () -> CSize -> IO CInt

foreign export ccall ha_engine_destroy
    :: Ptr () -> IO ()

foreign export ccall ha_engine_session_rename
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr SessionResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_session_delete
    :: Ptr () -> Ptr Word8 -> CSize
    -> FunPtr SessionResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_session_archive
    :: Ptr () -> Ptr Word8 -> CSize -> CInt
    -> FunPtr SessionResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_session_load_around
    :: Ptr Word8 -> CSize -> Int64 -> CInt
    -> FunPtr SessionTurnCallback -> Ptr () -> IO CInt

foreign export ccall ha_session_fork
    :: Ptr Word8 -> CSize -> Int64
    -> FunPtr SessionTransferResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_session_export
    :: Ptr Word8 -> CSize
    -> FunPtr SessionExportCallback -> Ptr () -> IO CInt

foreign export ccall ha_session_import
    :: Ptr Word8 -> CSize
    -> FunPtr SessionTransferResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_accounts_list
    :: FunPtr AccountListCallback -> FunPtr AccountUsageWindowCallback
    -> Ptr () -> IO CInt

foreign export ccall ha_account_oauth_start
    :: Ptr Word8 -> CSize -> FunPtr AccountOAuthStartCallback -> Ptr () -> IO CInt

foreign export ccall ha_account_oauth_poll
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CInt -> CInt
    -> FunPtr AccountResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_account_api_key_connect
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr AccountResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_account_set_enabled
    :: Ptr Word8 -> CSize -> CInt
    -> FunPtr AccountResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_account_delete
    :: Ptr Word8 -> CSize -> FunPtr AccountResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_gateway_status
    :: FunPtr GatewayStatusCallback -> Ptr () -> IO CInt

foreign export ccall ha_gateway_connect_start
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr GatewayConnectStartCallback -> Ptr () -> IO CInt

foreign export ccall ha_gateway_connect_poll
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr GatewayPollCallback -> Ptr () -> IO CInt

foreign export ccall ha_gateway_connect_exchange
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize
    -> FunPtr GatewayResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_gateway_disconnect
    :: FunPtr GatewayResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_learned_skills_list
    :: Ptr Word8 -> CSize -> FunPtr LearnedSkillsListCallback -> Ptr () -> IO CInt

foreign export ccall ha_mcp_servers_list
    :: FunPtr McpServerCallback -> FunPtr McpServerFieldCallback
    -> Ptr () -> IO CInt

foreign export ccall ha_mcp_server_read
    :: Ptr Word8 -> CSize -> FunPtr McpServerCallback
    -> FunPtr McpServerFieldCallback -> Ptr () -> IO CInt

foreign export ccall ha_mcp_server_status
    :: Ptr Word8 -> CSize -> FunPtr McpServerCallback
    -> FunPtr McpServerFieldCallback -> Ptr () -> IO CInt

foreign export ccall ha_mcp_server_add
    :: Word64 -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr () -> CSize -> Ptr Word8 -> CSize -> Ptr () -> CSize
    -> CInt -> CInt -> FunPtr McpResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_mcp_server_edit
    :: Word64 -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr () -> CSize -> Ptr Word8 -> CSize -> Ptr () -> CSize
    -> CInt -> CInt -> FunPtr McpResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_mcp_server_enable
    :: Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_mcp_server_restart
    :: Ptr () -> Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_mcp_server_disable
    :: Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_mcp_server_remove
    :: Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpResultCallback -> Ptr () -> IO CInt

ha_mcp_servers_list
    :: FunPtr McpServerCallback -> FunPtr McpServerFieldCallback
    -> Ptr () -> IO CInt
ha_mcp_servers_list callback fieldCallback context
    | callback == nullFunPtr || fieldCallback == nullFunPtr = pure 1
    | otherwise = do
        home <- getHomeDirectory
        _ <- forkIO $
            mcpAdminTry (listMcpAdminServers home) >>= emitMcpServers
                callback fieldCallback context
        pure 0

ha_engine_mcp_server_restart
    :: Ptr () -> Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpResultCallback -> Ptr () -> IO CInt
ha_engine_mcp_server_restart pointer expected nameBytes (CSize nameLength)
        callback context
    | pointer == nullPtr = pure 1
    | callback == nullFunPtr = pure 2
    | nameBytes == nullPtr || nameLength == 0
        || nameLength > maxMcpTextBytes = pure 2
    | otherwise = do
        decodeMcpInput nameBytes nameLength >>= \case
            Left _ -> pure 2
            Right name -> do
                accepted <- tryAny do
                    let stable =
                            castPtrToStablePtr pointer :: StablePtr Engine
                    engine <- deRefStablePtr stable
                    atomically $ acceptEngineCommand engine.engineCommands
                        (EngineMcpRestart expected name callback context)
                pure case accepted of
                    Left _ -> 3
                    Right False -> 3
                    Right True -> 0

ha_mcp_server_status
    :: Ptr Word8 -> CSize -> FunPtr McpServerCallback
    -> FunPtr McpServerFieldCallback -> Ptr () -> IO CInt
ha_mcp_server_status = ha_mcp_server_read

ha_mcp_server_read
    :: Ptr Word8 -> CSize -> FunPtr McpServerCallback
    -> FunPtr McpServerFieldCallback -> Ptr () -> IO CInt
ha_mcp_server_read nameBytes (CSize nameLength) callback fieldCallback context
    | callback == nullFunPtr || fieldCallback == nullFunPtr = pure 1
    | nameBytes == nullPtr || nameLength == 0
        || nameLength > maxMcpTextBytes = pure 2
    | otherwise = do
        decodeMcpInput nameBytes nameLength >>= \case
            Left _ -> pure 2
            Right name -> do
                home <- getHomeDirectory
                _ <- forkIO $
                    mcpAdminTry (readMcpAdminServer home name) >>= emitMcpServer
                        callback fieldCallback context
                pure 0

ha_mcp_server_add
    :: Word64 -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr () -> CSize -> Ptr Word8 -> CSize -> Ptr () -> CSize
    -> CInt -> CInt -> FunPtr McpResultCallback -> Ptr () -> IO CInt
ha_mcp_server_add =
    mcpServerWrite addMcpAdminServer

ha_mcp_server_edit
    :: Word64 -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr () -> CSize -> Ptr Word8 -> CSize -> Ptr () -> CSize
    -> CInt -> CInt -> FunPtr McpResultCallback -> Ptr () -> IO CInt
ha_mcp_server_edit =
    mcpServerWrite editMcpAdminServer

ha_mcp_server_enable
    :: Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpResultCallback -> Ptr () -> IO CInt
ha_mcp_server_enable = mcpServerSetEnabled True

ha_mcp_server_disable
    :: Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpResultCallback -> Ptr () -> IO CInt
ha_mcp_server_disable = mcpServerSetEnabled False

ha_mcp_server_remove
    :: Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpResultCallback -> Ptr () -> IO CInt
ha_mcp_server_remove expected nameBytes (CSize nameLength) callback context
    | callback == nullFunPtr = pure 1
    | nameBytes == nullPtr || nameLength == 0
        || nameLength > maxMcpTextBytes = pure 2
    | otherwise = do
        decodeMcpInput nameBytes nameLength >>= \case
            Left _ -> pure 2
            Right name -> do
                home <- getHomeDirectory
                _ <- forkIO $
                    mcpAdminTry (removeMcpAdminServer home expected name)
                        >>= emitMcpResult
                        callback context
                pure 0

mcpServerSetEnabled
    :: Bool -> Word64 -> Ptr Word8 -> CSize
    -> FunPtr McpResultCallback -> Ptr () -> IO CInt
mcpServerSetEnabled enabled expected nameBytes (CSize nameLength)
        callback context
    | callback == nullFunPtr = pure 1
    | nameBytes == nullPtr || nameLength == 0
        || nameLength > maxMcpTextBytes = pure 2
    | otherwise = do
        decodeMcpInput nameBytes nameLength >>= \case
            Left _ -> pure 2
            Right name -> do
                home <- getHomeDirectory
                _ <- forkIO $
                    mcpAdminTry
                        (setMcpAdminServerEnabled home expected name enabled)
                        >>= emitMcpResult callback context
                pure 0

mcpServerWrite
    :: (OsPath -> Word64 -> Text -> McpAdminServerInput
        -> IO (Either McpAdminError (McpAdminSnapshot McpAdminServer)))
    -> Word64 -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr () -> CSize -> Ptr Word8 -> CSize -> Ptr () -> CSize
    -> CInt -> CInt -> FunPtr McpResultCallback -> Ptr () -> IO CInt
mcpServerWrite write expected nameBytes (CSize nameLength)
        commandBytes (CSize commandLength) argsPointer argsCount
        cwdBytes (CSize cwdLength) envPointer envCount
        (CInt startupTimeout) (CInt requestTimeout) callback context
    | callback == nullFunPtr = pure 1
    | nameBytes == nullPtr || nameLength == 0
        || nameLength > maxMcpTextBytes
        || commandBytes == nullPtr || commandLength == 0
        || commandLength > maxMcpTextBytes
        || cwdLength > maxMcpTextBytes
        || argsCount > maxMcpFields || envCount > maxMcpFields = pure 2
    | argsPointer == nullPtr && argsCount > 0
        || envPointer == nullPtr && envCount > 0
        || cwdBytes == nullPtr && cwdLength > 0 = pure 2
    | otherwise = do
        decoded <- tryAny do
            name <- requireMcpInput nameBytes nameLength
            command <- requireMcpInput commandBytes commandLength
            cwd <- requireMcpInput cwdBytes cwdLength
            args <- mapM (peekUtf8Slice argsPointer)
                [0 .. fromIntegral argsCount - 1]
            env <- Map.fromList <$> mapM (peekEnvEntry envPointer)
                [0 .. fromIntegral envCount - 1]
            pure (name, McpAdminServerInput
                { mcpAdminInputCommand = command
                , mcpAdminInputArgs = args
                , mcpAdminInputCwd = nonEmptyText cwd
                , mcpAdminInputEnv = env
                , mcpAdminInputStartupTimeoutSeconds =
                    fromIntegral startupTimeout
                , mcpAdminInputRequestTimeoutSeconds =
                    fromIntegral requestTimeout
                })
        case decoded of
            Left _ -> pure 2
            Right (name, input) -> do
                home <- getHomeDirectory
                _ <- forkIO $
                    mcpAdminTry (write home expected name input)
                        >>= emitMcpResult callback context
                pure 0

peekUtf8Slice :: Ptr () -> Int -> IO Text
peekUtf8Slice pointer index = do
    let pointerSize = sizeOf (nullPtr :: Ptr ())
        sizeSize = sizeOf (undefined :: CSize)
        base = pointer `plusPtr` (index * (pointerSize + sizeSize))
    bytes <- peekByteOff base 0
    CSize length <- peekByteOff base pointerSize
    if (bytes == (nullPtr :: Ptr Word8) && length > 0)
            || length > maxMcpTextBytes
        then ioError (userError "null UTF-8 slice")
        else requireMcpInput bytes length

decodeMcpInput :: Ptr Word8 -> Word64 -> IO (Either Text Text)
decodeMcpInput pointer length
    | pointer == nullPtr || length == 0 = pure (Right "")
    | otherwise = do
        bytes <- BS.packCStringLen (castPtr pointer, fromIntegral length)
        pure (first (Text.pack . show) (TextEncoding.decodeUtf8' bytes))

requireMcpInput :: Ptr Word8 -> Word64 -> IO Text
requireMcpInput pointer length =
    decodeMcpInput pointer length >>=
        either (ioError . userError . Text.unpack) pure

maxMcpTextBytes :: Word64
maxMcpTextBytes = 1024 * 1024

maxMcpFields :: CSize
maxMcpFields = 4096

peekEnvEntry :: Ptr () -> Int -> IO (Text, Text)
peekEnvEntry pointer index = do
    let sliceSize =
            sizeOf (nullPtr :: Ptr ()) + sizeOf (undefined :: CSize)
        base = pointer `plusPtr` (index * sliceSize * 2)
    key <- peekUtf8Slice base 0
    value <- peekUtf8Slice (base `plusPtr` sliceSize) 0
    pure (key, value)

emitMcpServers
    :: FunPtr McpServerCallback -> FunPtr McpServerFieldCallback -> Ptr ()
    -> Either McpAdminError (McpAdminSnapshot [McpAdminServer]) -> IO ()
emitMcpServers callback fieldCallback context = \case
    Left err -> emitMcpServerError callback context err
    Right snapshot -> do
        forM_ snapshot.mcpAdminValue \server ->
            emitMcpServerItem
                callback fieldCallback context snapshot.mcpAdminRevision server
        invokeMcpServerCallback callback context 1 snapshot.mcpAdminRevision
            nullPtr 0 0 nullPtr 0 nullPtr 0 0 0 0 0 nullPtr 0

emitMcpServer
    :: FunPtr McpServerCallback -> FunPtr McpServerFieldCallback -> Ptr ()
    -> Either McpAdminError (McpAdminSnapshot McpAdminServer) -> IO ()
emitMcpServer callback fieldCallback context = \case
    Left err -> emitMcpServerError callback context err
    Right snapshot ->
        emitMcpServerItem
            callback fieldCallback context snapshot.mcpAdminRevision
            snapshot.mcpAdminValue

emitMcpServerItem
    :: FunPtr McpServerCallback -> FunPtr McpServerFieldCallback -> Ptr ()
    -> Word64 -> McpAdminServer -> IO ()
emitMcpServerItem callback fieldCallback context revision server = do
    withText server.mcpAdminName \name nameLength -> do
        forM_ (zip [0..] server.mcpAdminArgs) \(index, argument) ->
            withText argument $
                invokeMcpServerFieldCallback fieldCallback context
                    name nameLength 0 index
        forM_ (zip [0..] server.mcpAdminEnvKeys) \(index, key) ->
            withText key $
                invokeMcpServerFieldCallback fieldCallback context
                    name nameLength 1 index
        withText server.mcpAdminCommand \command commandLength ->
            withOptionalText server.mcpAdminCwd \cwd cwdLength ->
                invokeMcpServerCallback callback context 0 revision
                    name nameLength
                    (if server.mcpAdminEnabled then 1 else 0)
                    command commandLength cwd cwdLength
                    (fromIntegral server.mcpAdminStartupTimeoutSeconds)
                    (fromIntegral server.mcpAdminRequestTimeoutSeconds)
                    (fromIntegral (length server.mcpAdminArgs))
                    (fromIntegral (length server.mcpAdminEnvKeys))
                    nullPtr 0

emitMcpServerError
    :: FunPtr McpServerCallback -> Ptr () -> McpAdminError -> IO ()
emitMcpServerError callback context err =
    withText (mcpAdminErrorText err) \errorPtr errorLength ->
        invokeMcpServerCallback callback context (-1)
            (mcpAdminErrorRevision err)
            nullPtr 0 0 nullPtr 0 nullPtr 0 0 0 0 0
            errorPtr errorLength

emitMcpResult
    :: FunPtr McpResultCallback -> Ptr ()
    -> Either McpAdminError (McpAdminSnapshot a) -> IO ()
emitMcpResult callback context = \case
    Left err ->
        withText (mcpAdminErrorText err) $
            invokeMcpResultCallback callback context (-1)
                (mcpAdminErrorRevision err)
    Right snapshot ->
        invokeMcpResultCallback callback context 0 snapshot.mcpAdminRevision
            nullPtr 0

mcpAdminErrorRevision :: McpAdminError -> Word64
mcpAdminErrorRevision = \case
    McpAdminConflict revision -> revision
    _ -> 0

mcpAdminErrorText :: McpAdminError -> Text
mcpAdminErrorText = \case
    McpAdminConflict _ -> "MCP catalog changed; reload before editing"
    McpAdminNotFound name -> "MCP server not found: " <> name
    McpAdminAlreadyExists name -> "MCP server already exists: " <> name
    McpAdminInvalid err -> err

mcpAdminTry
    :: IO (Either McpAdminError a)
    -> IO (Either McpAdminError a)
mcpAdminTry action =
    tryAny action >>= \case
        Left exception ->
            pure (Left (McpAdminInvalid (Text.pack (show exception))))
        Right result -> pure result
foreign export ccall ha_repository_snapshot
    :: Ptr Word8 -> CSize
    -> FunPtr RepositorySnapshotCallback
    -> FunPtr RepositoryFileCallback
    -> FunPtr RepositoryResultCallback
    -> Ptr () -> IO CInt

foreign export ccall ha_repository_diff
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize
    -> FunPtr RepositoryDiffCallback
    -> FunPtr RepositoryHunkCallback
    -> FunPtr RepositoryResultCallback
    -> Ptr () -> IO CInt

foreign export ccall ha_repository_apply_path
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CInt
    -> Ptr Word8 -> CSize -> FunPtr RepositoryResultCallback
    -> Ptr () -> IO CInt

foreign export ccall ha_repository_apply_hunks
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CInt
    -> Ptr Word8 -> CSize -> Ptr CSize -> CSize
    -> FunPtr RepositoryResultCallback
    -> Ptr () -> IO CInt

foreign export ccall ha_repository_commit
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_repository_delivery_status
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryDeliveryStatusCallback -> Ptr () -> IO CInt

foreign export ccall ha_repository_push_preview
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryPushPreviewCallback -> Ptr () -> IO CInt

foreign export ccall ha_repository_push_confirm
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryPushResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_repository_pr_preview
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryPullRequestPreviewCallback -> Ptr () -> IO CInt

foreign export ccall ha_repository_pr_confirm
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryPullRequestResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_repository_cancel_all :: IO ()

foreign export ccall ha_repository_check_start
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr () -> CSize
    -> FunPtr RepositoryCheckOutputCallback
    -> FunPtr RepositoryCheckExitCallback
    -> Ptr () -> Ptr (Ptr ()) -> IO CInt

foreign export ccall ha_repository_check_cancel :: Ptr () -> IO ()

foreign export ccall ha_repository_check_destroy :: Ptr () -> IO ()

ha_repository_snapshot
    :: Ptr Word8 -> CSize
    -> FunPtr RepositorySnapshotCallback
    -> FunPtr RepositoryFileCallback
    -> FunPtr RepositoryResultCallback
    -> Ptr () -> IO CInt
ha_repository_snapshot pathBytes pathLength snapshotCallback fileCallback
    resultCallback context
    | snapshotCallback == nullFunPtr
        || fileCallback == nullFunPtr
        || resultCallback == nullFunPtr = pure 1
    | otherwise =
        copyRequiredText pathBytes pathLength >>= \case
            Left _ -> pure 2
            Right path -> do
                started <- startRepositoryWorker
                    (emitRepositoryCancelled resultCallback context) do
                    tryRepositorySynchronous
                        (RepositoryReview.repositorySnapshot (Text.unpack path))
                        >>= \case
                            Left exception ->
                                pure
                                    (emitRepositoryFailure
                                        resultCallback
                                        context
                                        (Text.pack (show exception)))
                            Right (Left err) ->
                                pure
                                    (emitRepositoryError
                                        resultCallback context err)
                            Right (Right snapshot) -> do
                                streamed <- tryRepositorySynchronous do
                                    withRepositorySnapshot snapshot $
                                        invokeRepositorySnapshotCallback
                                            snapshotCallback
                                            context
                                    forM_ snapshot.snapshotFiles \file ->
                                        withText
                                            (Text.pack file.repositoryFilePath)
                                            \pathPtr pathSize ->
                                        withNullableText
                                            (Text.pack
                                                <$> file.repositoryFileOriginalPath)
                                            \originalPtr originalSize ->
                                                invokeRepositoryFileCallback
                                                    fileCallback
                                                    context
                                                    pathPtr pathSize
                                                    originalPtr originalSize
                                                    (fromIntegral
                                                        (ord
                                                            file.repositoryFileIndexStatus))
                                                    (fromIntegral
                                                        (ord
                                                            file.repositoryFileWorktreeStatus))
                                case streamed of
                                    Left exception ->
                                        pure
                                            (emitRepositoryFailure
                                                resultCallback
                                                context
                                                (Text.pack (show exception)))
                                    Right () ->
                                        pure
                                            (emitRepositorySuccess
                                                resultCallback
                                                context
                                                snapshot.snapshotId)
                pure (if started then 0 else 3)

ha_repository_diff
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize
    -> FunPtr RepositoryDiffCallback
    -> FunPtr RepositoryHunkCallback
    -> FunPtr RepositoryResultCallback
    -> Ptr () -> IO CInt
ha_repository_diff pathBytes pathLength snapshotBytes snapshotLength
    diffKind fileBytes fileLength diffCallback hunkCallback resultCallback context
    | diffCallback == nullFunPtr
        || hunkCallback == nullFunPtr
        || resultCallback == nullFunPtr = pure 1
    | otherwise =
        copyRequiredTexts
            [ (pathBytes, pathLength)
            , (snapshotBytes, snapshotLength)
            , (fileBytes, fileLength)
            ] >>= \case
                Left _ -> pure 2
                Right [path, expected, file] ->
                    case repositoryDiffKind diffKind of
                        Nothing -> pure 2
                        Just kind -> do
                            started <- startRepositoryWorker
                                (emitRepositoryCancelled resultCallback context) do
                                tryRepositorySynchronous
                                    (RepositoryReview.repositoryDiff
                                        (Text.unpack path)
                                        expected
                                        kind
                                        (Text.unpack file))
                                    >>= \case
                                        Left exception ->
                                            pure
                                                (emitRepositoryFailure
                                                    resultCallback
                                                    context
                                                    (Text.pack (show exception)))
                                        Right (Left err) ->
                                            pure
                                                (emitRepositoryError
                                                    resultCallback context err)
                                        Right (Right diff) -> do
                                            streamed <- tryRepositorySynchronous do
                                                forM_
                                                    (byteStringChunks
                                                        (64 * 1024)
                                                        diff.repositoryDiffPatch)
                                                    \chunk ->
                                                        BS.useAsCStringLen chunk
                                                            \(pointer, length) ->
                                                                invokeRepositoryDiffCallback
                                                                    diffCallback
                                                                    context
                                                                    (castPtr pointer)
                                                                    (fromIntegral length)
                                                                    (if
                                                                        diff.repositoryDiffBinary
                                                                        then 1
                                                                        else 0)
                                                forM_
                                                    diff.repositoryDiffHunks
                                                    \hunk ->
                                                        withText
                                                            hunk.hunkHeader
                                                            \headerPtr
                                                                headerLength ->
                                                                    invokeRepositoryHunkCallback
                                                                        hunkCallback
                                                                        context
                                                                        (fromIntegral hunk.hunkOldStart)
                                                                        (fromIntegral hunk.hunkOldCount)
                                                                        (fromIntegral hunk.hunkNewStart)
                                                                        (fromIntegral hunk.hunkNewCount)
                                                                        headerPtr
                                                                        headerLength
                                            case streamed of
                                                Left exception ->
                                                    pure
                                                        (emitRepositoryFailure
                                                            resultCallback
                                                            context
                                                            (Text.pack
                                                                (show exception)))
                                                Right () ->
                                                    pure
                                                        (emitRepositorySuccess
                                                            resultCallback
                                                            context
                                                            expected)
                            pure (if started then 0 else 3)
                Right _ -> pure 3

ha_repository_apply_path
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CInt
    -> Ptr Word8 -> CSize -> FunPtr RepositoryResultCallback
    -> Ptr () -> IO CInt
ha_repository_apply_path pathBytes pathLength snapshotBytes snapshotLength
    operation fileBytes fileLength callback context
    | callback == nullFunPtr = pure 1
    | otherwise =
        copyRequiredTexts
            [ (pathBytes, pathLength)
            , (snapshotBytes, snapshotLength)
            , (fileBytes, fileLength)
            ] >>= \case
                Left _ -> pure 2
                Right [path, expected, file] ->
                    case repositoryPathMutation operation (Text.unpack file) of
                        Nothing -> pure 2
                        Just mutation -> do
                            started <- startRepositoryMutation
                                callback context (Text.unpack path) expected mutation
                            pure (if started then 0 else 3)
                Right _ -> pure 3

ha_repository_apply_hunks
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CInt
    -> Ptr Word8 -> CSize -> Ptr CSize -> CSize
    -> FunPtr RepositoryResultCallback
    -> Ptr () -> IO CInt
ha_repository_apply_hunks pathBytes pathLength snapshotBytes snapshotLength
    operation fileBytes fileLength hunkIndices hunkCount callback context
    | callback == nullFunPtr = pure 1
    | hunkIndices == nullPtr || hunkCount == 0 = pure 2
    | hunkCount > 4096 = pure 2
    | fromIntegral hunkCount
        > (maxBound :: Int) `div` sizeOf (undefined :: CSize) = pure 2
    | otherwise =
        copyRequiredTexts
            [ (pathBytes, pathLength)
            , (snapshotBytes, snapshotLength)
            , (fileBytes, fileLength)
            ]
            >>= \case
                Left _ -> pure 2
                Right [path, expected, file] -> do
                    rawIndices <- mapM
                        (\index ->
                            (peekByteOff
                                    hunkIndices
                                    (index * sizeOf (undefined :: CSize))
                                    :: IO CSize))
                        [0 .. fromIntegral hunkCount - 1]
                    if any
                        ((> toInteger (maxBound :: Int)) . toInteger)
                        rawIndices
                        then pure 2
                        else
                            case repositoryHunkMutation
                                operation
                                (Text.unpack file)
                                (map fromIntegral rawIndices) of
                                    Nothing -> pure 2
                                    Just mutation -> do
                                        started <- startRepositoryMutation
                                            callback
                                            context
                                            (Text.unpack path)
                                            expected
                                            mutation
                                        pure (if started then 0 else 3)
                Right _ -> pure 3

ha_repository_commit
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryResultCallback -> Ptr () -> IO CInt
ha_repository_commit pathBytes pathLength snapshotBytes snapshotLength
    messageBytes messageLength callback context
    | callback == nullFunPtr = pure 1
    | otherwise =
        copyRequiredTexts
            [ (pathBytes, pathLength)
            , (snapshotBytes, snapshotLength)
            , (messageBytes, messageLength)
            ] >>= \case
                Left _ -> pure 2
                Right [path, expected, message] -> do
                    started <- startRepositoryWorker
                        (emitRepositoryCancelled callback context) $
                        prepareRepositoryResult callback context $
                            (RepositoryReview.commitRepository
                                (Text.unpack path)
                                expected
                                message)
                    pure (if started then 0 else 3)
                Right _ -> pure 3

ha_repository_delivery_status
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryDeliveryStatusCallback -> Ptr () -> IO CInt
ha_repository_delivery_status pathBytes pathLength snapshotBytes snapshotLength
    callback context
    | callback == nullFunPtr = pure 1
    | not (deliveryInputValid pathBytes pathLength deliveryPathLimit)
        || not (deliveryInputValid
            snapshotBytes snapshotLength deliveryTokenLimit) = pure 2
    | otherwise =
        copyRequiredTexts
            [(pathBytes, pathLength), (snapshotBytes, snapshotLength)] >>= \case
                Right [path, snapshot] -> do
                    terminal <- newMVar False
                    started <- startRepositoryWorker
                        (emitDeliveryOnce terminal $
                            emitDeliveryStatusFailure callback context (-3)
                                "repository delivery was cancelled") $
                        prepareDeliveryResult terminal
                            (RepositoryDelivery.repositoryDeliveryStatus
                                (Text.unpack path)
                                snapshot)
                            (emitDeliveryStatusFailure callback context (-1)
                                "repository delivery failed")
                            (emitDeliveryStatusFailure callback context)
                            (emitDeliveryStatus callback context)
                    pure (if started then 0 else 3)
                _ -> pure 2

ha_repository_push_preview
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryPushPreviewCallback -> Ptr () -> IO CInt
ha_repository_push_preview pathBytes pathLength snapshotBytes snapshotLength
    callback context
    | callback == nullFunPtr = pure 1
    | not (deliveryInputValid pathBytes pathLength deliveryPathLimit)
        || not (deliveryInputValid
            snapshotBytes snapshotLength deliveryTokenLimit) = pure 2
    | otherwise =
        copyRequiredTexts
            [(pathBytes, pathLength), (snapshotBytes, snapshotLength)] >>= \case
                Right [path, snapshot] -> do
                    terminal <- newMVar False
                    started <- startRepositoryWorker
                        (emitDeliveryOnce terminal $
                            emitPushPreviewFailure callback context (-3)
                                "repository push preview was cancelled") $
                        prepareDeliveryResult terminal
                            (RepositoryDelivery.previewRepositoryPush
                                (Text.unpack path)
                                snapshot)
                            (emitPushPreviewFailure callback context (-1)
                                "repository push preview failed")
                            (emitPushPreviewFailure callback context)
                            (emitPushPreview callback context)
                    pure (if started then 0 else 3)
                _ -> pure 2

ha_repository_push_confirm
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryPushResultCallback -> Ptr () -> IO CInt
ha_repository_push_confirm pathBytes pathLength tokenBytes tokenLength
    callback context
    | callback == nullFunPtr = pure 1
    | not (deliveryInputValid pathBytes pathLength deliveryPathLimit)
        || not (deliveryInputValid tokenBytes tokenLength deliveryTokenLimit) =
            pure 2
    | otherwise =
        copyRequiredTexts
            [(pathBytes, pathLength), (tokenBytes, tokenLength)] >>= \case
                Right [path, token] -> do
                    terminal <- newMVar False
                    started <- startRepositoryWorker
                        (emitDeliveryOnce terminal $
                            emitPushResultFailure callback context (-3)
                                "repository push was cancelled") $
                        prepareDeliveryResult terminal
                            (RepositoryDelivery.confirmRepositoryPush
                                (Text.unpack path)
                                token)
                            (emitPushResultFailure callback context (-1)
                                "repository push failed")
                            (emitPushResultFailure callback context)
                            (\status ->
                                        withText status.deliverySnapshotId
                                            \snapshotPtr snapshotSize ->
                                        withText status.deliveryHeadOid
                                            \headPtr headSize ->
                                                invokeRepositoryPushResultCallback
                                                    callback context 0
                                                    snapshotPtr snapshotSize
                                                    headPtr headSize
                                                    nullPtr 0)
                    pure (if started then 0 else 3)
                _ -> pure 2

ha_repository_pr_preview
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryPullRequestPreviewCallback -> Ptr () -> IO CInt
ha_repository_pr_preview pathBytes pathLength snapshotBytes snapshotLength
    baseBytes baseLength titleBytes titleLength bodyBytes bodyLength
    callback context
    | callback == nullFunPtr = pure 1
    | not (deliveryInputValid pathBytes pathLength deliveryPathLimit)
        || not (deliveryInputValid
            snapshotBytes snapshotLength deliveryTokenLimit)
        || not (deliveryInputValid baseBytes baseLength deliveryRefLimit)
        || not (deliveryInputValid titleBytes titleLength deliveryTitleLimit)
        || not (deliveryInputValid bodyBytes bodyLength deliveryBodyLimit) =
            pure 2
    | otherwise =
        copyRequiredTexts
            [ (pathBytes, pathLength)
            , (snapshotBytes, snapshotLength)
            , (baseBytes, baseLength)
            , (titleBytes, titleLength)
            , (bodyBytes, bodyLength)
            ] >>= \case
                Right [path, snapshot, base, title, body] -> do
                    terminal <- newMVar False
                    started <- startRepositoryWorker
                        (emitDeliveryOnce terminal $
                            emitPullRequestPreviewFailure
                                callback context (-3)
                                "pull-request preview was cancelled") $
                        prepareDeliveryResult terminal
                            (RepositoryDelivery.previewPullRequest
                                (Text.unpack path)
                                snapshot base title body)
                            (emitPullRequestPreviewFailure
                                callback context (-1)
                                "pull-request preview failed")
                            (emitPullRequestPreviewFailure callback context)
                            (emitPullRequestPreview callback context)
                    pure (if started then 0 else 3)
                _ -> pure 2

ha_repository_pr_confirm
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr RepositoryPullRequestResultCallback -> Ptr () -> IO CInt
ha_repository_pr_confirm pathBytes pathLength tokenBytes tokenLength
    callback context
    | callback == nullFunPtr = pure 1
    | not (deliveryInputValid pathBytes pathLength deliveryPathLimit)
        || not (deliveryInputValid tokenBytes tokenLength deliveryTokenLimit) =
            pure 2
    | otherwise =
        copyRequiredTexts
            [(pathBytes, pathLength), (tokenBytes, tokenLength)] >>= \case
                Right [path, token] -> do
                    terminal <- newMVar False
                    started <- startRepositoryWorker
                        (emitDeliveryOnce terminal $
                            emitPullRequestResultFailure callback context (-3)
                                "pull-request creation was cancelled") $
                        prepareDeliveryResult terminal
                            (RepositoryDelivery.createPullRequest
                                (Text.unpack path)
                                token)
                            (emitPullRequestResultFailure callback context (-1)
                                "pull-request creation failed")
                            (emitPullRequestResultFailure callback context)
                            (\url ->
                                        withText url \urlPtr urlSize ->
                                            invokeRepositoryPullRequestResultCallback
                                                callback context 0
                                                urlPtr urlSize nullPtr 0)
                    pure (if started then 0 else 3)
                _ -> pure 2

startRepositoryMutation
    :: FunPtr RepositoryResultCallback
    -> Ptr ()
    -> FilePath
    -> Text
    -> RepositoryReview.RepositoryMutation
    -> IO Bool
startRepositoryMutation callback context path expected mutation =
    startRepositoryWorker (emitRepositoryCancelled callback context) $
        prepareRepositoryResult callback context $
            (RepositoryReview.mutateRepository path expected mutation)

prepareRepositoryResult
    :: FunPtr RepositoryResultCallback
    -> Ptr ()
    -> IO
        (Either
            RepositoryReview.RepositoryError
            RepositoryReview.RepositorySnapshot)
    -> IO (IO ())
prepareRepositoryResult callback context action =
    tryRepositorySynchronous action >>= \case
        Left exception ->
            pure
                (emitRepositoryFailure callback context
                    (Text.pack (show exception)))
        Right (Left err) ->
            pure (emitRepositoryError callback context err)
        Right (Right snapshot) ->
            pure
                (emitRepositorySuccess
                    callback context snapshot.snapshotId)

startRepositoryWorker :: IO () -> IO (IO ()) -> IO Bool
startRepositoryWorker onCancelled prepare =
    tryRepositorySynchronous
        (mask \_ -> do
            gate <- newEmptyMVar
            admitted <- modifyMVar repositoryWorkers \state ->
                case state.repositoryWorkerBarrier of
                    Just _ -> pure (state, False)
                    Nothing -> do
                        let workerId = state.repositoryWorkerNextId
                        worker <- asyncWithUnmask \unmask -> do
                            withRepositoryCallbackThread
                                (((Just <$> (readMVar gate >> unmask prepare))
                                    `catchAsync`
                                        \(_ :: SomeAsyncException) ->
                                            onCancelled >> pure Nothing)
                                    >>= mapM_ id)
                                `finally`
                                unregisterRepositoryWorker workerId
                        pure
                            ( state
                                { repositoryWorkerNextId = workerId + 1
                                , repositoryWorkerActive =
                                    Map.insert
                                        workerId
                                        worker
                                        state.repositoryWorkerActive
                                }
                            , True
                            )
            when admitted (putMVar gate ())
            pure admitted)
        >>= \case
            Left _ -> pure False
            Right admitted -> pure admitted

unregisterRepositoryWorker :: Int -> IO ()
unregisterRepositoryWorker workerId =
    modifyMVar repositoryWorkers \state ->
        pure
            ( state
                { repositoryWorkerActive =
                    Map.delete workerId state.repositoryWorkerActive
                }
            , ()
            )

ha_repository_cancel_all :: IO ()
ha_repository_cancel_all =
    isRepositoryCallbackThread >>= \case
        True -> pure ()
        False -> mask \restore -> do
            admission <- modifyMVar repositoryWorkers \state ->
                case state.repositoryWorkerBarrier of
                    Just barrier ->
                        pure (state, Left barrier)
                    Nothing -> do
                        barrier <- newEmptyMVar
                        pure
                            ( state
                                { repositoryWorkerActive = Map.empty
                                , repositoryWorkerBarrier = Just barrier
                                }
                            , Right
                                ( barrier
                                , Map.elems state.repositoryWorkerActive
                                )
                            )
            case admission of
                Left activeBarrier -> restore (readMVar activeBarrier)
                Right (barrier, workers) ->
                    restore
                        (mapM_ cancel workers >> mapM_ waitCatch workers)
                        `finally` do
                            modifyMVar repositoryWorkers \state ->
                                pure
                                    ( state
                                        { repositoryWorkerBarrier = Nothing }
                                    , ()
                                    )
                            putMVar barrier ()

-- Deterministic regression hook for the admission-barrier lifecycle. This is
-- a Haskell test hook, not part of the C ABI.
repositoryCancelAllAdmissionSmoke :: IO Bool
repositoryCancelAllAdmissionSmoke = do
    entered <- newEmptyMVar
    cancelled <- newEmptyMVar
    firstAccepted <- startRepositoryWorker (putMVar cancelled ()) do
        putMVar entered ()
        uninterruptibleMask_ (threadDelay 250_000)
        pure (pure ())
    if not firstAccepted
        then pure False
        else do
            readMVar entered
            withAsync ha_repository_cancel_all \canceller -> do
                rejectedDuringBarrier <- awaitRejection 1000
                _ <- waitCatch canceller
                cancellationDelivered <- readMVar cancelled >> pure True
                completed <- newEmptyMVar
                acceptedAfter <- startRepositoryWorker
                    (pure ())
                    (pure (putMVar completed ()))
                finishedAfter <- if acceptedAfter
                    then readMVar completed >> pure True
                    else pure False
                pure
                    ( rejectedDuringBarrier
                        && cancellationDelivered
                        && finishedAfter
                    )
  where
    awaitRejection attempts
        | attempts <= (0 :: Int) = pure False
        | otherwise =
            startRepositoryWorker (pure ()) (pure (pure ())) >>= \case
                False -> pure True
                True -> threadDelay 1000 >> awaitRejection (attempts - 1)

repositoryCancelAllReentrancySmoke :: IO Bool
repositoryCancelAllReentrancySmoke = do
    completed <- newEmptyMVar
    accepted <- startRepositoryWorker (pure ()) do
        pure do
            ha_repository_cancel_all
            putMVar completed ()
    if accepted
        then readMVar completed >> pure True
        else pure False

repositoryCancelClassificationSmoke :: IO Bool
repositoryCancelClassificationSmoke = do
    entered <- newEmptyMVar
    cancelled <- newEmptyMVar
    synthesizedFailure <- newEmptyMVar
    accepted <- startRepositoryWorker (putMVar cancelled ()) do
        putMVar entered ()
        tryRepositorySynchronous (threadDelay 30_000_000) >>= \case
            Left _ -> putMVar synthesizedFailure ()
            Right () -> pure ()
        pure (pure ())
    if not accepted
        then pure False
        else do
            readMVar entered
            ha_repository_cancel_all
            cancellation <- tryReadMVar cancelled
            failure <- tryReadMVar synthesizedFailure
            pure (isJust cancellation && isNothing failure)

repositoryTerminalThrowSmoke :: IO Bool
repositoryTerminalThrowSmoke = do
    cancelled <- newEmptyMVar
    terminal <- newEmptyMVar
    finished <- newEmptyMVar
    accepted <- startRepositoryWorker (putMVar cancelled ()) do
        pure
            ((putMVar terminal () >> throwString "terminal callback failed")
                `finally` putMVar finished ())
    if not accepted
        then pure False
        else do
            readMVar terminal
            readMVar finished
            ha_repository_cancel_all
            cancellation <- tryReadMVar cancelled
            pure (isNothing cancellation)

{-# NOINLINE repositoryWorkers #-}
repositoryWorkers :: MVar RepositoryWorkerState
repositoryWorkers = unsafePerformIO
    (newMVar RepositoryWorkerState
        { repositoryWorkerNextId = 0
        , repositoryWorkerActive = Map.empty
        , repositoryWorkerBarrier = Nothing
        })

data RepositoryWorkerState = RepositoryWorkerState
    { repositoryWorkerNextId :: !Int
    , repositoryWorkerActive :: !(Map Int (Async ()))
    , repositoryWorkerBarrier :: !(Maybe (MVar ()))
    }

{-# NOINLINE repositoryCallbackThreads #-}
repositoryCallbackThreads :: MVar (Set.Set ThreadId)
repositoryCallbackThreads = unsafePerformIO (newMVar Set.empty)

withRepositoryCallbackThread :: IO value -> IO value
withRepositoryCallbackThread action = do
    thread <- myThreadId
    modifyMVar repositoryCallbackThreads \threads ->
        pure (Set.insert thread threads, ())
    action `finally`
        modifyMVar repositoryCallbackThreads \threads ->
            pure (Set.delete thread threads, ())

isRepositoryCallbackThread :: IO Bool
isRepositoryCallbackThread = do
    thread <- myThreadId
    Set.member thread <$> readMVar repositoryCallbackThreads

tryRepositorySynchronous
    :: IO value
    -> IO (Either SomeException value)
tryRepositorySynchronous action =
    tryAny action >>= \case
        Left exception
            | isAsyncException exception -> throwIO exception
            | otherwise -> pure (Left exception)
        Right value -> pure (Right value)

data RepositoryCheckHandle = RepositoryCheckHandle
    { repositoryCheckValue :: !(IORef (Maybe RepositoryReview.RepositoryCheck))
    , repositoryCheckCancelRequested :: !(IORef Bool)
    , repositoryCheckOwner :: !(Async ())
    }

ha_repository_check_start
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr () -> CSize
    -> FunPtr RepositoryCheckOutputCallback
    -> FunPtr RepositoryCheckExitCallback
    -> Ptr () -> Ptr (Ptr ()) -> IO CInt
ha_repository_check_start pathBytes pathLength snapshotBytes snapshotLength
    executableBytes executableLength argumentsPointer argumentCount
    outputCallback exitCallback context outCheck
    | outputCallback == nullFunPtr
        || exitCallback == nullFunPtr
        || outCheck == nullPtr = pure 1
    | argumentsPointer == nullPtr && argumentCount > 0 = pure 2
    | argumentCount > fromIntegral maxRepositoryCheckArguments = pure 2
    | otherwise =
        copyRequiredTexts
            [ (pathBytes, pathLength)
            , (snapshotBytes, snapshotLength)
            , (executableBytes, executableLength)
            ] >>= \case
                Left _ -> pure 2
                Right [path, expected, executable] -> do
                    copiedArguments <- copyRepositoryCheckArguments
                        argumentsPointer argumentCount
                    case copiedArguments of
                        Left _ -> pure 2
                        Right arguments -> mask \_ -> do
                            poke outCheck nullPtr
                            checkRef <- newIORef Nothing
                            cancelRef <- newIORef False
                            gate <- newEmptyMVar
                            owner <- asyncWithUnmask \unmask ->
                                readMVar gate >> do
                                    -- asyncWithUnmask starts masked. Keep
                                    -- acquisition and publication in that
                                    -- masked region so ownership transfer is
                                    -- atomic with respect to cancellation. Its
                                    -- blocking I/O remains interruptible under
                                    -- masked-interruptible semantics and owns
                                    -- cleanup brackets.
                                    started <- tryRepositorySynchronous
                                        (RepositoryReview.startRepositoryCheck
                                            (Text.unpack path)
                                            expected
                                            (Text.unpack executable)
                                            (map Text.unpack arguments)
                                            (\stream bytes ->
                                                BS.useAsCStringLen bytes
                                                    \(pointer, length) ->
                                                        withRepositoryCallbackThread
                                                            (invokeRepositoryCheckOutputCallback
                                                                outputCallback
                                                                context
                                                                (case stream of
                                                                    RepositoryReview.RepositoryCheckStdout -> 1
                                                                    RepositoryReview.RepositoryCheckStderr -> 2)
                                                                (castPtr pointer)
                                                                (fromIntegral length)))
                                            (\cancelled exitCode ->
                                                withRepositoryCallbackThread
                                                    (invokeRepositoryCheckExitCallback
                                                        exitCallback
                                                        context
                                                        (if cancelled then 1 else 0)
                                                        (case exitCode of
                                                            System.Exit.ExitSuccess -> 0
                                                            System.Exit.ExitFailure code ->
                                                                fromIntegral code)
                                                        nullPtr 0)))
                                    case started of
                                        Left exception ->
                                            unmask
                                                (withText
                                                    (Text.pack (show exception))
                                                    \errorPtr errorLength ->
                                                        withRepositoryCallbackThread
                                                            (invokeRepositoryCheckExitCallback
                                                                exitCallback
                                                                context 0 (-1)
                                                                errorPtr errorLength))
                                        Right (Left err) ->
                                            unmask
                                                (withText
                                                    (RepositoryReview.repositoryErrorText
                                                        err)
                                                    \errorPtr errorLength ->
                                                        withRepositoryCallbackThread
                                                            (invokeRepositoryCheckExitCallback
                                                                exitCallback
                                                                context 0 (-1)
                                                                errorPtr errorLength))
                                        Right (Right check) -> do
                                            writeIORef checkRef (Just check)
                                            cancelRequested <- readIORef cancelRef
                                            when cancelRequested
                                                (RepositoryReview.cancelRepositoryCheck
                                                    check)
                                            unmask
                                                (RepositoryReview.waitRepositoryCheck
                                                    check)
                                                `onException`
                                                    RepositoryReview.cancelRepositoryCheck
                                                        check
                            stable <- newStablePtr RepositoryCheckHandle
                                { repositoryCheckValue = checkRef
                                , repositoryCheckCancelRequested = cancelRef
                                , repositoryCheckOwner = owner
                                }
                                `onException` cancel owner
                            (do
                                poke outCheck (castStablePtrToPtr stable)
                                -- No callback can begin before the owned
                                -- handle is visible through out_check.
                                putMVar gate ()
                                pure 0)
                                `onException` do
                                    cancel owner
                                    freeStablePtr stable
                                    poke outCheck nullPtr
                Right _ -> pure 3

ha_repository_check_cancel :: Ptr () -> IO ()
ha_repository_check_cancel pointer
    | pointer == nullPtr = pure ()
    | otherwise = do
        isRepositoryCallbackThread >>= \case
            True -> pure ()
            False -> do
                let stable =
                        castPtrToStablePtr pointer
                            :: StablePtr RepositoryCheckHandle
                handle <- deRefStablePtr stable
                writeIORef handle.repositoryCheckCancelRequested True
                readIORef handle.repositoryCheckValue >>= mapM_
                    RepositoryReview.cancelRepositoryCheck

ha_repository_check_destroy :: Ptr () -> IO ()
ha_repository_check_destroy pointer
    | pointer == nullPtr = pure ()
    | otherwise = do
        isRepositoryCallbackThread >>= \case
            True -> pure ()
            False -> do
                let stable =
                        castPtrToStablePtr pointer
                            :: StablePtr RepositoryCheckHandle
                handle <- deRefStablePtr stable
                _ <- waitCatch handle.repositoryCheckOwner
                freeStablePtr stable

repositoryCheckDestroyReentrancySmoke :: IO Bool
repositoryCheckDestroyReentrancySmoke = do
    gate <- newEmptyMVar
    owner <- asyncWithUnmask \unmask -> unmask (readMVar gate)
    value <- newIORef Nothing
    cancelled <- newIORef False
    stable <- newStablePtr RepositoryCheckHandle
        { repositoryCheckValue = value
        , repositoryCheckCancelRequested = cancelled
        , repositoryCheckOwner = owner
        }
    let pointer = castStablePtrToPtr stable
    result <- tryAny
        (withRepositoryCallbackThread
            (ha_repository_check_destroy pointer))
    putMVar gate ()
    ha_repository_check_destroy pointer
    pure (isRight result)

copyRepositoryCheckArguments
    :: Ptr () -> CSize -> IO (Either () [Text])
copyRepositoryCheckArguments pointer count
    | count > fromIntegral maxRepositoryCheckArguments = pure (Left ())
    | itemSize <= 0 = pure (Left ())
    | otherwise = go 0 (0 :: Int) []
  where
    pointerSize = sizeOf (nullPtr :: Ptr ())
    sizeSize = sizeOf (undefined :: CSize)
    itemSize = pointerSize + sizeSize
    countInt = fromIntegral count
    go index total acc
        | index >= countInt = pure (Right (reverse acc))
        | index > maxBound `div` itemSize = pure (Left ())
        | otherwise = do
            let base = castPtr pointer `plusPtr` (index * itemSize)
            bytes <- peekByteOff base 0 :: IO (Ptr Word8)
            length <- peekByteOff base pointerSize :: IO CSize
            if length > fromIntegral maxRepositoryCheckArgumentBytes
                || toInteger total + toInteger length
                    > toInteger maxRepositoryCheckTotalArgumentBytes
                then pure (Left ())
                else copyRequiredText bytes length >>= \case
                    Left _ -> pure (Left ())
                    Right value ->
                        go
                            (index + 1)
                            (total + fromIntegral length)
                            (value : acc)

maxRepositoryCheckArguments :: Int
maxRepositoryCheckArguments = 4096

maxRepositoryCheckArgumentBytes :: Int
maxRepositoryCheckArgumentBytes = 1024 * 1024

maxRepositoryCheckTotalArgumentBytes :: Int
maxRepositoryCheckTotalArgumentBytes = 8 * 1024 * 1024

repositoryPathMutation
    :: CInt -> FilePath -> Maybe RepositoryReview.RepositoryMutation
repositoryPathMutation operation path = case operation of
    0 -> Just (RepositoryReview.StagePath path)
    1 -> Just (RepositoryReview.UnstagePath path)
    2 -> Just (RepositoryReview.RestorePath path)
    _ -> Nothing

repositoryDiffKind
    :: CInt -> Maybe RepositoryReview.RepositoryDiffKind
repositoryDiffKind kind = case kind of
    0 -> Just RepositoryReview.RepositoryWorktreeDiff
    1 -> Just RepositoryReview.RepositoryStagedDiff
    _ -> Nothing

repositoryHunkMutation
    :: CInt
    -> FilePath
    -> [Int]
    -> Maybe RepositoryReview.RepositoryMutation
repositoryHunkMutation operation path hunks = case operation of
    0 -> Just (RepositoryReview.StageHunks path hunks)
    1 -> Just (RepositoryReview.UnstageHunks path hunks)
    2 -> Just (RepositoryReview.RestoreHunks path hunks)
    _ -> Nothing

withRepositorySnapshot
    :: RepositoryReview.RepositorySnapshot
    -> (CString -> CSize -> CString -> CSize -> CString -> CSize
        -> CString -> CSize -> CString -> CSize -> IO value)
    -> IO value
withRepositorySnapshot snapshot action =
    withText snapshot.snapshotId \snapshotPtr snapshotLength ->
    withText (Text.pack snapshot.snapshotRoot) \rootPtr rootLength ->
    withOptionalText snapshot.snapshotHead \headPtr headLength ->
    withText snapshot.snapshotIndexFingerprint \indexPtr indexLength ->
    withText snapshot.snapshotWorktreeFingerprint
        \worktreePtr worktreeLength ->
            action snapshotPtr snapshotLength rootPtr rootLength
                headPtr headLength indexPtr indexLength
                worktreePtr worktreeLength

emitRepositorySuccess
    :: FunPtr RepositoryResultCallback -> Ptr () -> Text -> IO ()
emitRepositorySuccess callback context snapshotId =
    withText snapshotId \snapshotPtr snapshotLength ->
        invokeRepositoryResultCallback callback context 0
            snapshotPtr snapshotLength nullPtr 0

emitRepositoryFailure
    :: FunPtr RepositoryResultCallback -> Ptr () -> Text -> IO ()
emitRepositoryFailure callback context message =
    withText message \errorPtr errorLength ->
        invokeRepositoryResultCallback callback context (-1)
            nullPtr 0 errorPtr errorLength

emitRepositoryCancelled
    :: FunPtr RepositoryResultCallback -> Ptr () -> IO ()
emitRepositoryCancelled callback context =
    withText "cancelled" \errorPtr errorLength ->
        invokeRepositoryResultCallback callback context (-3)
            nullPtr 0 errorPtr errorLength

emitRepositoryError
    :: FunPtr RepositoryResultCallback
    -> Ptr ()
    -> RepositoryReview.RepositoryError
    -> IO ()
emitRepositoryError callback context err =
    case err of
        RepositoryReview.StaleRepositorySnapshot _ actual ->
            withText actual \snapshotPtr snapshotLength ->
            withText (RepositoryReview.repositoryErrorText err)
                \errorPtr errorLength ->
                    invokeRepositoryResultCallback callback context (-2)
                        snapshotPtr snapshotLength errorPtr errorLength
        _ ->
            emitRepositoryFailure
                callback context (RepositoryReview.repositoryErrorText err)

emitDeliveryOnce :: MVar Bool -> IO () -> IO ()
emitDeliveryOnce terminal callback =
    mask \_ -> do
        shouldRun <- modifyMVar terminal \completed ->
            pure (True, not completed)
        when shouldRun callback

prepareDeliveryResult
    :: MVar Bool
    -> IO (Either RepositoryDelivery.DeliveryError value)
    -> IO ()
    -> (CInt -> Text -> IO ())
    -> (value -> IO ())
    -> IO (IO ())
prepareDeliveryResult terminal operation unexpectedFailure knownFailure success =
    tryRepositorySynchronous operation >>= \case
        Left _ ->
            pure (emitDeliveryOnce terminal unexpectedFailure)
        Right (Left err) ->
            pure
                (emitDeliveryOnce terminal $
                    knownFailure
                        (deliveryErrorStatus err)
                        (RepositoryDelivery.deliveryErrorText err))
        Right (Right value) ->
            pure (emitDeliveryOnce terminal (success value))

emitDeliveryStatus
    :: FunPtr RepositoryDeliveryStatusCallback
    -> Ptr ()
    -> RepositoryDelivery.DeliveryStatus
    -> IO ()
emitDeliveryStatus callback context status =
    withText status.deliverySnapshotId \snapshotPtr snapshotSize ->
    withText status.deliveryHeadOid \headPtr headSize ->
    withText status.deliveryBranch \branchPtr branchSize ->
    withText status.deliveryRemote \remotePtr remoteSize ->
    withText status.deliveryUpstreamRef \upstreamPtr upstreamSize ->
    withText status.deliveryUpstreamOid \upstreamOidPtr upstreamOidSize ->
        invokeRepositoryDeliveryStatusCallback
            callback context 0
            snapshotPtr snapshotSize headPtr headSize
            branchPtr branchSize remotePtr remoteSize
            upstreamPtr upstreamSize upstreamOidPtr upstreamOidSize
            (fromIntegral status.deliveryAhead)
            (fromIntegral status.deliveryBehind)
            nullPtr 0

emitDeliveryStatusFailure
    :: FunPtr RepositoryDeliveryStatusCallback
    -> Ptr () -> CInt -> Text -> IO ()
emitDeliveryStatusFailure callback context status message =
    withText message \errorPtr errorSize ->
        invokeRepositoryDeliveryStatusCallback
            callback context status
            nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
            nullPtr 0 nullPtr 0 0 0 errorPtr errorSize

emitPushPreview
    :: FunPtr RepositoryPushPreviewCallback
    -> Ptr ()
    -> RepositoryDelivery.PushPreview
    -> IO ()
emitPushPreview callback context preview =
    let status = preview.pushPreviewStatus
        expires = fromIntegral
            (floor preview.pushPreviewExpiresAt :: Integer)
    in withText preview.pushPreviewConfirmation \tokenPtr tokenSize ->
    withText status.deliveryHeadOid \headPtr headSize ->
    withText status.deliveryBranch \branchPtr branchSize ->
    withText status.deliveryRemote \remotePtr remoteSize ->
    withText status.deliveryUpstreamRef \upstreamPtr upstreamSize ->
        invokeRepositoryPushPreviewCallback
            callback context 0 tokenPtr tokenSize expires
            headPtr headSize branchPtr branchSize remotePtr remoteSize
            upstreamPtr upstreamSize
            (fromIntegral status.deliveryAhead)
            (fromIntegral status.deliveryBehind)
            nullPtr 0

emitPushPreviewFailure
    :: FunPtr RepositoryPushPreviewCallback
    -> Ptr () -> CInt -> Text -> IO ()
emitPushPreviewFailure callback context status message =
    withText message \errorPtr errorSize ->
        invokeRepositoryPushPreviewCallback
            callback context status nullPtr 0 0
            nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
            0 0 errorPtr errorSize

emitPushResultFailure
    :: FunPtr RepositoryPushResultCallback
    -> Ptr () -> CInt -> Text -> IO ()
emitPushResultFailure callback context status message =
    withText message \errorPtr errorSize ->
        invokeRepositoryPushResultCallback
            callback context status
            nullPtr 0 nullPtr 0 errorPtr errorSize

emitPullRequestPreview
    :: FunPtr RepositoryPullRequestPreviewCallback
    -> Ptr ()
    -> RepositoryDelivery.PullRequestPreview
    -> IO ()
emitPullRequestPreview callback context preview =
    let expires = fromIntegral
            (floor preview.pullRequestExpiresAt :: Integer)
    in withText preview.pullRequestConfirmation \tokenPtr tokenSize ->
    withText preview.pullRequestRepository \repositoryPtr repositorySize ->
    withText preview.pullRequestBaseRef \basePtr baseSize ->
    withText preview.pullRequestHeadRef \headPtr headSize ->
    withText preview.pullRequestTitle \titlePtr titleSize ->
        invokeRepositoryPullRequestPreviewCallback
            callback context 0 tokenPtr tokenSize expires
            repositoryPtr repositorySize basePtr baseSize
            headPtr headSize titlePtr titleSize nullPtr 0

emitPullRequestPreviewFailure
    :: FunPtr RepositoryPullRequestPreviewCallback
    -> Ptr () -> CInt -> Text -> IO ()
emitPullRequestPreviewFailure callback context status message =
    withText message \errorPtr errorSize ->
        invokeRepositoryPullRequestPreviewCallback
            callback context status nullPtr 0 0
            nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 errorPtr errorSize

emitPullRequestResultFailure
    :: FunPtr RepositoryPullRequestResultCallback
    -> Ptr () -> CInt -> Text -> IO ()
emitPullRequestResultFailure callback context status message =
    withText message \errorPtr errorSize ->
        invokeRepositoryPullRequestResultCallback
            callback context status nullPtr 0 errorPtr errorSize

deliveryErrorStatus :: RepositoryDelivery.DeliveryError -> CInt
deliveryErrorStatus = \case
    RepositoryDelivery.DeliveryStale _ -> -2
    RepositoryDelivery.DeliveryConfirmationRejected _ -> -4
    _ -> -1

deliveryInputValid :: Ptr Word8 -> CSize -> Int -> Bool
deliveryInputValid pointer length limit =
    pointer /= nullPtr
        && length > 0
        && toInteger length <= toInteger limit

deliveryPathLimit, deliveryTokenLimit, deliveryRefLimit :: Int
deliveryPathLimit = 16 * 1024
deliveryTokenLimit = 4 * 1024
deliveryRefLimit = 1024

deliveryTitleLimit, deliveryBodyLimit :: Int
deliveryTitleLimit = 4 * 1024
deliveryBodyLimit = 1024 * 1024

copyRequiredText :: Ptr Word8 -> CSize -> IO (Either () Text)
copyRequiredText pointer (CSize length)
    | pointer == nullPtr || length == 0 = pure (Left ())
    | toInteger length > toInteger (maxBound :: Int) = pure (Left ())
    | toInteger length > toInteger maxRepositoryRequestItemBytes =
        pure (Left ())
    | otherwise = do
        bytes <- BS.packCStringLen (castPtr pointer, fromIntegral length)
        pure (first (const ()) (TextEncoding.decodeUtf8' bytes))

copyRequiredTexts
    :: [(Ptr Word8, CSize)]
    -> IO (Either () [Text])
copyRequiredTexts fields
    | sum (map (toInteger . snd) fields)
        > toInteger maxRepositoryRequestTotalBytes = pure (Left ())
    | otherwise =
        fmap sequence (mapM (uncurry copyRequiredText) fields)

maxRepositoryRequestItemBytes :: Int
maxRepositoryRequestItemBytes = 8 * 1024 * 1024

maxRepositoryRequestTotalBytes :: Int
maxRepositoryRequestTotalBytes = 16 * 1024 * 1024

byteStringChunks :: Int -> BS.ByteString -> [BS.ByteString]
byteStringChunks size bytes
    | BS.null bytes = []
    | otherwise =
        let (chunk, remaining) = BS.splitAt size bytes
        in chunk : byteStringChunks size remaining
foreign export ccall ha_data_catalog_list
    :: Ptr Word8 -> CSize -> FunPtr DataCatalogCallback -> Ptr () -> IO CInt

foreign export ccall ha_data_rows_load
    :: Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize
    -> Int64 -> CInt -> FunPtr DataRowsCallback -> Ptr () -> IO CInt

ha_session_load_around
    :: Ptr Word8 -> CSize -> Int64 -> CInt
    -> FunPtr SessionTurnCallback -> Ptr () -> IO CInt
ha_session_load_around sessionBytes (CSize sessionLength) center radius
        callback context
    | callback == nullFunPtr = pure 2
    | sessionBytes == nullPtr || sessionLength == 0 = pure 2
    | toInteger sessionLength > toInteger (maxBound :: Int) = pure 2
    | center < 0 || radius < 0 = pure 2
    | otherwise = do
        decoded <- tryAny (decodeUtf8Input sessionBytes sessionLength)
        case decoded of
            Left _ -> pure 3
            Right (Left ()) -> pure 2
            Right (Right sessionId) -> do
                _ <- forkIO do
                    result <- tryAny $ withNativeSessionStore \pool root ->
                        loadSessionHistoryTurnsAround
                            pool root sessionId center (fromIntegral radius)
                    case result of
                        Left exception ->
                            sessionTurnFailure callback context
                                (Text.pack (show exception))
                        Right (Left err) ->
                            sessionTurnFailure callback context err
                        Right (Right page) -> do
                            forM_ page.pageTurns \(turnIndex, turn) ->
                                emitSessionTurn callback context turnIndex turn
                            sessionTurnTerminal callback context
                                page.pageHasOlder page.pageHasNewer
                pure 0

ha_session_fork
    :: Ptr Word8 -> CSize -> Int64
    -> FunPtr SessionTransferResultCallback -> Ptr () -> IO CInt
ha_session_fork sessionBytes (CSize sessionLength) throughIndex callback context
    | callback == nullFunPtr = pure 2
    | sessionBytes == nullPtr || sessionLength == 0 || throughIndex < 0 = pure 2
    | toInteger sessionLength > toInteger (maxBound :: Int) = pure 2
    | otherwise = do
        decoded <- tryAny (decodeUtf8Input sessionBytes sessionLength)
        case decoded of
            Left _ -> pure 3
            Right (Left ()) -> pure 2
            Right (Right sessionId) -> do
                _ <- forkIO do
                    result <- tryAny $ withNativeSessionStore \pool root ->
                        forkSessionAtTurn pool root sessionId throughIndex
                    completeSessionResult callback context result
                pure 0

ha_session_export
    :: Ptr Word8 -> CSize
    -> FunPtr SessionExportCallback -> Ptr () -> IO CInt
ha_session_export sessionBytes (CSize sessionLength) callback context
    | callback == nullFunPtr = pure 2
    | sessionBytes == nullPtr || sessionLength == 0 = pure 2
    | toInteger sessionLength > toInteger (maxBound :: Int) = pure 2
    | otherwise = do
        decoded <- tryAny (decodeUtf8Input sessionBytes sessionLength)
        case decoded of
            Left _ -> pure 3
            Right (Left ()) -> pure 2
            Right (Right sessionId) -> do
                _ <- forkIO do
                    result <- tryAny $ withNativeSessionStore \pool root ->
                        streamSessionTransfer pool root sessionId
                            (emitSessionExportChunk callback context)
                    case result of
                        Left exception ->
                            sessionExportFailure callback context
                                (Text.pack (show exception))
                        Right (Left err) ->
                            sessionExportFailure callback context err
                        Right (Right ()) ->
                            invokeSessionExportCallback callback
                                context 1 nullPtr 0 nullPtr 0
                pure 0

ha_session_import
    :: Ptr Word8 -> CSize
    -> FunPtr SessionTransferResultCallback -> Ptr () -> IO CInt
ha_session_import bytes (CSize length) callback context
    | callback == nullFunPtr = pure 2
    | bytes == nullPtr || length == 0 = pure 2
    | toInteger length > 512 * 1024 * 1024 = pure 2
    | otherwise = do
        payload <- BS.packCStringLen (castPtr bytes, fromIntegral length)
        _ <- forkIO do
            result <- tryAny $
                case TextEncoding.decodeUtf8' payload of
                    Left _ ->
                        pure
                            (Left
                                "invalid session transfer: invalid UTF-8")
                    Right _ ->
                        case
                            (Aeson.eitherDecodeStrict' payload
                                :: Either String SessionTransferEnvelope)
                        of
                            Left err ->
                                pure
                                    (Left
                                        ("invalid session transfer: "
                                            <> Text.pack err))
                            Right envelope ->
                                withNativeSessionStore \pool root ->
                                    importSessionTransferRemapped
                                        pool root Nothing envelope
            completeSessionResult callback context result
        pure 0

withNativeSessionStore
    :: (StorePool -> OsPath -> IO (Either Text a))
    -> IO (Either Text a)
withNativeSessionStore action = do
    home <- getHomeDirectory
    config <- managedPostgresConfigForHome home
    openStore config >>= \case
        Left err -> pure (Left (renderStoreError err))
        Right opened ->
            bracket (pure opened) closeStore \store ->
                action (trustedPool store) (sessionsRoot home)

emitSessionTurn
    :: FunPtr SessionTurnCallback
    -> Ptr ()
    -> Int64
    -> SessionTurn
    -> IO ()
emitSessionTurn callback context turnIndex turn =
    withText (Text.pack (show turn.turnAt)) \occurred occurredLength ->
    withText turn.turnUserText \user userLength ->
    withOptionalText turn.turnAssistantText \assistant assistantLength ->
    withOptionalText turn.turnError \turnError turnErrorLength ->
    withOptionalText turn.turnResponseId \response responseLength ->
    withText (transcriptEffectName turn.turnEffect) \effect effectLength ->
    BS.useAsCStringLen
        (LBS.toStrict (Aeson.encode turn.turnItems))
        \(items, itemsLength) -> do
            let (inputTokens', outputTokens', cachedTokens') =
                    maybe (-1, -1, -1)
                        (\usage ->
                            ( fromIntegral usage.inputTokens
                            , fromIntegral usage.outputTokens
                            , fromIntegral usage.cachedTokens
                            ))
                        turn.turnUsage
            invokeSessionTurnCallback callback context 0 turnIndex
                occurred occurredLength
                user userLength
                assistant assistantLength
                turnError turnErrorLength
                response responseLength
                effect effectLength
                items (fromIntegral itemsLength)
                inputTokens' outputTokens' cachedTokens'
                0 0 nullPtr 0

sessionTurnTerminal
    :: FunPtr SessionTurnCallback -> Ptr () -> Bool -> Bool -> IO ()
sessionTurnTerminal callback context hasOlder hasNewer =
    invokeSessionTurnCallback callback context 1 (-1)
        nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
        (-1) (-1) (-1)
        (if hasOlder then 1 else 0)
        (if hasNewer then 1 else 0)
        nullPtr 0

sessionTurnFailure
    :: FunPtr SessionTurnCallback -> Ptr () -> Text -> IO ()
sessionTurnFailure callback context err =
    withText err \errorPointer errorLength ->
        invokeSessionTurnCallback callback context (-1) (-1)
            nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
            nullPtr 0 (-1) (-1) (-1) 0 0 errorPointer errorLength

transcriptEffectName :: TranscriptEffect -> Text
transcriptEffectName = \case
    TranscriptAppend -> "append"
    TranscriptReplace -> "replace"
    TranscriptReset -> "reset"

completeSessionResult
    :: FunPtr SessionTransferResultCallback
    -> Ptr ()
    -> Either SomeException (Either Text Text)
    -> IO ()
completeSessionResult callback context = \case
    Left exception ->
        emitSessionResult callback context (-1) Nothing
            (Just (Text.pack (show exception)))
    Right (Left err) ->
        emitSessionResult callback context (-1) Nothing (Just err)
    Right (Right sessionId) ->
        emitSessionResult callback context 0 (Just sessionId) Nothing

emitSessionResult
    :: FunPtr SessionTransferResultCallback
    -> Ptr ()
    -> CInt
    -> Maybe Text
    -> Maybe Text
    -> IO ()
emitSessionResult callback context status sessionId err =
    withOptionalText sessionId \sessionPointer sessionLength ->
    withOptionalText err \errorPointer errorLength ->
        invokeSessionTransferResultCallback callback context status
            sessionPointer sessionLength errorPointer errorLength

emitSessionExportChunk
    :: FunPtr SessionExportCallback -> Ptr () -> BS.ByteString -> IO ()
emitSessionExportChunk callback context chunk =
    BS.useAsCStringLen chunk \(pointer, length) ->
        invokeSessionExportCallback callback context 0
            (castPtr pointer) (fromIntegral length) nullPtr 0

sessionExportFailure
    :: FunPtr SessionExportCallback -> Ptr () -> Text -> IO ()
sessionExportFailure callback context err =
    withText err \pointer length ->
        invokeSessionExportCallback callback context (-1)
            nullPtr 0 pointer length

ha_learned_skills_list
    :: Ptr Word8 -> CSize -> FunPtr LearnedSkillsListCallback -> Ptr () -> IO CInt
ha_learned_skills_list cwdBytes (CSize cwdLength) callback context
    | callback == nullFunPtr = pure 1
    | cwdBytes == nullPtr && cwdLength > 0 = pure 2
    | otherwise = do
        cwd <- decodeInput cwdBytes cwdLength
        _ <- forkIO do
            tryAny (listLearnedSkillsFor (Text.unpack cwd)) >>= \case
                Left exception ->
                    withText (Text.pack (show exception)) $ \errorPtr errorLength ->
                        learnedSkillsTerminal callback context (-1) errorPtr errorLength
                Right (Left err) ->
                    withText err $ \errorPtr errorLength ->
                        learnedSkillsTerminal callback context (-1) errorPtr errorLength
                Right (Right skills) -> do
                    forM_ skills \skill ->
                        withLearnedSkillStrings skill $
                            invokeLearnedSkillsListCallback callback context 0
                    learnedSkillsTerminal callback context 1 nullPtr 0
        pure 0
  where
    listLearnedSkillsFor cwd = do
        home <- getHomeDirectory
        projectRoot <- resolveProjectRoot (unsafeEncodeUtf cwd)
        stateDirectory <- decodeFS (takeDirectory (sessionsRoot home))
        projectRootPath <- decodeFS projectRoot
        scopes <- deriveDatabaseScopes stateDirectory projectRootPath
        case scopes of
            Left err -> pure (Left err)
            Right databaseScopes -> do
                store <- openStore =<< managedPostgresConfigForHome home
                case store of
                    Left err -> pure (Left (renderStoreError err))
                    Right opened ->
                        bracket (pure opened) closeStore \store ->
                            first renderStoreError
                                <$> listAllLearnedSkills
                                    (trustedPool store)
                                    (applicableDatabaseScopes databaseScopes)

ha_data_catalog_list
    :: Ptr Word8 -> CSize -> FunPtr DataCatalogCallback -> Ptr () -> IO CInt
ha_data_catalog_list cwdBytes (CSize cwdLength) callback context
    | callback == nullFunPtr = pure 1
    | cwdBytes == nullPtr && cwdLength > 0 = pure 2
    | otherwise = do
        cwd <- decodeInput cwdBytes cwdLength
        _ <- forkIO do
            tryAny (loadDataCatalogFor (Text.unpack cwd)) >>= \case
                Left exception ->
                    withText (Text.pack (show exception)) \errorPtr errorLength ->
                        dataCatalogTerminal
                            callback context (-1) errorPtr errorLength
                Right (Left err) ->
                    withText err \errorPtr errorLength ->
                        dataCatalogTerminal
                            callback context (-1) errorPtr errorLength
                Right (Right scopedObjects) -> do
                    forM_ scopedObjects \(scope, objects) ->
                        forM_ objects (emitDataCatalogObject callback context scope)
                    dataCatalogTerminal callback context 2 nullPtr 0
        pure 0

ha_data_rows_load
    :: Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize
    -> Int64 -> CInt -> FunPtr DataRowsCallback -> Ptr () -> IO CInt
ha_data_rows_load
    cwdBytes
    (CSize cwdLength)
    rawScope
    objectBytes
    (CSize objectLength)
    offset
    rawLimit
    callback
    context
    | callback == nullFunPtr = pure 1
    | cwdBytes == nullPtr && cwdLength > 0 = pure 2
    | objectBytes == nullPtr && objectLength > 0 = pure 2
    | offset /= 0 = pure 3
    | rawLimit < 1 || rawLimit > 500 = pure 3
    | Nothing <- dataScopeFromCode rawScope = pure 3
    | otherwise = do
        cwd <- decodeInput cwdBytes cwdLength
        objectName <- decodeInput objectBytes objectLength
        if Text.null objectName
            then pure 3
            else do
                let Just scope = dataScopeFromCode rawScope
                    limit = fromIntegral rawLimit
                _ <- forkIO do
                    tryAny
                        (loadDataPageFor
                            (Text.unpack cwd)
                            scope
                            objectName
                            offset
                            limit)
                        >>= \case
                            Left exception ->
                                withText
                                    (Text.pack (show exception))
                                    \errorPtr errorLength ->
                                        dataRowsTerminal
                                            callback context (-1) offset 0 False
                                            errorPtr errorLength
                            Right (Left err) ->
                                withText err \errorPtr errorLength ->
                                    dataRowsTerminal
                                        callback context (-1) offset 0 False
                                        errorPtr errorLength
                            Right (Right page) -> do
                                forM_
                                    (zip [0 :: Int64 ..] page.databaseBrowseRows)
                                    \(rowIndex, row) ->
                                        forM_
                                            (zip [0 :: Int ..] row)
                                            \(columnIndex, value) ->
                                                withDataValue value
                                                    \kind valuePtr valueLength ->
                                                        invokeDataRowsCallback
                                                            callback context 0
                                                            offset rowIndex
                                                            (fromIntegral columnIndex)
                                                            kind
                                                            valuePtr valueLength
                                                            0 0 nullPtr 0
                                dataRowsTerminal
                                    callback
                                    context
                                    1
                                    offset
                                    (fromIntegral
                                        (length page.databaseBrowseRows))
                                    page.databaseBrowseHasMore
                                    nullPtr
                                    0
                pure 0

loadDataCatalogFor
    :: FilePath
    -> IO (Either Text [(CInt, [CatalogObject])])
loadDataCatalogFor cwd =
    withDatabaseStoreFor cwd \store scopes -> do
        results <- mapM
            (\(scopeCode, scope) ->
                fmap (fmap ((,) scopeCode)) $
                    listDatabaseObjects store scopes scope)
            [ (0, DatabaseUserScope)
            , (1, DatabaseRepositoryScope)
            , (2, DatabaseCheckoutScope)
            ]
        pure (sequence results)

loadDataPageFor
    :: FilePath
    -> DatabaseScope
    -> Text
    -> Int64
    -> Int
    -> IO (Either Text DatabaseBrowsePage)
loadDataPageFor cwd selected objectName offset limit =
    withDatabaseStoreFor cwd \store scopes ->
        loadDatabaseRows store scopes selected objectName offset limit

withDatabaseStoreFor
    :: FilePath
    -> (Store -> DatabaseScopes -> IO (Either Text value))
    -> IO (Either Text value)
withDatabaseStoreFor cwd action = do
    home <- getHomeDirectory
    projectRoot <- resolveProjectRoot (unsafeEncodeUtf cwd)
    stateDirectory <- decodeFS (takeDirectory (sessionsRoot home))
    projectRootPath <- decodeFS projectRoot
    deriveDatabaseScopes stateDirectory projectRootPath >>= \case
        Left err -> pure (Left err)
        Right scopes -> do
            config <- managedPostgresConfigForHome home
            openStore config >>= \case
                Left err -> pure (Left (renderStoreError err))
                Right opened ->
                    bracket (pure opened) closeStore \store ->
                        action store scopes

emitDataCatalogObject
    :: FunPtr DataCatalogCallback
    -> Ptr ()
    -> CInt
    -> CatalogObject
    -> IO ()
emitDataCatalogObject callback context scope object =
    withText object.catalogObjectName \objectPtr objectLength ->
    withOptionalText definition.definitionComment \commentPtr commentLength -> do
        invokeDataCatalogCallback callback context
            0 scope kind
            objectPtr objectLength
            commentPtr commentLength
            nullPtr 0 nullPtr 0 0 nullPtr 0 nullPtr 0
        forM_ definition.definitionColumns \column ->
            withText column.columnName \columnPtr columnLength ->
            withText column.columnType \typePtr typeLength ->
            withOptionalText column.columnComment
                \columnCommentPtr columnCommentLength ->
                    invokeDataCatalogCallback callback context
                        1 scope kind
                        objectPtr objectLength
                        nullPtr 0
                        columnPtr columnLength
                        typePtr typeLength
                        (if column.columnNullable then 1 else 0)
                        columnCommentPtr columnCommentLength
                        nullPtr 0
  where
    definition = object.catalogObjectDefinition
    kind = dataObjectKind object.catalogObjectKind

dataObjectKind :: Text -> CInt
dataObjectKind = \case
    "view" -> 1
    "materialized_view" -> 1
    _ -> 0

dataScopeFromCode :: CInt -> Maybe DatabaseScope
dataScopeFromCode = \case
    0 -> Just DatabaseUserScope
    1 -> Just DatabaseRepositoryScope
    2 -> Just DatabaseCheckoutScope
    _ -> Nothing

dataCatalogTerminal
    :: FunPtr DataCatalogCallback
    -> Ptr ()
    -> CInt
    -> CString
    -> CSize
    -> IO ()
dataCatalogTerminal callback context status errorPtr errorLength =
    invokeDataCatalogCallback callback context
        status 0 0
        nullPtr 0 nullPtr 0
        nullPtr 0 nullPtr 0 0
        nullPtr 0 errorPtr errorLength

withDataValue
    :: Aeson.Value
    -> (CInt -> CString -> CSize -> IO value)
    -> IO value
withDataValue value action =
    case value of
        Aeson.Null -> action 0 nullPtr 0
        Aeson.String text -> withText text (action 1)
        Aeson.Number _ -> withEncoded 2
        Aeson.Bool True -> withText "true" (action 3)
        Aeson.Bool False -> withText "false" (action 3)
        Aeson.Array _ -> withEncoded 4
        Aeson.Object _ -> withEncoded 4
  where
    withEncoded kind =
        withText
            (TextEncoding.decodeUtf8 (LBS.toStrict (Aeson.encode value)))
            (action kind)

dataRowsTerminal
    :: FunPtr DataRowsCallback
    -> Ptr ()
    -> CInt
    -> Int64
    -> Int64
    -> Bool
    -> CString
    -> CSize
    -> IO ()
dataRowsTerminal
    callback context status offset rowCount hasMore errorPtr errorLength =
        invokeDataRowsCallback callback context
            status offset (-1) (-1) (-1)
            nullPtr 0 rowCount (if hasMore then 1 else 0)
            errorPtr errorLength

type LearnedSkillItemCallback =
    CString -> CSize -> CString -> CSize -> CLLong
    -> CString -> CSize -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize -> CString -> CSize
    -> CInt -> CString -> CSize -> CString -> CSize -> IO ()

withLearnedSkillStrings :: LearnedSkill -> LearnedSkillItemCallback -> IO ()
withLearnedSkillStrings skill action =
    withText (scopeKindText skill.learnedSkillScope.scopeKind) $ \scope scopeLength ->
    withText skill.learnedSkillSlug $ \slug slugLength ->
    withText skill.learnedSkillTitle $ \title titleLength ->
    withText skill.learnedSkillDescription $ \description descriptionLength ->
    withText skill.learnedSkillAppliesWhen $ \applies appliesLength ->
    withText skill.learnedSkillInstructions $ \instructions instructionsLength ->
    withText (learnedSkillActivationText skill.learnedSkillActivation) $ \activation activationLength ->
    withText (learnedSkillStatusText skill.learnedSkillStatus) $ \status statusLength ->
    withText (Text.pack (show skill.learnedSkillUpdatedAt)) $ \updated updatedLength ->
        action scope scopeLength slug slugLength
            (fromIntegral skill.learnedSkillRevision)
            title titleLength description descriptionLength
            applies appliesLength instructions instructionsLength
            activation activationLength status statusLength
            (fromIntegral skill.learnedSkillPriority) updated updatedLength
            nullPtr 0

learnedSkillsTerminal
    :: FunPtr LearnedSkillsListCallback
    -> Ptr () -> CInt -> CString -> CSize -> IO ()
learnedSkillsTerminal callback context status errorPtr errorLength =
    invokeLearnedSkillsListCallback callback context status
        nullPtr 0 nullPtr 0 0
        nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
        nullPtr 0 nullPtr 0
        0 nullPtr 0 errorPtr errorLength

ha_accounts_list
    :: FunPtr AccountListCallback -> FunPtr AccountUsageWindowCallback
    -> Ptr () -> IO CInt
ha_accounts_list callback usageCallback context
    | callback == nullFunPtr || usageCallback == nullFunPtr = pure 1
    | otherwise = do
        _ <- forkIO do
            tryAny
                (discoverLoginAccounts >>= mapConcurrently refreshLoginAccount)
                >>= \case
                Left exception ->
                    withText (Text.pack (show exception)) $ \errorPtr errorLength ->
                        invokeAccountListCallback callback context (-1)
                            nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
                            nullPtr 0 nullPtr 0 nullPtr 0
                            0 0 errorPtr errorLength
                Right accounts -> do
                    forM_ accounts \account -> do
                        withAccountStrings account $
                            invokeAccountListCallback callback context 0
                        invokeAccountUsageWindows usageCallback context account
                    invokeAccountListCallback callback context 1
                        nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
                        nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
                        0 0 nullPtr 0
        pure 0

ha_account_oauth_start
    :: Ptr Word8 -> CSize -> FunPtr AccountOAuthStartCallback -> Ptr () -> IO CInt
ha_account_oauth_start providerBytes (CSize providerLength) callback context
    | callback == nullFunPtr = pure 1
    | providerBytes == nullPtr && providerLength > 0 = pure 2
    | otherwise = do
        provider <- decodeInput providerBytes providerLength
        _ <- forkIO do
            tryAny (startAccountOAuth AccountProviderRequest
                { accountProvider = provider }) >>= \case
                Left exception -> withText (Text.pack (show exception)) $ \errorPtr errorLength ->
                    invokeAccountOAuthStartCallback callback context
                        (-1) nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 0 0
                        errorPtr errorLength
                Right (Left err) -> withText err $ \errorPtr errorLength ->
                    invokeAccountOAuthStartCallback callback context
                        (-1) nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 0 0
                        errorPtr errorLength
                Right (Right value) -> case parseChallenge value of
                    Nothing -> withText "invalid OAuth challenge"
                        (\errorPtr errorLength ->
                            invokeAccountOAuthStartCallback callback context
                                (-1) nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
                                0 0 errorPtr errorLength)
                    Just challenge ->
                        withChallengeStrings challenge
                            (invokeAccountOAuthStartCallback callback context 0)
        pure 0

ha_account_oauth_poll
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CInt -> CInt
    -> FunPtr AccountResultCallback -> Ptr () -> IO CInt
ha_account_oauth_poll providerBytes (CSize providerLength) urlBytes (CSize urlLength)
    userBytes (CSize userLength) authIdBytes (CSize authIdLength)
    deviceBytes (CSize deviceLength) pollInterval expires callback context
    | callback == nullFunPtr = pure 1
    | anyNonEmptyNull
        [ (providerBytes, providerLength)
        , (urlBytes, urlLength)
        , (userBytes, userLength)
        , (authIdBytes, authIdLength)
        , (deviceBytes, deviceLength)
        ] = pure 2
    | otherwise = do
        provider <- decodeInput providerBytes providerLength
        url <- decodeInput urlBytes urlLength
        user <- decodeInput userBytes userLength
        authId <- decodeInput authIdBytes authIdLength
        device <- decodeInput deviceBytes deviceLength
        _ <- forkIO do
            tryAny (pollAccountOAuth AccountOAuthPollRequest
                { oauthPollProvider = provider
                , oauthPollVerificationUrl = nonEmptyText url
                , oauthPollUserCode = nonEmptyText user
                , oauthPollDeviceAuthId = nonEmptyText authId
                , oauthPollDeviceCode = nonEmptyText device
                , oauthPollIntervalSeconds = Just (fromIntegral pollInterval)
                , oauthPollExpiresInSeconds = Just (fromIntegral expires)
                }) >>= \case
                    Left exception -> invokeExceptionResult callback context exception
                    Right result -> invokeResult callback context result
        pure 0

ha_account_api_key_connect
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr AccountResultCallback -> Ptr () -> IO CInt
ha_account_api_key_connect providerBytes (CSize providerLength) keyBytes (CSize keyLength) callback context
    | callback == nullFunPtr = pure 1
    | anyNonEmptyNull
        [ (providerBytes, providerLength), (keyBytes, keyLength) ] = pure 2
    | otherwise = do
        provider <- decodeInput providerBytes providerLength
        key <- decodeInput keyBytes keyLength
        _ <- forkIO do
            tryAny (connectAccountAPIKey AccountAPIKeyRequest
                { accountAPIKeyProvider = provider, accountAPIKey = key }
                ) >>= \case
                    Left exception -> invokeExceptionResult callback context exception
                    Right result -> invokeResult callback context result
        pure 0

ha_account_set_enabled
    :: Ptr Word8 -> CSize -> CInt -> FunPtr AccountResultCallback -> Ptr () -> IO CInt
ha_account_set_enabled idBytes (CSize idLength) enabled callback context
    | callback == nullFunPtr = pure 1
    | anyNonEmptyNull [(idBytes, idLength)] = pure 2
    | otherwise = do
        managedId <- decodeInput idBytes idLength
        _ <- forkIO do
            tryAny (setManagedCredentialEnabled managedId (enabled /= 0))
                >>= \case
                    Left exception -> invokeExceptionResult callback context exception
                    Right result -> invokeStoreResult callback context result
        pure 0

ha_account_delete
    :: Ptr Word8 -> CSize -> FunPtr AccountResultCallback -> Ptr () -> IO CInt
ha_account_delete idBytes (CSize idLength) callback context
    | callback == nullFunPtr = pure 1
    | anyNonEmptyNull [(idBytes, idLength)] = pure 2
    | otherwise = do
        managedId <- decodeInput idBytes idLength
        _ <- forkIO do
            tryAny (deleteManagedCredential managedId) >>= \case
                Left exception -> invokeExceptionResult callback context exception
                Right result -> invokeStoreResult callback context result
        pure 0

ha_gateway_status
    :: FunPtr GatewayStatusCallback -> Ptr () -> IO CInt
ha_gateway_status callback context
    | callback == nullFunPtr = pure 1
    | otherwise = do
        _ <- forkIO do
            tryAny loadGatewayCredential >>= \case
                Left exception ->
                    invokeGatewayStatusError callback context
                        (Text.pack (show exception))
                Right (Left err) ->
                    invokeGatewayStatusError callback context err
                Right (Right Nothing) ->
                    invokeGatewayStatusCallback callback context 1
                        nullPtr 0 nullPtr 0
                Right (Right (Just credential)) ->
                    withText credential.gatewayBaseUrl $ \baseUrl baseUrlLength ->
                        invokeGatewayStatusCallback callback context 0
                            baseUrl baseUrlLength nullPtr 0
        pure 0

ha_gateway_connect_start
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr GatewayConnectStartCallback -> Ptr () -> IO CInt
ha_gateway_connect_start
    baseUrlBytes (CSize baseUrlLength)
    clientNameBytes (CSize clientNameLength)
    callback context
    | callback == nullFunPtr = pure 1
    | anyNonEmptyNull
        [ (baseUrlBytes, baseUrlLength)
        , (clientNameBytes, clientNameLength)
        ] = pure 2
    | otherwise = do
        baseUrl <- decodeInput baseUrlBytes baseUrlLength
        clientName <- decodeInput clientNameBytes clientNameLength
        _ <- forkIO do
            tryAny (startGatewayAuthorization baseUrl clientName) >>= \case
                Left exception ->
                    invokeGatewayConnectStartError callback context
                        (Text.pack (show exception))
                Right (Left err) ->
                    invokeGatewayConnectStartError callback context err
                Right (Right device) ->
                    withGatewayDeviceStrings device $
                        invokeGatewayConnectStartCallback callback context 0
        pure 0

ha_gateway_connect_poll
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr GatewayPollCallback -> Ptr () -> IO CInt
ha_gateway_connect_poll
    baseUrlBytes (CSize baseUrlLength)
    deviceCodeBytes (CSize deviceCodeLength)
    callback context
    | callback == nullFunPtr = pure 1
    | anyNonEmptyNull
        [ (baseUrlBytes, baseUrlLength)
        , (deviceCodeBytes, deviceCodeLength)
        ] = pure 2
    | otherwise = do
        baseUrl <- decodeInput baseUrlBytes baseUrlLength
        deviceCode <- decodeInput deviceCodeBytes deviceCodeLength
        _ <- forkIO do
            tryAny
                (pollGatewayAuthorizationAndSave baseUrl deviceCode)
                >>= \case
                    Left exception ->
                        invokeGatewayPollError callback context
                            (Text.pack (show exception))
                    Right (Left err) ->
                        invokeGatewayPollError callback context err
                    Right (Right result) ->
                        invokeGatewayPollResult callback context result
        pure 0

ha_gateway_connect_exchange
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize
    -> FunPtr GatewayResultCallback -> Ptr () -> IO CInt
ha_gateway_connect_exchange
    baseUrlBytes (CSize baseUrlLength)
    clientIdBytes (CSize clientIdLength)
    codeBytes (CSize codeLength)
    verifierBytes (CSize verifierLength)
    redirectUriBytes (CSize redirectUriLength)
    callback context
    | callback == nullFunPtr = pure 1
    | anyNonEmptyNull
        [ (baseUrlBytes, baseUrlLength)
        , (clientIdBytes, clientIdLength)
        , (codeBytes, codeLength)
        , (verifierBytes, verifierLength)
        , (redirectUriBytes, redirectUriLength)
        ] = pure 2
    | otherwise = do
        baseUrl <- decodeInput baseUrlBytes baseUrlLength
        clientId <- decodeInput clientIdBytes clientIdLength
        code <- decodeInput codeBytes codeLength
        verifier <- decodeInput verifierBytes verifierLength
        redirectUri <- decodeInput redirectUriBytes redirectUriLength
        _ <- forkIO do
            tryAny
                (exchangeGatewayAuthorizationCode
                    baseUrl
                    clientId
                    code
                    verifier
                    redirectUri)
                >>= \case
                    Left exception ->
                        invokeGatewayResultError callback context
                            (Text.pack (show exception))
                    Right (Left err) ->
                        invokeGatewayResultError callback context err
                    Right (Right ()) ->
                        invokeGatewayResultCallback callback context
                            0 nullPtr 0
        pure 0

ha_gateway_disconnect
    :: FunPtr GatewayResultCallback -> Ptr () -> IO CInt
ha_gateway_disconnect callback context
    | callback == nullFunPtr = pure 1
    | otherwise = do
        _ <- forkIO do
            tryAny removeGatewayCredential >>= \case
                Left exception ->
                    invokeGatewayResultError callback context
                        (Text.pack (show exception))
                Right (Left err) ->
                    invokeGatewayResultError callback context err
                Right (Right ()) ->
                    invokeGatewayResultCallback callback context 0 nullPtr 0
        pure 0

invokeGatewayStatusError
    :: FunPtr GatewayStatusCallback -> Ptr () -> Text -> IO ()
invokeGatewayStatusError callback context err =
    withText err $ \errorPtr errorLength ->
        invokeGatewayStatusCallback callback context (-1)
            nullPtr 0 errorPtr errorLength

invokeGatewayConnectStartError
    :: FunPtr GatewayConnectStartCallback -> Ptr () -> Text -> IO ()
invokeGatewayConnectStartError callback context err =
    withText err $ \errorPtr errorLength ->
        invokeGatewayConnectStartCallback callback context (-1)
            nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
            0 0 errorPtr errorLength

withGatewayDeviceStrings
    :: GatewayDeviceAuthorization
    -> (CString -> CSize -> CString -> CSize
        -> CString -> CSize -> CString -> CSize
        -> CInt -> CInt -> CString -> CSize -> IO a)
    -> IO a
withGatewayDeviceStrings device action =
    withText device.userCode $ \userCode userCodeLength ->
    withText device.verificationUri $ \verificationUri verificationUriLength ->
    withText device.verificationUriComplete $
        \verificationComplete verificationCompleteLength ->
    withText device.deviceCode $ \deviceCode deviceCodeLength ->
        action
            userCode userCodeLength
            verificationUri verificationUriLength
            verificationComplete verificationCompleteLength
            deviceCode deviceCodeLength
            (fromIntegral device.pollIntervalSeconds)
            (fromIntegral device.expiresInSeconds)
            nullPtr 0

invokeGatewayPollResult
    :: FunPtr GatewayPollCallback -> Ptr () -> GatewayPollResult -> IO ()
invokeGatewayPollResult callback context = \case
    GatewayAuthorized _ _ ->
        invokeGatewayPollCallback callback context 0 0 nullPtr 0
    GatewayAuthorizationPending retryInterval ->
        invokeGatewayPollCallback callback context 1
            (fromIntegral (fromMaybe 0 retryInterval)) nullPtr 0
    GatewaySlowDown retryInterval ->
        invokeGatewayPollCallback callback context 2
            (fromIntegral (fromMaybe 0 retryInterval)) nullPtr 0
    GatewayAccessDenied ->
        invokeGatewayPollError callback context
            "Gateway authorization was denied."
    GatewayExpired ->
        invokeGatewayPollError callback context
            "Gateway authorization expired."
    GatewayPollFailed code ->
        invokeGatewayPollError callback context
            ("Gateway authorization failed: " <> code)

invokeGatewayPollError
    :: FunPtr GatewayPollCallback -> Ptr () -> Text -> IO ()
invokeGatewayPollError callback context err =
    withText err $ \errorPtr errorLength ->
        invokeGatewayPollCallback callback context (-1) 0 errorPtr errorLength

invokeGatewayResultError
    :: FunPtr GatewayResultCallback -> Ptr () -> Text -> IO ()
invokeGatewayResultError callback context err =
    withText err $ \errorPtr errorLength ->
        invokeGatewayResultCallback callback context (-1) errorPtr errorLength

anyNonEmptyNull :: [(Ptr Word8, Word64)] -> Bool
anyNonEmptyNull = any \(pointer, length) ->
    pointer == nullPtr && length > 0

withAccountStrings :: LoginAccount -> (CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize -> CString -> CSize
    -> CInt -> CInt -> CString -> CSize -> IO a) -> IO a
withAccountStrings account action =
    withText (providerSlug account.loginProvider) $ \provider providerLength ->
    withText (billingText account.loginBilling) $ \billing billingLength ->
    withText (loginAccountSelectionId account) $ \selection selectionLength ->
    withText account.loginAccountId $ \accountId accountIdLength ->
    withText account.loginLabel $ \label labelLength ->
    withText account.loginSource $ \source sourceLength ->
        withText
            (accountUsageSummary account)
            $ \detail detailLength ->
        withOptionalText account.loginManagedId $ \managedId managedLength ->
        action provider providerLength billing billingLength
            selection selectionLength accountId accountIdLength label labelLength
            detail detailLength managedId managedLength source sourceLength
            (if account.loginEnabled then 1 else 0)
            (if account.loginManagedId == Nothing then 0 else 1)
            nullPtr 0

billingText :: AccountBilling -> Text
billingText = \case
    SubscriptionBilling _ -> "subscription"
    ApiCreditsBilling -> "api"

accountUsageSummary :: LoginAccount -> Text
accountUsageSummary account =
    Text.intercalate " · " $
        billing <> case account.loginUsage of
            UsageNotChecked -> []
            UsageUnavailable _ -> ["usage unavailable"]
            UsageAvailable usage ->
                maybeToList usage.usagePlan
                    <> maybeToList
                        (("credits " <>) <$> usage.creditsRemaining)
                    <> maybeToList
                        (("used " <>) <$> usage.creditsUsed)
  where
    billing = case account.loginBilling of
        ApiCreditsBilling -> ["API credits"]
        SubscriptionBilling plan ->
            maybe ["subscription"] (\value -> ["subscription", value]) plan
    maybeToList = maybe [] pure

invokeAccountUsageWindows
    :: FunPtr AccountUsageWindowCallback -> Ptr () -> LoginAccount -> IO ()
invokeAccountUsageWindows callback context account =
    case account.loginUsage of
        UsageAvailable usage ->
            forM_ usage.usageWindows \window ->
                withText (loginAccountSelectionId account) $ \selection selectionLength ->
                withText window.windowName $ \name nameLength ->
                    invokeAccountUsageWindowCallback callback context
                        selection selectionLength name nameLength
                        (fromIntegral window.usedPercent)
                        (fromIntegral window.windowSeconds)
                        (round (utcTimeToPOSIXSeconds window.resetsAt))
        _ -> pure ()

parseChallenge :: Aeson.Value
    -> Maybe (Text, Text, Maybe Text, Maybe Text, Int, Int)
parseChallenge = Aeson.parseMaybe $ Aeson.withObject "challenge" \object ->
    (,,,,,)
        <$> object Aeson..: "verificationUrl"
        <*> object Aeson..: "userCode"
        <*> object Aeson..:? "deviceAuthId"
        <*> object Aeson..:? "deviceCode"
        <*> (object Aeson..:? "pollIntervalSeconds" Aeson..!= 5)
        <*> (object Aeson..:? "expiresInSeconds" Aeson..!= 0)

withChallengeStrings
    :: (Text, Text, Maybe Text, Maybe Text, Int, Int)
    -> (CString -> CSize -> CString -> CSize -> CString -> CSize -> CString -> CSize
        -> CInt -> CInt -> CString -> CSize -> IO a)
    -> IO a
withChallengeStrings (url, user, authId, device, interval, expires) action =
    withText url $ \urlPtr urlLength ->
    withText user $ \userPtr userLength ->
    withOptionalText authId $ \authPtr authLength ->
    withOptionalText device $ \devicePtr deviceLength ->
        action urlPtr urlLength userPtr userLength authPtr authLength
            devicePtr deviceLength (fromIntegral interval) (fromIntegral expires)
            nullPtr 0

invokeResult :: FunPtr AccountResultCallback -> Ptr ()
    -> Either Text Aeson.Value -> IO ()
invokeResult callback context result = case result of
    Left errorText -> withText errorText $ \errorPtr errorLength ->
        invokeAccountResultCallback callback context (-1)
            nullPtr 0 errorPtr errorLength
    Right value ->
        let status = Aeson.parseMaybe (Aeson.withObject "result"
                (\object -> object Aeson..: "status")) value
            accountId = Aeson.parseMaybe (Aeson.withObject "result"
                (\object -> object Aeson..:? "accountID")) value >>= id
        in case status of
            Just ("pending" :: Text) ->
                invokeAccountResultCallback callback context 1 nullPtr 0 nullPtr 0
            _ -> withOptionalText accountId $ \idPtr idLength ->
                invokeAccountResultCallback callback context 0 idPtr idLength nullPtr 0

invokeStoreResult :: FunPtr AccountResultCallback -> Ptr ()
    -> Either Text () -> IO ()
invokeStoreResult callback context result = case result of
    Left errorText -> withText errorText $ \errorPtr errorLength ->
        invokeAccountResultCallback callback context (-1)
            nullPtr 0 errorPtr errorLength
    Right () -> invokeAccountResultCallback callback context 0 nullPtr 0 nullPtr 0

invokeExceptionResult :: FunPtr AccountResultCallback -> Ptr ()
    -> SomeException -> IO ()
invokeExceptionResult callback context exception =
    withText (Text.pack (show exception)) $ \errorPtr errorLength ->
        invokeAccountResultCallback callback context (-1)
            nullPtr 0 errorPtr errorLength

foreign export ccall ha_engine_search_conversations
    :: Ptr () -> Ptr Word8 -> CSize -> CSize
    -> FunPtr SearchCallback -> Ptr () -> IO CInt

ha_engine_create :: FunPtr EventCallback -> Ptr () -> IO (Ptr ())
ha_engine_create callback context
    | callback == nullFunPtr = pure nullPtr
    | otherwise = do
        created <- tryAny do
            home <- getHomeDirectory
            config <- managedPostgresConfigForHome home
            commands <- newEngineMailboxIO
            done <- newEmptyMVar
            stagedImages <- newTVarIO Map.empty
            browser <- BrowserHost <$> newMVar Nothing
            _ <- forkFinally
                (workerLifecycle
                    callback
                    context
                    config
                    (sessionsRoot home)
                    commands
                    stagedImages
                    browser)
                (const (putMVar done ()))
            stable <- newStablePtr Engine
                { engineCommands = commands
                , engineDone = done
                , engineStagedImages = stagedImages
                , engineBrowser = browser
                }
            pure (castStablePtrToPtr stable)
        case created of
            Left exception -> do
                sendEvent callback context $
                    failureEvent "_engine" (Text.pack (show exception))
                pure nullPtr
            Right pointer -> pure pointer

ha_engine_send_json :: Ptr () -> Ptr Word8 -> CSize -> IO CInt
ha_engine_send_json pointer bytes (CSize length)
    | pointer == nullPtr = pure 1
    | bytes == nullPtr && length > 0 = pure 2
    | otherwise = do
        accepted <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            payload <- BS.packCStringLen
                (castPtr bytes, fromIntegral length)
            case (Aeson.eitherDecodeStrict' payload
                :: Either String BridgeRequest) of
                Left _ -> do
                    atomically $ writeTVar engine.engineStagedImages Map.empty
                    pure Nothing
                Right request -> Just <$> atomically
                    (acceptEngineCommand
                        engine.engineCommands
                        (EngineRequest request))
        pure $ case accepted of
            Left _ -> 3
            Right Nothing -> 4
            Right (Just False) -> 3
            Right (Just True) -> 0

ha_engine_set_browser_callback
    :: Ptr () -> FunPtr BrowserCallback -> Ptr () -> IO CInt
ha_engine_set_browser_callback pointer callback context
    | pointer == nullPtr = pure 1
    | otherwise = do
        updated <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            modifyMVar_ engine.engineBrowser.browserRegistration $ \_ ->
                pure
                    if callback == nullFunPtr
                        then Nothing
                        else Just BrowserRegistration
                            { browserCallback = callback
                            , browserContext = context
                            }
        pure $ case updated of
            Left _ -> 2
            Right () -> 0

ha_engine_search_conversations
    :: Ptr () -> Ptr Word8 -> CSize -> CSize
    -> FunPtr SearchCallback -> Ptr () -> IO CInt
ha_engine_search_conversations pointer bytes (CSize length) rawLimit callback context
    | pointer == nullPtr = pure 1
    | callback == nullFunPtr = pure 2
    | bytes == nullPtr || length == 0 = pure 2
    | otherwise = do
        accepted <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            payload <- BS.packCStringLen (castPtr bytes, fromIntegral length)
            case TextEncoding.decodeUtf8' payload of
                Left _ -> pure Nothing
                Right query
                    | Text.null (Text.strip query) -> pure Nothing
                    | otherwise -> do
                        let requested = fromIntegral rawLimit :: Integer
                            limit = fromInteger (max 1 (min 100 requested))
                        Just <$> atomically
                            (acceptEngineCommand engine.engineCommands
                                (EngineSearch
                                    query limit callback context))
        pure $ case accepted of
            Left _ -> 3
            Right Nothing -> 2
            Right (Just False) -> 3
            Right (Just True) -> 0

ha_engine_stage_turn_images
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr () -> CSize -> IO CInt
ha_engine_stage_turn_images pointer turnID turnIDLength imagePointer imageCount
    | pointer == nullPtr = pure 1
    | turnID == nullPtr || turnIDLength == 0 = pure 2
    | imagePointer == nullPtr && imageCount > 0 = pure 4
    | toInteger imageCount > toInteger (maxBound :: Int) = pure 4
    | otherwise = do
        accepted <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            turnIDBytes <- BS.packCStringLen
                (castPtr turnID, fromIntegral turnIDLength)
            let turnIDText = TextEncoding.decodeUtf8' turnIDBytes
            imageResults <- mapM peekImage
                [0 .. fromIntegral imageCount - 1]
            case (turnIDText, sequence imageResults) of
                (Right turnIDValue, Just images) -> do
                    atomically $ modifyTVar' engine.engineStagedImages $
                        if null images
                            then Map.delete turnIDValue
                            else Map.insert turnIDValue images
                    pure True
                _ -> pure False
        pure $ case accepted of
            Left _ -> 3
            Right False -> 4
            Right True -> 0
  where
    pointerSize = sizeOf (nullPtr :: Ptr ())
    sizeSize = sizeOf (undefined :: CSize)
    imageSize = pointerSize + sizeSize + pointerSize + sizeSize

    peekImage index = do
        let base = castPtr imagePointer `plusPtr` (index * imageSize)
            readPointer offset =
                peekByteOff base offset :: IO (Ptr Word8)
            readLength offset =
                peekByteOff base offset :: IO CSize
        mimePointer <- readPointer 0
        mimeLength <- readLength pointerSize
        bytesPointer <- readPointer (pointerSize + sizeSize)
        bytesLength <- readLength (pointerSize + sizeSize + pointerSize)
        if
            (mimePointer == nullPtr && mimeLength > 0)
                || (bytesPointer == nullPtr && bytesLength > 0)
                || mimeLength == 0
                || bytesLength == 0
        then pure Nothing
        else do
            mimeBytes <- BS.packCStringLen
                (castPtr mimePointer, fromIntegral mimeLength)
            let mime = TextEncoding.decodeUtf8' mimeBytes
            bytes <- BS.packCStringLen
                (castPtr bytesPointer, fromIntegral bytesLength)
            pure $ case mime of
                Left _ -> Nothing
                Right mimeValue -> Just ImageAttachment
                    { imageMime = mimeValue
                    , imageBytes = bytes
                    }

ha_engine_session_rename
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr SessionResultCallback -> Ptr () -> IO CInt
ha_engine_session_rename engine idBytes (CSize idLength)
    titleBytes (CSize titleLength) callback context
    | anyNonEmptyNull
        [ (idBytes, idLength), (titleBytes, titleLength) ] = pure 2
    | otherwise = do
        sessionId <- decodeInput idBytes idLength
        title <- decodeInput titleBytes titleLength
        enqueueSessionMutation engine (SessionRename sessionId title) callback context

ha_engine_session_delete
    :: Ptr () -> Ptr Word8 -> CSize
    -> FunPtr SessionResultCallback -> Ptr () -> IO CInt
ha_engine_session_delete engine idBytes (CSize idLength) callback context
    | anyNonEmptyNull [(idBytes, idLength)] = pure 2
    | otherwise = do
        sessionId <- decodeInput idBytes idLength
        enqueueSessionMutation engine (SessionDelete sessionId) callback context

ha_engine_session_archive
    :: Ptr () -> Ptr Word8 -> CSize -> CInt
    -> FunPtr SessionResultCallback -> Ptr () -> IO CInt
ha_engine_session_archive engine idBytes (CSize idLength)
    archived callback context
    | anyNonEmptyNull [(idBytes, idLength)] = pure 2
    | otherwise = do
        sessionId <- decodeInput idBytes idLength
        enqueueSessionMutation
            engine (SessionArchive sessionId (archived /= 0)) callback context

enqueueSessionMutation
    :: Ptr ()
    -> SessionMutation
    -> FunPtr SessionResultCallback
    -> Ptr ()
    -> IO CInt
enqueueSessionMutation pointer mutation callback context
    | pointer == nullPtr = pure 1
    | callback == nullFunPtr = pure 2
    | otherwise = do
        accepted <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            atomically $ acceptEngineCommand engine.engineCommands
                (EngineSessionMutation mutation callback context)
        pure $ case accepted of
            Left _ -> 3
            Right False -> 3
            Right True -> 0

ha_engine_cancel_task
    :: Ptr () -> Ptr Word8 -> CSize -> IO CInt
ha_engine_cancel_task pointer taskID (CSize taskIDLength)
    | pointer == nullPtr = pure 1
    | taskID == nullPtr || taskIDLength == 0 = pure 2
    | otherwise = do
        accepted <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            taskIDBytes <- BS.packCStringLen
                (castPtr taskID, fromIntegral taskIDLength)
            case TextEncoding.decodeUtf8' taskIDBytes of
                Left _ -> pure Nothing
                Right taskIDText
                    | Text.null taskIDText -> pure Nothing
                    | otherwise -> Just <$> atomically
                        (acceptEngineCommand
                            engine.engineCommands
                            (EngineCancelTask taskIDText))
        pure $ case accepted of
            Left _ -> 3
            Right Nothing -> 2
            Right (Just False) -> 3
            Right (Just True) -> 0

ha_engine_list_tasks
    :: Ptr () -> FunPtr TaskSnapshotCallback -> Ptr () -> IO CInt
ha_engine_list_tasks pointer callback context
    | pointer == nullPtr = pure 1
    | callback == nullFunPtr = pure 2
    | otherwise = do
        accepted <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            atomically $ acceptEngineCommand
                engine.engineCommands
                (EngineTaskSnapshot callback context)
        pure $ case accepted of
            Left _ -> 3
            Right False -> 3
            Right True -> 0

ha_engine_set_task_limit :: Ptr () -> CSize -> IO CInt
ha_engine_set_task_limit pointer rawLimit
    | pointer == nullPtr = pure 1
    | limit < 1 || limit > 32 = pure 2
    | otherwise = do
        accepted <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            atomically $ acceptEngineCommand
                engine.engineCommands
                (EngineSetTaskLimit limit)
        pure $ case accepted of
            Left _ -> 3
            Right False -> 3
            Right True -> 0
  where
    limit = fromIntegral rawLimit

ha_engine_destroy :: Ptr () -> IO ()
ha_engine_destroy pointer
    | pointer == nullPtr = pure ()
    | otherwise = void $ tryAny do
        let stable = castPtrToStablePtr pointer :: StablePtr Engine
        (do
            engine <- deRefStablePtr stable
            _ <- atomically
                (closeEngineMailbox engine.engineCommands EngineStop)
            readMVar engine.engineDone)
            `finally` freeStablePtr stable

workerLifecycle
    :: FunPtr EventCallback
    -> Ptr ()
    -> ManagedPostgresConfig
    -> OsPath
    -> EngineMailbox EngineCommand
    -> TVar (Map Text [ImageAttachment])
    -> BrowserHost
    -> IO ()
workerLifecycle callback context config root commands stagedImages browser =
    (do
        store <- newMVar Nothing
        processRuntime <- newNativeProcessRuntime root
        workerRegistry <- newTVarIO Map.empty
        let cleanup =
                shutdownRunningTurns workerRegistry
                    `finally` closeNativeProcessRuntime processRuntime
                    `finally` closeEngineStore store
        supervisorLoop
            callback
            context
            config
            store
            root
            processRuntime
            commands
            stagedImages
            browser
            workerRegistry
            TaskSupervisor
                { supervisorLimit = defaultTaskLimit
                , supervisorPending = Seq.empty
                , supervisorRunning = Map.empty
                , supervisorKnownTaskIds = Set.empty
                }
            `finally` cleanup)
        `finally` cancelPendingMcpRestarts commands

cancelPendingMcpRestarts :: EngineMailbox EngineCommand -> IO ()
cancelPendingMcpRestarts commands = do
    pending <- atomically do
        _ <- closeEngineMailbox commands EngineStop
        drainEngineCommands commands
    forM_ pending \case
        EngineMcpRestart expected _ callback context ->
            void $ tryAny $
                withText "engine stopped before MCP restart completed" $
                    invokeMcpResultCallback callback context (-1) expected
        _ -> pure ()

supervisorLoop
    :: FunPtr EventCallback
    -> Ptr ()
    -> ManagedPostgresConfig
    -> MVar (Maybe Store)
    -> OsPath
    -> NativeProcessRuntime
    -> EngineMailbox EngineCommand
    -> TVar (Map Text [ImageAttachment])
    -> BrowserHost
    -> TVar (Map Text RunningTurn)
    -> TaskSupervisor
    -> IO ()
supervisorLoop
        callback context config store root processRuntime commands stagedImages browser
        workerRegistry =
    go
  where
    go supervisor0 = do
        supervisor <- startRunnableTasks supervisor0
        atomically (readEngineCommand commands) >>= handleCommand supervisor

    handleCommand supervisor = \case
        EngineStop ->
            shutdownSupervisor supervisor
        EngineSearch query limit searchCallback searchContext -> do
            runConversationSearch
                config store query limit searchCallback searchContext
            go supervisor
        EngineSessionMutation mutation resultCallback resultContext -> do
            runSessionMutation
                config store root mutation resultCallback resultContext
            go supervisor
        EngineMcpRestart expected name resultCallback resultContext -> do
            if Map.null supervisor.supervisorRunning
                then do
                    restarted <- tryAny do
                        home <- getHomeDirectory
                        mcpAdminTry
                            (restartMcpAdminServer home expected name
                                (restartNativeMcpRuntime processRuntime))
                    case restarted of
                        Left exception ->
                            withText (Text.pack (show exception)) $
                                invokeMcpResultCallback resultCallback
                                    resultContext (-1) expected
                        Right (Left err) ->
                            emitMcpResult resultCallback resultContext
                                (Left err
                                    :: Either
                                        McpAdminError
                                        (McpAdminSnapshot ()))
                        Right (Right snapshot) ->
                            invokeMcpResultCallback
                                resultCallback
                                resultContext
                                0
                                snapshot.mcpAdminRevision
                                nullPtr
                                0
                else
                    withText "cannot restart MCP while tasks are active" $
                        invokeMcpResultCallback resultCallback resultContext
                            (-1) expected
            go supervisor
        EngineCancelTask taskId -> do
            next <- cancelTaskById supervisor taskId
            go next
        EngineTaskSnapshot snapshotCallback snapshotContext -> do
            sendTaskSnapshot snapshotCallback snapshotContext supervisor
            go supervisor
        EngineSetTaskLimit limit ->
            go supervisor { supervisorLimit = limit }
        EngineTaskSession taskId sessionId -> do
            case Map.lookup taskId supervisor.supervisorRunning of
                Nothing -> pure ()
                Just running -> atomically $
                    writeTVar
                        running.runningTurnControl.turnControlSessionId
                        (Just sessionId)
            go supervisor
        EngineTaskFinished taskId outcome -> do
            case Map.lookup taskId supervisor.supervisorRunning of
                Nothing -> pure ()
                Just running -> do
                    _ <- waitCatch running.runningTurnWorker
                    sessionId <- readTVarIO
                        running.runningTurnControl.turnControlSessionId
                    cancelled <- readTVarIO
                        running.runningTurnControl.turnControlCancelled
                    if cancelled
                        then do
                            sendTaskState taskId sessionId "cancelled"
                            sendEvent callback context $
                                turnFailedEvent taskId "turn cancelled"
                        else do
                            sendTaskState
                                taskId
                                (taskResultSessionId sessionId outcome)
                                (taskResultState outcome)
                            finishTurnEvent callback context taskId outcome
            atomically $ modifyTVar' workerRegistry (Map.delete taskId)
            go supervisor
                { supervisorRunning =
                    Map.delete taskId supervisor.supervisorRunning
                }
        EngineRequest request ->
            handleEngineRequest supervisor request >>= go

    handleEngineRequest supervisor request
        | request.requestMethod == "turn.start" =
            enqueueTurn supervisor request
        | request.requestMethod == "turn.cancel" =
            case (parseParams request :: Either Text TurnReference) of
                Left err -> do
                    sendEvent callback context
                        (failureEvent request.requestId err)
                    pure supervisor
                Right reference -> do
                    let active =
                            Map.member
                                reference.turnReferenceId
                                supervisor.supervisorRunning
                        queued = any
                            ((== reference.turnReferenceId)
                                . (.turnStartId)
                                . (.pendingTurnStart))
                            supervisor.supervisorPending
                    next <- cancelTaskById
                        supervisor
                        reference.turnReferenceId
                    sendEvent callback context $
                        if active || queued
                            then successEvent request.requestId True
                            else failureEvent
                                request.requestId
                                "turn id is not active"
                    pure next
        | request.requestMethod == "approval.resolve" =
            case (parseParams request :: Either Text ApprovalResolution) of
                Left err -> do
                    sendEvent callback context
                        (failureEvent request.requestId err)
                    pure supervisor
                Right resolution -> do
                    matching <- filterM
                        (approvalIsActive resolution.approvalResolutionId
                            . (.runningTurnControl))
                        (Map.elems supervisor.supervisorRunning)
                    case matching of
                        [running] ->
                            resolveApproval running.runningTurnControl request
                                >>= sendEvent callback context
                        _ ->
                            sendEvent callback context $
                                failureEvent
                                    request.requestId
                                    "approval request is no longer active"
                    pure supervisor
        | request.requestMethod == "turn.agents" =
            selectRunningTurn request.requestParams supervisor >>= \case
                Left err -> do
                    sendEvent callback context
                        (failureEvent request.requestId err)
                    pure supervisor
                Right Nothing -> do
                    sendEvent callback context $
                        successEvent request.requestId ([] :: [Aeson.Value])
                    pure supervisor
                Right (Just running) ->
                    activeAgentSnapshot running.runningTurnControl request
                        >>= sendEvent callback context
                        >> pure supervisor
        | otherwise = do
            event <- handleRequest config store root request
            sendEvent callback context event
            pure supervisor

    selectRunningTurn params supervisor =
        case Aeson.parseEither
            (Aeson.withObject "turn reference" (.:? "turnId"))
            params of
            Left err -> pure (Left (Text.pack err))
            Right (Just taskId) ->
                pure $ maybe
                    (Left "turn id is not active")
                    (Right . Just)
                    (Map.lookup taskId supervisor.supervisorRunning)
            Right Nothing ->
                pure $ case Map.elems supervisor.supervisorRunning of
                    [running] -> Right (Just running)
                    [] -> Right Nothing
                    _ -> Left "turnId is required while multiple turns run"

    approvalIsActive approvalId control =
        Map.member approvalId
            <$> readTVarIO control.turnControlApprovals

    enqueueTurn supervisor request =
        case (parseParams request :: Either Text TurnStart) of
            Left err -> do
                atomically $ modifyTVar' stagedImages
                    (Map.delete request.requestId)
                sendEvent callback context
                    (failureEvent request.requestId err)
                pure supervisor
            Right start
                | taskExists start.turnStartId supervisor -> do
                    atomically $ modifyTVar' stagedImages
                        (Map.delete start.turnStartId)
                    sendEvent callback context $
                        failureEvent request.requestId "turn id already exists"
                    pure supervisor
                | otherwise -> do
                    images <- atomically $ do
                        staged <- readTVar stagedImages
                        writeTVar stagedImages
                            (Map.delete start.turnStartId staged)
                        pure
                            (Map.findWithDefault
                                []
                                start.turnStartId
                                staged)
                    sendEvent callback context $
                        successEvent request.requestId $
                            Aeson.object
                                [ "turnId" Aeson..= start.turnStartId
                                , "state" Aeson..= ("queued" :: Text)
                                ]
                    sendTaskState start.turnStartId start.turnStartSessionId
                        "queued"
                    pure supervisor
                        { supervisorPending =
                            supervisor.supervisorPending
                                Seq.|> PendingTurn start images
                        , supervisorKnownTaskIds =
                            Set.insert
                                start.turnStartId
                                supervisor.supervisorKnownTaskIds
                        }

    startRunnableTasks supervisor = do
        sessionIds <- activeSessionIds supervisor
        let available =
                supervisor.supervisorLimit
                    - Map.size supervisor.supervisorRunning
            pending = toList supervisor.supervisorPending
            candidates =
                [ ( TaskIdentity
                        pending.pendingTurnStart.turnStartId
                        pending.pendingTurnStart.turnStartSessionId
                  , pending
                  )
                | pending <- pending
                ]
            (selected, remaining) =
                selectRunnableTasks available sessionIds candidates
        running <- foldM
            startTask
            supervisor.supervisorRunning
            (map snd selected)
        pure supervisor
            { supervisorPending = Seq.fromList (map snd remaining)
            , supervisorRunning = running
            }

    startTask
        :: Map Text RunningTurn
        -> PendingTurn
        -> IO (Map Text RunningTurn)
    startTask running pending = do
        let start = pending.pendingTurnStart
        control <- newTurnControl
            start.turnStartId
            start.turnStartSessionId
        sendTaskState start.turnStartId start.turnStartSessionId "running"
        sendTurnStatus
            callback
            context
            start.turnStartId
            (if start.turnStartWorktree
                then "Creating worktree…"
                else "Starting…")
        nativeBrowserTools <- browserToolsWhenEnabled browser
        worker <- launchTrackedWorker start.turnStartId do
            runNativeTurn
                callback
                context
                commands
                processRuntime
                control
                nativeBrowserTools
                start
                pending.pendingTurnImages
        let runningTurn =
                RunningTurn
                    { runningTurnControl = control
                    , runningTurnWorker = worker
                    }
        atomically $ modifyTVar' workerRegistry $
            Map.insert start.turnStartId runningTurn
        pure $ Map.insert
            start.turnStartId
            runningTurn
            running

    launchTrackedWorker taskId action =
        mask \_ -> do
            gate <- newEmptyMVar
            worker <- asyncWithUnmask \unmask -> do
                takeMVar gate
                outcome <- newIORef (TaskFailure "turn cancelled")
                (tryAny (unmask action) >>= \case
                    Left exception ->
                        writeIORef outcome
                            (TaskFailure (Text.pack (show exception)))
                    Right value ->
                        writeIORef outcome (TaskOutcome value))
                    `finally` do
                        result <- readIORef outcome
                        void $ atomically $ acceptEngineCommand
                            commands
                            (EngineTaskFinished taskId result)
            putMVar gate ()
            pure worker

    activeSessionIds supervisor = do
        sessions <- mapM
            (readTVarIO . (.turnControlSessionId) . (.runningTurnControl))
            (Map.elems supervisor.supervisorRunning)
        pure (Set.fromList [session | Just session <- sessions])

    cancelTaskById supervisor taskId =
        case Map.lookup taskId supervisor.supervisorRunning of
            Just running -> do
                cancelTurn running.runningTurnControl
                cancel running.runningTurnWorker
                pure supervisor
            Nothing -> do
                let (cancelled, retained) = partition
                        ((== taskId)
                            . (.turnStartId)
                            . (.pendingTurnStart))
                        (toList supervisor.supervisorPending)
                forM_ cancelled \pending ->
                    let start = pending.pendingTurnStart
                    in do
                        sendTaskState
                            start.turnStartId
                            start.turnStartSessionId
                            "cancelled"
                        sendEvent callback context $
                            turnFailedEvent
                                start.turnStartId
                                "turn cancelled"
                pure supervisor
                    { supervisorPending = Seq.fromList retained }

    taskExists taskId supervisor =
        Set.member taskId supervisor.supervisorKnownTaskIds

    shutdownSupervisor _ =
        shutdownRunningTurns workerRegistry

    sendTaskState :: Text -> Maybe Text -> Text -> IO ()
    sendTaskState taskId sessionId state =
        sendEvent callback context $
            Aeson.object
                [ "event" Aeson..= ("task.state" :: Text)
                , "taskId" Aeson..= taskId
                , "sessionId" Aeson..= sessionId
                , "state" Aeson..= state
                ]

    sendTaskSnapshot snapshotCallback snapshotContext supervisor = do
        forM_ supervisor.supervisorPending \pending ->
            sendSnapshotItem
                snapshotCallback
                snapshotContext
                pending.pendingTurnStart.turnStartId
                pending.pendingTurnStart.turnStartSessionId
                0
        forM_ (Map.elems supervisor.supervisorRunning) \running -> do
            sessionId <- readTVarIO
                running.runningTurnControl.turnControlSessionId
            sendSnapshotItem
                snapshotCallback
                snapshotContext
                running.runningTurnControl.turnControlId
                sessionId
                1
        invokeTaskSnapshotCallback snapshotCallback snapshotContext
            1 nullPtr 0 nullPtr 0 0 nullPtr 0

    sendSnapshotItem snapshotCallback snapshotContext taskId sessionId state =
        withTextBytes taskId \taskPointer taskLength ->
        withMaybeTextBytes sessionId \sessionPointer sessionLength ->
            invokeTaskSnapshotCallback snapshotCallback snapshotContext
                0 taskPointer taskLength sessionPointer sessionLength
                state nullPtr 0

shutdownRunningTurns :: TVar (Map Text RunningTurn) -> IO ()
shutdownRunningTurns workerRegistry = do
    running <- atomically do
        current <- readTVar workerRegistry
        writeTVar workerRegistry Map.empty
        pure (Map.elems current)
    forM_ running (cancelTurn . (.runningTurnControl))
    mapM_ (cancel . (.runningTurnWorker)) running
    mapM_ (waitCatch . (.runningTurnWorker)) running

runSessionMutation
    :: ManagedPostgresConfig
    -> MVar (Maybe Store)
    -> OsPath
    -> SessionMutation
    -> FunPtr SessionResultCallback
    -> Ptr ()
    -> IO ()
runSessionMutation config store root mutation callback context = do
    outcome <- tryAny do
        activeStore <- acquireStore config store
        let pool = trustedPool activeStore
        case mutation of
            SessionRename sessionId title -> do
                result <- renameSession pool root sessionId title
                pure (() <$ result)
            SessionDelete sessionId ->
                deleteSession pool root sessionId
            SessionArchive sessionId archived ->
                setSessionArchived pool root sessionId archived
    case outcome of
        Left exception ->
            sendSessionMutationFailure
                callback context (Text.pack (show exception))
        Right (Left err) ->
            sendSessionMutationFailure callback context err
        Right (Right ()) ->
            invokeSessionResultCallback callback context 0 nullPtr 0

sendSessionMutationFailure
    :: FunPtr SessionResultCallback -> Ptr () -> Text -> IO ()
sendSessionMutationFailure callback context message =
    withText message \errorPtr errorLength ->
        invokeSessionResultCallback callback context (-1) errorPtr errorLength

runConversationSearch
    :: ManagedPostgresConfig
    -> MVar (Maybe Store)
    -> Text
    -> Int
    -> FunPtr SearchCallback
    -> Ptr ()
    -> IO ()
runConversationSearch config store query limit callback context = do
    outcome <- tryAny do
        activeStore <- acquireStore config store
        searchNativeConversations (trustedPool activeStore) query limit
    case outcome of
        Left exception ->
            sendSearchFailure callback context (Text.pack (show exception))
        Right (Left err) ->
            sendSearchFailure callback context (renderStoreError err)
        Right (Right results) -> do
            forM_ results (sendSearchResult callback context)
            invokeSearchCallback callback context
                1 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
                0 0 (-1) 0 0 nullPtr 0 nullPtr 0 0 nullPtr 0

sendSearchFailure :: FunPtr SearchCallback -> Ptr () -> Text -> IO ()
sendSearchFailure callback context message =
    withTextBytes message \errorPointer errorLength ->
        invokeSearchCallback callback context
            (-1) nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
            0 0 (-1) 0 0 nullPtr 0 nullPtr 0 0
            errorPointer errorLength

sendSearchResult
    :: FunPtr SearchCallback
    -> Ptr ()
    -> NativeConversationSearchResult
    -> IO ()
sendSearchResult callback context result =
    withTextBytes result.nativeSearchSessionId \sessionPointer sessionLength ->
    withTextBytes result.nativeSearchTitle \titlePointer titleLength ->
    withTextBytes result.nativeSearchCwd \cwdPointer cwdLength ->
    withTextBytes result.nativeSearchProvider \providerPointer providerLength ->
    withTextBytes result.nativeSearchModel \modelPointer modelLength ->
    withMaybeTextBytes result.nativeSearchUserText \userPointer userLength ->
    withMaybeTextBytes result.nativeSearchAssistantText
        \assistantPointer assistantLength ->
            invokeSearchCallback callback context
                0
                sessionPointer sessionLength
                titlePointer titleLength
                cwdPointer cwdLength
                providerPointer providerLength
                modelPointer modelLength
                (epochMilliseconds result.nativeSearchUpdatedAt)
                (if result.nativeSearchArchived then 1 else 0)
                (fromMaybe (-1) result.nativeSearchTurnIndex)
                (maybe 0 epochMilliseconds result.nativeSearchOccurredAt)
                (searchRoleCode result.nativeSearchRole)
                userPointer userLength
                assistantPointer assistantLength
                (realToFrac result.nativeSearchRank)
                nullPtr 0

withTextBytes :: Text -> (Ptr Word8 -> CSize -> IO a) -> IO a
withTextBytes value action =
    BS.useAsCStringLen (TextEncoding.encodeUtf8 value) \(pointer, length) ->
        action (castPtr pointer) (fromIntegral length)

withMaybeTextBytes
    :: Maybe Text
    -> (Ptr Word8 -> CSize -> IO a)
    -> IO a
withMaybeTextBytes Nothing action = action nullPtr 0
withMaybeTextBytes (Just value) action = withTextBytes value action

epochMilliseconds :: UTCTime -> Int64
epochMilliseconds =
    floor . (* 1000) . utcTimeToPOSIXSeconds

searchRoleCode :: Maybe Text -> CInt
searchRoleCode = \case
    Just "user" -> 1
    Just "assistant" -> 2
    _ -> 0

browserToolsWhenEnabled :: BrowserHost -> IO [AppTool]
browserToolsWhenEnabled host =
    withMVar host.browserRegistration \case
        Nothing -> pure []
        Just _ -> pure (browserTools (invokeBrowserCommand host))

invokeBrowserCommand
    :: BrowserHost
    -> BrowserCommand
    -> IO (Either Text Text)
invokeBrowserCommand host command =
    tryAny (withMVar host.browserRegistration invoke) >>= \case
        Left exception -> pure (Left (Text.pack (show exception)))
        Right result -> pure result
  where
    invoke Nothing =
        pure (Left "The browser view is not active.")
    invoke (Just registration) = do
        let
            ( commandCode
                , argument1
                , argument2
                , scrollDeltaX
                , scrollDeltaY
                , flags
                ) =
                browserCommandABI command
        withText argument1 \argument1Ptr argument1Length ->
            withText argument2 \argument2Ptr argument2Length ->
                allocaBytes browserOutputCapacity \output ->
                    alloca \outputLength -> do
                        poke outputLength 0
                        status <- invokeBrowserCallback
                            registration.browserCallback
                            registration.browserContext
                            commandCode
                            (castPtr argument1Ptr)
                            argument1Length
                            (castPtr argument2Ptr)
                            argument2Length
                            scrollDeltaX
                            scrollDeltaY
                            flags
                            output
                            (fromIntegral browserOutputCapacity)
                            outputLength
                        CSize length <- peek outputLength
                        if length > fromIntegral browserOutputCapacity
                            then pure (Left
                                "The browser returned more than the 256 KiB output limit.")
                            else do
                                bytes <- BS.packCStringLen
                                    (castPtr output, fromIntegral length)
                                pure $ case TextEncoding.decodeUtf8' bytes of
                                    Left _ ->
                                        Left "The browser returned invalid UTF-8."
                                    Right text
                                        | status == 0 -> Right text
                                        | Text.null text ->
                                            Left (browserStatusMessage status)
                                        | otherwise -> Left text

browserCommandABI
    :: BrowserCommand
    -> (CInt, Text, Text, CDouble, CDouble, CInt)
browserCommandABI = \case
    BrowserNavigate url -> (1, url, "", 0, 0, 0)
    BrowserSnapshot -> (2, "", "", 0, 0, 0)
    BrowserClick selector -> (3, selector, "", 0, 0, 0)
    BrowserType selector text submit ->
        (4, selector, text, 0, 0, if submit then 1 else 0)
    BrowserBack -> (5, "", "", 0, 0, 0)
    BrowserForward -> (6, "", "", 0, 0, 0)
    BrowserReload -> (7, "", "", 0, 0, 0)
    BrowserKey key -> (8, key, "", 0, 0, 0)
    BrowserScroll deltaX deltaY ->
        (9, "", "", realToFrac deltaX, realToFrac deltaY, 0)

browserOutputCapacity :: Int
browserOutputCapacity = 256 * 1024

browserStatusMessage :: CInt -> Text
browserStatusMessage = \case
    1 -> "The browser host rejected an invalid argument."
    2 -> "The native browser bridge is unavailable."
    3 -> "The browser command timed out."
    4 -> "The browser host denied website or extension access."
    5 -> "The browser host does not support this operation."
    6 -> "The native browser bridge failed internally."
    7 -> "The browser host returned a result that was too large."
    status ->
        "The browser command failed (status "
            <> Text.pack (show status)
            <> ")."

runNativeTurn
    :: FunPtr EventCallback
    -> Ptr ()
    -> EngineMailbox EngineCommand
    -> NativeProcessRuntime
    -> TurnControl
    -> [AppTool]
    -> TurnStart
    -> [ImageAttachment]
    -> IO TurnOutcome
runNativeTurn
        callback context commands processRuntime control nativeBrowserTools
        start images = do
    sessionIdRef <- newIORef start.turnStartSessionId
    completedRef <- newIORef False
    let hooks = NativeRunHooks
            { nativeOnLoopEvent = \event -> do
                case event of
                    TurnFinished _ -> writeIORef completedRef True
                    _ -> pure ()
                case encodeNativeLoopEvent control.turnControlId event of
                    Just bytes -> sendBinaryEvent callback context bytes
                    Nothing ->
                        forM_ (nativeLoopEvent control.turnControlId event)
                            (sendEvent callback context)
            , nativeOnSessionId = \sessionId -> do
                writeIORef sessionIdRef (Just sessionId)
                void $ atomically $ acceptEngineCommand
                    commands
                    (EngineTaskSession control.turnControlId sessionId)
                sendEvent callback context $
                    Aeson.object
                        [ "event" Aeson..= ("turn.session" :: Text)
                        , "turnId" Aeson..= control.turnControlId
                        , "sessionId" Aeson..= sessionId
                        ]
            , nativeRegisterCancel =
                atomically . writeTVar control.turnControlCancel
            , nativeRegisterAgentSnapshot =
                atomically . writeTVar control.turnControlAgentSnapshot
            , nativeRequestApproval =
                requestApproval callback context control
            , nativeTools = nativeBrowserTools
            }
        args = nativeTurnArguments start
    result <- tryAny $
        withTurnImages start.turnStartPrompt images \managedFile ->
            withFile "/dev/null" WriteMode \output ->
                runNativeAgent
                    processRuntime
                    output
                    (unsafeEncodeUtf start.turnStartCwd)
                    hooks
                    (args <> maybe ["--prompt", Text.unpack start.turnStartPrompt]
                        (\path -> ["--managed-turn-file", path])
                        managedFile)
    completed <- readIORef completedRef
    sessionId <- readIORef sessionIdRef
    pure TurnOutcome
        { turnOutcomeSessionId = sessionId
        , turnOutcomeError =
            case result of
                Left exception -> Just (nativeExceptionMessage exception)
                Right (Left err) -> Just err
                Right (Right ())
                    | completed -> Nothing
                    | otherwise ->
                        Just
                            "turn ended without a completion event"
        }

nativeExceptionMessage :: SomeException -> Text
nativeExceptionMessage exception =
    case fromException exception of
        Just (StartupFailure message) -> Text.pack message
        Nothing -> Text.pack (show exception)

nativeTurnArguments :: TurnStart -> [String]
nativeTurnArguments start =
    [ "--minimal"
    , "--motion", "off"
    , "--save-session"
    , "--no-yolo"
    ]
        <> maybe
            []
            (\sessionId -> ["--resume", Text.unpack sessionId])
            start.turnStartSessionId
        <> (if start.turnStartWorktree then ["--worktree"] else [])
        <> (if start.turnStartComputerUse
            then ["--computer-use"]
            else ["--no-computer-use"])
        <> modelArgs
        <> maybe
            []
            (\effort -> ["--effort", Text.unpack effort])
            start.turnStartEffort
  where
    modelArgs = case
        (start.turnStartProvider, start.turnStartModel) of
            (Just provider, Just model) ->
                [ "--provider", Text.unpack provider
                , "--model", Text.unpack model
                ]
            _ -> []

withTurnImages
    :: Text
    -> [ImageAttachment]
    -> (Maybe FilePath -> IO a)
    -> IO a
withTurnImages _ [] action = action Nothing
withTurnImages prompt images action = do
    temporaryDirectory <- getTemporaryDirectory
    withImageFiles temporaryDirectory images \paths -> do
        let request = managedTurnRequestWithImages prompt
                [ ManagedTurnMedia
                    { managedTurnMediaPath = path
                    , managedTurnMediaMime = image.imageMime
                    , managedTurnMediaName = Nothing
                    }
                | (path, image) <- zip paths images
                ]
        bracket
            (openBinaryTempFile temporaryDirectory "ha-native-turn-")
            (\(path, handle) -> do
                hClose handle
                void (tryAny (removeFile path)))
            \(path, handle) -> do
                hClose handle
                BS.writeFile path
                    (TextEncoding.encodeUtf8 (renderManagedTurnPrompt request))
                action (Just path)
  where
    withImageFiles _ [] action = action []
    withImageFiles directory (image : rest) action =
        bracket
            (openBinaryTempFile directory "ha-native-image-")
            (\(path, handle) -> do
                hClose handle
                void (tryAny (removeFile path)))
            \(path, handle) -> do
                hClose handle
                BS.writeFile path image.imageBytes
                withImageFiles directory rest (action . (path :))

newTurnControl :: Text -> Maybe Text -> IO TurnControl
newTurnControl turnId sessionId =
    TurnControl turnId
        <$> newTVarIO sessionId
        <*> newTVarIO False
        <*> newTVarIO (pure ())
        <*> newTVarIO Map.empty
        <*> newTVarIO 0
        <*> newTVarIO Set.empty
        <*> newTVarIO (pure [])

activeAgentSnapshot :: TurnControl -> BridgeRequest -> IO Aeson.Value
activeAgentSnapshot control request = do
    loadSnapshot <- readTVarIO control.turnControlAgentSnapshot
    tryAny loadSnapshot >>= \case
        Left exception -> pure $
            failureEvent request.requestId (Text.pack (show exception))
        Right agents -> pure $
            successEvent request.requestId (map agentEntryJSON agents)

agentEntryJSON :: Viewport.AgentEntry -> Aeson.Value
agentEntryJSON entry =
    Aeson.object
        [ "path" Aeson..= entry.agentPath
        , "status" Aeson..= entry.agentStatus
        , "model" Aeson..= entry.agentModel
        , "steps" Aeson..= map agentStepJSON entry.agentSteps
        ]

agentStepJSON :: Viewport.AgentStep -> Aeson.Value
agentStepJSON step =
    Aeson.object
        [ "state" Aeson..= agentStepStateText step.agentStepState
        , "title" Aeson..= step.agentStepTitle
        , "detail" Aeson..= step.agentStepDetail
        ]

agentStepStateText :: Viewport.AgentStepState -> Text
agentStepStateText = \case
    Viewport.AgentStepRunning -> "running"
    Viewport.AgentStepCompleted -> "completed"
    Viewport.AgentStepFailed -> "failed"
    Viewport.AgentStepInfo -> "info"

cancelTurn :: TurnControl -> IO ()
cancelTurn control = do
    atomically $ writeTVar control.turnControlCancelled True
    cancelAction <- readTVarIO control.turnControlCancel
    cancelAction
    waiters <- atomically do
        current <- readTVar control.turnControlApprovals
        writeTVar control.turnControlApprovals Map.empty
        pure (Map.elems current)
    atomically $
        forM_ waiters \waiter ->
            void (tryPutTMVar waiter PermissionDeny)

requestApproval
    :: FunPtr EventCallback
    -> Ptr ()
    -> TurnControl
    -> ToolCall
    -> IO (Maybe PermissionChoice)
requestApproval callback context control call = do
    alreadyAllowed <- Set.member call.name
        <$> readTVarIO control.turnControlAllowedTools
    if alreadyAllowed && not (isComputerToolCallKind call.callKind)
      then pure (Just PermissionAllowOnce)
      else requestApprovalFromClient callback context control call

requestApprovalFromClient
    :: FunPtr EventCallback
    -> Ptr ()
    -> TurnControl
    -> ToolCall
    -> IO (Maybe PermissionChoice)
requestApprovalFromClient callback context control call = do
    waiter <- newEmptyTMVarIO
    approvalId <- atomically do
        current <- readTVar control.turnControlApprovalCounter
        let next = current + 1
            approvalId =
                control.turnControlId
                    <> "-approval-"
                    <> Text.pack (show next)
        writeTVar control.turnControlApprovalCounter next
        modifyTVar'
            control.turnControlApprovals
            (Map.insert approvalId waiter)
        pure approvalId
    let (arguments, truncated) =
            boundedEventText
                (if call.argumentsEncrypted then "" else call.arguments)
    sendEvent callback context $
        Aeson.object
            [ "event" Aeson..= ("approval.requested" :: Text)
            , "turnId" Aeson..= control.turnControlId
            , "approval" Aeson..= Aeson.object
                [ "id" Aeson..= approvalId
                , "callId" Aeson..= call.callId
                , "name" Aeson..= call.name
                , "summary" Aeson..= summarizeToolCall call
                , "arguments" Aeson..= arguments
                , "argumentsEncrypted" Aeson..= call.argumentsEncrypted
                , "truncated" Aeson..= truncated
                ]
            ]
    choice <- atomically (takeTMVar waiter)
    atomically $
        modifyTVar'
            control.turnControlApprovals
            (Map.delete approvalId)
    case choice of
        PermissionAllowTool
            | not (isComputerToolCallKind call.callKind) ->
            atomically $
                modifyTVar'
                    control.turnControlAllowedTools
                    (Set.insert call.name)
        _ -> pure ()
    pure (Just choice)

resolveApproval :: TurnControl -> BridgeRequest -> IO Aeson.Value
resolveApproval control request =
    case (parseParams request
        :: Either Text ApprovalResolution) of
        Left err -> pure (failureEvent request.requestId err)
        Right resolution ->
            case permissionChoice resolution.approvalResolutionDecision of
                Nothing ->
                    pure $ failureEvent
                        request.requestId
                        "unknown approval decision"
                Just choice -> do
                    accepted <- atomically do
                        current <- readTVar control.turnControlApprovals
                        case Map.lookup
                            resolution.approvalResolutionId
                            current of
                                Nothing -> pure False
                                Just waiter -> do
                                    published <- tryPutTMVar waiter choice
                                    if published
                                        then writeTVar
                                            control.turnControlApprovals
                                            (Map.delete
                                                resolution.approvalResolutionId
                                                current)
                                        else pure ()
                                    pure published
                    pure $
                        if accepted
                            then successEvent request.requestId True
                            else failureEvent
                                request.requestId
                                "approval request is no longer active"

permissionChoice :: Text -> Maybe PermissionChoice
permissionChoice = \case
    "allow_once" -> Just PermissionAllowOnce
    "allow_tool" -> Just PermissionAllowTool
    "deny" -> Just PermissionDeny
    _ -> Nothing

finishTurnEvent
    :: FunPtr EventCallback
    -> Ptr ()
    -> Text
    -> TaskResult
    -> IO ()
finishTurnEvent callback context turnId = \case
    TaskFailure message ->
        sendEvent callback context $
            turnFailedEvent turnId message
    TaskOutcome outcome ->
        case outcome.turnOutcomeError of
            Just err ->
                sendEvent callback context (turnFailedEvent turnId err)
            Nothing ->
                sendEvent callback context $
                    Aeson.object
                        [ "event" Aeson..= ("turn.completed" :: Text)
                        , "turnId" Aeson..= turnId
                        , "sessionId" Aeson..= outcome.turnOutcomeSessionId
                        ]

taskResultSessionId :: Maybe Text -> TaskResult -> Maybe Text
taskResultSessionId fallback = \case
    TaskFailure _ -> fallback
    TaskOutcome outcome -> outcome.turnOutcomeSessionId <|> fallback

taskResultState :: TaskResult -> Text
taskResultState = \case
    TaskFailure _ -> "failed"
    TaskOutcome outcome ->
        case outcome.turnOutcomeError of
            Just _ -> "failed"
            Nothing -> "succeeded"

nativeLoopEvent :: Text -> LoopEvent -> Maybe Aeson.Value
nativeLoopEvent turnId = \case
    ActivityUpdated status -> Just $ turnStatusEvent turnId status
    WarningRaised warning -> Just $ turnStatusEvent turnId warning
    ResponseRestarted message -> Just $ turnStatusEvent turnId message
    _ -> Nothing

turnStatusEvent :: Text -> Text -> Aeson.Value
turnStatusEvent turnId status =
    Aeson.object
        [ "event" Aeson..= ("turn.status" :: Text)
        , "turnId" Aeson..= turnId
        , "status" Aeson..= status
        ]

sendTurnStatus
    :: FunPtr EventCallback
    -> Ptr ()
    -> Text
    -> Text
    -> IO ()
sendTurnStatus callback context turnId =
    sendEvent callback context . turnStatusEvent turnId

turnFailedEvent :: Text -> Text -> Aeson.Value
turnFailedEvent turnId message =
    Aeson.object
        [ "event" Aeson..= ("turn.failed" :: Text)
        , "turnId" Aeson..= turnId
        , "error" Aeson..= message
        ]

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
                sessions <- listSessions pool root
                archivedIds <- listArchivedSessionIds pool
                    >>= either (fail . Text.unpack) pure
                let archived = Set.fromList archivedIds
                summaries <- mapM
                    (\session -> sessionSummaryWithStatusJSON
                        root
                        (Set.member session.metaId archived)
                        session)
                    sessions
                pure $ successEvent current.requestId
                    summaries
            "sessions.show" ->
                case (parseParams current
                    :: Either Text SessionPageRequest) of
                    Left err ->
                        pure (failureEvent current.requestId err)
                    Right page -> do
                        activeStore <- acquireStore config store
                        snapshot <- loadSessionPageJSON
                            (trustedPool activeStore)
                            root
                            page.sessionPageId
                            page.sessionPageBefore
                            (max 1 (min 200 (maybe 50 id page.sessionPageLimit)))
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
                        catalogResult <- loadNativeModelCatalog
                            activeStore
                            root
                            parameters
                        pure $ either
                            (failureEvent current.requestId)
                            (successEvent current.requestId)
                            catalogResult
            method ->
                pure $ failureEvent current.requestId
                    ("unknown method: " <> method)

loadNativeModelCatalog
    :: Store
    -> OsPath
    -> ModelsListRequest
    -> IO (Either Text Aeson.Value)
loadNativeModelCatalog store root request = do
    let home = takeDirectory (takeDirectory root)
        requestedCwd = unsafeEncodeUtf request.modelsListCwd
    contextResult <- currentModelContext
        store
        root
        requestedCwd
        request.modelsListSessionId
    case contextResult of
        Left err -> pure (Left err)
        Right (cwd, maybeTarget) ->
            loadGatewayModelCatalogAt home cwd >>= \case
                Left err -> pure (Left err)
                Right catalog -> do
                    let configuredTarget = do
                            target <- maybeTarget
                            option <-
                                resolveConfiguredModel
                                    catalog
                                    target.targetModelId
                            if option.modelTarget.targetConnectionId
                                == target.targetConnectionId
                                then Just option
                                else Nothing
                    selectedTarget <-
                        traverse
                            (fmap (.modelTarget)
                                . resolveModelOptionDialect)
                            ( configuredTarget
                                <|> defaultModelOptionFor
                                    catalog
                                    OpenAIProvider
                                <|> listToMaybe (modelCatalog catalog)
                            )
                    case selectedTarget of
                        Nothing -> pure (Left "model catalog is empty")
                        Just target -> do
                            picker <- initialPickerStateResolved
                                catalog
                                target.targetConnectionId
                                target.targetProvider
                                target.targetModelId
                                target.targetDialect
                            pure $ Right $ Aeson.object
                                [ "options" Aeson..=
                                    map (modelOptionJSON catalog)
                                        picker.pickerAll
                                , "current" Aeson..=
                                    fmap (modelOptionJSON catalog)
                                        (selectedOption picker)
                                ]

currentModelContext
    :: Store
    -> OsPath
    -> OsPath
    -> Maybe Text
    -> IO (Either Text (OsPath, Maybe ModelTarget))
currentModelContext store root cwd = \case
    Just sessionId ->
        fmap (fmap (\meta ->
            (meta.metaCwd, Just (sessionModelTarget meta)))) $
            loadSessionMeta (trustedPool store) root sessionId
    Nothing -> do
        projectRoot <- resolveProjectRoot cwd
        settings <- loadProjectSettings projectRoot
        pure $ Right
            ( cwd
            , (.projectModelTarget) <$> settings.settingsLastModel
            )

sessionModelTarget :: SessionMeta -> ModelTarget
sessionModelTarget meta =
    ModelTarget
        { targetProvider = meta.metaProvider
        , targetConnectionId = meta.metaConnection
        , targetModelId = meta.metaModel
        , targetWireModelId =
            fromMaybe meta.metaModel meta.metaTransportModel
        , targetDialect = meta.metaDialect
        }

modelOptionJSON :: ModelCatalog -> ModelOption -> Aeson.Value
modelOptionJSON catalog option =
    let target = option.modelTarget
        configured =
            catalogModelById catalog target.targetModelId
    in Aeson.object
        [ "id" Aeson..= target.targetModelId
        , "provider" Aeson..= providerSlug target.targetProvider
        , "connection" Aeson..= target.targetConnectionId
        , "wireModel" Aeson..= target.targetWireModelId
        , "dialect" Aeson..= dialectSlug target.targetDialect
        , "label" Aeson..= option.modelLabel
        , "supportedReasoningEfforts" Aeson..=
            (configured >>= (.catalogModelReasoningEfforts))
        , "defaultReasoningEffort" Aeson..=
            (configured >>= (.catalogModelDefaultReasoningEffort))
        ]

parseParams :: Aeson.FromJSON value => BridgeRequest -> Either Text value
parseParams request =
    case Aeson.parseEither Aeson.parseJSON request.requestParams of
        Left err -> Left (Text.pack err)
        Right value -> Right value

startAccountOAuth
    :: AccountProviderRequest
    -> IO (Either Text Aeson.Value)
startAccountOAuth request =
    case parseProvider request.accountProvider of
        Just OpenAIProvider -> do
            clientId <- openAIOAuthClientId <$> lookupNonEmpty
                "OPENAI_OAUTH_CLIENT_ID"
            OpenAILogin.requestDeviceCode
                (OpenAILogin.defaultLoginOptions clientId) >>= \case
                    Left err -> pure (Left err)
                    Right code -> pure $ Right (Aeson.object
                        [ "provider" Aeson..= ("openai" :: Text)
                        , "verificationUrl" Aeson..= code.verificationUrl
                        , "userCode" Aeson..= code.userCode
                        , "deviceAuthId" Aeson..= code.deviceAuthId
                        , "pollIntervalSeconds" Aeson..=
                            code.pollIntervalSeconds
                        ])
        Just XAIProvider -> do
            clientId <- xaiOAuthClientId <$> lookupNonEmpty
                "XAI_OAUTH_CLIENT_ID"
            XAIAuth.requestDeviceAuthorization
                (XAIAuth.defaultOAuthOptions clientId) >>= \case
                    Left err -> pure (Left err)
                    Right code -> pure $ Right (Aeson.object
                        [ "provider" Aeson..= ("xai" :: Text)
                        , "verificationUrl" Aeson..= code.verificationUrl
                        , "userCode" Aeson..= code.userCode
                        , "deviceCode" Aeson..= code.deviceCode
                        , "pollIntervalSeconds" Aeson..=
                            code.pollIntervalSeconds
                        , "expiresInSeconds" Aeson..=
                            code.expiresInSeconds
                        ])
        _ -> pure (Left "OAuth account connection is not supported for this provider")

pollAccountOAuth
    :: AccountOAuthPollRequest
    -> IO (Either Text Aeson.Value)
pollAccountOAuth request =
    case parseProvider request.oauthPollProvider of
        Just OpenAIProvider -> case
            (request.oauthPollVerificationUrl, request.oauthPollUserCode,
                request.oauthPollDeviceAuthId) of
            (Just url, Just userCode, Just authId) ->
                do
                    clientId <- openAIOAuthClientId <$> lookupNonEmpty
                        "OPENAI_OAUTH_CLIENT_ID"
                    OpenAILogin.pollDeviceCode
                        (OpenAILogin.defaultLoginOptions clientId)
                        OpenAILogin.DeviceCode
                            { OpenAILogin.verificationUrl = Text.unpack url
                            , OpenAILogin.userCode = userCode
                            , OpenAILogin.deviceAuthId = authId
                            , OpenAILogin.pollIntervalSeconds =
                                fromMaybe 5 request.oauthPollIntervalSeconds
                            } >>= \case
                            Left err -> pure (Left err)
                            Right Nothing -> pure $ Right
                                (Aeson.object ["status" Aeson..= ("pending" :: Text)])
                            Right (Just authJson) -> do
                                now <- getCurrentTime
                                case openaiAuthStateFromJson now
                                    (Aeson.encode authJson) of
                                    Nothing -> pure (Left
                                        "OpenAI returned invalid account data")
                                    Just auth -> do
                                        let accountId =
                                                case auth of
                                                    OpenAIAuthTypes.AuthState
                                                        _ _ value _ _ -> value
                                        stored <- storeConnectedCredential
                                            False OpenAIProvider
                                                accountId
                                            "ChatGPT" SubscriptionBilled
                                            ManagedOpenAIAuthJson
                                            (TextEncoding.decodeUtf8
                                                (LBS.toStrict
                                                    (Aeson.encode authJson)))
                                        pure $ if stored
                                            then Right (Aeson.object
                                                [ "status" Aeson..=
                                                    ("connected" :: Text)
                                                , "accountID" Aeson..=
                                                    auth.accountId
                                                ])
                                            else Left "could not store account"
            _ -> pure (Left "OAuth challenge is missing required fields")
        Just XAIProvider -> case
            (request.oauthPollUserCode, request.oauthPollDeviceCode) of
            (Just _, Just deviceCode) -> do
                clientId <- xaiOAuthClientId <$> lookupNonEmpty
                    "XAI_OAUTH_CLIENT_ID"
                let challenge = XAIAuth.DeviceAuthorization
                        { XAIAuth.deviceCode = deviceCode
                        , XAIAuth.userCode = fromMaybe "" request.oauthPollUserCode
                        , XAIAuth.verificationUrl =
                            fromMaybe "" request.oauthPollVerificationUrl
                        , XAIAuth.pollIntervalSeconds =
                            fromMaybe 5 request.oauthPollIntervalSeconds
                        , XAIAuth.expiresInSeconds =
                            request.oauthPollExpiresInSeconds
                        }
                XAIAuth.pollDeviceAuthorization
                    (XAIAuth.defaultOAuthOptions clientId) challenge >>= \case
                        Left err -> pure (Left err)
                        Right Nothing -> pure $ Right
                            (Aeson.object ["status" Aeson..= ("pending" :: Text)])
                        Right (Just tokens) -> do
                            now <- getCurrentTime
                            let accountId = fromMaybe "grok"
                                    (XAIAuth.accountIdFromAccessToken
                                        tokens.accessToken)
                                label = fromMaybe "Grok" $
                                    (tokens.idToken >>= XAIAuth.emailFromToken)
                                    <|> XAIAuth.emailFromToken tokens.accessToken
                                authJson = grokAuthStateToJson GrokAuthState
                                    { grokAccessToken = tokens.accessToken
                                    , grokRefreshToken = tokens.refreshToken
                                    , grokIdToken = tokens.idToken
                                    , grokExpiresAt =
                                        ((`addUTCTime` now) . fromIntegral
                                            <$> tokens.expiresInSeconds)
                                    }
                            case tokens.refreshToken of
                                Nothing -> pure (Left
                                    "Grok login did not return a refresh token")
                                Just _ -> do
                                    stored <- storeConnectedCredential
                                        False XAIProvider accountId label
                                        SubscriptionBilled ManagedGrokAuthJson
                                        (TextEncoding.decodeUtf8
                                            (LBS.toStrict
                                                (Aeson.encode authJson)))
                                    pure $ if stored
                                        then Right (Aeson.object
                                            [ "status" Aeson..=
                                                ("connected" :: Text)
                                            , "accountID" Aeson..= accountId
                                            ])
                                        else Left "could not store account"
            _ -> pure (Left "OAuth challenge is missing required fields")
        _ -> pure (Left "OAuth account connection is not supported for this provider")

connectAccountAPIKey
    :: AccountAPIKeyRequest
    -> IO (Either Text Aeson.Value)
connectAccountAPIKey request =
    case parseProvider request.accountAPIKeyProvider of
        Just OpenRouterProvider
            | not (Text.null (Text.strip request.accountAPIKey)) ->
                OpenRouter.fetchOpenRouterUsage request.accountAPIKey >>= \case
                    Left err -> pure (Left ("OpenRouter rejected the key: " <> err))
                    Right usage -> do
                        let accountId = fromMaybe "openrouter" usage.keyLabel
                            label = fromMaybe "OpenRouter" usage.keyLabel
                        stored <- storeConnectedCredential
                            False OpenRouterProvider accountId label ApiBilled
                            ManagedBearerToken request.accountAPIKey
                        pure $ if stored
                            then Right (Aeson.object
                                [ "status" Aeson..= ("connected" :: Text)
                                , "accountID" Aeson..= accountId
                                ])
                            else Left "could not store account"
        _ -> pure (Left "API-key connections are supported for OpenRouter")

acquireStore :: ManagedPostgresConfig -> MVar (Maybe Store) -> IO Store
acquireStore config state =
    modifyMVar state \case
        Just store -> pure (Just store, store)
        Nothing ->
            openStore config >>= \case
                Left err -> fail (Text.unpack (renderStoreError err))
                Right store -> pure (Just store, store)

closeEngineStore :: MVar (Maybe Store) -> IO ()
closeEngineStore state =
    modifyMVar state \case
        Nothing -> pure (Nothing, ())
        Just store -> closeStore store >> pure (Nothing, ())

boundedEventText :: Text -> (Text, Bool)
boundedEventText value =
    let (visible, remainder) = Text.splitAt 8192 value
    in (visible, not (Text.null remainder))

successEvent :: Aeson.ToJSON value => Text -> value -> Aeson.Value
successEvent requestId result =
    Aeson.object
        [ "id" Aeson..= requestId
        , "ok" Aeson..= True
        , "result" Aeson..= result
        ]

failureEvent :: Text -> Text -> Aeson.Value
failureEvent requestId message =
    Aeson.object
        [ "id" Aeson..= requestId
        , "ok" Aeson..= False
        , "error" Aeson..= message
        ]

sendEvent :: FunPtr EventCallback -> Ptr () -> Aeson.Value -> IO ()
sendEvent callback context event =
    void $ tryAny $
        BS.useAsCStringLen (LBS.toStrict (Aeson.encode event)) \(bytes, length) ->
            sendCallbackBytes callback context (castPtr bytes) length

sendBinaryEvent :: FunPtr EventCallback -> Ptr () -> BS.ByteString -> IO ()
sendBinaryEvent callback context bytes =
    void $ tryAny $
        BS.useAsCStringLen bytes \ (pointer, length) ->
            sendCallbackBytes callback context (castPtr pointer) length

sendCallbackBytes
    :: FunPtr EventCallback
    -> Ptr ()
    -> Ptr Word8
    -> Int
    -> IO ()
sendCallbackBytes callback context bytes length =
    invokeEventCallback callback
        context
        (castPtr bytes)
        (fromIntegral length)
