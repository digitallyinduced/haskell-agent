{-# LANGUAGE ForeignFunctionInterface #-}

module Agent.CLI.MacOS.Bridge
    ( repositoryCancelAllAdmissionSmoke
    , repositoryCancelClassificationSmoke
    , repositoryCancelAllReentrancySmoke
    , repositoryCheckDestroyReentrancySmoke
    ) where

import qualified Agent.CLI.AgentViewport as Viewport
import Agent.CLI.NativeRuntime
    ( NativeProcessRuntime
    , NativeRunHooks(..)
    , closeNativeProcessRuntime
    , newNativeProcessRuntime
    , runNativeAgent
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
import qualified Agent.OpenAI.Login as OpenAILogin
import qualified Agent.OpenAI.Auth as OpenAIAuth
import qualified Agent.OpenAI.Auth.Types as OpenAIAuthTypes
import qualified Agent.XAI.Auth as XAIAuth
import qualified Agent.OpenRouter.Usage as OpenRouter
import Agent.CLI.ModelConfig
    ( CatalogModel(..)
    , ModelCatalog
    , catalogModelById
    , loadModelCatalogAt
    )
import Agent.CLI.Database.Store
    ( applicableDatabaseScopes
    , deriveDatabaseScopes
    )
import Agent.CLI.Models
    ( ModelOption(..)
    , ModelTarget(..)
    , PickerState(..)
    , defaultModelOptionFor
    , initialPickerStateResolved
    , modelCatalog
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
import qualified Agent.CLI.RepositoryReview as RepositoryReview
import Agent.CLI.Options (parseEffort)
import Agent.CLI.Session
    ( SessionMeta(..)
    , deleteSession
    , listArchivedSessionIds
    , listSessions
    , loadSessionMeta
    , renameSession
    , setSessionArchived
    , sessionsRoot
    )
import Agent.CLI.SessionAdmin
    ( loadSessionPageJSON
    , managedPostgresConfigForHome
    , sessionSummaryWithStatusJSON
    )
import Agent.Loop (ImageAttachment(..), LoopEvent(..))
import Agent.Dialect (dialectSlug)
import Agent.Provider (Provider(..), providerSlug, parseProvider, BillingMode(..))
import Agent.Store.Postgres
    ( ManagedPostgresConfig
    , Store
    , closeStore
    , openStore
    , trustedPool
    )
import Agent.Store.Types (renderStoreError)
import Agent.ToolDispatch
    ( ToolCall(..)
    )
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
    , waitCatchSTM
    , withAsync
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newEmptyMVar
    , newMVar
    , putMVar
    , readMVar
    , tryReadMVar
    )
import Control.Concurrent.STM
    ( TMVar
    , TQueue
    , TVar
    , atomically
    , modifyTVar'
    , newEmptyTMVarIO
    , newTQueueIO
    , newTVarIO
    , orElse
    , readTQueue
    , readTVar
    , readTVarIO
    , takeTMVar
    , tryPutTMVar
    , writeTQueue
    , writeTVar
    )
import Control.Applicative ((<|>))
import Control.Exception.Safe
    ( SomeAsyncException
    , SomeException
    , bracket
    , catchAsync
    , finally
    , isAsyncException
    , mask
    , throwIO
    , tryAny
    , uninterruptibleMask_
    )
import Control.Monad
    ( forM_
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
    , plusPtr
    , poke
    , peekByteOff
    , sizeOf
    )
import Foreign.C.String (CString)
import Foreign.C.Types (CDouble(..), CInt(..), CLLong(..), CSize(..))
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

type RepositoryCheckOutputCallback =
    Ptr () -> CInt -> Ptr Word8 -> CSize -> IO ()

type RepositoryCheckExitCallback =
    Ptr () -> CInt -> CInt -> CString -> CSize -> IO ()

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
    invokeSessionResultCallback
        :: FunPtr SessionResultCallback -> SessionResultCallback

foreign import ccall "dynamic"
    invokeAccountOAuthStartCallback
        :: FunPtr AccountOAuthStartCallback -> AccountOAuthStartCallback

foreign import ccall "dynamic"
    invokeSearchCallback :: FunPtr SearchCallback -> SearchCallback

foreign import ccall "dynamic"
    invokeLearnedSkillsListCallback
        :: FunPtr LearnedSkillsListCallback -> LearnedSkillsListCallback

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
    invokeRepositoryCheckOutputCallback
        :: FunPtr RepositoryCheckOutputCallback -> RepositoryCheckOutputCallback

foreign import ccall "dynamic"
    invokeRepositoryCheckExitCallback
        :: FunPtr RepositoryCheckExitCallback -> RepositoryCheckExitCallback

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
        let start = TurnStart
                { turnStartId
                , turnStartPrompt
                , turnStartSessionId
                , turnStartCwd
                , turnStartProvider
                , turnStartModel
                , turnStartEffort
                , turnStartWorktree
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
    | EngineStop

data SessionMutation
    = SessionRename !Text !Text
    | SessionDelete !Text
    | SessionArchive !Text !Bool

data Engine = Engine
    { engineCommands :: !(TQueue EngineCommand)
    , engineDone :: !(MVar ())
    , engineStagedImages :: !(TVar (Map Text [ImageAttachment]))
    }

data TurnControl = TurnControl
    { turnControlId :: !Text
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

data ActiveExit
    = ActiveContinue
    | ActiveStop

foreign export ccall ha_engine_create
    :: FunPtr EventCallback -> Ptr () -> IO (Ptr ())

foreign export ccall ha_engine_send_json
    :: Ptr () -> Ptr Word8 -> CSize -> IO CInt

foreign export ccall ha_engine_stage_turn_images
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr () -> CSize -> IO CInt

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

foreign export ccall ha_learned_skills_list
    :: Ptr Word8 -> CSize -> FunPtr LearnedSkillsListCallback -> Ptr () -> IO CInt

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
                    (emitRepositoryCancelled resultCallback context) $
                    tryRepositorySynchronous
                        (RepositoryReview.repositorySnapshot (Text.unpack path))
                        >>= \case
                            Left exception ->
                                emitRepositoryFailure
                                    resultCallback
                                    context
                                    (Text.pack (show exception))
                            Right (Left err) ->
                                emitRepositoryError resultCallback context err
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
                                        emitRepositoryFailure
                                            resultCallback
                                            context
                                            (Text.pack (show exception))
                                    Right () ->
                                        emitRepositorySuccess
                                            resultCallback
                                            context
                                            snapshot.snapshotId
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
                                (emitRepositoryCancelled resultCallback context) $
                                tryRepositorySynchronous
                                    (RepositoryReview.repositoryDiff
                                        (Text.unpack path)
                                        expected
                                        kind
                                        (Text.unpack file))
                                    >>= \case
                                        Left exception ->
                                            emitRepositoryFailure
                                                resultCallback
                                                context
                                                (Text.pack (show exception))
                                        Right (Left err) ->
                                            emitRepositoryError
                                                resultCallback context err
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
                                                    emitRepositoryFailure
                                                        resultCallback
                                                        context
                                                        (Text.pack
                                                            (show exception))
                                                Right () ->
                                                    emitRepositorySuccess
                                                        resultCallback
                                                        context
                                                        expected
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
    | otherwise =
        copyRequiredTexts
            [ (pathBytes, pathLength)
            , (snapshotBytes, snapshotLength)
            , (fileBytes, fileLength)
            ]
            >>= \case
                Left _ -> pure 2
                Right [path, expected, file] -> do
                    indices <- mapM
                        (\index ->
                            fromIntegral
                                <$> (peekByteOff
                                    hunkIndices
                                    (index * sizeOf (undefined :: CSize))
                                    :: IO CSize))
                        [0 .. fromIntegral hunkCount - 1]
                    case repositoryHunkMutation
                        operation (Text.unpack file) indices of
                        Nothing -> pure 2
                        Just mutation -> do
                            started <- startRepositoryMutation
                                callback context (Text.unpack path) expected mutation
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
                        tryRepositorySynchronous
                            (RepositoryReview.commitRepository
                                (Text.unpack path)
                                expected
                                message)
                            >>= \case
                                Left exception ->
                                    emitRepositoryFailure
                                        callback context (Text.pack (show exception))
                                Right (Left err) ->
                                    emitRepositoryError callback context err
                                Right (Right snapshot) ->
                                    emitRepositorySuccess
                                        callback context snapshot.snapshotId
                    pure (if started then 0 else 3)
                Right _ -> pure 3

startRepositoryMutation
    :: FunPtr RepositoryResultCallback
    -> Ptr ()
    -> FilePath
    -> Text
    -> RepositoryReview.RepositoryMutation
    -> IO Bool
startRepositoryMutation callback context path expected mutation =
    startRepositoryWorker (emitRepositoryCancelled callback context) $
        tryRepositorySynchronous
            (RepositoryReview.mutateRepository path expected mutation)
            >>= \case
                Left exception ->
                    emitRepositoryFailure callback context
                        (Text.pack (show exception))
                Right (Left err) ->
                    emitRepositoryError callback context err
                Right (Right snapshot) ->
                    emitRepositorySuccess callback context snapshot.snapshotId

startRepositoryWorker :: IO () -> IO () -> IO Bool
startRepositoryWorker onCancelled action =
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
                                (((readMVar gate >> unmask action)
                                    `catchAsync`
                                        \(_ :: SomeAsyncException) ->
                                            onCancelled))
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
                    (putMVar completed ())
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
            startRepositoryWorker (pure ()) (pure ()) >>= \case
                False -> pure True
                True -> threadDelay 1000 >> awaitRejection (attempts - 1)

repositoryCancelAllReentrancySmoke :: IO Bool
repositoryCancelAllReentrancySmoke = do
    completed <- newEmptyMVar
    accepted <- startRepositoryWorker (pure ()) do
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
    if not accepted
        then pure False
        else do
            readMVar entered
            ha_repository_cancel_all
            cancellation <- tryReadMVar cancelled
            failure <- tryReadMVar synthesizedFailure
            pure (isJust cancellation && isNothing failure)

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
                        Right arguments -> do
                            checkRef <- newIORef Nothing
                            cancelRef <- newIORef False
                            gate <- newEmptyMVar
                            owner <- asyncWithUnmask \unmask ->
                                readMVar gate >> unmask do
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
                                            withText
                                                (Text.pack (show exception))
                                                \errorPtr errorLength ->
                                                    withRepositoryCallbackThread
                                                        (invokeRepositoryCheckExitCallback
                                                            exitCallback
                                                            context 0 (-1)
                                                            errorPtr errorLength)
                                        Right (Left err) ->
                                            withText
                                                (RepositoryReview.repositoryErrorText
                                                    err)
                                                \errorPtr errorLength ->
                                                    withRepositoryCallbackThread
                                                        (invokeRepositoryCheckExitCallback
                                                            exitCallback
                                                            context 0 (-1)
                                                            errorPtr errorLength)
                                        Right (Right check) -> do
                                            writeIORef checkRef (Just check)
                                            cancelRequested <- readIORef cancelRef
                                            when cancelRequested
                                                (RepositoryReview.cancelRepositoryCheck
                                                    check)
                                            RepositoryReview.waitRepositoryCheck check
                            stable <- newStablePtr RepositoryCheckHandle
                                { repositoryCheckValue = checkRef
                                , repositoryCheckCancelRequested = cancelRef
                                , repositoryCheckOwner = owner
                                }
                            poke outCheck (castStablePtrToPtr stable)
                            -- No callback can begin before the owned handle is
                            -- visible through out_check.
                            putMVar gate ()
                            pure 0
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

copyRequiredText :: Ptr Word8 -> CSize -> IO (Either () Text)
copyRequiredText pointer (CSize length)
    | pointer == nullPtr || length == 0 = pure (Left ())
    | toInteger length > toInteger (maxBound :: Int) = pure (Left ())
    | otherwise = do
        bytes <- BS.packCStringLen (castPtr pointer, fromIntegral length)
        pure (first (const ()) (TextEncoding.decodeUtf8' bytes))

copyRequiredTexts
    :: [(Ptr Word8, CSize)]
    -> IO (Either () [Text])
copyRequiredTexts = fmap sequence . mapM (uncurry copyRequiredText)

byteStringChunks :: Int -> BS.ByteString -> [BS.ByteString]
byteStringChunks size bytes
    | BS.null bytes = []
    | otherwise =
        let (chunk, remaining) = BS.splitAt size bytes
        in chunk : byteStringChunks size remaining

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
            commands <- newTQueueIO
            done <- newEmptyMVar
            stagedImages <- newTVarIO Map.empty
            _ <- forkFinally
                (workerLifecycle
                    callback
                    context
                    config
                    (sessionsRoot home)
                    commands
                    stagedImages)
                (const (putMVar done ()))
            stable <- newStablePtr Engine
                { engineCommands = commands
                , engineDone = done
                , engineStagedImages = stagedImages
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
                    pure False
                Right request -> do
                    atomically
                        (writeTQueue
                            engine.engineCommands
                            (EngineRequest request))
                    pure True
        pure $ case accepted of
            Left _ -> 3
            Right False -> 4
            Right True -> 0

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
                Left _ -> pure False
                Right query
                    | Text.null (Text.strip query) -> pure False
                    | otherwise -> do
                        let requested = fromIntegral rawLimit :: Integer
                            limit = fromInteger (max 1 (min 100 requested))
                        atomically $ writeTQueue engine.engineCommands
                            (EngineSearch query limit callback context)
                        pure True
        pure $ case accepted of
            Left _ -> 3
            Right False -> 2
            Right True -> 0

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
            atomically $ writeTQueue engine.engineCommands
                (EngineSessionMutation mutation callback context)
        pure $ case accepted of
            Left _ -> 3
            Right () -> 0

ha_engine_destroy :: Ptr () -> IO ()
ha_engine_destroy pointer
    | pointer == nullPtr = pure ()
    | otherwise = void $ tryAny do
        let stable = castPtrToStablePtr pointer :: StablePtr Engine
        (do
            engine <- deRefStablePtr stable
            atomically (writeTQueue engine.engineCommands EngineStop)
            readMVar engine.engineDone)
            `finally` freeStablePtr stable

workerLifecycle
    :: FunPtr EventCallback
    -> Ptr ()
    -> ManagedPostgresConfig
    -> OsPath
    -> TQueue EngineCommand
    -> TVar (Map Text [ImageAttachment])
    -> IO ()
workerLifecycle callback context config root commands stagedImages = do
    store <- newMVar Nothing
    processRuntime <- newNativeProcessRuntime root
    let cleanup =
            closeNativeProcessRuntime processRuntime
                `finally` closeEngineStore store
    idleLoop
        callback
        context
        config
        store
        root
        processRuntime
        commands
        stagedImages
        `finally` cleanup

idleLoop
    :: FunPtr EventCallback
    -> Ptr ()
    -> ManagedPostgresConfig
    -> MVar (Maybe Store)
    -> OsPath
    -> NativeProcessRuntime
    -> TQueue EngineCommand
    -> TVar (Map Text [ImageAttachment])
    -> IO ()
idleLoop callback context config store root processRuntime commands stagedImages =
    atomically (readTQueue commands) >>= \case
        EngineStop -> pure ()
        EngineSearch query limit searchCallback searchContext -> do
            runConversationSearch
                config store query limit searchCallback searchContext
            continue
        EngineSessionMutation mutation resultCallback resultContext -> do
            runSessionMutation
                config store root mutation resultCallback resultContext
            continue
        EngineRequest request
            | request.requestMethod == "turn.start" ->
                case (parseParams request
                    :: Either Text TurnStart) of
                    Left err -> do
                        atomically $ modifyTVar' stagedImages
                            (Map.delete request.requestId)
                        sendEvent callback context
                            (failureEvent request.requestId err)
                        continue
                    Right start -> do
                        images <- atomically $ do
                            staged <- readTVar stagedImages
                            writeTVar stagedImages
                                (Map.delete start.turnStartId staged)
                            pure (Map.findWithDefault [] start.turnStartId staged)
                        control <- newTurnControl start.turnStartId
                        sendEvent callback context $
                            successEvent request.requestId $
                                Aeson.object
                                    [ "turnId" Aeson..= start.turnStartId
                                    ]
                        sendTurnStatus
                            callback
                            context
                            start.turnStartId
                            (if start.turnStartWorktree
                                then "Creating worktree…"
                                else "Starting…")
                        withAsync
                            (runNativeTurn
                                callback
                                context
                                processRuntime
                                control
                                start
                                images)
                            \running ->
                                activeLoop
                                    callback
                                    context
                                    config
                                    store
                                    root
                                    commands
                                    control
                                    running >>= \case
                                        ActiveContinue -> continue
                                        ActiveStop -> pure ()
            | request.requestMethod `elem`
                ["turn.cancel", "approval.resolve"] -> do
                    sendEvent callback context $
                        failureEvent
                            request.requestId
                            "there is no active turn"
                    continue
            | otherwise -> do
                event <- handleRequest config store root request
                sendEvent callback context event
                continue
  where
    continue =
        idleLoop
            callback
            context
            config
            store
            root
            processRuntime
            commands
            stagedImages

activeLoop
    :: FunPtr EventCallback
    -> Ptr ()
    -> ManagedPostgresConfig
    -> MVar (Maybe Store)
    -> OsPath
    -> TQueue EngineCommand
    -> TurnControl
    -> Async TurnOutcome
    -> IO ActiveExit
activeLoop callback context config store root commands control running =
    atomically
        ((Left <$> readTQueue commands)
            `orElse` (Right <$> waitCatchSTM running)) >>= \case
        Right outcome -> do
            finishTurnEvent callback context control.turnControlId outcome
            pure ActiveContinue
        Left EngineStop -> do
            cancelTurn control
            cancel running
            pure ActiveStop
        Left (EngineSearch query limit searchCallback searchContext) -> do
            runConversationSearch
                config store query limit searchCallback searchContext
            activeLoop
                callback context config store root commands control running
        Left (EngineSessionMutation mutation resultCallback resultContext) -> do
            runSessionMutation
                config store root mutation resultCallback resultContext
            activeLoop
                callback context config store root commands control running
        Left (EngineRequest request) -> do
            if request.requestMethod == "turn.cancel"
              then
                case (parseParams request
                    :: Either Text TurnReference) of
                    Right reference
                        | reference.turnReferenceId == control.turnControlId -> do
                            cancelTurn control
                            cancel running
                            sendEvent callback context $
                                successEvent request.requestId True
                    _ ->
                        sendEvent callback context $
                            failureEvent request.requestId "turn id is not active"
              else if request.requestMethod == "approval.resolve"
              then
                resolveApproval control request >>= sendEvent callback context
              else if request.requestMethod == "turn.agents"
              then
                activeAgentSnapshot control request
                    >>= sendEvent callback context
              else if request.requestMethod == "turn.start"
              then
                sendEvent callback context $
                    failureEvent request.requestId "a turn is already running"
              else
                handleRequest config store root request
                    >>= sendEvent callback context
            activeLoop
                callback
                context
                config
                store
                root
                commands
                control
                running

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

runNativeTurn
    :: FunPtr EventCallback
    -> Ptr ()
    -> NativeProcessRuntime
    -> TurnControl
    -> TurnStart
    -> [ImageAttachment]
    -> IO TurnOutcome
runNativeTurn callback context processRuntime control start images = do
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
            }
        modelArgs = case
            (start.turnStartProvider, start.turnStartModel) of
                (Just provider, Just model) ->
                    [ "--provider", Text.unpack provider
                    , "--model", Text.unpack model
                    ]
                _ -> []
        args =
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
                <> modelArgs
                <> maybe
                    []
                    (\effort -> ["--effort", Text.unpack effort])
                    start.turnStartEffort
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
                Left exception -> Just (Text.pack (show exception))
                Right (Left err) -> Just err
                Right (Right ())
                    | completed -> Nothing
                    | otherwise ->
                        Just
                            "turn ended without a completion event"
        }

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

newTurnControl :: Text -> IO TurnControl
newTurnControl turnId =
    TurnControl turnId
        <$> newTVarIO (pure ())
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
    if alreadyAllowed
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
        PermissionAllowTool ->
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
    -> Either SomeException TurnOutcome
    -> IO ()
finishTurnEvent callback context turnId = \case
    Left exception ->
        sendEvent callback context $
            turnFailedEvent turnId (Text.pack (show exception))
    Right outcome ->
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
            loadModelCatalogAt home cwd >>= \case
                Left err -> pure (Left err)
                Right catalog -> do
                    selectedTarget <- case maybeTarget of
                        Just target -> pure (Just target)
                        Nothing ->
                            traverse
                                (fmap (.modelTarget)
                                    . resolveModelOptionDialect)
                                ( defaultModelOptionFor
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
