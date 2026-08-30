{-# LANGUAGE ForeignFunctionInterface #-}

module Agent.CLI.MacOS.Bridge
    ( NativeInteractionResolution(..)
    , PendingInteraction(..)
    , cancelPendingInteractions
    , discardStagedTurn
    , discardStagedTurnById
    , resolvePendingInteraction
    , turnStartCleanupId
    ) where

import qualified Agent.CLI.AgentViewport as Viewport
import Agent.CLI.NativeRuntime
    ( NativeInteractionMode(..)
    , NativeProcessRuntime
    , NativeRunHooks(..)
    , NativeShellMode(..)
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
    , encodeNativeUsageEvent
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
import Agent.Loop
    ( ImageAttachment(..)
    , LoopEvent(..)
    , TokenUsage
    , TurnOutput(..)
    , emptyTokenUsage
    )
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
import Agent.Tools.PlanMode
    ( PlanDecision(..)
    , PlanModeHooks(..)
    )
import Control.Concurrent (forkFinally, forkIO)
import Control.Concurrent.Async
    ( Async
    , cancel
    , mapConcurrently
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
    , withMVar
    )
import Control.Concurrent.STM
    ( STM
    , TMVar
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
    ( SomeException
    , bracket
    , finally
    , onException
    , tryAny
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
import Data.IORef
    ( modifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, listToMaybe)
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
    , Storable(..)
    , allocaArray
    , castPtr
    , castPtrToStablePtr
    , castStablePtrToPtr
    , deRefStablePtr
    , freeStablePtr
    , newStablePtr
    , nullFunPtr
    , nullPtr
    , plusPtr
    , peekByteOff
    , pokeByteOff
    , pokeElemOff
    , sizeOf
    )
import Foreign.C.String (CString)
import Foreign.C.Types (CDouble(..), CInt(..), CLLong(..), CSize(..))
import System.Directory
    ( getTemporaryDirectory
    , removeFile
    )
import System.Directory.OsPath (getHomeDirectory)
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

type EventCallback = Ptr () -> Ptr Word8 -> CSize -> IO ()

data CInteractionOption = CInteractionOption
    { cInteractionOptionLabel :: !(Ptr Word8)
    , cInteractionOptionLabelLength :: !CSize
    }

instance Storable CInteractionOption where
    sizeOf _ = sizeOf (nullPtr :: Ptr Word8) + sizeOf (undefined :: CSize)
    alignment _ =
        max
            (alignment (nullPtr :: Ptr Word8))
            (alignment (undefined :: CSize))
    peek pointer =
        CInteractionOption
            <$> peekByteOff pointer 0
            <*> peekByteOff pointer (sizeOf (nullPtr :: Ptr Word8))
    poke pointer option = do
        pokeByteOff pointer 0 option.cInteractionOptionLabel
        pokeByteOff
            pointer
            (sizeOf (nullPtr :: Ptr Word8))
            option.cInteractionOptionLabelLength

type InteractionCallback =
    Ptr ()
    -> Ptr Word8 -> CSize -- turn id
    -> Ptr Word8 -> CSize -- interaction id
    -> CInt -- kind
    -> Ptr Word8 -> CSize -- prompt/body
    -> Ptr CInteractionOption -> CSize
    -> IO ()

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

foreign import ccall "dynamic"
    invokeEventCallback :: FunPtr EventCallback -> EventCallback

foreign import ccall "dynamic"
    invokeInteractionCallback
        :: FunPtr InteractionCallback -> InteractionCallback

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

turnStartCleanupId :: Text -> Aeson.Value -> Text
turnStartCleanupId requestId params =
    fromMaybe requestId $
        Aeson.parseMaybe
            (Aeson.withObject "TurnStartCleanup" (.:? "turnId"))
            params
            >>= id
            >>= nonBlank
  where
    nonBlank value
        | Text.null (Text.strip value) = Nothing
        | otherwise = Just value

discardStagedTurn
    :: Text
    -> Aeson.Value
    -> TVar (Map Text a)
    -> TVar (Map Text b)
    -> STM ()
discardStagedTurn requestId params stagedImages stagedOptions = do
    discardStagedTurnById
        (turnStartCleanupId requestId params)
        stagedImages
        stagedOptions

discardStagedTurnById
    :: Text
    -> TVar (Map Text a)
    -> TVar (Map Text b)
    -> STM ()
discardStagedTurnById turnId stagedImages stagedOptions = do
    modifyTVar' stagedImages (Map.delete turnId)
    modifyTVar' stagedOptions (Map.delete turnId)

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

data NativeTurnOptions = NativeTurnOptions
    { nativeTurnInteractionMode :: !NativeInteractionMode
    , nativeTurnShellMode :: !NativeShellMode
    } deriving (Eq, Show)

defaultNativeTurnOptions :: NativeTurnOptions
defaultNativeTurnOptions = NativeTurnOptions
    { nativeTurnInteractionMode = NativeAsk
    , nativeTurnShellMode = NativeShellBash
    }

data NativeInteractionResolution = NativeInteractionResolution
    { interactionSelectedIndex :: !Int
    , interactionCustomText :: !(Maybe Text)
    } deriving (Eq, Show)

data PendingInteraction = PendingInteraction
    { pendingInteractionOptionCount :: !Int
    , pendingInteractionWaiter :: !(TMVar NativeInteractionResolution)
    }

resolvePendingInteraction
    :: TVar (Map (Text, Text) PendingInteraction)
    -> (Text, Text)
    -> NativeInteractionResolution
    -> STM Bool
resolvePendingInteraction pendingRef key resolution = do
    pending <- readTVar pendingRef
    case Map.lookup key pending of
        Nothing -> pure False
        Just interaction@PendingInteraction
            { pendingInteractionOptionCount = optionCount
            }
            | resolution.interactionSelectedIndex < (-1)
                || resolution.interactionSelectedIndex >= optionCount ->
                pure False
            | otherwise -> do
                published <- tryPutTMVar
                    interaction.pendingInteractionWaiter
                    resolution
                when published $
                    writeTVar pendingRef (Map.delete key pending)
                pure published

cancelPendingInteractions
    :: TVar (Map (Text, Text) PendingInteraction)
    -> STM ()
cancelPendingInteractions pendingRef = do
    pending <- readTVar pendingRef
    writeTVar pendingRef Map.empty
    forM_ (Map.elems pending) \interaction ->
        void $ tryPutTMVar
            interaction.pendingInteractionWaiter
            cancelledInteractionResolution

cancelledInteractionResolution :: NativeInteractionResolution
cancelledInteractionResolution = NativeInteractionResolution
    { interactionSelectedIndex = -1
    , interactionCustomText = Nothing
    }

data InteractionCallbackTarget = InteractionCallbackTarget
    { interactionTargetCallback :: !(FunPtr InteractionCallback)
    , interactionTargetContext :: !(Ptr ())
    }

data InteractionRuntime = InteractionRuntime
    { interactionCallbackTarget :: !(TVar (Maybe InteractionCallbackTarget))
    , interactionCallbackLock :: !(MVar ())
    , interactionPending
        :: !(TVar (Map (Text, Text) PendingInteraction))
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
    , engineStagedTurnOptions :: !(TVar (Map Text NativeTurnOptions))
    , engineInteractions :: !InteractionRuntime
    }

data TurnControl = TurnControl
    { turnControlId :: !Text
    , turnControlCancel :: !(TVar (IO ()))
    , turnControlApprovals
        :: !(TVar (Map Text (TMVar PermissionChoice)))
    , turnControlApprovalCounter :: !(TVar Int)
    , turnControlInteractionCounter :: !(TVar Int)
    , turnControlAllowedTools :: !(TVar (Set.Set Text))
    , turnControlAgentSnapshot :: !(TVar (IO [Viewport.AgentEntry]))
    , turnControlInteractions :: !InteractionRuntime
    }

data TurnOutcome = TurnOutcome
    { turnOutcomeSessionId :: !(Maybe Text)
    , turnOutcomeError :: !(Maybe Text)
    , turnOutcomeUsage :: !TokenUsage
    , turnOutcomeProviderCostUSD :: !(Maybe Double)
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

foreign export ccall ha_engine_stage_turn_options
    :: Ptr () -> Ptr Word8 -> CSize -> CInt -> CInt -> IO CInt

foreign export ccall ha_engine_discard_turn_staging
    :: Ptr () -> Ptr Word8 -> CSize -> IO CInt

foreign export ccall ha_engine_set_interaction_callback
    :: Ptr () -> FunPtr InteractionCallback -> Ptr () -> IO CInt

foreign export ccall ha_engine_resolve_interaction
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> CInt -> Ptr Word8 -> CSize -> IO CInt

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
            stagedTurnOptions <- newTVarIO Map.empty
            interactionTarget <- newTVarIO Nothing
            interactionLock <- newMVar ()
            pendingInteractions <- newTVarIO Map.empty
            let interactions = InteractionRuntime
                    { interactionCallbackTarget = interactionTarget
                    , interactionCallbackLock = interactionLock
                    , interactionPending = pendingInteractions
                    }
            _ <- forkFinally
                (workerLifecycle
                    callback
                    context
                    config
                    (sessionsRoot home)
                    commands
                    stagedImages
                    stagedTurnOptions
                    interactions)
                (const (putMVar done ()))
            stable <- newStablePtr Engine
                { engineCommands = commands
                , engineDone = done
                , engineStagedImages = stagedImages
                , engineStagedTurnOptions = stagedTurnOptions
                , engineInteractions = interactions
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
                    atomically $
                        writeTVar engine.engineStagedTurnOptions Map.empty
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
    | turnID == nullPtr || not (validNativeTurnIDLength turnIDLength) = pure 2
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

ha_engine_stage_turn_options
    :: Ptr () -> Ptr Word8 -> CSize -> CInt -> CInt -> IO CInt
ha_engine_stage_turn_options pointer turnID turnIDLength rawMode rawShell
    | pointer == nullPtr = pure 1
    | turnID == nullPtr || not (validNativeTurnIDLength turnIDLength) = pure 2
    | otherwise =
        case (interactionModeFromCode rawMode, shellModeFromCode rawShell) of
            (Just interactionMode, Just shellMode) -> do
                accepted <- tryAny do
                    let stable =
                            castPtrToStablePtr pointer :: StablePtr Engine
                    engine <- deRefStablePtr stable
                    bytes <- BS.packCStringLen
                        (castPtr turnID, fromIntegral turnIDLength)
                    case TextEncoding.decodeUtf8' bytes of
                        Left _ -> pure False
                        Right turnIDText
                            | Text.null turnIDText -> pure False
                            | otherwise -> do
                                atomically $ modifyTVar'
                                    engine.engineStagedTurnOptions
                                    (Map.insert
                                        turnIDText
                                        NativeTurnOptions
                                            { nativeTurnInteractionMode =
                                                interactionMode
                                            , nativeTurnShellMode = shellMode
                                            })
                                pure True
                pure $ case accepted of
                    Left _ -> 3
                    Right False -> 2
                    Right True -> 0
            _ -> pure 4

ha_engine_discard_turn_staging
    :: Ptr () -> Ptr Word8 -> CSize -> IO CInt
ha_engine_discard_turn_staging pointer turnID turnIDLength
    | pointer == nullPtr = pure 1
    | turnID == nullPtr || not (validNativeTurnIDLength turnIDLength) = pure 2
    | otherwise = do
        result <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            bytes <- BS.packCStringLen
                (castPtr turnID, fromIntegral turnIDLength)
            case TextEncoding.decodeUtf8' bytes of
                Left _ -> pure False
                Right turnIDText
                    | Text.null turnIDText -> pure False
                    | otherwise -> do
                        atomically $ discardStagedTurnById
                            turnIDText
                            engine.engineStagedImages
                            engine.engineStagedTurnOptions
                        pure True
        pure $ case result of
            Left _ -> 3
            Right False -> 2
            Right True -> 0

maxNativeTurnIDBytes :: Integer
maxNativeTurnIDBytes = 1_024

validNativeTurnIDLength :: CSize -> Bool
validNativeTurnIDLength length =
    let integerLength = toInteger length
    in integerLength > 0
        && integerLength <= toInteger (maxBound :: Int)
        && integerLength <= maxNativeTurnIDBytes

ha_engine_set_interaction_callback
    :: Ptr () -> FunPtr InteractionCallback -> Ptr () -> IO CInt
ha_engine_set_interaction_callback pointer callback callbackContext
    | pointer == nullPtr = pure 1
    | otherwise = do
        result <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            withMVar
                engine.engineInteractions.interactionCallbackLock
                \_ -> atomically do
                    writeTVar
                        engine.engineInteractions.interactionCallbackTarget
                        (if callback == nullFunPtr
                            then Nothing
                            else Just InteractionCallbackTarget
                                { interactionTargetCallback = callback
                                , interactionTargetContext = callbackContext
                                })
                    cancelPendingInteractions
                        engine.engineInteractions.interactionPending
        pure $ either (const 3) (const 0) result

ha_engine_resolve_interaction
    :: Ptr () -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> CInt -> Ptr Word8 -> CSize -> IO CInt
ha_engine_resolve_interaction
        pointer
        turnID
        (CSize turnIDLength)
        interactionID
        (CSize interactionIDLength)
        (CInt selectedIndex)
        customText
        (CSize customTextLength)
    | pointer == nullPtr = pure 1
    | turnID == nullPtr || turnIDLength == 0 = pure 2
    | interactionID == nullPtr || interactionIDLength == 0 = pure 2
    | customText == nullPtr && customTextLength > 0 = pure 2
    | otherwise = do
        let selectedIndexValue = fromIntegral selectedIndex :: Int
        result <- tryAny do
            let stable = castPtrToStablePtr pointer :: StablePtr Engine
            engine <- deRefStablePtr stable
            let pendingRef =
                    engine.engineInteractions.interactionPending
            turnBytes <- BS.packCStringLen
                (castPtr turnID, fromIntegral turnIDLength)
            interactionBytes <- BS.packCStringLen
                (castPtr interactionID, fromIntegral interactionIDLength)
            customBytes <-
                if customTextLength == 0
                    then pure (Right Nothing)
                    else fmap (fmap Just . TextEncoding.decodeUtf8')
                        (BS.packCStringLen
                            (castPtr customText, fromIntegral customTextLength))
            case
                ( TextEncoding.decodeUtf8' turnBytes
                , TextEncoding.decodeUtf8' interactionBytes
                , customBytes
                )
              of
                (Right turnIDText, Right interactionIDText, Right custom) ->
                    fmap (\published -> if published then 0 else 4) $
                        atomically $
                            resolvePendingInteraction
                                pendingRef
                                (turnIDText, interactionIDText)
                                NativeInteractionResolution
                                    { interactionSelectedIndex =
                                        selectedIndexValue
                                    , interactionCustomText = custom
                                    }
                _ -> pure 2
        pure $ case result of
            Left _ -> 3
            Right status -> status

interactionModeFromCode :: CInt -> Maybe NativeInteractionMode
interactionModeFromCode = \case
    0 -> Just NativeAsk
    1 -> Just NativePlan
    2 -> Just NativeYolo
    _ -> Nothing

shellModeFromCode :: CInt -> Maybe NativeShellMode
shellModeFromCode = \case
    0 -> Just NativeShellNone
    1 -> Just NativeShellBash
    2 -> Just NativeShellGhci
    3 -> Just NativeShellBoth
    _ -> Nothing
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
    -> TVar (Map Text NativeTurnOptions)
    -> InteractionRuntime
    -> IO ()
workerLifecycle
        callback context config root commands stagedImages stagedTurnOptions
        interactions = do
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
        stagedTurnOptions
        interactions
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
    -> TVar (Map Text NativeTurnOptions)
    -> InteractionRuntime
    -> IO ()
idleLoop
        callback context config store root processRuntime commands stagedImages
        stagedTurnOptions interactions =
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
                        atomically $ discardStagedTurn
                            request.requestId
                            request.requestParams
                            stagedImages
                            stagedTurnOptions
                        sendEvent callback context
                            (failureEvent request.requestId err)
                        continue
                    Right start -> do
                        (images, turnOptions) <- atomically $ do
                            staged <- readTVar stagedImages
                            writeTVar stagedImages
                                (Map.delete start.turnStartId staged)
                            options <- readTVar stagedTurnOptions
                            writeTVar stagedTurnOptions
                                (Map.delete start.turnStartId options)
                            pure
                                ( Map.findWithDefault
                                    []
                                    start.turnStartId
                                    staged
                                , Map.findWithDefault
                                    defaultNativeTurnOptions
                                    start.turnStartId
                                    options
                                )
                        control <-
                            newTurnControl start.turnStartId interactions
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
                                images
                                turnOptions
                                interactions)
                            \running ->
                                activeLoop
                                    callback
                                    context
                                    config
                                    store
                                    root
                                    commands
                                    stagedImages
                                    stagedTurnOptions
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
            stagedTurnOptions
            interactions

activeLoop
    :: FunPtr EventCallback
    -> Ptr ()
    -> ManagedPostgresConfig
    -> MVar (Maybe Store)
    -> OsPath
    -> TQueue EngineCommand
    -> TVar (Map Text [ImageAttachment])
    -> TVar (Map Text NativeTurnOptions)
    -> TurnControl
    -> Async TurnOutcome
    -> IO ActiveExit
activeLoop
        callback context config store root commands stagedImages
        stagedTurnOptions control running =
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
                callback context config store root commands stagedImages
                stagedTurnOptions control running
        Left (EngineSessionMutation mutation resultCallback resultContext) -> do
            runSessionMutation
                config store root mutation resultCallback resultContext
            activeLoop
                callback context config store root commands stagedImages
                stagedTurnOptions control running
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
              then do
                atomically $ discardStagedTurn
                    request.requestId
                    request.requestParams
                    stagedImages
                    stagedTurnOptions
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
                stagedImages
                stagedTurnOptions
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
    -> NativeTurnOptions
    -> InteractionRuntime
    -> IO TurnOutcome
runNativeTurn
        callback context processRuntime control start images turnOptions
        interactions = do
    sessionIdRef <- newIORef start.turnStartSessionId
    completedRef <- newIORef False
    usageRef <- newIORef emptyTokenUsage
    let hooks = NativeRunHooks
            { nativeOnLoopEvent = \event -> do
                case event of
                    TurnFinished output -> do
                        writeIORef completedRef True
                        modifyIORef' usageRef (<> output.tokenUsage)
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
            , nativePlanHooks =
                nativePlanModeHooks control interactions
            , nativeInteractionMode =
                turnOptions.nativeTurnInteractionMode
            , nativeShellMode = turnOptions.nativeTurnShellMode
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
    usage <- readIORef usageRef
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
        , turnOutcomeUsage = usage
        , turnOutcomeProviderCostUSD = Nothing
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

newTurnControl :: Text -> InteractionRuntime -> IO TurnControl
newTurnControl turnId interactions = do
    cancelAction <- newTVarIO (pure ())
    approvals <- newTVarIO Map.empty
    approvalCounter <- newTVarIO 0
    interactionCounter <- newTVarIO 0
    allowedTools <- newTVarIO Set.empty
    agentSnapshot <- newTVarIO (pure [])
    pure TurnControl
        { turnControlId = turnId
        , turnControlCancel = cancelAction
        , turnControlApprovals = approvals
        , turnControlApprovalCounter = approvalCounter
        , turnControlInteractionCounter = interactionCounter
        , turnControlAllowedTools = allowedTools
        , turnControlAgentSnapshot = agentSnapshot
        , turnControlInteractions = interactions
        }

nativePlanModeHooks
    :: TurnControl
    -> InteractionRuntime
    -> PlanModeHooks
nativePlanModeHooks control interactions = PlanModeHooks
    { planConfirmEnter = \reason ->
        requestNativeInteraction
            control interactions 1 reason
            [ "Enter plan mode"
            , "Stay in normal mode"
            ] >>= \case
                Just resolution ->
                    pure (resolution.interactionSelectedIndex == 0)
                Nothing -> pure False
    , planDecideExit = \planBody ->
        requestNativeInteraction
            control interactions 2 planBody
            [ "Approve and implement"
            , "Request changes"
            , "Cancel plan"
            ] >>= \case
                Just resolution ->
                    pure $ case resolution.interactionSelectedIndex of
                        0 -> PlanApprove
                        1 ->
                            PlanRequestChanges
                                (fromMaybe
                                    "(no notes)"
                                    (nonBlank
                                        resolution.interactionCustomText))
                        _ -> PlanCancel
                Nothing -> pure PlanCancel
    , planAskQuestion = \question options ->
        requestNativeInteraction
            control interactions 3 question options >>= \case
                Nothing -> pure Nothing
                Just resolution
                    | resolution.interactionSelectedIndex >= 0 ->
                        pure $
                            atMay
                                resolution.interactionSelectedIndex
                                options
                    | otherwise ->
                        pure (nonBlank resolution.interactionCustomText)
    }
  where
    nonBlank = (>>= \text ->
        let stripped = Text.strip text
        in if Text.null stripped then Nothing else Just stripped)

requestNativeInteraction
    :: TurnControl
    -> InteractionRuntime
    -> CInt
    -> Text
    -> [Text]
    -> IO (Maybe NativeInteractionResolution)
requestNativeInteraction control interactions kind prompt options = do
    waiter <- newEmptyTMVarIO
    registration <-
        withMVar interactions.interactionCallbackLock \_ -> do
            registered <- atomically do
                target <- readTVar interactions.interactionCallbackTarget
                case target of
                    Nothing -> pure Nothing
                    Just callbackTarget -> do
                        interactionID <- register waiter
                        pure (Just (callbackTarget, interactionID))
            forM_ registered \(callbackTarget, interactionID) ->
                sendNativeInteraction
                    callbackTarget
                    control.turnControlId
                    interactionID
                    kind
                    prompt
                    options
                    `onException`
                        atomically
                            (modifyTVar'
                                interactions.interactionPending
                                (Map.delete
                                    (control.turnControlId, interactionID)))
            pure registered
    case registration of
        Nothing -> pure Nothing
        Just (_, interactionID) -> do
            let cleanup =
                    atomically $ modifyTVar'
                        interactions.interactionPending
                        (Map.delete
                            (control.turnControlId, interactionID))
            (Just <$> atomically (takeTMVar waiter))
                `finally` cleanup
  where
    register waiter = do
        current <- readTVar control.turnControlInteractionCounter
        let next = current + 1
            interactionID =
                control.turnControlId
                    <> "-interaction-"
                    <> Text.pack (show next)
        writeTVar control.turnControlInteractionCounter next
        modifyTVar'
            interactions.interactionPending
            (Map.insert
                (control.turnControlId, interactionID)
                PendingInteraction
                    { pendingInteractionOptionCount = length options
                    , pendingInteractionWaiter = waiter
                    })
        pure interactionID

sendNativeInteraction
    :: InteractionCallbackTarget
    -> Text
    -> Text
    -> CInt
    -> Text
    -> [Text]
    -> IO ()
sendNativeInteraction target turnID interactionID kind prompt options =
    withTextBytes turnID \turnPointer turnLength ->
    withTextBytes interactionID
        \interactionPointer interactionLength ->
    withTextBytes prompt \promptPointer promptLength ->
    withInteractionOptions options \optionPointer optionCount ->
        invokeInteractionCallback
            target.interactionTargetCallback
            target.interactionTargetContext
            turnPointer
            turnLength
            interactionPointer
            interactionLength
            kind
            promptPointer
            promptLength
            optionPointer
            optionCount

withInteractionOptions
    :: [Text]
    -> (Ptr CInteractionOption -> CSize -> IO a)
    -> IO a
withInteractionOptions [] action = action nullPtr 0
withInteractionOptions options action =
    withEncodedOptions options \encoded ->
        allocaArray (length encoded) \pointer -> do
            forM_ (zip [0..] encoded) \(index, (label, labelLength)) ->
                pokeElemOff pointer index CInteractionOption
                    { cInteractionOptionLabel = label
                    , cInteractionOptionLabelLength = labelLength
                    }
            action pointer (fromIntegral (length encoded))

withEncodedOptions
    :: [Text]
    -> ([(Ptr Word8, CSize)] -> IO a)
    -> IO a
withEncodedOptions [] action = action []
withEncodedOptions (option : rest) action =
    withTextBytes option \pointer length ->
        withEncodedOptions rest
            (action . ((pointer, length) :))

atMay :: Int -> [a] -> Maybe a
atMay index values
    | index < 0 = Nothing
    | otherwise = case drop index values of
        value : _ -> Just value
        [] -> Nothing

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
    interactionWaiters <- atomically do
        current <- readTVar
            control.turnControlInteractions.interactionPending
        let (owned, remaining) =
                Map.partitionWithKey
                    (\(turnID, _) _ ->
                        turnID == control.turnControlId)
                    current
        writeTVar
            control.turnControlInteractions.interactionPending
            remaining
        pure (map (.pendingInteractionWaiter) (Map.elems owned))
    atomically $
        forM_ interactionWaiters \waiter ->
            void $ tryPutTMVar waiter cancelledInteractionResolution

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
    Right outcome -> do
        forM_
            (encodeNativeUsageEvent
                True
                turnId
                outcome.turnOutcomeUsage
                outcome.turnOutcomeProviderCostUSD)
            (sendBinaryEvent callback context)
        case outcome.turnOutcomeError of
            Just err ->
                sendEvent callback context (turnFailedEvent turnId err)
            Nothing ->
                sendEvent callback context $
                    Aeson.object
                        [ "event" Aeson..= ("turn.completed" :: Text)
                        , "turnId" Aeson..= turnId
                        , "sessionId" Aeson..=
                            outcome.turnOutcomeSessionId
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
