-- | Multi-account OAuth pool for the ChatGPT Codex Responses API.
--
-- The pool manages a list of 'AuthState' values (one per ChatGPT account),
-- dispenses access tokens in round-robin order, tracks per-account cooldowns
-- after rate-limit or auth-broken signals, and triggers a user-supplied
-- refresh callback when an access token's JWT exp approaches.
--
-- The library is persistence-agnostic: it never touches a database or a
-- filesystem. If you want tokens to survive restarts, load them before
-- calling 'newPool' and wrap 'refreshAccessTokenHTTP' with your own lock +
-- persist logic. See the README for an IHP + Postgres example.
module Agent.OpenAI.Auth
    ( -- * Pool
      Pool
    , newPool
    , newDiscoveringPool
    , newUnavailableDiscoveringPool
    , getAccessToken

      -- * Cooldown management
    , reportRateLimit
    , reportRateLimitWithReason
    , reportAuthBroken
    , reportAuthBrokenWithReason
    , refreshAfterAuthFailure
    , recoverAfterAuthFailure
    , authFailureRetrySeconds

      -- * Inspection and manual refresh
    , allAccountIds
    , readAccountState
    , AccountSnapshot(..)
    , snapshotAccounts
    , getAccessTokenForAccount
    , discoverAccounts
    , forceRefresh

      -- * State
    , AuthState(..)

      -- * HTTP refresh (default callback for 'newPool')
    , refreshAccessTokenHTTP

      -- * JWT helpers
    , parseJwtExp
    , needsRefresh
    , deriveAccountId
    , deriveEmail
    ) where

import Agent.Error
    ( ApiError(..)
    , CredentialExhaustionReason(..)
    , ErrorType(..)
    , credentialExhaustionReasonFromApiError
    , credentialsExhaustedWithReasons
    )
import Agent.OpenAI.Auth.JWT
    ( deriveAccountId
    , deriveEmail
    , needsRefresh
    , parseJwtExp
    )
import Agent.OpenAI.Auth.Refresh (refreshAccessTokenHTTP)
import Agent.OpenAI.Auth.Types (AuthState(..))
import Control.Concurrent.MVar
    ( MVar
    , newEmptyMVar
    , newMVar
    , putMVar
    , readMVar
    , withMVar
    )
import qualified Control.Exception.Safe as Safe
import Control.Monad (forM, when)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    )
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time.Clock
    ( UTCTime
    , addUTCTime
    , diffUTCTime
    , getCurrentTime
    , utctDayTime
    )

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

data AccountCooldown = AccountCooldown
    { cooldownUntil :: !UTCTime
    , cooldownReason :: !CredentialExhaustionReason
    }

data AccountCooldowns = AccountCooldowns
    { cooldownRateLimit :: !(Maybe AccountCooldown)
    , cooldownAuthBroken :: !(Maybe AccountCooldown)
    }

-- | All mutable state for one account. Auth, cooldowns, and recovery
-- throttling are committed with one atomic transition, so snapshots cannot
-- pair fields from different transitions.
data AccountState = AccountState
    { accountAuth :: !AuthState
    , accountCooldowns :: !AccountCooldowns
    , accountLastAuthRecovery :: !(Maybe UTCTime)
    }

data AccountEntry = AccountEntry
    { entryAccountId :: !Text
    , entryState :: !(IORef AccountState)
    , entryRefreshLock :: !(MVar ())
    }

-- | Optional source of newly available accounts. The callback receives every
-- account id already known to this process and returns additional accounts
-- that have become available since the pool was built. It is consulted only
-- after all currently known accounts are cooling down.
type AccountDiscovery = [Text] -> IO (Either ApiError [AuthState])

data PoolState = PoolState
    { stateEntries :: !(Seq AccountEntry)
    , stateEmptyExhaustion
        :: !(Maybe (UTCTime, [CredentialExhaustionReason]))
    , stateCounter :: !Int
    , stateDiscovery :: !(Maybe (MVar Bool))
    }

-- | Opaque pool of one or more ChatGPT accounts. Share one 'Pool' per process.
--
-- Pool transitions only hold the atomic state cell long enough to select,
-- append, or claim work. Refresh and discovery callbacks run after ownership
-- has been claimed, never while a global pool lock is held.
data Pool = Pool
    { poolState :: !(IORef PoolState)
    , poolRefresh :: !(AuthState -> IO (Either ApiError AuthState))
    , poolDiscovery :: !(Maybe AccountDiscovery)
    }

data DiscoveryClaim
    = DiscoveryAlreadyAdded
    | DiscoveryWait !(MVar Bool)
    | DiscoveryOwner ![Text] !(MVar Bool)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

rateLimitCooldownSeconds :: Int
rateLimitCooldownSeconds = 60

-- | Retry window when an account returns 401/403 from Agent.OpenAI.
-- Authentication recovery already forces an immediate refresh; if that still
-- fails, retry soon instead of black-holing the only configured account.
authFailureRetrySeconds :: Int
authFailureRetrySeconds = 60

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | Build a pool from a non-empty list of 'AuthState' values and a refresh
-- callback. The callback is invoked when an access token needs rotating.
--
-- Throws @error@ if @initial@ is empty.
newPool
    :: [AuthState]
    -> (AuthState -> IO (Either ApiError AuthState))
    -> IO Pool
newPool initial refresh = newPoolWithDiscovery initial refresh Nothing

newDiscoveringPool
    :: [AuthState]
    -> (AuthState -> IO (Either ApiError AuthState))
    -> AccountDiscovery
    -> IO Pool
newDiscoveringPool initial refresh discover =
    newPoolWithDiscovery initial refresh (Just discover)

newPoolWithDiscovery
    :: [AuthState]
    -> (AuthState -> IO (Either ApiError AuthState))
    -> Maybe AccountDiscovery
    -> IO Pool
newPoolWithDiscovery initial refresh discovery = do
    when (null initial) $
        error "Agent.OpenAI.Auth.newPool: called with empty account list"
    buildPool initial Nothing refresh discovery

-- | Build a broker-backed pool when no account is currently available but the
-- broker supplied the earliest reset.
newUnavailableDiscoveringPool
    :: UTCTime
    -> (AuthState -> IO (Either ApiError AuthState))
    -> AccountDiscovery
    -> IO Pool
newUnavailableDiscoveringPool retryAt refresh discover =
    buildPool [] (Just retryAt) refresh (Just discover)

buildPool
    :: [AuthState]
    -> Maybe UTCTime
    -> (AuthState -> IO (Either ApiError AuthState))
    -> Maybe AccountDiscovery
    -> IO Pool
buildPool initial emptyRetryAt refresh discovery = do
    entries <- Seq.fromList <$> mapM mkEntry initial
    now <- getCurrentTime
    let offset = floor (toRational (utctDayTime now) * 1000) :: Int
    state <- newIORef PoolState
        { stateEntries = entries
        , stateEmptyExhaustion = (, []) <$> emptyRetryAt
        , stateCounter = offset `mod` max 1 (Seq.length entries)
        , stateDiscovery = Nothing
        }
    pure Pool
        { poolState = state
        , poolRefresh = refresh
        , poolDiscovery = discovery
        }

mkEntry :: AuthState -> IO AccountEntry
mkEntry auth = do
    state <- newIORef AccountState
        { accountAuth = auth
        , accountCooldowns = emptyCooldowns
        , accountLastAuthRecovery = Nothing
        }
    refreshLock <- newMVar ()
    pure AccountEntry
        { entryAccountId = auth.accountId
        , entryState = state
        , entryRefreshLock = refreshLock
        }

--------------------------------------------------------------------------------
-- Access-token dispensing
--------------------------------------------------------------------------------

getAccessToken :: Pool -> IO (Either ApiError (Text, Text))
getAccessToken pool = getAccessTokenWithDiscovery pool True

getAccessTokenWithDiscovery
    :: Pool
    -> Bool
    -> IO (Either ApiError (Text, Text))
getAccessTokenWithDiscovery pool allowDiscovery = do
    poolState <- readIORef pool.poolState
    let entries = poolState.stateEntries
        accountIdsAtCheckout = map (.entryAccountId) (toList entries)
    result <- go (Seq.length entries)
    case result of
        Left exhausted@CredentialsExhausted
            { retryAt = previousRetryAt
            , exhaustionReasons = previousReasons
            }
            | allowDiscovery ->
                discoverAdditionalAccounts pool accountIdsAtCheckout >>= \case
                    True -> getAccessTokenWithDiscovery pool False
                    False -> do
                        current <- readIORef pool.poolState
                        if Seq.null current.stateEntries
                            then
                                let (retryAt, reasons) =
                                        fromMaybe
                                            (previousRetryAt, previousReasons)
                                            current.stateEmptyExhaustion
                                in pure $ Left $
                                    credentialsExhaustedWithReasons
                                        retryAt reasons
                            else pure (Left exhausted)
        _ -> pure result
  where
    go attemptsLeft
        | attemptsLeft <= 0 = do
            pickAccount pool >>= \case
                Left (earliest, reasons) ->
                    pure $ Left $
                        credentialsExhaustedWithReasons earliest reasons
                Right _ -> pure $ Left $ ConnectionError
                    "Agent.OpenAI.Auth.getAccessToken: failover budget exhausted"
        | otherwise =
            pickAccount pool >>= \case
                Left (earliest, reasons) ->
                    pure $ Left $
                        credentialsExhaustedWithReasons earliest reasons
                Right entry ->
                    currentAuthState pool entry >>= \case
                        Right state ->
                            pure $ Right (state.accessToken, state.accountId)
                        Left err
                            | CredentialsExhausted{} <- err ->
                                go (attemptsLeft - 1)
                            | isAuthError err -> do
                                reportAuthBrokenWithReason
                                    pool
                                    entry.entryAccountId
                                    (authReasonFromApiError err)
                                go (attemptsLeft - 1)
                            | otherwise -> pure (Left err)

currentAuthState
    :: Pool
    -> AccountEntry
    -> IO (Either ApiError AuthState)
currentAuthState pool entry =
    withMVar entry.entryRefreshLock \_ -> do
        now <- getCurrentTime
        state <- atomicModifyAccount entry \current ->
            let updated = expireCooldowns now current
            in (updated, updated)
        case effectiveCooldown state.accountCooldowns of
            Just (until_, reasons) ->
                pure $ Left $
                    credentialsExhaustedWithReasons until_ reasons
            Nothing ->
                if needsRefresh state.accountAuth now
                    then refreshEntry pool entry state.accountAuth
                    else pure (Right state.accountAuth)

-- | Invoke the refresh callback and commit its result. The caller must hold
-- 'entryRefreshLock'. The callback runs without holding the account-state or
-- pool-state cells.
refreshEntry
    :: Pool
    -> AccountEntry
    -> AuthState
    -> IO (Either ApiError AuthState)
refreshEntry = refreshEntryWithCooldownTransition id

refreshEntryAfterAuthFailure
    :: Pool
    -> AccountEntry
    -> AuthState
    -> IO (Either ApiError AuthState)
refreshEntryAfterAuthFailure =
    refreshEntryWithCooldownTransition \cooldowns ->
        cooldowns { cooldownAuthBroken = Nothing }

refreshEntryWithCooldownTransition
    :: (AccountCooldowns -> AccountCooldowns)
    -> Pool
    -> AccountEntry
    -> AuthState
    -> IO (Either ApiError AuthState)
refreshEntryWithCooldownTransition transition pool entry stale =
    pool.poolRefresh stale >>= \case
        Right refreshed
            | refreshed.accountId /= entry.entryAccountId ->
                pure $ Left $ CredentialError
                    "OpenAI refresh changed account identity"
            | otherwise -> do
                atomicModifyAccount entry \current ->
                    ( current
                        { accountAuth = refreshed
                        , accountCooldowns =
                            transition current.accountCooldowns
                        }
                    , ()
                    )
                pure (Right refreshed)
        Left err -> pure (Left err)

--------------------------------------------------------------------------------
-- Discovery
--------------------------------------------------------------------------------

discoverAdditionalAccounts :: Pool -> [Text] -> IO Bool
discoverAdditionalAccounts pool accountIdsAtCheckout =
    case pool.poolDiscovery of
        Nothing -> pure False
        Just discover -> Safe.mask \restore -> do
            promise <- newEmptyMVar
            claim <- atomicModifyIORef' pool.poolState $
                claimDiscovery accountIdsAtCheckout promise
            case claim of
                DiscoveryAlreadyAdded -> pure True
                DiscoveryWait active -> restore (readMVar active)
                DiscoveryOwner knownAccountIds active -> do
                    prepared <- Safe.tryAny $ restore do
                        discovered <- discover knownAccountIds
                        candidates <- case discovered of
                            Right states -> mapM mkEntry states
                            Left _ -> pure []
                        pure (discovered, candidates)
                    let (discoveryResult, candidates) =
                            case prepared of
                                Left _ -> (Nothing, [])
                                Right (discovered, entries) ->
                                    (Just discovered, entries)
                    added <- atomicModifyIORef' pool.poolState $
                        finishDiscovery discoveryResult candidates
                    putMVar active added
                    case prepared of
                        Left exception -> Safe.throwIO exception
                        Right _ -> pure added

claimDiscovery
    :: [Text]
    -> MVar Bool
    -> PoolState
    -> (PoolState, DiscoveryClaim)
claimDiscovery accountIdsAtCheckout promise current =
    let knownAccountIds = map (.entryAccountId) (toList current.stateEntries)
        checkout = Set.fromList accountIdsAtCheckout
    in if any (`Set.notMember` checkout) knownAccountIds
        then (current, DiscoveryAlreadyAdded)
        else case current.stateDiscovery of
            Just active -> (current, DiscoveryWait active)
            Nothing ->
                ( current { stateDiscovery = Just promise }
                , DiscoveryOwner knownAccountIds promise
                )

finishDiscovery
    :: Maybe (Either ApiError [AuthState])
    -> [AccountEntry]
    -> PoolState
    -> (PoolState, Bool)
finishDiscovery outcome candidates current =
    let withoutOwner = current { stateDiscovery = Nothing }
    in case outcome of
        Just (Left CredentialsExhausted{retryAt, exhaustionReasons}) ->
            ( withoutOwner
                { stateEmptyExhaustion =
                    Just (retryAt, exhaustionReasons)
                }
            , False
            )
        Just (Right _) ->
            appendEntries candidates withoutOwner
        _ ->
            (withoutOwner, False)

appendEntries :: [AccountEntry] -> PoolState -> (PoolState, Bool)
appendEntries candidates current =
    let known = Set.fromList (map (.entryAccountId) (toList current.stateEntries))
        (newEntries, _) = foldl addCandidate ([], known) candidates
        added = not (null newEntries)
    in
        ( current
            { stateEntries =
                current.stateEntries <> Seq.fromList (reverse newEntries)
            , stateEmptyExhaustion =
                if added then Nothing else current.stateEmptyExhaustion
            }
        , added
        )
  where
    addCandidate (newEntries, known) entry
        | entry.entryAccountId `Set.member` known = (newEntries, known)
        | otherwise =
            ( entry : newEntries
            , Set.insert entry.entryAccountId known
            )

discoverAccounts :: Pool -> IO Bool
discoverAccounts pool = do
    knownAccountIds <- allAccountIds pool
    discoverAdditionalAccounts pool knownAccountIds

--------------------------------------------------------------------------------
-- Selection and cooldowns
--------------------------------------------------------------------------------

pickAccount
    :: Pool
    -> IO
        (Either
            (UTCTime, [CredentialExhaustionReason])
            AccountEntry)
pickAccount pool = do
    now <- getCurrentTime
    (entries, startIdx, emptyExhaustion) <-
        atomicModifyIORef' pool.poolState selectStart
    if Seq.null entries
        then pure (Left (fromMaybe (now, []) emptyExhaustion))
        else do
            available <- tryFrom entries startIdx now 0
            case available of
                Just entry -> pure (Right entry)
                Nothing -> do
                    cooldowns <- fmap catMaybes $ forM (toList entries) \entry ->
                        effectiveCooldown . (.accountCooldowns)
                            <$> readIORef entry.entryState
                    case cooldowns of
                        [] -> pure (Left (now, []))
                        (first : rest) ->
                            let active = first : rest
                            in pure $ Left
                                ( minimum (map fst active)
                                , nubOrd (concatMap snd active)
                                )
  where
    selectStart current
        | Seq.null current.stateEntries =
            ( current
            , (Seq.empty, 0, current.stateEmptyExhaustion)
            )
        | otherwise =
            let entries = current.stateEntries
                n = Seq.length entries
                startIdx = current.stateCounter `mod` n
                nextCounter = (current.stateCounter + 1) `mod` n
            in
                ( current { stateCounter = nextCounter }
                , (entries, startIdx, current.stateEmptyExhaustion)
                )

    tryFrom entries startIdx now offset
        | offset >= Seq.length entries = pure Nothing
        | otherwise = do
            let n = Seq.length entries
                entry =
                    Seq.index entries ((startIdx + offset) `mod` n)
            cooldown <- atomicModifyAccount entry \current ->
                let updated = expireCooldowns now current
                in (updated, effectiveCooldown updated.accountCooldowns)
            case cooldown of
                Nothing -> pure (Just entry)
                Just _ -> tryFrom entries startIdx now (offset + 1)

reportRateLimit :: Pool -> Text -> Maybe Int -> IO ()
reportRateLimit pool limitedAccountId retryAfter =
    reportRateLimitWithReason
        pool
        limitedAccountId
        retryAfter
        ExhaustedByRateLimit
            { exhaustionErrorType = Nothing
            , exhaustionStatusCode = Nothing
            , exhaustionRetryAfter = retryAfter
            }

reportRateLimitWithReason
    :: Pool
    -> Text
    -> Maybe Int
    -> CredentialExhaustionReason
    -> IO ()
reportRateLimitWithReason pool limitedAccountId retryAfter reason = do
    now <- getCurrentTime
    let seconds = fromMaybe rateLimitCooldownSeconds retryAfter
        until_ = addUTCTime (fromIntegral seconds) now
    updateAccountLocked pool limitedAccountId \current ->
        current
            { accountCooldowns = current.accountCooldowns
                { cooldownRateLimit =
                    extendCooldown until_ reason
                        current.accountCooldowns.cooldownRateLimit
                }
            }

reportAuthBroken :: Pool -> Text -> IO ()
reportAuthBroken pool brokenAccountId =
    reportAuthBrokenWithReason pool brokenAccountId genericAuthReason

reportAuthBrokenWithReason
    :: Pool
    -> Text
    -> CredentialExhaustionReason
    -> IO ()
reportAuthBrokenWithReason pool brokenAccountId reason = do
    now <- getCurrentTime
    updateAccountLocked pool brokenAccountId $
        setAuthBrokenCooldownTransition now reason

setAuthBrokenCooldownAt
    :: Pool
    -> Text
    -> UTCTime
    -> CredentialExhaustionReason
    -> IO ()
setAuthBrokenCooldownAt pool accountId now reason =
    updateAccount pool accountId
        (setAuthBrokenCooldownTransition now reason)

setAuthBrokenCooldownTransition
    :: UTCTime
    -> CredentialExhaustionReason
    -> AccountState
    -> AccountState
setAuthBrokenCooldownTransition now reason current =
    let until_ =
            addUTCTime (fromIntegral authFailureRetrySeconds) now
    in current
        { accountCooldowns = current.accountCooldowns
            { cooldownAuthBroken =
                extendCooldown until_ reason
                    current.accountCooldowns.cooldownAuthBroken
            }
        }

-- | Force-rotate an access token after an authentication failure.
refreshAfterAuthFailure :: Pool -> Text -> IO (Either ApiError AuthState)
refreshAfterAuthFailure pool rejectedAccountId =
    findEntry pool rejectedAccountId >>= \case
        Nothing ->
            pure $ Left $ ProviderError AuthenticationError
                ("Agent.OpenAI.Auth.refreshAfterAuthFailure: unknown accountId "
                    <> rejectedAccountId)
                Nothing
        Just entry ->
            withMVar entry.entryRefreshLock \_ -> do
                stale <- (.accountAuth) <$> readIORef entry.entryState
                refreshEntryAfterAuthFailure pool entry stale >>= \case
                    Right refreshed ->
                        pure (Right refreshed)
                    Left err -> do
                        now <- getCurrentTime
                        setAuthBrokenCooldownAt
                            pool
                            rejectedAccountId
                            now
                            (authReasonFromApiError err)
                        pure (Left err)

-- | Recover a specifically rejected token. Recovery ownership and throttling
-- live in the pool account, so multiple 'TokenProvider' values sharing the
-- same pool cannot each refresh the same rejection.
--
-- If another caller already replaced the rejected token, return that
-- replacement. If the replacement itself is rejected inside the retry
-- window, cool the account down instead of rotating repeatedly.
recoverAfterAuthFailure
    :: Pool
    -> Text
    -> Text
    -> IO (Either ApiError AuthState)
recoverAfterAuthFailure pool rejectedAccountId rejectedAccessToken =
    findEntry pool rejectedAccountId >>= \case
        Nothing ->
            pure $ Left $ ProviderError AuthenticationError
                ("Agent.OpenAI.Auth.recoverAfterAuthFailure: unknown accountId "
                    <> rejectedAccountId)
                Nothing
        Just entry ->
            withMVar entry.entryRefreshLock \_ -> do
                now <- getCurrentTime
                decision <- atomicModifyAccount entry $
                    claimAuthRecoveryAt now rejectedAccessToken
                case decision of
                    AuthAlreadyRecovered current ->
                        pure (Right current)
                    AuthRecoveryCoolingDown retryAt -> do
                        setAuthBrokenCooldownAt
                            pool rejectedAccountId now genericAuthReason
                        pure $ Left $
                            credentialsExhaustedWithReasons
                                retryAt [genericAuthReason]
                    AuthRecoveryOwner stale ->
                        refreshEntryAfterAuthFailure
                            pool entry stale >>= \case
                            Right refreshed ->
                                pure (Right refreshed)
                            Left err -> do
                                setAuthBrokenCooldownAt
                                    pool
                                    rejectedAccountId
                                    now
                                    (authReasonFromApiError err)
                                pure (Left err)

data AuthRecoveryDecision
    = AuthAlreadyRecovered !AuthState
    | AuthRecoveryCoolingDown !UTCTime
    | AuthRecoveryOwner !AuthState

claimAuthRecoveryAt
    :: UTCTime
    -> Text
    -> AccountState
    -> (AccountState, AuthRecoveryDecision)
claimAuthRecoveryAt now rejectedAccessToken current
    | current.accountAuth.accessToken /= rejectedAccessToken =
        (current, AuthAlreadyRecovered current.accountAuth)
    | Just attemptedAt <- current.accountLastAuthRecovery
    , diffUTCTime now attemptedAt
        < fromIntegral authFailureRetrySeconds =
        let retryAt =
                addUTCTime
                    (fromIntegral authFailureRetrySeconds)
                    attemptedAt
        in (current, AuthRecoveryCoolingDown retryAt)
    | otherwise =
        ( current { accountLastAuthRecovery = Just now }
        , AuthRecoveryOwner current.accountAuth
        )

--------------------------------------------------------------------------------
-- Inspection and manual refresh
--------------------------------------------------------------------------------

allAccountIds :: Pool -> IO [Text]
allAccountIds pool = do
    poolState <- readIORef pool.poolState
    pure (map (.entryAccountId) (toList poolState.stateEntries))

readAccountState :: Pool -> Text -> IO (Maybe AuthState)
readAccountState pool targetAccountId =
    findEntry pool targetAccountId >>= traverse
        (fmap (.accountAuth) . readIORef . (.entryState))

data AccountSnapshot = AccountSnapshot
    { snapshotAuth :: !AuthState
    , snapshotCooldownUntil :: !(Maybe UTCTime)
    , snapshotCooldownReasons :: ![CredentialExhaustionReason]
    }
    deriving (Show)

snapshotAccounts :: Pool -> IO [AccountSnapshot]
snapshotAccounts pool = do
    poolState <- readIORef pool.poolState
    forM (toList poolState.stateEntries) \entry -> do
        state <- readIORef entry.entryState
        pure AccountSnapshot
            { snapshotAuth = state.accountAuth
            , snapshotCooldownUntil =
                fst <$> effectiveCooldown state.accountCooldowns
            , snapshotCooldownReasons =
                maybe [] snd (effectiveCooldown state.accountCooldowns)
            }

getAccessTokenForAccount
    :: Pool
    -> Text
    -> IO (Either ApiError (Text, Text))
getAccessTokenForAccount pool targetAccountId =
    findEntry pool targetAccountId >>= \case
        Nothing ->
            pure $ Left $ CredentialError
                ("unknown OpenAI account " <> targetAccountId)
        Just entry ->
            withMVar entry.entryRefreshLock \_ -> do
                now <- getCurrentTime
                cooldown <- atomicModifyAccount entry \current ->
                    let updated = expireCooldowns now current
                    in
                        ( updated
                        , effectiveCooldown updated.accountCooldowns
                        )
                case cooldown of
                    Just (until_, reasons) ->
                        pure $ Left $
                            credentialsExhaustedWithReasons until_ reasons
                    Nothing -> do
                        state <- (.accountAuth) <$> readIORef entry.entryState
                        if needsRefresh state now
                            then fmap credentialPair
                                <$> refreshEntry pool entry state
                            else pure (Right (credentialPair state))
  where
    credentialPair state = (state.accessToken, state.accountId)

forceRefresh :: Pool -> Text -> IO (Either ApiError AuthState)
forceRefresh pool targetAccountId =
    findEntry pool targetAccountId >>= \case
        Nothing ->
            pure $ Left $ ProviderError AuthenticationError
                ("Agent.OpenAI.Auth.forceRefresh: unknown accountId "
                    <> targetAccountId)
                Nothing
        Just entry ->
            withMVar entry.entryRefreshLock \_ -> do
                state <- (.accountAuth) <$> readIORef entry.entryState
                refreshEntry pool entry state

findEntry :: Pool -> Text -> IO (Maybe AccountEntry)
findEntry pool targetAccountId = do
    poolState <- readIORef pool.poolState
    let entries = poolState.stateEntries
    pure $
        Seq.findIndexL
            ((== targetAccountId) . (.entryAccountId))
            entries
            >>= (`Seq.lookup` entries)

updateAccount
    :: Pool
    -> Text
    -> (AccountState -> AccountState)
    -> IO ()
updateAccount pool targetAccountId transition =
    findEntry pool targetAccountId >>= mapM_ \entry ->
        atomicModifyAccount entry \current ->
            (transition current, ())

updateAccountLocked
    :: Pool
    -> Text
    -> (AccountState -> AccountState)
    -> IO ()
updateAccountLocked pool targetAccountId transition =
    findEntry pool targetAccountId >>= mapM_ \entry ->
        withMVar entry.entryRefreshLock \_ ->
            atomicModifyAccount entry \current ->
                (transition current, ())

atomicModifyAccount
    :: AccountEntry
    -> (AccountState -> (AccountState, result))
    -> IO result
atomicModifyAccount entry =
    atomicModifyIORef' entry.entryState

emptyCooldowns :: AccountCooldowns
emptyCooldowns = AccountCooldowns
    { cooldownRateLimit = Nothing
    , cooldownAuthBroken = Nothing
    }

extendCooldown
    :: UTCTime
    -> CredentialExhaustionReason
    -> Maybe AccountCooldown
    -> Maybe AccountCooldown
extendCooldown until_ reason = \case
    Just current
        | current.cooldownUntil > until_ -> Just current
    _ ->
        Just AccountCooldown
            { cooldownUntil = until_
            , cooldownReason = reason
            }

expireCooldowns :: UTCTime -> AccountState -> AccountState
expireCooldowns now current =
    current
        { accountCooldowns = AccountCooldowns
            { cooldownRateLimit =
                keepFuture current.accountCooldowns.cooldownRateLimit
            , cooldownAuthBroken =
                keepFuture current.accountCooldowns.cooldownAuthBroken
            }
        }
  where
    keepFuture cooldown
        | maybe False ((> now) . (.cooldownUntil)) cooldown = cooldown
        | otherwise = Nothing

effectiveCooldown
    :: AccountCooldowns
    -> Maybe (UTCTime, [CredentialExhaustionReason])
effectiveCooldown cooldowns =
    case catMaybes
        [ cooldowns.cooldownRateLimit
        , cooldowns.cooldownAuthBroken
        ] of
        [] -> Nothing
        active ->
            Just
                ( maximum (map (.cooldownUntil) active)
                , nubOrd (map (.cooldownReason) active)
                )

genericAuthReason :: CredentialExhaustionReason
genericAuthReason = ExhaustedByAuthentication
    { exhaustionErrorType = Nothing
    , exhaustionStatusCode = Nothing
    }

authReasonFromApiError :: ApiError -> CredentialExhaustionReason
authReasonFromApiError err =
    fromMaybe genericAuthReason
        (credentialExhaustionReasonFromApiError err)

isAuthError :: ApiError -> Bool
isAuthError (HttpError 401 _) = True
isAuthError (HttpError 403 _) = True
isAuthError (ProviderError AuthenticationError _ _) = True
isAuthError CredentialError{} = True
isAuthError _ = False
