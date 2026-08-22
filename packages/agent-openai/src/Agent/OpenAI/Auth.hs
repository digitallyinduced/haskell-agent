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
    , reportAuthBroken
    , refreshAfterAuthFailure
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

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.OpenAI.Auth.JWT
    ( deriveAccountId
    , deriveEmail
    , needsRefresh
    , parseJwtExp
    )
import Agent.OpenAI.Auth.Refresh (refreshAccessTokenHTTP)
import Agent.OpenAI.Auth.Types (AuthState(..))
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad (forM, when)
import Data.IORef
import Data.List (find)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime, utctDayTime)

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

-- | Per-account mutable state in the pool.
data AccountEntry = AccountEntry
    { entryAccountId     :: !Text
    , entryAuthRef       :: !(IORef AuthState)
    , entryCooldownUntil :: !(IORef (Maybe UTCTime))
    , entryRefreshLock   :: !(MVar ())
    }

-- | Optional source of newly available accounts. The callback receives every
-- account id already known to this process and returns additional accounts
-- that have become available since the pool was built. It is consulted only
-- after all currently known accounts are cooling down.
type AccountDiscovery = [Text] -> IO (Either ApiError [AuthState])

-- | Opaque pool of one or more ChatGPT accounts. Share one 'Pool' per process.
--
-- The refresh callback is invoked whenever an access token is within
-- 'refreshMarginSeconds' of its JWT exp. Pass
-- @refreshAccessTokenHTTP oauthClientId@ for simple single-process setups, or
-- wrap it with your own
-- cross-process lock + persistence when running multiple workers against a
-- shared token store.
data Pool = Pool
    { poolEntries :: !(IORef [AccountEntry])
    , poolEmptyRetryAt :: !(IORef (Maybe UTCTime))
    , poolCounter :: !(IORef Int)
    , poolRefresh :: !(AuthState -> IO (Either ApiError AuthState))
    , poolDiscovery :: !(Maybe AccountDiscovery)
    , poolDiscoveryLock :: !(MVar ())
    }

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Refresh when the access-token JWT has less than this many seconds left.
-- ChatGPT Codex access tokens are long-lived (~9 days), so this is the only
-- trigger the pool uses — no periodic wall-clock refresh. A looser
-- interval-based refresh would just burn through refresh tokens
-- unnecessarily, and an exhausted refresh-token family can't be recovered
-- without re-running @npx \@openai/codex login@ interactively.
-- | How long to mark an account as unavailable after a rate-limit error
-- when the server does not provide @resets_in_seconds@.
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
-- callback. The callback is invoked when an access token needs rotating;
-- pass @refreshAccessTokenHTTP oauthClientId@ if you have no external
-- persistence.
--
-- Throws @error@ if @initial@ is empty.
newPool
    :: [AuthState]
    -> (AuthState -> IO (Either ApiError AuthState))
    -> IO Pool
newPool initial refresh = newPoolWithDiscovery initial refresh Nothing

-- | Build a pool that can discover accounts which become available after the
-- process starts. This is intended for central brokers: an account may be
-- cooling down when the service boots and become healthy later, while another
-- account already cached by the process can subsequently hit its own limit.
--
-- Discovery is deliberately additive. A freshly reported local cooldown is
-- never cleared just because the broker has not observed the upstream limit
-- yet; the callback is asked only for account ids not already in the pool.
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
-- broker supplied the earliest reset. The pool remains usable: checkouts
-- return 'CredentialsExhausted' with that timestamp and retry discovery on later
-- calls, so a long-lived worker need not be restarted when capacity returns.
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
    entries <- mapM mkEntry initial
    entriesRef <- newIORef entries
    emptyRetryAtRef <- newIORef emptyRetryAt
    -- Seed the round-robin counter at a time-based offset so multiple worker
    -- processes booting simultaneously don't all start on the same account.
    now <- getCurrentTime
    let offset = floor (toRational (utctDayTime now) * 1000) :: Int
    counter <- newIORef (offset `mod` max 1 (length entries))
    discoveryLock <- newMVar ()
    pure Pool
        { poolEntries = entriesRef
        , poolEmptyRetryAt = emptyRetryAtRef
        , poolCounter = counter
        , poolRefresh = refresh
        , poolDiscovery = discovery
        , poolDiscoveryLock = discoveryLock
        }

mkEntry :: AuthState -> IO AccountEntry
mkEntry state = do
    authRef <- newIORef state
    cooldownRef <- newIORef Nothing
    refreshLock <- newMVar ()
    pure AccountEntry
        { entryAccountId = state.accountId
        , entryAuthRef = authRef
        , entryCooldownUntil = cooldownRef
        , entryRefreshLock = refreshLock
        }

--------------------------------------------------------------------------------
-- Access-token dispensing
--------------------------------------------------------------------------------

-- | Get a fresh @(accessToken, accountId)@ pair, refreshing if needed.
--
-- Picks an account via round-robin, skipping any in cooldown. If every
-- account is cooling down, returns @Left (CredentialsExhausted earliest)@ so the
-- caller can reschedule to exactly the reset time instead of burning
-- requests on an account that will just rate-limit again.
--
-- Authentication failures while refreshing mark that selected account as
-- auth-broken and continue to another account in the same pool checkout. This
-- matters for WebSocket callers: once 'withCodexWs' receives only a bare
-- 'ApiError', the selected account id has otherwise been lost.
getAccessToken :: Pool -> IO (Either ApiError (Text, Text))
getAccessToken pool = getAccessTokenWithDiscovery pool True

getAccessTokenWithDiscovery
    :: Pool
    -> Bool
    -> IO (Either ApiError (Text, Text))
getAccessTokenWithDiscovery pool allowDiscovery = do
    entries <- readIORef pool.poolEntries
    let accountIdsAtCheckout = map (.entryAccountId) entries
    result <- go (length entries)
    case result of
        Left exhausted@CredentialsExhausted{retryAt = previousRetryAt}
            | allowDiscovery -> discoverAdditionalAccounts pool accountIdsAtCheckout >>= \case
                True -> getAccessTokenWithDiscovery pool False
                False -> do
                    currentEntries <- readIORef pool.poolEntries
                    if null currentEntries
                        then do
                            currentRetryAt <- readIORef pool.poolEmptyRetryAt
                            pure (Left (CredentialsExhausted (fromMaybe previousRetryAt currentRetryAt)))
                        else pure (Left exhausted)
        _ -> pure result
  where
    go attemptsLeft
        | attemptsLeft <= 0 = do
            picked <- pickAccount pool
            case picked of
                Left earliest -> pure $ Left (CredentialsExhausted earliest)
                Right _       -> pure $ Left (ConnectionError "Agent.OpenAI.Auth.getAccessToken: failover budget exhausted")
        | otherwise = do
            picked <- pickAccount pool
            case picked of
                Left earliest -> pure $ Left (CredentialsExhausted earliest)
                Right entry ->
                    currentAuthState pool entry >>= \case
                        Right state ->
                            pure $ Right (state.accessToken, state.accountId)
                        Left err
                            | isAuthError err -> do
                                reportAuthBroken pool entry.entryAccountId
                                go (attemptsLeft - 1)
                            | otherwise -> pure $ Left err

-- | Read a checkout state while serialized with every refresh for this
-- account. Always taking the lock prevents an in-flight forced refresh from
-- handing another caller the rejected token it is replacing. Re-checking
-- expiry after acquiring it makes concurrent expiry-driven checkouts share
-- the first successful refresh.
currentAuthState
    :: Pool
    -> AccountEntry
    -> IO (Either ApiError AuthState)
currentAuthState pool entry =
    withMVar entry.entryRefreshLock \_ -> do
        state <- readIORef entry.entryAuthRef
        now <- getCurrentTime
        if needsRefresh state now
            then refreshEntry pool entry state
            else pure (Right state)

-- | Invoke the refresh callback and commit its result. The caller must hold
-- 'entryRefreshLock'.
refreshEntry
    :: Pool
    -> AccountEntry
    -> AuthState
    -> IO (Either ApiError AuthState)
refreshEntry pool entry state =
    pool.poolRefresh state >>= \case
        Right new -> do
            writeIORef entry.entryAuthRef new
            pure (Right new)
        Left err -> pure (Left err)

-- | Ask the dynamic source only for accounts this process has never seen.
-- Broker outages do not replace a precise local 'CredentialsExhausted' reset time
-- with a generic transport error; the current job can still reschedule while
-- a later checkout tries discovery again.
discoverAdditionalAccounts :: Pool -> [Text] -> IO Bool
discoverAdditionalAccounts pool accountIdsAtCheckout = case pool.poolDiscovery of
    Nothing -> pure False
    Just discover -> withMVar pool.poolDiscoveryLock \_ -> do
        knownAccountIds <- allAccountIds pool
        if any (`notElem` accountIdsAtCheckout) knownAccountIds
            then pure True
            else discover knownAccountIds >>= \case
                Left CredentialsExhausted{retryAt} -> do
                    writeIORef pool.poolEmptyRetryAt (Just retryAt)
                    pure False
                Left _ -> pure False
                Right [] -> pure False
                Right states -> appendNewAccounts pool states

appendNewAccounts :: Pool -> [AuthState] -> IO Bool
appendNewAccounts pool states = do
    candidates <- mapM mkEntry states
    added <- atomicModifyIORef' pool.poolEntries \existing ->
        let known = Set.fromList (map (.entryAccountId) existing)
            (newEntries, _) = foldl addCandidate ([], known) candidates
            updated = existing <> reverse newEntries
        in (updated, not (null newEntries))
    when added $ writeIORef pool.poolEmptyRetryAt Nothing
    pure added
  where
    addCandidate (newEntries, known) entry
        | entry.entryAccountId `Set.member` known = (newEntries, known)
        | otherwise =
            ( entry : newEntries
            , Set.insert entry.entryAccountId known
            )

-- | Round-robin pick, skipping accounts whose cooldown has not yet expired.
-- Returns 'Left' with the earliest cooldown expiry if every account is in
-- cooldown.
pickAccount :: Pool -> IO (Either UTCTime AccountEntry)
pickAccount pool = do
    entries <- readIORef pool.poolEntries
    now <- getCurrentTime
    case entries of
        [] -> do
            retryAt <- readIORef pool.poolEmptyRetryAt
            pure (Left (fromMaybe now retryAt))
        _ -> do
            let n = length entries
            startIdx <- atomicModifyIORef' pool.poolCounter
                (\i -> let next = (i + 1) `mod` n in (next, i `mod` n))
            let tryFrom k
                    | k >= n = pure Nothing
                    | otherwise = do
                        let entry = entries !! ((startIdx + k) `mod` n)
                        cooldown <- readIORef entry.entryCooldownUntil
                        case cooldown of
                            Nothing -> pure (Just entry)
                            Just t | t <= now -> do
                                writeIORef entry.entryCooldownUntil Nothing
                                pure (Just entry)
                            _ -> tryFrom (k + 1)
            available <- tryFrom 0
            case available of
                Just entry -> pure (Right entry)
                Nothing -> do
                    expirations <- fmap catMaybes $ forM entries $ \entry ->
                        readIORef entry.entryCooldownUntil
                    case expirations of
                        []       -> pure (Left now)
                        (t : ts) -> pure (Left (minimum (t : ts)))

--------------------------------------------------------------------------------
-- Cooldown management
--------------------------------------------------------------------------------

-- | Mark the account with the given OpenAI @accountId@ as rate-limited.
--
-- @retryAfter = Just n@ sets the cooldown to exactly @n@ seconds — pass the
-- server-provided @resets_in_seconds@ from a @usage_limit_reached@ payload
-- so an exhausted quota window is skipped for its full duration. When
-- @Nothing@, falls back to the ~60s default for transient @rate_limit_error@
-- spikes.
reportRateLimit :: Pool -> Text -> Maybe Int -> IO ()
reportRateLimit pool limitedAccountId retryAfter = do
    now <- getCurrentTime
    let seconds = fromMaybe rateLimitCooldownSeconds retryAfter
        until_ = addUTCTime (fromIntegral seconds) now
    setCooldown pool limitedAccountId until_

-- | Mark the account with the given OpenAI @accountId@ as auth-broken
-- (401/403 from Codex). Uses the same cooldown mechanism as
-- 'reportRateLimit' with 'authFailureRetrySeconds'.
reportAuthBroken :: Pool -> Text -> IO ()
reportAuthBroken pool brokenAccountId = do
    now <- getCurrentTime
    let until_ = addUTCTime (fromIntegral authFailureRetrySeconds) now
    setCooldown pool brokenAccountId until_

-- | Force-rotate an access token that Codex has rejected with HTTP 401/403,
-- even when its JWT expiry is still in the future. ChatGPT can invalidate an
-- otherwise unexpired token, so waiting for the normal expiry margin would
-- leave every caller broken until the next scheduled refresh.
--
-- A successful refresh clears the account's auth cooldown and makes the new
-- token immediately available. A failed refresh cools the rejected account so
-- the caller can fail over without selecting the same known-bad token again.
refreshAfterAuthFailure :: Pool -> Text -> IO (Either ApiError AuthState)
refreshAfterAuthFailure pool rejectedAccountId = do
    result <- forceRefresh pool rejectedAccountId
    case result of
        Right newState -> do
            clearCooldown pool newState.accountId
            pure (Right newState)
        Left err -> do
            reportAuthBroken pool rejectedAccountId
            pure (Left err)

isAuthError :: ApiError -> Bool
isAuthError (HttpError 401 _) = True
isAuthError (HttpError 403 _) = True
isAuthError (ProviderError AuthenticationError _ _) = True
isAuthError CredentialError{} = True
isAuthError _ = False

setCooldown :: Pool -> Text -> UTCTime -> IO ()
setCooldown pool targetAccountId until_ =
    readIORef pool.poolEntries >>= mapM_ go
  where
    go entry = do
        state <- readIORef entry.entryAuthRef
        when (state.accountId == targetAccountId) $
            atomicModifyIORef' entry.entryCooldownUntil \current ->
                (Just (maybe until_ (max until_) current), ())

clearCooldown :: Pool -> Text -> IO ()
clearCooldown pool targetAccountId =
    readIORef pool.poolEntries >>= mapM_ go
  where
    go entry = do
        state <- readIORef entry.entryAuthRef
        when (state.accountId == targetAccountId) $
            writeIORef entry.entryCooldownUntil Nothing

--------------------------------------------------------------------------------
-- Inspection and manual refresh
--------------------------------------------------------------------------------

-- | OpenAI @accountId@ for every account currently in the pool, in load order.
allAccountIds :: Pool -> IO [Text]
allAccountIds pool = map (.entryAccountId) <$> readIORef pool.poolEntries

-- | Read the current (in-memory) 'AuthState' for an account, or 'Nothing' if
-- no account with that id is in the pool.
readAccountState :: Pool -> Text -> IO (Maybe AuthState)
readAccountState pool targetAccountId = do
    found <- findEntry pool targetAccountId
    case found of
        Nothing -> pure Nothing
        Just entry -> Just <$> readIORef entry.entryAuthRef

-- | One pool account plus its local cooldown, used by @/usage@.
data AccountSnapshot = AccountSnapshot
    { snapshotAuth :: !AuthState
    , snapshotCooldownUntil :: !(Maybe UTCTime)
    }
    deriving (Show)

-- | Snapshot every account currently in the pool, in load order.
snapshotAccounts :: Pool -> IO [AccountSnapshot]
snapshotAccounts pool = do
    entries <- readIORef pool.poolEntries
    forM entries \entry -> do
        auth <- readIORef entry.entryAuthRef
        cooldown <- readIORef entry.entryCooldownUntil
        pure AccountSnapshot
            { snapshotAuth = auth
            , snapshotCooldownUntil = cooldown
            }

-- | Get a fresh token for one specific account without changing round-robin
-- state. Cooldowns and token refresh use the same rules as normal checkout.
getAccessTokenForAccount
    :: Pool
    -> Text
    -> IO (Either ApiError (Text, Text))
getAccessTokenForAccount pool targetAccountId =
    findEntry pool targetAccountId >>= \case
        Nothing ->
            pure $ Left $
                CredentialError
                    ("unknown OpenAI account " <> targetAccountId)
        Just entry -> do
            now <- getCurrentTime
            readIORef entry.entryCooldownUntil >>= \case
                Just until_
                    | until_ > now ->
                        pure (Left (CredentialsExhausted until_))
                Just _ -> do
                    writeIORef entry.entryCooldownUntil Nothing
                    loadEntry entry
                Nothing ->
                    loadEntry entry
  where
    loadEntry entry =
        fmap
            (\state -> (state.accessToken, state.accountId))
            <$> currentAuthState pool entry

-- | Ask the configured dynamic source for accounts added since pool creation.
-- Returns whether at least one new account was appended.
discoverAccounts :: Pool -> IO Bool
discoverAccounts pool = do
    knownAccountIds <- allAccountIds pool
    discoverAdditionalAccounts pool knownAccountIds

-- | Force a refresh of the given account, regardless of JWT expiry. Useful
-- for scheduled refresh jobs that rotate ahead of the natural expiry window.
--
-- Refreshes for the same account are serialized within this pool. The callback
-- remains responsible for cross-process serialization (e.g. via a Postgres
-- row-level lock). On success the new state is written back to the pool's
-- in-memory cache.
forceRefresh :: Pool -> Text -> IO (Either ApiError AuthState)
forceRefresh pool targetAccountId = do
    found <- findEntry pool targetAccountId
    case found of
        Nothing ->
            pure $ Left (ProviderError AuthenticationError
                ("Agent.OpenAI.Auth.forceRefresh: unknown accountId " <> targetAccountId) Nothing)
        Just entry -> withMVar entry.entryRefreshLock \_ -> do
            state <- readIORef entry.entryAuthRef
            refreshEntry pool entry state

findEntry :: Pool -> Text -> IO (Maybe AccountEntry)
findEntry pool targetAccountId = do
    entries <- readIORef pool.poolEntries
    pure (find ((== targetAccountId) . (.entryAccountId)) entries)
