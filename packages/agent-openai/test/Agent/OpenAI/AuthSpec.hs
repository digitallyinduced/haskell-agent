module Agent.OpenAI.AuthSpec (spec) where

import Agent.OpenAI.Auth
import Agent.OpenAI.Auth.Refresh (RefreshResponse(RefreshResponse), decodeRefreshResponse)
import Agent.Error
    ( ApiError(..)
    , CredentialExhaustionReason(..)
    , CredentialRefreshFailure(..)
    , ErrorType(..)
    , credentialsExhausted
    )
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , takeMVar
    )
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified "base64-bytestring" Data.ByteString.Base64 as B64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), addUTCTime, getCurrentTime)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
    describe "decodeRefreshResponse" do
        it "keeps optional tokens when they are absent or null" do
            decodeRefreshResponse "{\"access_token\":\"a\"}"
                `shouldBe` Right (RefreshResponse "a" Nothing Nothing)
            decodeRefreshResponse
                "{\"access_token\":\"a\",\"refresh_token\":null,\"id_token\":null}"
                `shouldBe` Right (RefreshResponse "a" Nothing Nothing)
            decodeRefreshResponse
                "{\"access_token\":\"a\",\"refresh_token\":\"r\",\"id_token\":\"i\"}"
                `shouldBe` Right (RefreshResponse "a" (Just "r") (Just "i"))

        it "reports an authentication error for unreadable bodies" do
            decodeRefreshResponse "{\"refresh_token\":\"r\"}"
                `shouldSatisfy` \case
                    Left (ProviderError AuthenticationError _ Nothing) -> True
                    _ -> False

    describe "Show" do
        it "redacts every token while retaining account metadata" do
            let auth = AuthState
                    { accessToken = "access-secret"
                    , refreshToken = "refresh-secret"
                    , accountId = "acc-visible"
                    , idToken = Just "identity-secret"
                    , lastRefresh = epoch
                    }
                rendered = show auth
            rendered `shouldContain` "acc-visible"
            rendered `shouldContain` show epoch
            rendered `shouldNotContain` "access-secret"
            rendered `shouldNotContain` "refresh-secret"
            rendered `shouldNotContain` "identity-secret"

        it "keeps AccountSnapshot token-safe through its Show instance" do
            let auth = AuthState
                    { accessToken = "snapshot-access-secret"
                    , refreshToken = "snapshot-refresh-secret"
                    , accountId = "snapshot-account"
                    , idToken = Just "snapshot-id-secret"
                    , lastRefresh = epoch
                    }
                snapshot = AccountSnapshot
                    { snapshotAuth = auth
                    , snapshotCooldownUntil = Just epoch
                    , snapshotCooldownReasons = []
                    }
                rendered = show snapshot
            rendered `shouldContain` "snapshot-account"
            rendered `shouldNotContain` "snapshot-access-secret"
            rendered `shouldNotContain` "snapshot-refresh-secret"
            rendered `shouldNotContain` "snapshot-id-secret"

    describe "parseJwtExp" $ do
        it "parses exp claim from a well-formed JWT" $ do
            let expSeconds = 4_102_444_800 :: Int  -- 2100-01-01 00:00:00 UTC
                token = mkJwt (Aeson.object [ "exp" .= expSeconds ])
                expected = addUTCTime (fromIntegral expSeconds) epoch
            parseJwtExp token `shouldBe` Just expected

        it "returns Nothing for a malformed JWT" $ do
            parseJwtExp "not.a.jwt" `shouldBe` Nothing
            parseJwtExp "" `shouldBe` Nothing
            parseJwtExp "only-one-part" `shouldBe` Nothing

        it "returns Nothing when the payload has no exp claim" $ do
            let token = mkJwt (Aeson.object [ "iss" .= ("issuer" :: Text) ])
            parseJwtExp token `shouldBe` Nothing

        it "handles base64url-encoded payloads without padding" $ do
            -- A payload whose base64 encoding drops its `=` padding.
            let expSeconds = 1_800_000_000 :: Int
                token = mkJwt (Aeson.object [ "exp" .= expSeconds ])
            parseJwtExp token `shouldSatisfy` (/= Nothing)

    describe "deriveAccountId" $ do
        it "extracts chatgpt_account_id from id_token claims" $ do
            let idTok = mkJwt $ Aeson.object
                    [ "https://api.openai.com/auth" .= Aeson.object
                        [ "chatgpt_account_id" .= ("acc_abc123" :: Text) ]
                    ]
            deriveAccountId idTok `shouldBe` Just "acc_abc123"

        it "returns Nothing when the claim is missing" $ do
            let idTok = mkJwt (Aeson.object [ "exp" .= (1_800_000_000 :: Int) ])
            deriveAccountId idTok `shouldBe` Nothing

    describe "deriveEmail" $ do
        it "extracts the standard email claim" $ do
            let idTok = mkJwt $ Aeson.object
                    [ "email" .= ("person@example.com" :: Text) ]
            deriveEmail idTok `shouldBe` Just "person@example.com"

        it "returns Nothing when the email claim is absent or empty" $ do
            deriveEmail (mkJwt (Aeson.object [])) `shouldBe` Nothing
            deriveEmail (mkJwt (Aeson.object ["email" .= ("" :: Text)]))
                `shouldBe` Nothing

    describe "needsRefresh" $ do
        it "returns False for tokens with an exp far in the future" $ do
            let state = mkFreshAuth "acc"
            now <- getCurrentTime
            needsRefresh state now `shouldBe` False

        it "returns True for tokens that are already past exp" $ do
            let state = mkExpiredAuth "acc"
            now <- getCurrentTime
            needsRefresh state now `shouldBe` True

        it "returns True when exp cannot be parsed" $ do
            let state = (mkFreshAuth "acc") { accessToken = "not-a-jwt" }
            now <- getCurrentTime
            needsRefresh state now `shouldBe` True

        it "does not refresh static bearer credentials" $ do
            let state = (mkFreshAuth "acc")
                    { accessToken = "not-a-jwt"
                    , refreshToken = ""
                    }
            now <- getCurrentTime
            needsRefresh state now `shouldBe` False

    describe "newPool + getAccessToken" $ do
        it "dispenses fresh tokens without calling the refresh callback" $ do
            callCounter <- newIORef (0 :: Int)
            pool <- newPool [mkFreshAuth "acc-1"] (countingRefresh callCounter)
            result <- getAccessToken pool
            result `shouldSatisfy` isRight
            readIORef callCounter `shouldReturn` 0

        it "shares one successful refresh across concurrent expired checkouts" do
            refreshCalls <- newIORef (0 :: Int)
            firstRefreshStarted <- newEmptyMVar
            laterRefreshStarted <- newEmptyMVar
            releaseFirstRefresh <- newEmptyMVar
            secondCheckoutStarted <- newEmptyMVar
            let refreshed = (mkFreshAuth "acc")
                    { refreshToken = "rotated-refresh" }
                refresh _ = do
                    callNumber <- atomicModifyIORef' refreshCalls \n ->
                        let next = n + 1
                        in (next, next)
                    if callNumber == 1
                        then do
                            putMVar firstRefreshStarted ()
                            takeMVar releaseFirstRefresh
                        else putMVar laterRefreshStarted ()
                    pure (Right refreshed)
            pool <- newPool [mkExpiredAuth "acc"] refresh

            withAsync (getAccessToken pool) \firstCheckout -> do
                takeMVar firstRefreshStarted
                withAsync
                    (putMVar secondCheckoutStarted () >> getAccessToken pool)
                    \secondCheckout -> do
                        takeMVar secondCheckoutStarted
                        overlappingRefresh <- timeout concurrencyProbeMicros
                            (takeMVar laterRefreshStarted)
                        putMVar releaseFirstRefresh ()
                        firstResult <- wait firstCheckout
                        secondResult <- wait secondCheckout

                        overlappingRefresh `shouldBe` Nothing
                        readIORef refreshCalls `shouldReturn` 1
                        firstResult `shouldBe`
                            Right (refreshed.accessToken, refreshed.accountId)
                        secondResult `shouldBe`
                            Right (refreshed.accessToken, refreshed.accountId)

        it "alternates between accounts on consecutive calls (round-robin)" $ do
            callCounter <- newIORef (0 :: Int)
            let states = [ mkFreshAuth "acc-1", mkFreshAuth "acc-2" ]
            pool <- newPool states (countingRefresh callCounter)
            ids <- mapM (const (accountIdOf <$> getAccessToken pool)) [1 .. 3 :: Int]
            -- Three consecutive calls with N=2 accounts must hit both, and
            -- the third call must match the first (counter wraps).
            case ids of
                (a : _ : c : _) -> do
                    length (uniq ids) `shouldBe` 2
                    a `shouldBe` c
                _ -> expectationFailure "expected three round-robin samples"

        it "checks out a requested account without depending on round-robin" $ do
            let states = [ mkFreshAuth "acc-1", mkFreshAuth "acc-2" ]
            pool <- newPool states neverRefresh

            getAccessTokenForAccount pool "acc-2"
                `shouldReturn`
                    Right
                        ( (mkFreshAuth "acc-2").accessToken
                        , "acc-2"
                        )

        it "respects cooldown for a requested account" $ do
            pool <- newPool
                [mkFreshAuth "paced", mkFreshAuth "healthy"]
                neverRefresh
            reportRateLimit pool "paced" (Just 60)

            getAccessTokenForAccount pool "paced" >>= \case
                Left CredentialsExhausted{} -> pure ()
                other ->
                    expectationFailure
                        ("expected requested account cooldown, got "
                            <> show other)

        it "reports an unknown requested account" $ do
            pool <- newPool [mkFreshAuth "known"] neverRefresh
            getAccessTokenForAccount pool "missing" >>= \case
                Left CredentialError{} -> pure ()
                other ->
                    expectationFailure
                        ("expected unknown-account credential error, got "
                            <> show other)

        it "fails over when the selected account cannot refresh" $ do
            attempts <- newIORef ([] :: [Text])
            let states = [ mkExpiredAuth "acc-broken", mkExpiredAuth "acc-ok" ]
                refresh state = do
                    previous <- readIORef attempts
                    modifyIORef' attempts (<> [state.accountId])
                    if null previous
                        then pure $ Left (ProviderError AuthenticationError "refresh_token_reused" Nothing)
                        else pure $ Right state
            pool <- newPool states refresh
            result <- getAccessToken pool
            tried <- readIORef attempts
            case tried of
                [first, second] -> do
                    first `shouldNotBe` second
                    accountIdOf result `shouldBe` second
                _ -> expectationFailure ("expected two refresh attempts, got " <> show tried)

        it "fails over when the selected account has a local credential error" $ do
            attempts <- newIORef ([] :: [Text])
            let states = [ mkExpiredAuth "acc-broken", mkExpiredAuth "acc-ok" ]
                refresh state = do
                    previous <- readIORef attempts
                    modifyIORef' attempts (<> [state.accountId])
                    if null previous
                        then pure $ Left
                            (CredentialError "credential source is unavailable")
                        else pure $ Right state
            pool <- newPool states refresh
            result <- getAccessToken pool
            tried <- readIORef attempts
            case tried of
                [first, second] -> do
                    first `shouldNotBe` second
                    accountIdOf result `shouldBe` second
                _ -> expectationFailure
                    ("expected two refresh attempts, got " <> show tried)

        it "retains a token-endpoint rejection without its response body" do
            let secretBody = "TOP_SECRET_REFRESH_RESPONSE"
                refresh _ = pure $ Left $ HttpError 403 secretBody
            pool <- newPool [mkExpiredAuth "acc-broken"] refresh

            result <- getAccessToken pool

            case result of
                Left CredentialsExhausted{exhaustionReasons} ->
                    exhaustionReasons `shouldBe`
                        [ ExhaustedByCredentialRefresh
                            { refreshFailure = RefreshProviderFailed
                            , exhaustionErrorType = Nothing
                            , exhaustionStatusCode = Just 403
                            }
                        ]
                other -> expectationFailure
                    ("expected CredentialsExhausted, got " <> show other)
            show result `shouldNotContain` "TOP_SECRET_REFRESH_RESPONSE"

    describe "reportRateLimit" $ do
        it "skips rate-limited accounts on subsequent picks" $ do
            callCounter <- newIORef (0 :: Int)
            let states = [ mkFreshAuth "acc-1", mkFreshAuth "acc-2" ]
            pool <- newPool states (countingRefresh callCounter)
            reportRateLimit pool "acc-1" (Just 60)
            -- Every pick for the next 60s must land on acc-2.
            ids <- mapM (const (accountIdOf <$> getAccessToken pool)) [1 .. 4 :: Int]
            ids `shouldSatisfy` all (== "acc-2")

        it "does not shorten an existing longer cooldown" $ do
            pool <- newPool [mkFreshAuth "only-acc"] neverRefresh
            reportRateLimit pool "only-acc" (Just 3600)
            [before] <- snapshotAccounts pool
            reportRateLimit pool "only-acc" (Just 60)
            [after] <- snapshotAccounts pool
            after.snapshotCooldownUntil `shouldBe` before.snapshotCooldownUntil

        it "returns CredentialsExhausted when every account is cooling down" $ do
            pool <- newPool [mkFreshAuth "only-acc"] neverRefresh
            reportRateLimit pool "only-acc" (Just 60)
            result <- getAccessToken pool
            case result of
                Left CredentialsExhausted{exhaustionReasons} ->
                    exhaustionReasons `shouldBe`
                        [ ExhaustedByRateLimit
                            { exhaustionErrorType = Nothing
                            , exhaustionStatusCode = Nothing
                            , exhaustionRetryAfter = Just 60
                            }
                        ]
                other -> expectationFailure ("expected CredentialsExhausted, got " <> show other)

        it "discovers a broker account that became available after startup" $ do
            discoveryCalls <- newIORef ([] :: [[Text]])
            let discover excluded = do
                    modifyIORef' discoveryCalls (<> [excluded])
                    pure (Right [mkFreshAuth "newly-healthy"])
            pool <- newDiscoveringPool
                [mkFreshAuth "initial-account"]
                neverRefresh
                discover
            reportRateLimit pool "initial-account" (Just 60)

            result <- getAccessToken pool

            accountIdOf result `shouldBe` "newly-healthy"
            readIORef discoveryCalls `shouldReturn` [["initial-account"]]
            allAccountIds pool `shouldReturn` ["initial-account", "newly-healthy"]

        it "explicitly discovers a connected account while existing capacity is healthy" $ do
            discoveryCalls <- newIORef (0 :: Int)
            let discover excluded = do
                    excluded `shouldBe` ["initial-account"]
                    modifyIORef' discoveryCalls (+ 1)
                    pure (Right [mkFreshAuth "new-account"])
            pool <- newDiscoveringPool
                [mkFreshAuth "initial-account"]
                neverRefresh
                discover

            discoverAccounts pool `shouldReturn` True

            readIORef discoveryCalls `shouldReturn` 1
            allAccountIds pool
                `shouldReturn` ["initial-account", "new-account"]

        it "shares one in-flight discovery without locking pool inspection" do
            discoveryCalls <- newIORef (0 :: Int)
            discoveryStarted <- newEmptyMVar
            releaseDiscovery <- newEmptyMVar
            let discover excluded = do
                    excluded `shouldBe` ["initial-account"]
                    modifyIORef' discoveryCalls (+ 1)
                    putMVar discoveryStarted ()
                    takeMVar releaseDiscovery
                    pure (Right [mkFreshAuth "discovered-account"])
            pool <- newDiscoveringPool
                [mkFreshAuth "initial-account"]
                neverRefresh
                discover

            withAsync (discoverAccounts pool) \firstDiscovery -> do
                takeMVar discoveryStarted
                withAsync (discoverAccounts pool) \secondDiscovery -> do
                    -- Give the second worker a chance to observe the in-flight
                    -- discovery before releasing its owner.
                    threadDelay 10_000
                    inspected <- timeout concurrencyProbeMicros
                        (allAccountIds pool)
                    inspected `shouldBe` Just ["initial-account"]
                    putMVar releaseDiscovery ()
                    wait firstDiscovery `shouldReturn` True
                    wait secondDiscovery `shouldReturn` True

            readIORef discoveryCalls `shouldReturn` 1
            allAccountIds pool
                `shouldReturn`
                    ["initial-account", "discovered-account"]

        it "releases discovery ownership when the callback throws" do
            discoveryCalls <- newIORef (0 :: Int)
            let discover _ = do
                    call <- atomicModifyIORef' discoveryCalls \count ->
                        (count + 1, count + 1)
                    if call == 1
                        then ioError (userError "broker crashed")
                        else pure
                            (Right [mkFreshAuth "recovered-account"])
            pool <- newDiscoveringPool
                [mkFreshAuth "initial-account"]
                neverRefresh
                discover

            discoverAccounts pool `shouldThrow` anyIOException
            timeout concurrencyProbeMicros (discoverAccounts pool)
                `shouldReturn` Just True
            readIORef discoveryCalls `shouldReturn` 2

        it "keeps the precise local reset when broker discovery is unavailable" $ do
            let brokerUnavailable _ =
                    pure (Left (HttpError 503 "broker temporarily unavailable"))
            pool <- newDiscoveringPool
                [mkFreshAuth "only-account"]
                neverRefresh
                brokerUnavailable
            reportRateLimit pool "only-account" (Just 60)

            result <- getAccessToken pool

            case result of
                Left CredentialsExhausted{} -> pure ()
                other -> expectationFailure ("expected CredentialsExhausted, got " <> show other)

        it "starts empty at the broker reset and discovers capacity later" $ do
            let initialReset = UTCTime (fromGregorian 2026 7 18) 0
            discoveryResult <- newIORef (Left (credentialsExhausted initialReset))
            let discover _ = readIORef discoveryResult
            pool <- newUnavailableDiscoveringPool initialReset neverRefresh discover

            first <- getAccessToken pool
            first `shouldBe` Left (credentialsExhausted initialReset)
            allAccountIds pool `shouldReturn` []

            writeIORef discoveryResult (Right [mkFreshAuth "capacity-returned"])
            second <- getAccessToken pool

            accountIdOf second `shouldBe` "capacity-returned"
            allAccountIds pool `shouldReturn` ["capacity-returned"]

        it "updates an empty pool when the broker reports a later reset" $ do
            let initialReset = UTCTime (fromGregorian 2026 7 18) 0
                laterReset = UTCTime (fromGregorian 2026 7 21) 0
                discover _ = pure (Left (credentialsExhausted laterReset))
            pool <- newUnavailableDiscoveringPool initialReset neverRefresh discover

            getAccessToken pool
                `shouldReturn` Left (credentialsExhausted laterReset)

    describe "reportAuthBroken" $ do
        it "cools the account down" $ do
            let states = [ mkFreshAuth "acc-a", mkFreshAuth "acc-b" ]
            pool <- newPool states neverRefresh
            reportAuthBroken pool "acc-a"
            ids <- mapM (const (accountIdOf <$> getAccessToken pool)) [1 .. 3 :: Int]
            ids `shouldSatisfy` all (== "acc-b")

        it "retries an auth-broken account after about one minute" $ do
            now <- getCurrentTime
            pool <- newPool [mkFreshAuth "only-account"] neverRefresh
            reportAuthBroken pool "only-account"

            result <- getAccessToken pool

            case result of
                Left CredentialsExhausted{retryAt} -> do
                    retryAt `shouldSatisfy` (> addUTCTime 50 now)
                    retryAt `shouldSatisfy` (< addUTCTime 70 now)
                other -> expectationFailure
                    ("expected CredentialsExhausted, got " <> show other)

    describe "forceRefresh" $ do
        it "invokes the refresh callback and caches the returned state" $ do
            callCounter <- newIORef (0 :: Int)
            let startState = mkFreshAuth "acc"
                rotated    = startState
                    { refreshToken = "rotated-refresh"
                    , accessToken  = (mkFreshAuth "acc-rotated").accessToken
                    }
                refresh _ = do
                    modifyIORef' callCounter (+ 1)
                    pure (Right rotated)
            pool <- newPool [startState] refresh
            outcome <- forceRefresh pool "acc"
            outcome `shouldSatisfy` isRight
            readIORef callCounter `shouldReturn` 1
            -- Subsequent state read reflects the rotation.
            mAfter <- readAccountState pool "acc"
            fmap (.refreshToken) mAfter `shouldBe` Just "rotated-refresh"

        it "blocks checkouts until the forced replacement is committed" do
            refreshStarted <- newEmptyMVar
            releaseRefresh <- newEmptyMVar
            checkoutStarted <- newEmptyMVar
            let startState = mkFreshAuth "acc"
                rotated = startState
                    { accessToken = (mkFreshAuth "rotated").accessToken
                    , refreshToken = "rotated-refresh"
                    }
                refresh _ = do
                    putMVar refreshStarted ()
                    takeMVar releaseRefresh
                    pure (Right rotated)
            pool <- newPool [startState] refresh

            withAsync (forceRefresh pool "acc") \forcedRefresh -> do
                takeMVar refreshStarted
                withAsync
                    (putMVar checkoutStarted () >> getAccessToken pool)
                    \checkout -> do
                        takeMVar checkoutStarted
                        earlyCheckout <- timeout concurrencyProbeMicros
                            (wait checkout)
                        putMVar releaseRefresh ()
                        forcedResult <- wait forcedRefresh
                        checkoutResult <- wait checkout

                        earlyCheckout `shouldBe` Nothing
                        fmap (.accessToken) forcedResult
                            `shouldBe` Right rotated.accessToken
                        checkoutResult `shouldBe`
                            Right (rotated.accessToken, rotated.accountId)

        it "returns Left with an AuthenticationError when the accountId is unknown" $ do
            pool <- newPool [mkFreshAuth "acc"] neverRefresh
            outcome <- forceRefresh pool "does-not-exist"
            outcome `shouldSatisfy` isLeft

        it "rejects refreshes that change account identity" do
            let startState = mkFreshAuth "acc"
                changedIdentity =
                    (mkFreshAuth "other")
                        { accessToken = (mkFreshAuth "rotated").accessToken }
            pool <- newPool [startState] (const (pure (Right changedIdentity)))

            forceRefresh pool "acc" >>= \case
                Left CredentialError{} -> pure ()
                other -> expectationFailure
                    ("expected account identity error, got " <> show other)
            readAccountState pool "acc" >>= \case
                Just current -> do
                    current.accountId `shouldBe` startState.accountId
                    current.accessToken `shouldBe` startState.accessToken
                    current.refreshToken `shouldBe` startState.refreshToken
                Nothing ->
                    expectationFailure "original account disappeared"
            readAccountState pool "other" >>= \other ->
                other `shouldSatisfy` isNothing

    describe "refreshAfterAuthFailure" $ do
        it "rotates a rejected unexpired token and caches the replacement" $ do
            callCounter <- newIORef (0 :: Int)
            let startState = mkFreshAuth "acc"
                rotated = startState
                    { accessToken = (mkFreshAuth "rotated").accessToken
                    , refreshToken = "rotated-refresh"
                    }
                refresh _ = do
                    modifyIORef' callCounter (+ 1)
                    pure (Right rotated)
            pool <- newPool [startState] refresh

            outcome <- refreshAfterAuthFailure pool "acc"

            outcome `shouldSatisfy` isRight
            readIORef callCounter `shouldReturn` 1
            token <- getAccessToken pool
            fmap fst token `shouldBe` Right rotated.accessToken

        it "does not erase a concurrent rate-limit cooldown after recovery" do
            refreshStarted <- newEmptyMVar
            releaseRefresh <- newEmptyMVar
            let startState = mkFreshAuth "acc"
                rotated = startState
                    { accessToken = (mkFreshAuth "rotated").accessToken
                    , refreshToken = "rotated-refresh"
                    }
                refresh _ = do
                    putMVar refreshStarted ()
                    takeMVar releaseRefresh
                    pure (Right rotated)
            pool <- newPool [startState] refresh
            beforeCooldown <- getCurrentTime
            cooldownReportStarted <- newEmptyMVar

            withAsync (refreshAfterAuthFailure pool "acc") \recovery -> do
                takeMVar refreshStarted
                withAsync
                    (putMVar cooldownReportStarted ()
                        >> reportRateLimit pool "acc" (Just 3600))
                    \cooldownReport -> do
                        takeMVar cooldownReportStarted
                        putMVar releaseRefresh ()
                        wait recovery >>= \case
                            Right recovered -> do
                                recovered.accountId
                                    `shouldBe` rotated.accountId
                                recovered.accessToken
                                    `shouldBe` rotated.accessToken
                            Left err ->
                                expectationFailure
                                    ("expected successful recovery, got "
                                        <> show err)
                        wait cooldownReport

            [snapshot] <- snapshotAccounts pool
            snapshot.snapshotAuth.accountId
                `shouldBe` rotated.accountId
            snapshot.snapshotAuth.accessToken
                `shouldBe` rotated.accessToken
            snapshot.snapshotCooldownUntil
                `shouldSatisfy`
                    maybe False (> addUTCTime 3500 beforeCooldown)

        it "cools a rejected account when forced refresh fails" $ do
            let states = [mkFreshAuth "broken", mkFreshAuth "healthy"]
                refresh state
                    | state.accountId == "broken" =
                        pure (Left (ProviderError AuthenticationError "refresh rejected" Nothing))
                    | otherwise = pure (Right state)
            pool <- newPool states refresh

            outcome <- refreshAfterAuthFailure pool "broken"

            outcome `shouldSatisfy` isLeft
            ids <- mapM (const (accountIdOf <$> getAccessToken pool)) [1 .. 3 :: Int]
            ids `shouldSatisfy` all (== "healthy")

        it "retains transport provenance when forced refresh fails" do
            let refreshError =
                    ConnectionError "TOP_SECRET_REFRESH_EXCEPTION"
                refresh _ = pure (Left refreshError)
            pool <- newPool [mkFreshAuth "broken"] refresh

            refreshAfterAuthFailure pool "broken" >>= \case
                Left err -> err `shouldBe` refreshError
                Right _ -> expectationFailure
                    "expected forced refresh to fail"
            getAccessToken pool >>= \case
                Left CredentialsExhausted{exhaustionReasons} ->
                    exhaustionReasons `shouldBe`
                        [ ExhaustedByCredentialRefresh
                            { refreshFailure = RefreshTransportFailed
                            , exhaustionErrorType = Nothing
                            , exhaustionStatusCode = Nothing
                            }
                        ]
                other -> expectationFailure
                    ("expected CredentialsExhausted, got " <> show other)

    describe "allAccountIds + readAccountState" $ do
        it "lists every account currently in the pool" $ do
            let states = [ mkFreshAuth "a", mkFreshAuth "b", mkFreshAuth "c" ]
            pool <- newPool states neverRefresh
            ids <- allAccountIds pool
            uniq ids `shouldMatchList` ["a", "b", "c"]

        it "returns Nothing for unknown accountIds" $ do
            pool <- newPool [mkFreshAuth "known"] neverRefresh
            mState <- readAccountState pool "unknown"
            mState `shouldSatisfy` isNothing

        it "snapshots cooldown alongside auth state" $ do
            pool <- newPool [mkFreshAuth "paced"] neverRefresh
            reportRateLimit pool "paced" (Just 120)
            snaps <- snapshotAccounts pool
            case snaps of
                [snap] -> do
                    snap.snapshotAuth.accountId `shouldBe` "paced"
                    snap.snapshotCooldownUntil `shouldSatisfy` isJust
                other -> expectationFailure ("expected one snapshot, got " <> show (length other))

--------------------------------------------------------------------------------
-- Fixtures and helpers
--------------------------------------------------------------------------------

epoch :: UTCTime
epoch = UTCTime (fromGregorian 1970 1 1) 0

concurrencyProbeMicros :: Int
concurrencyProbeMicros = 250_000

-- | Build an 'AuthState' with a JWT whose exp is the year 2100 — well past
-- the refresh-margin threshold, so 'needsRefresh' returns False.
mkFreshAuth :: Text -> AuthState
mkFreshAuth name = AuthState
    { accessToken  = mkJwt (Aeson.object [ "exp" .= (4_102_444_800 :: Int) ])
    , refreshToken = name <> "-refresh"
    , accountId    = name
    , idToken      = Nothing
    , lastRefresh  = UTCTime (fromGregorian 2026 1 1) 0
    }

-- | Build an 'AuthState' with a JWT whose exp is in 2020 — well before now,
-- so 'needsRefresh' returns True.
mkExpiredAuth :: Text -> AuthState
mkExpiredAuth name = (mkFreshAuth name)
    { accessToken = mkJwt (Aeson.object [ "exp" .= (1_577_836_800 :: Int) ])  -- 2020-01-01
    }

-- | Refresh callback that records its invocation count and returns the
-- input unchanged. Useful to assert "refresh was/wasn't called".
countingRefresh :: IORef Int -> AuthState -> IO (Either ApiError AuthState)
countingRefresh ref s = do
    modifyIORef' ref (+ 1)
    pure (Right s)

-- | Refresh callback that fails the test if invoked.
neverRefresh :: AuthState -> IO (Either ApiError AuthState)
neverRefresh _ = error "neverRefresh: refresh callback should not have been invoked"

accountIdOf :: Either ApiError (Text, Text) -> Text
accountIdOf (Right (_, accId)) = accId
accountIdOf (Left err) = error ("getAccessToken failed: " <> show err)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

uniq :: Eq a => [a] -> [a]
uniq []     = []
uniq (x:xs) = x : uniq (filter (/= x) xs)

-- | Build a test JWT: @base64url(header).base64url(payload).signature@.
-- The signature is not verified by 'parseJwtExp' / 'deriveAccountId', so a
-- placeholder string is fine.
mkJwt :: Aeson.Value -> Text
mkJwt payload =
    encode headerJson <> "." <> encode payloadJson <> ".sig"
  where
    headerJson = Aeson.encode (Aeson.object
        [ "alg" .= ("none" :: Text), "typ" .= ("JWT" :: Text) ])
    payloadJson = Aeson.encode payload

    encode :: LBS.ByteString -> Text
    encode =
        Text.decodeUtf8
        . BS.filter (/= 0x3D)           -- strip '=' padding
        . BS.map urlSafeByte
        . B64.encode
        . LBS.toStrict

    urlSafeByte 0x2B = 0x2D  -- '+' -> '-'
    urlSafeByte 0x2F = 0x5F  -- '/' -> '_'
    urlSafeByte b    = b
