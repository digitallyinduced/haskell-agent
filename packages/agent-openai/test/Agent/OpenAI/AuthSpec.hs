module Agent.OpenAI.AuthSpec (spec) where

import Agent.OpenAI.Auth
import Agent.Error (ApiError(..), ErrorType(..))
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
import Test.Hspec

spec :: Spec
spec = do
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
                Left (CredentialsExhausted _) -> pure ()
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
                Left (CredentialsExhausted _) -> pure ()
                other -> expectationFailure ("expected CredentialsExhausted, got " <> show other)

        it "starts empty at the broker reset and discovers capacity later" $ do
            let initialReset = UTCTime (fromGregorian 2026 7 18) 0
            discoveryResult <- newIORef (Left (CredentialsExhausted initialReset))
            let discover _ = readIORef discoveryResult
            pool <- newUnavailableDiscoveringPool initialReset neverRefresh discover

            first <- getAccessToken pool
            first `shouldBe` Left (CredentialsExhausted initialReset)
            allAccountIds pool `shouldReturn` []

            writeIORef discoveryResult (Right [mkFreshAuth "capacity-returned"])
            second <- getAccessToken pool

            accountIdOf second `shouldBe` "capacity-returned"
            allAccountIds pool `shouldReturn` ["capacity-returned"]

        it "updates an empty pool when the broker reports a later reset" $ do
            let initialReset = UTCTime (fromGregorian 2026 7 18) 0
                laterReset = UTCTime (fromGregorian 2026 7 21) 0
                discover _ = pure (Left (CredentialsExhausted laterReset))
            pool <- newUnavailableDiscoveringPool initialReset neverRefresh discover

            getAccessToken pool `shouldReturn` Left (CredentialsExhausted laterReset)

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

        it "returns Left with an AuthenticationError when the accountId is unknown" $ do
            pool <- newPool [mkFreshAuth "acc"] neverRefresh
            outcome <- forceRefresh pool "does-not-exist"
            outcome `shouldSatisfy` isLeft

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
