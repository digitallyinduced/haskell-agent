{-# LANGUAGE NumericUnderscores #-}

module Agent.Store.Postgres.ConnectionSpec (spec) where

import Control.Exception.Safe (bracket, throwIO)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , modifyIORef'
    , newIORef
    , readIORef
    )
import Data.List (isInfixOf)
import qualified Data.Text as Text
import qualified Hasql.Errors as Errors
import qualified Hasql.Pool as Pool
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

import Agent.Store.Postgres.Config (defaultManagedPostgresConfig)
import Agent.Store.Postgres.Connection
    ( ReconnectionPolicy(..)
    , connectionSettingsForRole
    , defaultReconnectionPolicy
    , formatPostgresApplicationName
    , isTransientUsageError
    , noReconnectionPolicy
    , postgresApplicationName
    , postgresApplicationNameFromEnvironment
    , retryTransientUsageErrors
    )

spec :: Spec
spec = do
    describe "connection settings" do
        it "sets PostgreSQL application_name for every role" do
            let settings =
                    connectionSettingsForRole
                        (defaultManagedPostgresConfig "/tmp/agent" "")
                        "ha_runtime"
            show settings `shouldSatisfy`
                (("application_name=" <> Text.unpack postgresApplicationName)
                    `isInfixOf`)

        describe "formatPostgresApplicationName" do
            it "includes a sanitized build commit" do
                formatPostgresApplicationName "abcdef12"
                    `shouldBe` postgresApplicationName <> "/abcdef12"

            it "keeps nix dirty short revisions" do
                formatPostgresApplicationName "abcdef1-dirty"
                    `shouldBe` postgresApplicationName <> "/abcdef1-dirty"

            it "defaults missing commits to development" do
                formatPostgresApplicationName ""
                    `shouldBe` postgresApplicationName <> "/development"

            it "rejects unsafe commit text" do
                formatPostgresApplicationName "bad\r\nInjected: value"
                    `shouldBe` postgresApplicationName <> "/development"

            it "stays within PostgreSQL's application_name limit" do
                let name =
                        formatPostgresApplicationName
                            (Text.replicate 80 "a")
                Text.length name `shouldBe` 63
                name `shouldSatisfy` Text.isPrefixOf (postgresApplicationName <> "/")

        it "reads AGENT_BUILD_COMMIT from the process environment" do
            name <-
                withEnvironment "AGENT_BUILD_COMMIT" "abcdef12"
                    postgresApplicationNameFromEnvironment
            name `shouldBe` postgresApplicationName <> "/abcdef12"

        it "ignores an unsafe AGENT_BUILD_COMMIT value" do
            name <-
                withEnvironment "AGENT_BUILD_COMMIT" "bad\r\nInjected: value"
                    postgresApplicationNameFromEnvironment
            name `shouldBe` postgresApplicationName <> "/development"

    describe "pooled session reconnection" do
        describe "isTransientUsageError" do
            it "retries when the server cannot be reached" do
                isTransientUsageError unreachableServer `shouldBe` True

            it "retries when the server closed the pooled connection" do
                isTransientUsageError closedConnection `shouldBe` True

            it "does not retry a rejected login" do
                isTransientUsageError rejectedLogin `shouldBe` False

            it "does not retry a statement the server rejected" do
                isTransientUsageError rejectedStatement `shouldBe` False

            it "does not retry pool acquisition timeouts" do
                isTransientUsageError Pool.AcquisitionTimeoutUsageError
                    `shouldBe` False

        describe "retryTransientUsageErrors" do
            it "returns the first success after transient failures" do
                attempts <- plannedAttempts
                    [ Left unreachableServer
                    , Left closedConnection
                    , Right (3 :: Int)
                    ]
                delays <- newIORef []
                result <- retryTransientUsageErrors
                    (ReconnectionPolicy [10, 20, 30])
                    (recordDelay delays)
                    (nextAttempt attempts)
                result `shouldBe` Right 3
                readIORef delays `shouldReturn` [10, 20]

            it "reports a non-transient error without waiting" do
                attempts <- plannedAttempts
                    [ Left unreachableServer
                    , Left rejectedStatement
                    , Right (0 :: Int)
                    ]
                delays <- newIORef []
                result <- retryTransientUsageErrors
                    (ReconnectionPolicy [10, 20, 30])
                    (recordDelay delays)
                    (nextAttempt attempts)
                result `shouldBe` Left rejectedStatement
                readIORef delays `shouldReturn` [10]

            it "reports the last transient error once the policy is exhausted" do
                attempts <- plannedAttempts
                    [ Left closedConnection
                    , Left unreachableServer
                    , Left closedConnection
                    , Right (0 :: Int)
                    ]
                delays <- newIORef []
                result <- retryTransientUsageErrors
                    (ReconnectionPolicy [10, 20])
                    (recordDelay delays)
                    (nextAttempt attempts)
                result `shouldBe` Left closedConnection
                readIORef delays `shouldReturn` [10, 20]

            it "makes exactly one attempt without a reconnection policy" do
                attempts <- plannedAttempts
                    [ Left unreachableServer
                    , Right (0 :: Int)
                    ]
                delays <- newIORef []
                result <- retryTransientUsageErrors
                    noReconnectionPolicy
                    (recordDelay delays)
                    (nextAttempt attempts)
                result `shouldBe` Left unreachableServer
                readIORef delays `shouldReturn` []

            it "waits about one minute in total by default" do
                let delays = defaultReconnectionPolicy.reconnectionDelays
                sum delays `shouldBe` 60_000_000
                delays `shouldSatisfy` all (> 0)
                -- Early retries follow closely so a fast restart costs little.
                take 2 delays `shouldBe` [1_000_000, 2_000_000]

unreachableServer :: Pool.UsageError
unreachableServer =
    Pool.ConnectionUsageError
        (Errors.NetworkingConnectionError
            "connection to server on socket \"/run/.s.PGSQL.55432\" failed: \
            \No such file or directory")

closedConnection :: Pool.UsageError
closedConnection =
    Pool.SessionUsageError
        (Errors.ConnectionSessionError
            "server closed the connection unexpectedly")

rejectedLogin :: Pool.UsageError
rejectedLogin =
    Pool.ConnectionUsageError
        (Errors.AuthenticationConnectionError
            "role \"ha_runtime\" does not exist")

rejectedStatement :: Pool.UsageError
rejectedStatement =
    Pool.SessionUsageError
        (Errors.ScriptSessionError
            "SELEC 1"
            (Errors.ServerError
                "42601"
                "syntax error at or near \"SELEC\""
                Nothing
                Nothing
                (Just 1)))

plannedAttempts
    :: [Either Pool.UsageError a]
    -> IO (IORef [Either Pool.UsageError a])
plannedAttempts = newIORef

nextAttempt
    :: IORef [Either Pool.UsageError a]
    -> IO (Either Pool.UsageError a)
nextAttempt attempts =
    atomicModifyIORef' attempts \case
        [] -> ([], Nothing)
        outcome : remaining -> (remaining, Just outcome)
    >>= maybe (throwIO (userError "more attempts than planned")) pure

recordDelay :: IORef [Int] -> Int -> IO ()
recordDelay delays delay = modifyIORef' delays (<> [delay])

withEnvironment :: String -> String -> IO a -> IO a
withEnvironment name value =
    bracket
        (lookupEnv name <* setEnv name value)
        (maybe (unsetEnv name) (setEnv name))
        . const
