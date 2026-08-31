module Agent.CLI.CredentialStoreSpec (spec) where

import Agent.CLI.CredentialStore
import Agent.Json (RawJson, rawJsonDecoder)
import qualified Agent.Json.Decode as Json
import Agent.Provider (BillingMode(..), Provider(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, wait)
import Control.Exception.Safe (SomeException, bracket, try)
import Control.Monad (void)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Bits ((.&.))
import Data.List (isInfixOf)
import qualified Data.Text as Text
import System.OsPath
    ( OsPath
    , decodeUtf
    , takeDirectory
    , unsafeEncodeUtf
    , (</>)
    )
import System.Directory
    ( createDirectory
    , doesFileExist
    , getTemporaryDirectory
    , removeFile
    , removePathForcibly
    )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (hClose, openTempFile)
import System.Posix.Files (fileMode, getFileStatus)
import System.Posix.Process (forkProcess, getProcessStatus)
import System.Posix.Signals (sigKILL, signalProcess)
import Test.Hspec

fromFilePath = unsafeEncodeUtf
toFilePath path = either (error . show) id (decodeUtf path)

spec :: Spec
spec =
    describe "managed credential store" do
        it "round-trips metadata and a separately stored secret" $
            withTempHome \home -> do
                upsertManagedCredential account secret `shouldReturn` Right ()
                loadManagedCredentials `shouldReturn` Right [(account, secret)]

                metadata <- LBS.readFile (toFilePath (managedCredentialsPath home))
                secrets <- LBS.readFile (toFilePath (managedSecretsPath home))
                LBS.unpack metadata `shouldNotSatisfy` isInfixOf "super-secret"
                LBS.unpack secrets `shouldSatisfy` isInfixOf "super-secret"

        it "round-trips managed Gemini OAuth metadata" $
            withTempHome \_ -> do
                let geminiAccount = account
                        { managedId = "gemini-google"
                        , managedProvider = GeminiProvider
                        , managedAccountId = "person@example.com"
                        , managedBilling = SubscriptionBilled
                        , managedAuthKind = ManagedGeminiAuthJson
                        }
                    geminiSecret = secret
                        { secretManagedId = "gemini-google"
                        , secretPayload = "{\"access_token\":\"redacted\"}"
                        }
                upsertManagedCredential geminiAccount geminiSecret
                    `shouldReturn` Right ()
                loadManagedCredentials
                    `shouldReturn` Right [(geminiAccount, geminiSecret)]

        it "writes private directories and files" $
            withTempHome \home -> do
                upsertManagedCredential account secret `shouldReturn` Right ()
                withCredentialRefreshFileLock (pure ())
                let directory = takeDirectory (managedCredentialsPath home)
                    storeLock = directory </> fromFilePath "store.lock"
                    refreshLock = directory </> fromFilePath "refresh.lock"
                directoryStatus <- getFileStatus (toFilePath directory)
                metadataStatus <- getFileStatus (toFilePath (managedCredentialsPath home))
                secretStatus <- getFileStatus (toFilePath (managedSecretsPath home))
                storeLockStatus <- getFileStatus (toFilePath storeLock)
                refreshLockStatus <- getFileStatus (toFilePath refreshLock)
                fileMode directoryStatus .&. 0o777 `shouldBe` 0o700
                fileMode metadataStatus .&. 0o777 `shouldBe` 0o600
                fileMode secretStatus .&. 0o777 `shouldBe` 0o600
                fileMode storeLockStatus .&. 0o777 `shouldBe` 0o600
                fileMode refreshLockStatus .&. 0o777 `shouldBe` 0o600

        it "serializes refreshes across processes" $
            withTempHome \home -> do
                let directory = takeDirectory (managedCredentialsPath home)
                    locked = toFilePath (directory </> fromFilePath "child-locked")
                    release = toFilePath (directory </> fromFilePath "release-child")
                    acquired = toFilePath (directory </> fromFilePath "parent-acquired")
                bracket
                    (forkProcess $
                        withCredentialRefreshFileLock do
                            writeFile locked ""
                            waitForFile release)
                    (\pid -> do
                        writeFile release ""
                        void $ try @_ @SomeException (signalProcess sigKILL pid)
                        void $ try @_ @SomeException
                            (getProcessStatus False False pid))
                    \pid -> do
                        waitForFile locked
                        withAsync
                            (withCredentialRefreshFileLock
                                (writeFile acquired ""))
                            \waiting -> do
                                threadDelay 100000
                                doesFileExist acquired `shouldReturn` False
                                writeFile release ""
                                _ <- getProcessStatus True False pid
                                wait waiting
                                doesFileExist acquired `shouldReturn` True

        it "enables, disables, and deletes a managed credential" $
            withTempHome \_ -> do
                upsertManagedCredential account secret `shouldReturn` Right ()
                setManagedCredentialEnabled account.managedId False
                    `shouldReturn` Right ()
                loaded <- loadManagedCredentials
                fmap (map ((.managedEnabled) . fst)) loaded
                    `shouldBe` Right [False]
                deleteManagedCredential account.managedId
                    `shouldReturn` Right ()
                loadManagedCredentials `shouldReturn` Right []

        it "does not resurrect a credential disabled or deleted during refresh" $
            withTempHome \_ -> do
                let rotated = secret { secretPayload = "rotated-secret" }
                upsertManagedCredential account secret `shouldReturn` Right ()
                setManagedCredentialEnabled account.managedId False
                    `shouldReturn` Right ()
                upsertManagedCredentialAfterRefresh account rotated
                    `shouldReturn` Left
                        "managed credential openrouter-test is disabled"
                loadManagedCredentials `shouldReturn`
                    Right
                        [ ( account { managedEnabled = False }
                          , secret
                          )
                        ]
                deleteManagedCredential account.managedId
                    `shouldReturn` Right ()
                upsertManagedCredentialAfterRefresh account rotated
                    `shouldReturn` Left
                        "managed credential openrouter-test no longer exists during refresh"
                loadManagedCredentials `shouldReturn` Right []

        it "updates one secret without replacing sibling accounts" $
            withTempHome \_ -> do
                upsertManagedCredential account secret `shouldReturn` Right ()
                upsertManagedCredential sibling siblingSecret
                    `shouldReturn` Right ()
                updateManagedCredentialSecret account.managedId "rotated-secret"
                    `shouldReturn` Right ()
                loaded <- loadManagedCredentials
                loaded `shouldBe` Right
                    [ (sibling, siblingSecret)
                    , (account, secret { secretPayload = "rotated-secret" })
                    ]

        it "associates secrets by id while preserving metadata order" $
            withTempHome \home -> do
                upsertManagedCredential account secret `shouldReturn` Right ()
                upsertManagedCredential sibling siblingSecret
                    `shouldReturn` Right ()
                writeStoreFiles
                    home
                    [account, sibling]
                    [siblingSecret, secret]

                loadManagedCredentials `shouldReturn` Right
                    [ (account, secret)
                    , (sibling, siblingSecret)
                    ]

        it "re-upserts one paired entry at the front of both files" $
            withTempHome \home -> do
                let updatedAccount = account { managedLabel = "Updated" }
                    updatedSecret =
                        secret { secretPayload = "updated-secret" }
                upsertManagedCredential account secret `shouldReturn` Right ()
                upsertManagedCredential sibling siblingSecret
                    `shouldReturn` Right ()
                upsertManagedCredential updatedAccount updatedSecret
                    `shouldReturn` Right ()

                loadManagedCredentials `shouldReturn` Right
                    [ (updatedAccount, updatedSecret)
                    , (sibling, siblingSecret)
                    ]
                readMetadataSnapshot home
                    `shouldReturn`
                        Right
                            (MetadataSnapshot
                                1
                                (map rawJsonValue [updatedAccount, sibling]))
                readSecretsSnapshot home
                    `shouldReturn`
                        Right
                            (SecretsSnapshot
                                1
                                (map rawJsonValue [updatedSecret, siblingSecret]))

        it "rejects mismatched metadata and secret ids" $
            withTempHome \_ ->
                upsertManagedCredential
                    account
                    secret { secretManagedId = "different-id" }
                    `shouldReturn` Left
                        "managed credential metadata and secret ids do not match"

account :: ManagedCredential
account = ManagedCredential
    { managedId = "openrouter-test"
    , managedProvider = OpenRouterProvider
    , managedAccountId = "account"
    , managedLabel = "Test"
    , managedBilling = ApiBilled
    , managedAuthKind = ManagedBearerToken
    , managedEnabled = True
    }

secret :: ManagedSecret
secret = ManagedSecret
    { secretManagedId = account.managedId
    , secretPayload = "super-secret"
    }

sibling :: ManagedCredential
sibling = account
    { managedId = "openrouter-sibling"
    , managedAccountId = "sibling"
    }

siblingSecret :: ManagedSecret
siblingSecret = ManagedSecret
    { secretManagedId = sibling.managedId
    , secretPayload = "sibling-secret"
    }

data MetadataSnapshot = MetadataSnapshot
    !Int
    ![RawJson]
    deriving (Eq, Show)

data SecretsSnapshot = SecretsSnapshot
    !Int
    ![RawJson]
    deriving (Eq, Show)

writeStoreFiles
    :: OsPath
    -> [ManagedCredential]
    -> [ManagedSecret]
    -> IO ()
writeStoreFiles home accounts secrets = do
    LBS.writeFile
        (toFilePath (managedCredentialsPath home))
        (Aeson.encode
            (Aeson.object
                [ "version" .= (1 :: Int)
                , "accounts" .= accounts
                ]))
    LBS.writeFile
        (toFilePath (managedSecretsPath home))
        (Aeson.encode
            (Aeson.object
                [ "version" .= (1 :: Int)
                , "secrets" .= secrets
                ]))

readMetadataSnapshot :: OsPath -> IO (Either String MetadataSnapshot)
readMetadataSnapshot home =
    decodeSnapshot metadataSnapshotDecoder
        <$> LBS.readFile (toFilePath (managedCredentialsPath home))

readSecretsSnapshot :: OsPath -> IO (Either String SecretsSnapshot)
readSecretsSnapshot home =
    decodeSnapshot secretsSnapshotDecoder
        <$> LBS.readFile (toFilePath (managedSecretsPath home))

metadataSnapshotDecoder :: Json.Decoder MetadataSnapshot
metadataSnapshotDecoder =
    Json.object $
        MetadataSnapshot
            <$> Json.atKey "version" Json.int
            <*> Json.atKey "accounts" (Json.list rawJsonDecoder)

secretsSnapshotDecoder :: Json.Decoder SecretsSnapshot
secretsSnapshotDecoder =
    Json.object $
        SecretsSnapshot
            <$> Json.atKey "version" Json.int
            <*> Json.atKey "secrets" (Json.list rawJsonDecoder)

decodeSnapshot
    :: Json.Decoder a
    -> LBS.ByteString
    -> Either String a
decodeSnapshot decoder bytes =
    either (Left . Text.unpack . Json.jsonErrorMessage) Right
        (Json.decodeEither decoder (LBS.toStrict bytes))

rawJsonValue :: Aeson.ToJSON a => a -> RawJson
rawJsonValue value =
    either (error . Text.unpack . Json.jsonErrorMessage) id
        (Json.decodeEither rawJsonDecoder (LBS.toStrict (Aeson.encode value)))

withTempHome :: (OsPath -> IO a) -> IO a
withTempHome action =
    bracket create removePathForcibly \home ->
        bracket
            (do
                old <- lookupEnv "HOME"
                setEnv "HOME" home
                pure old)
            (\case
                Just old -> setEnv "HOME" old
                Nothing -> unsetEnv "HOME")
            (\_ -> action (fromFilePath home))
  where
    create = do
        temporary <- getTemporaryDirectory
        (path, handle) <- openTempFile temporary "agent-credential-store"
        hClose handle
        removeFile path
        createDirectory path
        pure path

waitForFile :: FilePath -> IO ()
waitForFile path = go (100 :: Int)
  where
    go remaining
        | remaining <= 0 =
            expectationFailure ("timed out waiting for " <> path)
        | otherwise =
            doesFileExist path >>= \case
                True -> pure ()
                False -> threadDelay 10000 >> go (remaining - 1)
