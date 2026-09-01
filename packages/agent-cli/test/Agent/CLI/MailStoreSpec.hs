module Agent.CLI.MailStoreSpec (spec) where

import Agent.CLI.Mail.Store
import Agent.OsPath (unsafeToFilePath)
import Control.Concurrent.Async (concurrently)
import Control.Exception.Safe (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Bits ((.&.))
import Data.Either (isLeft)
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , getTemporaryDirectory
    , removeFile
    , removePathForcibly
    )
import System.IO (hClose, openTempFile)
import System.OsPath (OsPath, unsafeEncodeUtf)
import System.Posix.Files (fileMode, getFileStatus)
import Test.Hspec

spec :: Spec
spec =
    describe "mail account store" do
        it "keeps the metadata mirror secret-free and redacts Show" $
            withTempHome \home -> do
                now <- getCurrentTime
                let account = exampleAccount now
                    secret = MailImapSecret account.mailAccountId "super-secret"
                upsertMailAccountAt home account secret `shouldReturn` Right ()
                loadMailCredentialsAt home
                    `shouldReturn` Right [MailCredential account secret]
                metadata <- LBS.readFile
                    (unsafePath (mailAccountsPath home))
                store <- LBS.readFile
                    (unsafePath (mailStorePath home))
                LBS.unpack metadata `shouldNotSatisfy` isInfixOf "super-secret"
                LBS.unpack store `shouldSatisfy` isInfixOf "super-secret"
                show secret `shouldNotSatisfy` isInfixOf "super-secret"
                doesFileExist (unsafePath (mailSecretsPath home))
                    `shouldReturn` False

                -- The metadata mirror is never authoritative for credentials.
                LBS.writeFile
                    (unsafePath (mailAccountsPath home))
                    "{\"version\":1,\"accounts\":[]}"
                loadMailCredentialsAt home
                    `shouldReturn` Right [MailCredential account secret]

        it "writes owner-only metadata and canonical store files" $
            withTempHome \home -> do
                now <- getCurrentTime
                let account = exampleAccount now
                upsertMailAccountAt home account
                    (MailImapSecret account.mailAccountId "secret")
                    `shouldReturn` Right ()
                metadataMode <- fileMode <$> getFileStatus
                    (unsafePath (mailAccountsPath home))
                storeMode <- fileMode <$> getFileStatus
                    (unsafePath (mailStorePath home))
                metadataMode .&. 0o777 `shouldBe` 0o600
                storeMode .&. 0o777 `shouldBe` 0o600

        it "migrates the legacy split files into one atomic snapshot" $
            withTempHome \home -> do
                now <- getCurrentTime
                let account = exampleAccount now
                    secret = MailImapSecret account.mailAccountId "secret"
                createDirectoryIfMissing True
                    (unsafePath (mailStoreDirectory home))
                LBS.writeFile (unsafePath (mailAccountsPath home)) $
                    Aeson.encode $ Aeson.object
                        [ "version" Aeson..= (1 :: Int)
                        , "accounts" Aeson..= [account]
                        ]
                LBS.writeFile (unsafePath (mailSecretsPath home)) $
                    Aeson.encode $ Aeson.object
                        [ "version" Aeson..= (1 :: Int)
                        , "secrets" Aeson..= [secret]
                        ]
                loadMailCredentialsAt home
                    `shouldReturn` Right [MailCredential account secret]
                setMailAccountEnabledAt home account.mailAccountId False
                    `shouldReturn` Right ()
                doesFileExist (unsafePath (mailStorePath home))
                    `shouldReturn` True
                doesFileExist (unsafePath (mailSecretsPath home))
                    `shouldReturn` False

        it "rejects unsupported canonical store versions" $
            withTempHome \home -> do
                createDirectoryIfMissing True
                    (unsafePath (mailStoreDirectory home))
                LBS.writeFile
                    (unsafePath (mailStorePath home))
                    "{\"version\":2,\"accounts\":[],\"secrets\":[]}"
                loadMailAccountsAt home >>= (`shouldSatisfy` isLeft)

        it "rejects plaintext, invalid hosts, and mismatched secret ids" do
            validateMailImapSettings MailImapSettings
                { mailImapHost = "localhost"
                , mailImapPort = 143
                , mailImapTLSMode = MailStartTLS
                , mailImapUsername = "user"
                } `shouldSatisfy` either (const True) (const False)
            now <- getCurrentTime
            let account = exampleAccount now
            withTempHome \home ->
                upsertMailAccountAt home account
                    (MailImapSecret "different-id" "secret")
                    `shouldReturn` Left "mail account and secret ids do not match"

        it "prevents concurrent connects from duplicating one mailbox identity" $
            withTempHome \home -> do
                now <- getCurrentTime
                let first = exampleAccount now
                    duplicate = first { mailAccountId = "imap-duplicate" }
                results <- concurrently
                    (upsertMailAccountAt home first
                        (MailImapSecret first.mailAccountId "first-secret"))
                    (upsertMailAccountAt home duplicate
                        (MailImapSecret
                            duplicate.mailAccountId
                            "second-secret"))
                length (filter (== Right ()) [fst results, snd results])
                    `shouldBe` 1
                length (filter isLeft [fst results, snd results])
                    `shouldBe` 1
                fmap length <$> loadMailAccountsAt home `shouldReturn` Right 1

        it "rejects whitespace and control injection in IMAP connection fields" do
            let settings = MailImapSettings
                    { mailImapHost = "imap.example.com"
                    , mailImapPort = 993
                    , mailImapTLSMode = MailImplicitTLS
                    , mailImapUsername = "person@example.com"
                    }
            validateMailImapSettings
                (settings { mailImapHost = " imap.example.com" })
                `shouldSatisfy` either (const True) (const False)
            validateMailImapSettings
                (settings { mailImapUsername = "\r\nA001 LOGOUT" })
                `shouldSatisfy` either (const True) (const False)
            validateMailSecret (MailImapSecret "imap-test" "secret\tvalue")
                `shouldSatisfy` either (const True) (const False)

        it "validates provider-specific public OAuth client IDs" do
            validateMailOAuthClientId GmailProvider
                "client.apps.googleusercontent.com"
                `shouldBe` Right ()
            validateMailOAuthClientId GmailProvider "not-a-google-client"
                `shouldSatisfy` either (const True) (const False)
            validateMailOAuthClientId MicrosoftProvider
                "12345678-1234-1234-1234-123456789abc"
                `shouldBe` Right ()
            validateMailOAuthClientId MicrosoftProvider "not-a-uuid"
                `shouldSatisfy` either (const True) (const False)

        it "discovers conservative provider defaults without network access" do
            discoverMailSettings "person@gmail.com"
                `shouldBe` Right (MailOAuthDiscovery GmailProvider)
            discoverMailSettings "person@outlook.com"
                `shouldBe` Right (MailOAuthDiscovery MicrosoftProvider)

        it "does not let a stale token refresh re-enable a disabled account" $
            withTempHome \home -> do
                now <- getCurrentTime
                let account = exampleOAuthAccount now
                    originalSecret = exampleOAuthSecret "old-access-token"
                    refreshedSecret = exampleOAuthSecret "new-access-token"
                upsertMailAccountAt home account originalSecret
                    `shouldReturn` Right ()
                setMailAccountEnabledAt home account.mailAccountId False
                    `shouldReturn` Right ()
                upsertMailAccountAfterRefreshAt home account refreshedSecret
                    `shouldReturn` Left "Mail account is disabled."
                loaded <- loadMailCredentialsAt home
                case loaded of
                    Right [MailCredential current secret] -> do
                        current.mailAccountEnabled `shouldBe` False
                        secret `shouldBe` originalSecret
                    other -> expectationFailure $
                        "unexpected stored credentials: " <> show other

        it "does not resurrect an account deleted during token refresh" $
            withTempHome \home -> do
                now <- getCurrentTime
                let account = exampleOAuthAccount now
                    originalSecret = exampleOAuthSecret "old-access-token"
                    refreshedSecret = exampleOAuthSecret "new-access-token"
                upsertMailAccountAt home account originalSecret
                    `shouldReturn` Right ()
                deleteMailAccountAt home account.mailAccountId
                    `shouldReturn` Right ()
                upsertMailAccountAfterRefreshAt home account refreshedSecret
                    `shouldReturn` Left "Mail account no longer exists."
                loadMailCredentialsAt home `shouldReturn` Right []

        it "does not persist a late provider error over a reconnected account" $
            withTempHome \home -> do
                now <- getCurrentTime
                let account = exampleOAuthAccount now
                    reconnected = account
                        { mailAccountUpdatedAt = addUTCTime 1 now }
                    originalSecret = exampleOAuthSecret "old-access-token"
                    replacementSecret = exampleOAuthSecret "new-access-token"
                upsertMailAccountAt home account originalSecret
                    `shouldReturn` Right ()
                upsertMailAccountAt home reconnected replacementSecret
                    `shouldReturn` Right ()
                setMailAccountStateIfUnchangedAt
                    home
                    account
                    MailNeedsReauthorization
                    (Just "provider_auth_failed")
                    `shouldReturn` Left
                        "Mail account changed while the request was running."
                loaded <- loadMailCredentialsAt home
                case loaded of
                    Right [MailCredential current secret] -> do
                        current.mailAccountState `shouldBe` MailConnected
                        secret `shouldBe` replacementSecret
                    other -> expectationFailure $
                        "unexpected stored credentials: " <> show other

exampleAccount :: UTCTime -> MailAccount
exampleAccount now = MailAccount
    { mailAccountId = "imap-test"
    , mailAccountProvider = ImapProvider
    , mailAccountEmail = "person@example.com"
    , mailAccountLabel = "Example"
    , mailAccountEnabled = True
    , mailAccountState = MailConnected
    , mailAccountImapSettings = Just MailImapSettings
        { mailImapHost = "imap.example.com"
        , mailImapPort = 993
        , mailImapTLSMode = MailImplicitTLS
        , mailImapUsername = "person@example.com"
        }
    , mailAccountOAuthClientId = Nothing
    , mailAccountCreatedAt = now
    , mailAccountUpdatedAt = now
    , mailAccountLastVerifiedAt = Just now
    , mailAccountLastErrorCode = Nothing
    }

exampleOAuthAccount :: UTCTime -> MailAccount
exampleOAuthAccount now =
    (exampleAccount now)
        { mailAccountId = "gmail-test"
        , mailAccountProvider = GmailProvider
        , mailAccountEmail = "person@gmail.com"
        , mailAccountImapSettings = Nothing
        , mailAccountOAuthClientId = Just
            "client.apps.googleusercontent.com"
        }

exampleOAuthSecret :: Text -> MailSecret
exampleOAuthSecret accessToken = MailOAuthSecret
    { mailSecretAccountId = "gmail-test"
    , mailOAuthAccessToken = accessToken
    , mailOAuthRefreshToken = Just "refresh-token"
    , mailOAuthExpiresAt = Nothing
    , mailOAuthScopes =
        ["https://www.googleapis.com/auth/gmail.readonly"]
    }

withTempHome :: (OsPath -> IO value) -> IO value
withTempHome action =
    bracket make removePathForcibly (action . unsafeEncodeUtf)
  where
    make = do
        temporary <- getTemporaryDirectory
        (path, handle) <- openTempFile temporary "agent-mail-test"
        hClose handle
        removeFile path
        pure path

unsafePath :: OsPath -> FilePath
unsafePath = unsafeToFilePath
