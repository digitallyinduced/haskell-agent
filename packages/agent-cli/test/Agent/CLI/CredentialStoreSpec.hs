module Agent.CLI.CredentialStoreSpec (spec) where

import Agent.CLI.CredentialStore
import Agent.Provider (BillingMode(..), Provider(..))
import Control.Exception.Safe (bracket)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.=))
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Bits ((.&.))
import Data.List (isInfixOf)
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf)
import System.Directory
    ( createDirectory
    , getTemporaryDirectory
    , removeFile
    , removePathForcibly
    )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (hClose, openTempFile)
import System.Posix.Files (fileMode, getFileStatus)
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

        it "writes private directories and files" $
            withTempHome \home -> do
                upsertManagedCredential account secret `shouldReturn` Right ()
                metadataStatus <- getFileStatus (toFilePath (managedCredentialsPath home))
                secretStatus <- getFileStatus (toFilePath (managedSecretsPath home))
                fileMode metadataStatus .&. 0o777 `shouldBe` 0o600
                fileMode secretStatus .&. 0o777 `shouldBe` 0o600

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
                                [updatedAccount, sibling])
                readSecretsSnapshot home
                    `shouldReturn`
                        Right
                            (SecretsSnapshot
                                1
                                [updatedSecret, siblingSecret])

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
    ![ManagedCredential]
    deriving (Eq, Show)

instance Aeson.FromJSON MetadataSnapshot where
    parseJSON = Aeson.withObject "Credential metadata" \object ->
        MetadataSnapshot
            <$> object .: "version"
            <*> object .: "accounts"

data SecretsSnapshot = SecretsSnapshot
    !Int
    ![ManagedSecret]
    deriving (Eq, Show)

instance Aeson.FromJSON SecretsSnapshot where
    parseJSON = Aeson.withObject "Credential secrets" \object ->
        SecretsSnapshot
            <$> object .: "version"
            <*> object .: "secrets"

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
    Aeson.eitherDecode
        <$> LBS.readFile (toFilePath (managedCredentialsPath home))

readSecretsSnapshot :: OsPath -> IO (Either String SecretsSnapshot)
readSecretsSnapshot home =
    Aeson.eitherDecode
        <$> LBS.readFile (toFilePath (managedSecretsPath home))

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
