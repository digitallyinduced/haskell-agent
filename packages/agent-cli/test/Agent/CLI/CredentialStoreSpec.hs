module Agent.CLI.CredentialStoreSpec (spec) where

import Agent.CLI.CredentialStore
import Agent.OsPath (OsPath, fromFilePath, toFilePath)
import Agent.Provider (Provider(..))
import Control.Exception.Safe (bracket)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Bits ((.&.))
import Data.List (isInfixOf)
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
                let sibling = account
                        { managedId = "openrouter-sibling"
                        , managedAccountId = "sibling"
                        }
                    siblingSecret = ManagedSecret
                        { secretManagedId = sibling.managedId
                        , secretPayload = "sibling-secret"
                        }
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
    , managedBilling = ManagedApiCredits
    , managedAuthKind = ManagedBearerToken
    , managedEnabled = True
    }

secret :: ManagedSecret
secret = ManagedSecret
    { secretManagedId = account.managedId
    , secretPayload = "super-secret"
    }

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
