module Agent.CLI.CredentialStoreSpec (spec) where

import Agent.CLI.CredentialStore
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

                metadata <- LBS.readFile (managedCredentialsPath home)
                secrets <- LBS.readFile (managedSecretsPath home)
                LBS.unpack metadata `shouldNotSatisfy` isInfixOf "super-secret"
                LBS.unpack secrets `shouldSatisfy` isInfixOf "super-secret"

        it "writes private directories and files" $
            withTempHome \home -> do
                upsertManagedCredential account secret `shouldReturn` Right ()
                metadataStatus <- getFileStatus (managedCredentialsPath home)
                secretStatus <- getFileStatus (managedSecretsPath home)
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

withTempHome :: (FilePath -> IO a) -> IO a
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
            (\_ -> action home)
  where
    create = do
        temporary <- getTemporaryDirectory
        (path, handle) <- openTempFile temporary "agent-credential-store"
        hClose handle
        removeFile path
        createDirectory path
        pure path
