-- | Restricted-file credential store used by the interactive login manager.
module Agent.CLI.CredentialStore
    ( ManagedAuthKind(..)
    , ManagedBilling(..)
    , ManagedCredential(..)
    , ManagedSecret(..)
    , deleteManagedCredential
    , loadManagedCredentials
    , managedCredentialsPath
    , managedSecretsPath
    , newManagedCredentialId
    , setManagedCredentialEnabled
    , upsertManagedCredential
    ) where

import Agent.Provider (Provider(..), parseProvider, providerSlug)
import Control.Exception.Safe (tryIO)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , getHomeDirectory
    , renameFile
    )
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (setFileMode)

data ManagedAuthKind
    = ManagedBearerToken
    | ManagedOpenAIAuthJson
    | ManagedGrokAuthJson
    deriving (Eq, Show)

data ManagedBilling
    = ManagedSubscription
    | ManagedApiCredits
    deriving (Eq, Show)

data ManagedCredential = ManagedCredential
    { managedId :: !Text
    , managedProvider :: !Provider
    , managedAccountId :: !Text
    , managedLabel :: !Text
    , managedBilling :: !ManagedBilling
    , managedAuthKind :: !ManagedAuthKind
    , managedEnabled :: !Bool
    }
    deriving (Eq, Show)

data ManagedSecret = ManagedSecret
    { secretManagedId :: !Text
    , secretPayload :: !Text
    }
    deriving (Eq)

instance Show ManagedSecret where
    show secret =
        "ManagedSecret { secretManagedId = "
            <> show secret.secretManagedId
            <> ", secretPayload = <redacted> }"

instance Aeson.ToJSON ManagedAuthKind where
    toJSON = Aeson.String . \case
        ManagedBearerToken -> "bearer"
        ManagedOpenAIAuthJson -> "openai_oauth"
        ManagedGrokAuthJson -> "grok_oauth"

instance Aeson.FromJSON ManagedAuthKind where
    parseJSON = Aeson.withText "ManagedAuthKind" \case
        "bearer" -> pure ManagedBearerToken
        "openai_oauth" -> pure ManagedOpenAIAuthJson
        "grok_oauth" -> pure ManagedGrokAuthJson
        other -> fail ("unknown managed auth kind: " <> Text.unpack other)

instance Aeson.ToJSON ManagedBilling where
    toJSON = Aeson.String . \case
        ManagedSubscription -> "subscription"
        ManagedApiCredits -> "api_credits"

instance Aeson.FromJSON ManagedBilling where
    parseJSON = Aeson.withText "ManagedBilling" \case
        "subscription" -> pure ManagedSubscription
        "api_credits" -> pure ManagedApiCredits
        other -> fail ("unknown managed billing kind: " <> Text.unpack other)

instance Aeson.ToJSON ManagedCredential where
    toJSON credential = Aeson.object
        [ "id" .= credential.managedId
        , "provider" .= providerSlug credential.managedProvider
        , "account_id" .= credential.managedAccountId
        , "label" .= credential.managedLabel
        , "billing" .= credential.managedBilling
        , "auth_kind" .= credential.managedAuthKind
        , "enabled" .= credential.managedEnabled
        ]

instance Aeson.FromJSON ManagedCredential where
    parseJSON = Aeson.withObject "ManagedCredential" \object -> do
        providerText <- object .: "provider"
        managedProvider <- maybe
            (fail ("unknown provider: " <> Text.unpack providerText))
            pure
            (parseProvider providerText)
        ManagedCredential
            <$> object .: "id"
            <*> pure managedProvider
            <*> object .: "account_id"
            <*> object .: "label"
            <*> object .: "billing"
            <*> object .: "auth_kind"
            <*> object .:? "enabled" Aeson..!= True

instance Aeson.ToJSON ManagedSecret where
    toJSON secret = Aeson.object
        [ "id" .= secret.secretManagedId
        , "payload" .= secret.secretPayload
        ]

instance Aeson.FromJSON ManagedSecret where
    parseJSON = Aeson.withObject "ManagedSecret" \object ->
        ManagedSecret
            <$> object .: "id"
            <*> object .: "payload"

data MetadataFile = MetadataFile
    { metadataVersion :: !Int
    , metadataAccounts :: ![ManagedCredential]
    }

instance Aeson.ToJSON MetadataFile where
    toJSON file = Aeson.object
        [ "version" .= file.metadataVersion
        , "accounts" .= file.metadataAccounts
        ]

instance Aeson.FromJSON MetadataFile where
    parseJSON = Aeson.withObject "Credential metadata" \object ->
        MetadataFile
            <$> object .:? "version" Aeson..!= 1
            <*> object .:? "accounts" Aeson..!= []

data SecretsFile = SecretsFile
    { secretsVersion :: !Int
    , storedSecrets :: ![ManagedSecret]
    }

instance Aeson.ToJSON SecretsFile where
    toJSON file = Aeson.object
        [ "version" .= file.secretsVersion
        , "secrets" .= file.storedSecrets
        ]

instance Aeson.FromJSON SecretsFile where
    parseJSON = Aeson.withObject "Credential secrets" \object ->
        SecretsFile
            <$> object .:? "version" Aeson..!= 1
            <*> object .:? "secrets" Aeson..!= []

managedCredentialsPath :: FilePath -> FilePath
managedCredentialsPath home =
    home </> ".haskell-agent" </> "credentials" </> "accounts.json"

managedSecretsPath :: FilePath -> FilePath
managedSecretsPath home =
    home </> ".haskell-agent" </> "credentials" </> "secrets.json"

loadManagedCredentials
    :: IO (Either Text [(ManagedCredential, ManagedSecret)])
loadManagedCredentials = do
    home <- getHomeDirectory
    metadataResult <- decodeFileOrEmpty
        (managedCredentialsPath home)
        (MetadataFile 1 [])
    secretsResult <- decodeFileOrEmpty
        (managedSecretsPath home)
        (SecretsFile 1 [])
    pure do
        metadata <- metadataResult
        secrets <- secretsResult
        traverse (attachSecret secrets.storedSecrets) metadata.metadataAccounts
  where
    attachSecret secrets credential =
        case find
            ((== credential.managedId) . (.secretManagedId))
            secrets of
            Nothing ->
                Left
                    ("missing secret for managed credential "
                        <> credential.managedId)
            Just secret -> Right (credential, secret)

upsertManagedCredential
    :: ManagedCredential
    -> ManagedSecret
    -> IO (Either Text ())
upsertManagedCredential credential secret = mutateStore \accounts secrets ->
    ( upsertBy (.managedId) credential accounts
    , upsertBy (.secretManagedId) secret secrets
    )

setManagedCredentialEnabled :: Text -> Bool -> IO (Either Text ())
setManagedCredentialEnabled credentialId enabled =
    mutateStore \accounts secrets ->
        ( map
            (\account ->
                if account.managedId == credentialId
                    then account { managedEnabled = enabled }
                    else account)
            accounts
        , secrets
        )

deleteManagedCredential :: Text -> IO (Either Text ())
deleteManagedCredential credentialId =
    mutateStore \accounts secrets ->
        ( filter ((/= credentialId) . (.managedId)) accounts
        , filter ((/= credentialId) . (.secretManagedId)) secrets
        )

newManagedCredentialId :: Provider -> Text -> IO Text
newManagedCredentialId provider accountId = do
    micros <- floor . (* 1_000_000) <$> getPOSIXTime :: IO Integer
    let suffix
            | Text.null (Text.strip accountId) = Text.pack (show micros)
            | otherwise =
                Text.take 24 $
                    Text.map
                        (\c -> if allowed c then c else '-')
                        accountId
    pure (providerSlug provider <> "-" <> suffix <> "-" <> Text.pack (show micros))
  where
    allowed c =
        (c >= 'a' && c <= 'z')
            || (c >= 'A' && c <= 'Z')
            || (c >= '0' && c <= '9')

mutateStore
    :: ([ManagedCredential] -> [ManagedSecret] -> ([ManagedCredential], [ManagedSecret]))
    -> IO (Either Text ())
mutateStore update = do
    home <- getHomeDirectory
    loaded <- loadManagedCredentials
    case loaded of
        Left err -> pure (Left err)
        Right entries -> do
            let (accounts, secrets) = unzip entries
                (accounts', secrets') = update accounts secrets
            writeResult <- writePrivateJson
                (managedCredentialsPath home)
                (MetadataFile 1 accounts')
            case writeResult of
                Left err -> pure (Left err)
                Right () ->
                    writePrivateJson
                        (managedSecretsPath home)
                        (SecretsFile 1 secrets')

decodeFileOrEmpty :: Aeson.FromJSON value => FilePath -> value -> IO (Either Text value)
decodeFileOrEmpty path empty = do
    exists <- doesFileExist path
    if not exists
        then pure (Right empty)
        else tryIO (LBS.readFile path) >>= \case
            Left exception ->
                pure $ Left
                    ("could not read " <> Text.pack path <> ": "
                        <> Text.pack (show exception))
            Right bytes -> pure case Aeson.eitherDecode bytes of
                Left err ->
                    Left
                        ("invalid credential store " <> Text.pack path <> ": "
                            <> Text.pack err)
                Right value -> Right value

writePrivateJson :: Aeson.ToJSON value => FilePath -> value -> IO (Either Text ())
writePrivateJson path value =
    tryIO action >>= \case
        Left exception ->
            pure $ Left
                ("could not write " <> Text.pack path <> ": "
                    <> Text.pack (show exception))
        Right () -> pure (Right ())
  where
    action = do
        createDirectoryIfMissing True (takeDirectory path)
        setFileMode (takeDirectory path) 0o700
        let temporary = path <> ".tmp"
        LBS.writeFile temporary (Aeson.encode value)
        setFileMode temporary 0o600
        renameFile temporary path

upsertBy :: Eq key => (value -> key) -> value -> [value] -> [value]
upsertBy keyOf value values =
    value : filter ((/= keyOf value) . keyOf) values
