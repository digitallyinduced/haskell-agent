-- | Restricted-file credential store used by the interactive login manager.
module Agent.CLI.CredentialStore
    ( ManagedAuthKind(..)
    , ManagedCredential(..)
    , ManagedSecret(..)
    , deleteManagedCredential
    , loadManagedCredentials
    , managedCredentialsPath
    , managedSecretsPath
    , newManagedCredentialId
    , setManagedCredentialEnabled
    , updateManagedCredentialSecret
    , upsertManagedCredential
    , upsertManagedCredentialAfterRefresh
    , withCredentialRefreshFileLock
    ) where

import Agent.CLI.Error (formatException)
import Agent.FileRetry (retryOnFileBusy, writeLazyFileAtomically)
import Agent.OsPath (toText, unsafeToFilePath)
import Agent.Provider
    ( BillingMode
    , Provider(..)
    , parseProvider
    , providerSlug
    )
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception.Safe (bracket, bracket_, tryIO)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory.OsPath
    ( createDirectoryIfMissing
    , doesFileExist
    , getHomeDirectory
    )
import System.OsPath (OsPath, takeDirectory, unsafeEncodeUtf, (</>))
import System.IO (SeekMode(AbsoluteSeek))
import System.Posix.Files (setFileMode)
import System.Posix.IO
    ( LockRequest(Unlock, WriteLock)
    , OpenFileFlags(..)
    , OpenMode(ReadWrite)
    , closeFd
    , defaultFileFlags
    , openFd
    , setLock
    , waitToSetLock
    )
import System.IO.Unsafe (unsafePerformIO)

data ManagedAuthKind
    = ManagedBearerToken
    | ManagedOpenAIAuthJson
    | ManagedGrokAuthJson
    deriving (Eq, Show)

data ManagedCredential = ManagedCredential
    { managedId :: !Text
    , managedProvider :: !Provider
    , managedAccountId :: !Text
    , managedLabel :: !Text
    , managedBilling :: !BillingMode
    , managedAuthKind :: !ManagedAuthKind
    , managedEnabled :: !Bool
    }
    deriving (Eq, Show)

data ManagedSecret = ManagedSecret
    { secretManagedId :: !Text
    , secretPayload :: !Text
    }
    deriving (Eq)

data ManagedCredentialEntry = ManagedCredentialEntry
    { entryManagedId :: !Text
    , entryCredential :: !ManagedCredential
    , entrySecret :: !ManagedSecret
    }

newtype ManagedCredentialStore = ManagedCredentialStore
    { storeEntries :: [ManagedCredentialEntry]
    }

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

managedCredentialsPath :: OsPath -> OsPath
managedCredentialsPath home =
    home
        </> unsafeEncodeUtf ".haskell-agent"
        </> unsafeEncodeUtf "credentials"
        </> unsafeEncodeUtf "accounts.json"

managedSecretsPath :: OsPath -> OsPath
managedSecretsPath home =
    home
        </> unsafeEncodeUtf ".haskell-agent"
        </> unsafeEncodeUtf "credentials"
        </> unsafeEncodeUtf "secrets.json"

-- | Serialize OAuth rotation across threads and harness processes. POSIX
-- record locks are released automatically if a process exits.
withCredentialRefreshFileLock :: IO value -> IO value
withCredentialRefreshFileLock action =
    withMVar credentialRefreshThreadLock
        (const (withCredentialRefreshFileLockUnlocked action))

withCredentialRefreshFileLockUnlocked :: IO value -> IO value
withCredentialRefreshFileLockUnlocked action = do
    home <- getHomeDirectory
    let directory = takeDirectory (managedSecretsPath home)
        lockPath = directory </> unsafeEncodeUtf "refresh.lock"
    createDirectoryIfMissing True directory
    setFileMode (unsafeToFilePath directory) 0o700
    bracket
        (openFd (unsafeToFilePath lockPath) ReadWrite lockFlags)
        closeFd
        \fd -> do
            setFileMode (unsafeToFilePath lockPath) 0o600
            waitToSetLock fd (WriteLock, AbsoluteSeek, 0, 0)
            action
  where
    lockFlags = defaultFileFlags
        { creat = Just 0o600
        , cloexec = True
        }

credentialRefreshThreadLock :: MVar ()
credentialRefreshThreadLock = unsafePerformIO (newMVar ())
{-# NOINLINE credentialRefreshThreadLock #-}

managedCredentialsLockPath :: OsPath -> OsPath
managedCredentialsLockPath home =
    home
        </> unsafeEncodeUtf ".haskell-agent"
        </> unsafeEncodeUtf "credentials"
        </> unsafeEncodeUtf "store.lock"

loadManagedCredentials
    :: IO (Either Text [(ManagedCredential, ManagedSecret)])
loadManagedCredentials = do
    home <- getHomeDirectory
    withCredentialStoreLock home (loadManagedCredentialsUnlocked home)

loadManagedCredentialsUnlocked
    :: OsPath
    -> IO (Either Text [(ManagedCredential, ManagedSecret)])
loadManagedCredentialsUnlocked home =
    fmap (fmap managedCredentialPairs)
        (loadManagedCredentialStoreUnlocked home)

loadManagedCredentialStoreUnlocked
    :: OsPath
    -> IO (Either Text ManagedCredentialStore)
loadManagedCredentialStoreUnlocked home = do
    metadataResult <- decodeFileOrEmpty
        (managedCredentialsPath home)
        (MetadataFile 1 [])
    secretsResult <- decodeFileOrEmpty
        (managedSecretsPath home)
        (SecretsFile 1 [])
    pure do
        metadata <- metadataResult
        secrets <- secretsResult
        ManagedCredentialStore
            <$> traverse
                (attachSecret secrets.storedSecrets)
                metadata.metadataAccounts
  where
    attachSecret secrets credential =
        case find
            ((== credential.managedId) . (.secretManagedId))
            secrets of
            Nothing ->
                Left
                    ("missing secret for managed credential "
                        <> credential.managedId)
            Just secret -> managedCredentialEntry credential secret

upsertManagedCredential
    :: ManagedCredential
    -> ManagedSecret
    -> IO (Either Text ())
upsertManagedCredential credential secret =
    case managedCredentialEntry credential secret of
        Left err -> pure (Left err)
        Right entry -> mutateStore (upsertStoreEntry entry)

-- | Persist a rotated OAuth secret before derived metadata. If the process
-- exits between writes, the new one-time refresh token is already durable.
upsertManagedCredentialAfterRefresh
    :: ManagedCredential
    -> ManagedSecret
    -> IO (Either Text ())
upsertManagedCredentialAfterRefresh credential secret =
    case managedCredentialEntry credential secret of
        Left err -> pure (Left err)
        Right entry -> do
            home <- getHomeDirectory
            withCredentialStoreLock home do
                loaded <- loadManagedCredentialStoreUnlocked home
                case loaded of
                    Left err -> pure (Left err)
                    Right store -> do
                        let store' = upsertStoreEntry entry store
                        writePrivateJson
                            (managedSecretsPath home)
                            (secretsFile store')
                            >>= \case
                                Left err -> pure (Left err)
                                Right () ->
                                    writePrivateJson
                                        (managedCredentialsPath home)
                                        (metadataFile store')

setManagedCredentialEnabled :: Text -> Bool -> IO (Either Text ())
setManagedCredentialEnabled credentialId enabled =
    mutateStore $
        mapStoreEntries \entry ->
            if entry.entryManagedId == credentialId
                then entry
                    { entryCredential =
                        entry.entryCredential { managedEnabled = enabled }
                    }
                else entry

updateManagedCredentialSecret :: Text -> Text -> IO (Either Text ())
updateManagedCredentialSecret credentialId payload = do
    home <- getHomeDirectory
    withCredentialStoreLock home do
        loaded <- loadManagedCredentialStoreUnlocked home
        case loaded of
            Left err -> pure (Left err)
            Right store ->
                case updateStoreEntries credentialId updateSecret store of
                    Nothing -> pure $ Left
                        ("managed credential secret " <> credentialId
                            <> " no longer exists")
                    Just store' ->
                        writePrivateJson
                            (managedSecretsPath home)
                            (secretsFile store')
  where
    updateSecret entry =
        entry
            { entrySecret =
                entry.entrySecret { secretPayload = payload }
            }

deleteManagedCredential :: Text -> IO (Either Text ())
deleteManagedCredential credentialId =
    mutateStore (deleteStoreEntries credentialId)

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
    :: (ManagedCredentialStore -> ManagedCredentialStore)
    -> IO (Either Text ())
mutateStore update = do
    home <- getHomeDirectory
    withCredentialStoreLock home do
        loaded <- loadManagedCredentialStoreUnlocked home
        case loaded of
            Left err -> pure (Left err)
            Right store -> do
                let store' = update store
                writeResult <- writePrivateJson
                    (managedCredentialsPath home)
                    (metadataFile store')
                case writeResult of
                    Left err -> pure (Left err)
                    Right () ->
                        writePrivateJson
                            (managedSecretsPath home)
                            (secretsFile store')

withCredentialStoreLock :: OsPath -> IO a -> IO a
withCredentialStoreLock home action =
    withMVar credentialStoreProcessLock \_ ->
        withCredentialStoreFileLock home action

withCredentialStoreFileLock :: OsPath -> IO a -> IO a
withCredentialStoreFileLock home action = do
    let path = managedCredentialsLockPath home
        directoryPath = takeDirectory path
        lock = (WriteLock, AbsoluteSeek, 0, 0)
        unlock = (Unlock, AbsoluteSeek, 0, 0)
        flags = defaultFileFlags
            { creat = Just 0o600
            , cloexec = True
            }
    createDirectoryIfMissing True directoryPath
    setFileMode (unsafeToFilePath directoryPath) 0o700
    bracket
        (openFd (unsafeToFilePath path) ReadWrite flags)
        closeFd
        (\fd ->
            bracket_
                (waitToSetLock fd lock)
                (setLock fd unlock)
                action)

credentialStoreProcessLock :: MVar ()
credentialStoreProcessLock = unsafePerformIO (newMVar ())
{-# NOINLINE credentialStoreProcessLock #-}

decodeFileOrEmpty :: Aeson.FromJSON value => OsPath -> value -> IO (Either Text value)
decodeFileOrEmpty path empty = do
    exists <- doesFileExist path
    if not exists
        then pure (Right empty)
        else tryIO (retryOnFileBusy (LBS.readFile (unsafeToFilePath path))) >>= \case
            Left exception ->
                pure $ Left
                    ("could not read " <> toText path <> ": "
                        <> formatException exception)
            Right bytes -> pure case Aeson.eitherDecode bytes of
                Left err ->
                    Left
                        ("invalid credential store " <> toText path <> ": "
                            <> Text.pack err)
                Right value -> Right value

writePrivateJson :: Aeson.ToJSON value => OsPath -> value -> IO (Either Text ())
writePrivateJson path value =
    tryIO action >>= \case
        Left exception ->
            pure $ Left
                ("could not write " <> toText path <> ": "
                    <> formatException exception)
        Right () -> pure (Right ())
  where
    action = do
        createDirectoryIfMissing True (takeDirectory path)
        setFileMode (unsafeToFilePath (takeDirectory path)) 0o700
        writeLazyFileAtomically path 0o600 (Aeson.encode value)

managedCredentialEntry
    :: ManagedCredential
    -> ManagedSecret
    -> Either Text ManagedCredentialEntry
managedCredentialEntry credential secret
    | credential.managedId /= secret.secretManagedId =
        Left "managed credential metadata and secret ids do not match"
    | otherwise =
        Right ManagedCredentialEntry
            { entryManagedId = credential.managedId
            , entryCredential = credential
            , entrySecret = secret
            }

managedCredentialPairs
    :: ManagedCredentialStore
    -> [(ManagedCredential, ManagedSecret)]
managedCredentialPairs store =
    map
        (\entry -> (entry.entryCredential, entry.entrySecret))
        store.storeEntries

metadataFile :: ManagedCredentialStore -> MetadataFile
metadataFile store =
    MetadataFile 1 (map (.entryCredential) store.storeEntries)

secretsFile :: ManagedCredentialStore -> SecretsFile
secretsFile store =
    SecretsFile 1 (map (.entrySecret) store.storeEntries)

upsertStoreEntry
    :: ManagedCredentialEntry
    -> ManagedCredentialStore
    -> ManagedCredentialStore
upsertStoreEntry entry store =
    ManagedCredentialStore
        (entry : filter
            ((/= entry.entryManagedId) . (.entryManagedId))
            store.storeEntries)

mapStoreEntries
    :: (ManagedCredentialEntry -> ManagedCredentialEntry)
    -> ManagedCredentialStore
    -> ManagedCredentialStore
mapStoreEntries update store =
    ManagedCredentialStore (map update store.storeEntries)

updateStoreEntries
    :: Text
    -> (ManagedCredentialEntry -> ManagedCredentialEntry)
    -> ManagedCredentialStore
    -> Maybe ManagedCredentialStore
updateStoreEntries credentialId update store
    | any ((== credentialId) . (.entryManagedId)) store.storeEntries =
        Just $ mapStoreEntries
            (\entry ->
                if entry.entryManagedId == credentialId
                    then update entry
                    else entry)
            store
    | otherwise = Nothing

deleteStoreEntries :: Text -> ManagedCredentialStore -> ManagedCredentialStore
deleteStoreEntries credentialId store =
    ManagedCredentialStore
        (filter
            ((/= credentialId) . (.entryManagedId))
            store.storeEntries)
