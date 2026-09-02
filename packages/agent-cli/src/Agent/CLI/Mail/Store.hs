-- | Private storage and typed account model for connected mail.
--
-- Metadata is safe for the native settings UI and tool registration. OAuth
-- tokens and IMAP passwords live only in an owner-only atomic snapshot and
-- are never returned by the C ABI. A separate metadata-only mirror contains
-- no secrets. Mutations are protected across threads and processes.
module Agent.CLI.Mail.Store
    ( MailProvider(..)
    , mailProviderSlug
    , parseMailProvider
    , MailTLSMode(..)
    , mailTLSModeSlug
    , MailImapSettings(..)
    , MailAccountState(..)
    , mailAccountStateSlug
    , MailAccount(..)
    , MailSecret(..)
    , MailCredential(..)
    , MailAccountDiscovery(..)
    , mailStoreDirectory
    , mailStorePath
    , mailAccountsPath
    , mailSecretsPath
    , loadMailAccounts
    , loadMailAccountsAt
    , loadMailCredentials
    , loadMailCredentialsAt
    , lookupMailCredential
    , lookupMailCredentialAt
    , newMailAccountId
    , upsertMailAccount
    , upsertMailAccountAt
    , upsertMailAccountAfterRefresh
    , upsertMailAccountAfterRefreshAt
    , setMailAccountEnabled
    , setMailAccountEnabledAt
    , setMailAccountState
    , setMailAccountStateAt
    , setMailAccountStateIfUnchanged
    , setMailAccountStateIfUnchangedAt
    , setMailAccountError
    , setMailAccountErrorAt
    , deleteMailAccount
    , deleteMailAccountAt
    , updateMailSecret
    , updateMailSecretAt
    , withMailRefreshLock
    , normalizeMailEmail
    , discoverMailSettings
    , validateMailAccount
    , validateMailOAuthClientId
    , validateMailImapSettings
    , validateMailSecret
    ) where

import Agent.CLI.Error (formatException)
import Agent.Mail.Types
    ( MailProvider(..), mailProviderSlug, parseMailProvider
    , MailTLSMode(..), mailTLSModeSlug, MailImapSettings(..)
    , MailAccountState(..), mailAccountStateSlug, MailAccount(..)
    , MailSecret(..), MailCredential(..)
    , normalizeMailEmail, validateMailImapSettings, validateMailOAuthClientId
    )
import Agent.CLI.PrivateFileLock (withPrivateFileLock)
import Agent.FileRetry (retryOnFileBusy, writeLazyFileAtomically)
import Agent.OsPath (unsafeToFilePath)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception.Safe (Exception, tryIO)
import Control.Monad (unless, when)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:?), (.=))
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isControl, isSpace)
import Data.Foldable (traverse_)
import Data.List (find, nub, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Entropy (getEntropy)
import System.Directory.OsPath
    ( createDirectoryIfMissing
    , doesFileExist
    , getHomeDirectory
    , removeFile
    )
import System.IO.Unsafe (unsafePerformIO)
import System.OsPath (OsPath, takeDirectory, unsafeEncodeUtf, (</>))
import System.Posix.Files (setFileMode)

data MailAccountDiscovery
    = MailOAuthDiscovery !MailProvider
    | MailImapDiscovery !MailImapSettings
    deriving (Eq, Show)

data MetadataFile = MetadataFile
    { metadataVersion :: !Int
    , metadataAccounts :: ![MailAccount]
    }

data SecretsFile = SecretsFile
    { secretsVersion :: !Int
    , storedSecrets :: ![MailSecret]
    }

data MailStore = MailStore
    { storeMetadata :: !MetadataFile
    , storeSecrets :: !SecretsFile
    }

instance Aeson.ToJSON MailStore where
    toJSON store = Aeson.object
        [ "version" .= (1 :: Int)
        , "accounts" .= store.storeMetadata.metadataAccounts
        , "secrets" .= store.storeSecrets.storedSecrets
        ]

instance Aeson.FromJSON MailStore where
    parseJSON = Aeson.withObject "mail account store" \object -> do
        version <- object .:? "version" Aeson..!= 1
        when (version /= (1 :: Int)) $
            fail "unsupported mail account store version"
        accounts <- object .:? "accounts" Aeson..!= []
        secrets <- object .:? "secrets" Aeson..!= []
        pure MailStore
            { storeMetadata = MetadataFile version accounts
            , storeSecrets = SecretsFile version secrets
            }

instance Aeson.ToJSON MetadataFile where
    toJSON file = Aeson.object
        [ "version" .= file.metadataVersion
        , "accounts" .= file.metadataAccounts
        ]

instance Aeson.FromJSON MetadataFile where
    parseJSON = Aeson.withObject "mail account metadata" \object -> do
        version <- object .:? "version" Aeson..!= 1
        when (version /= (1 :: Int)) $
            fail "unsupported mail account metadata version"
        MetadataFile version <$> object .:? "accounts" Aeson..!= []

instance Aeson.ToJSON SecretsFile where
    toJSON file = Aeson.object
        [ "version" .= file.secretsVersion
        , "secrets" .= file.storedSecrets
        ]

instance Aeson.FromJSON SecretsFile where
    parseJSON = Aeson.withObject "mail account secrets" \object -> do
        version <- object .:? "version" Aeson..!= 1
        when (version /= (1 :: Int)) $
            fail "unsupported mail account secrets version"
        SecretsFile version <$> object .:? "secrets" Aeson..!= []

mailStoreDirectory :: OsPath -> OsPath
mailStoreDirectory home =
    home </> unsafeEncodeUtf ".haskell-agent" </> unsafeEncodeUtf "mail"

-- | The authoritative account+credential snapshot. Keeping the related values
-- in one atomically replaced, owner-only file prevents a crash during
-- reconnect from pairing a new password with stale server metadata.
mailStorePath :: OsPath -> OsPath
mailStorePath home =
    mailStoreDirectory home </> unsafeEncodeUtf "store.json"

-- | Non-sensitive metadata mirror for inspection and compatibility. Runtime
-- reads use 'mailStorePath' once it exists.
mailAccountsPath :: OsPath -> OsPath
mailAccountsPath home =
    mailStoreDirectory home </> unsafeEncodeUtf "accounts.json"

-- | Legacy pre-atomic credential file. It is read only for migration and
-- removed after the canonical store is written successfully.
mailSecretsPath :: OsPath -> OsPath
mailSecretsPath home =
    mailStoreDirectory home </> unsafeEncodeUtf "secrets.json"

storeLockPath :: OsPath -> OsPath
storeLockPath home =
    mailStoreDirectory home </> unsafeEncodeUtf "store.lock"

refreshLockPath :: OsPath -> OsPath
refreshLockPath home =
    mailStoreDirectory home </> unsafeEncodeUtf "refresh.lock"

loadMailAccounts :: IO (Either Text [MailAccount])
loadMailAccounts = getHomeDirectory >>= loadMailAccountsAt

loadMailAccountsAt :: OsPath -> IO (Either Text [MailAccount])
loadMailAccountsAt home =
    withStoreLock home do
        decoded <- loadStoreUnlocked home
        pure do
            store <- decoded
            let metadata = store.storeMetadata
            validateAccounts metadata.metadataAccounts
            pure (sortOn (.mailAccountId) metadata.metadataAccounts)

loadMailCredentials :: IO (Either Text [MailCredential])
loadMailCredentials = getHomeDirectory >>= loadMailCredentialsAt

loadMailCredentialsAt :: OsPath -> IO (Either Text [MailCredential])
loadMailCredentialsAt home =
    withStoreLock home do
        loaded <- loadStoreUnlocked home
        pure (loaded >>= credentialsFromStore)

lookupMailCredential :: Text -> IO (Either Text (Maybe MailCredential))
lookupMailCredential accountId =
    getHomeDirectory >>= \home -> lookupMailCredentialAt home accountId

lookupMailCredentialAt
    :: OsPath
    -> Text
    -> IO (Either Text (Maybe MailCredential))
lookupMailCredentialAt home rawAccountId =
    fmap
        (fmap (find ((== accountId) . (.mailCredentialAccount.mailAccountId))))
        (loadMailCredentialsAt home)
  where
    accountId = Text.strip rawAccountId

newMailAccountId :: MailProvider -> IO Text
newMailAccountId provider = do
    random <- TextEncoding.decodeUtf8 . Base64URL.encodeUnpadded
        <$> getEntropy 18
    pure (mailProviderSlug provider <> "-" <> random)

upsertMailAccount :: MailAccount -> MailSecret -> IO (Either Text ())
upsertMailAccount account secret =
    getHomeDirectory >>= \home -> upsertMailAccountAt home account secret

upsertMailAccountAt
    :: OsPath
    -> MailAccount
    -> MailSecret
    -> IO (Either Text ())
upsertMailAccountAt home account secret =
    case validateCredential account secret of
        Left err -> pure (Left err)
        Right () ->
            withStoreLock home do
                loaded <- loadStoreUnlocked home
                case loaded of
                    Left err -> pure (Left err)
                    Right store -> do
                        let metadata = store.storeMetadata
                            secrets = store.storeSecrets
                            replacement = MailStore
                                (metadata
                                    { metadataAccounts =
                                        upsertAccount account metadata.metadataAccounts
                                    })
                                (secrets
                                    { storedSecrets =
                                        upsertSecret secret secrets.storedSecrets
                                    })
                        case credentialsFromStore replacement of
                            Left err -> pure (Left err)
                            Right _ -> persistStore home replacement

upsertMailAccountAfterRefresh
    :: MailAccount
    -> MailSecret
    -> IO (Either Text ())
upsertMailAccountAfterRefresh account secret =
    getHomeDirectory >>= \home ->
        upsertMailAccountAfterRefreshAt home account secret

upsertMailAccountAfterRefreshAt
    :: OsPath
    -> MailAccount
    -> MailSecret
    -> IO (Either Text ())
upsertMailAccountAfterRefreshAt home refreshed secret =
    case validateCredential refreshed secret of
        Left err -> pure (Left err)
        Right () ->
            withStoreLock home do
                loaded <- loadStoreUnlocked home
                case loaded of
                    Left err -> pure (Left err)
                    Right store ->
                        case find ((== refreshed.mailAccountId) . (.mailAccountId))
                            store.storeMetadata.metadataAccounts of
                            Nothing ->
                                pure (Left "Mail account no longer exists.")
                            Just current
                                | not current.mailAccountEnabled ->
                                    pure (Left "Mail account is disabled.")
                                | current.mailAccountUpdatedAt
                                    /= refreshed.mailAccountUpdatedAt ->
                                    pure (Left
                                        "Mail account changed while credentials were refreshing.")
                                | otherwise -> do
                                    now <- getCurrentTime
                                    let current' = current
                                            { mailAccountState =
                                                refreshed.mailAccountState
                                            , mailAccountUpdatedAt = now
                                            , mailAccountLastErrorCode =
                                                refreshed.mailAccountLastErrorCode
                                            }
                                        metadata = store.storeMetadata
                                        secrets = store.storeSecrets
                                        replacement = MailStore
                                            (metadata
                                                { metadataAccounts =
                                                    upsertAccount current'
                                                        metadata.metadataAccounts
                                                })
                                            (secrets
                                                { storedSecrets =
                                                    upsertSecret secret
                                                        secrets.storedSecrets
                                                })
                                    case validateCredential current' secret of
                                        Left err -> pure (Left err)
                                        Right () ->
                                            persistStore home replacement

setMailAccountEnabled :: Text -> Bool -> IO (Either Text ())
setMailAccountEnabled accountId enabled =
    getHomeDirectory >>= \home ->
        setMailAccountEnabledAt home accountId enabled

setMailAccountEnabledAt :: OsPath -> Text -> Bool -> IO (Either Text ())
setMailAccountEnabledAt home rawAccountId enabled =
    updateMetadata home rawAccountId \account now ->
        account
            { mailAccountEnabled = enabled
            , mailAccountUpdatedAt = now
            }

setMailAccountState
    :: Text
    -> MailAccountState
    -> Maybe Text
    -> IO (Either Text ())
setMailAccountState accountId state detail =
    getHomeDirectory >>= \home ->
        setMailAccountStateAt home accountId state detail

setMailAccountStateAt
    :: OsPath
    -> Text
    -> MailAccountState
    -> Maybe Text
    -> IO (Either Text ())
setMailAccountStateAt home rawAccountId state detail =
    updateMetadata home rawAccountId \account now ->
        account
            { mailAccountState = state
            , mailAccountUpdatedAt = now
            , mailAccountLastErrorCode = safeErrorCode detail
            }

-- | Persist an operational result only if the account is still the same
-- enabled configuration that initiated the request. This prevents a late
-- provider response from overwriting a concurrent disable or reconnect.
setMailAccountStateIfUnchanged
    :: MailAccount
    -> MailAccountState
    -> Maybe Text
    -> IO (Either Text ())
setMailAccountStateIfUnchanged expected state detail =
    getHomeDirectory >>= \home ->
        setMailAccountStateIfUnchangedAt home expected state detail

setMailAccountStateIfUnchangedAt
    :: OsPath
    -> MailAccount
    -> MailAccountState
    -> Maybe Text
    -> IO (Either Text ())
setMailAccountStateIfUnchangedAt home expected state detail =
    withStoreLock home do
        decoded <- loadStoreUnlocked home
        case decoded of
            Left err -> pure (Left err)
            Right store ->
                let metadata = store.storeMetadata
                in
                case find ((== expected.mailAccountId) . (.mailAccountId))
                    metadata.metadataAccounts of
                    Nothing -> pure (Left "Mail account no longer exists.")
                    Just current
                        | not current.mailAccountEnabled ->
                            pure (Left "Mail account is disabled.")
                        | current.mailAccountUpdatedAt
                            /= expected.mailAccountUpdatedAt ->
                            pure (Left
                                "Mail account changed while the request was running.")
                        | otherwise -> do
                            now <- getCurrentTime
                            let current' = current
                                    { mailAccountState = state
                                    , mailAccountUpdatedAt = now
                                    , mailAccountLastErrorCode =
                                        safeErrorCode detail
                                    }
                                accounts = upsertAccount current'
                                    metadata.metadataAccounts
                            case validateAccounts accounts of
                                Left err -> pure (Left err)
                                Right () -> persistStore home store
                                    { storeMetadata =
                                        metadata { metadataAccounts = accounts }
                                    }

-- | Accept an error from a provider, but retain only a fixed local code. Raw
-- server text can contain an echoed password, token, or untrusted content.
setMailAccountError :: Text -> Maybe Text -> IO (Either Text ())
setMailAccountError accountId detail =
    setMailAccountState accountId MailConnectionError detail

setMailAccountErrorAt
    :: OsPath
    -> Text
    -> Maybe Text
    -> IO (Either Text ())
setMailAccountErrorAt home accountId detail =
    setMailAccountStateAt home accountId MailConnectionError detail

deleteMailAccount :: Text -> IO (Either Text ())
deleteMailAccount accountId =
    getHomeDirectory >>= \home -> deleteMailAccountAt home accountId

deleteMailAccountAt :: OsPath -> Text -> IO (Either Text ())
deleteMailAccountAt home rawAccountId
    | Text.null accountId = pure (Left "mail account id is required")
    | otherwise =
        withStoreLock home do
            loaded <- loadStoreUnlocked home
            case loaded of
                Left err -> pure (Left err)
                Right store
                    | not hasAccount && not hasSecret ->
                            pure (Left "mail account was not found")
                    | otherwise -> do
                        let metadata = store.storeMetadata
                            secrets = store.storeSecrets
                            metadata' = metadata
                                { metadataAccounts = filter
                                    ((/= accountId) . (.mailAccountId))
                                    metadata.metadataAccounts
                                }
                            secrets' = secrets
                                { storedSecrets = filter
                                    ((/= accountId) . (.mailSecretAccountId))
                                    secrets.storedSecrets
                                }
                        persistStore home MailStore
                            { storeMetadata = metadata'
                            , storeSecrets = secrets'
                            }
                  where
                    hasAccount = any ((== accountId) . (.mailAccountId))
                        store.storeMetadata.metadataAccounts
                    hasSecret = any ((== accountId) . (.mailSecretAccountId))
                        store.storeSecrets.storedSecrets
  where
    accountId = Text.strip rawAccountId

updateMailSecret :: Text -> MailSecret -> IO (Either Text ())
updateMailSecret accountId secret =
    getHomeDirectory >>= \home -> updateMailSecretAt home accountId secret

updateMailSecretAt :: OsPath -> Text -> MailSecret -> IO (Either Text ())
updateMailSecretAt home rawAccountId secret
    | accountId /= secret.mailSecretAccountId =
        pure (Left "mail account and secret ids do not match")
    | otherwise =
        withStoreLock home do
            loaded <- loadStoreUnlocked home
            case loaded of
                Left err -> pure (Left err)
                Right store ->
                    case find ((== accountId) . (.mailAccountId))
                        store.storeMetadata.metadataAccounts of
                        Nothing -> pure (Left "mail account was not found")
                        Just account ->
                            case validateCredential account secret of
                                Left err -> pure (Left err)
                                Right () -> persistStore home store
                                    { storeSecrets =
                                        store.storeSecrets
                                            { storedSecrets =
                                                upsertSecret secret
                                                    store.storeSecrets.storedSecrets
                                            }
                                    }
  where
    accountId = Text.strip rawAccountId

withMailRefreshLock :: IO value -> IO value
withMailRefreshLock action = do
    home <- getHomeDirectory
    withMVar refreshProcessLock \_ ->
        withPrivateFileLock (refreshLockPath home) action

refreshProcessLock :: MVar ()
refreshProcessLock = unsafePerformIO (newMVar ())
{-# NOINLINE refreshProcessLock #-}

discoverMailSettings :: Text -> Either Text MailAccountDiscovery
discoverMailSettings rawEmail = do
    email <- normalizeMailEmail rawEmail
    let domain = Text.drop 1 (Text.dropWhile (/= '@') email)
    pure $ case domain of
        "gmail.com" -> MailOAuthDiscovery GmailProvider
        "googlemail.com" -> MailOAuthDiscovery GmailProvider
        "outlook.com" -> MailOAuthDiscovery MicrosoftProvider
        "hotmail.com" -> MailOAuthDiscovery MicrosoftProvider
        "live.com" -> MailOAuthDiscovery MicrosoftProvider
        _ -> MailImapDiscovery MailImapSettings
            { mailImapHost = fromMaybe ("imap." <> domain)
                (lookup domain knownImapHosts)
            , mailImapPort = 993
            , mailImapTLSMode = MailImplicitTLS
            , mailImapUsername = email
            }
  where
    knownImapHosts =
        [ ("icloud.com", "imap.mail.me.com")
        , ("me.com", "imap.mail.me.com")
        , ("mac.com", "imap.mail.me.com")
        , ("fastmail.com", "imap.fastmail.com")
        , ("gmx.de", "imap.gmx.net")
        , ("mailbox.org", "imap.mailbox.org")
        , ("posteo.de", "posteo.de")
        , ("web.de", "imap.web.de")
        ]

validateMailAccount :: MailAccount -> Either Text ()
validateMailAccount account = do
    validateId "mail account id" account.mailAccountId
    _ <- normalizeMailEmail account.mailAccountEmail
    when (Text.length account.mailAccountLabel > 256
        || Text.any isControl account.mailAccountLabel) $
        Left "mail account label is invalid"
    traverse_ validateErrorCode account.mailAccountLastErrorCode
    case account.mailAccountProvider of
        ImapProvider ->
            case account.mailAccountImapSettings of
                Nothing -> Left "custom IMAP account is missing connection settings"
                Just settings -> do
                    validateMailImapSettings settings
                    when (account.mailAccountOAuthClientId /= Nothing) $
                        Left "custom IMAP account must not have an OAuth client id"
        GmailProvider -> validateOAuthMetadata account
        MicrosoftProvider -> validateOAuthMetadata account

validateMailSecret :: MailSecret -> Either Text ()
validateMailSecret = \case
    MailOAuthSecret
        { mailSecretAccountId
        , mailOAuthAccessToken
        , mailOAuthRefreshToken
        , mailOAuthScopes
        } -> do
            validateId "mail secret account id" mailSecretAccountId
            validateSecret "OAuth access token" 65536 mailOAuthAccessToken
            traverse_ (validateSecret "OAuth refresh token" 65536)
                mailOAuthRefreshToken
            when (length mailOAuthScopes > 64
                || any invalidScope mailOAuthScopes) $
                Left "OAuth scopes are invalid"
    MailImapSecret { mailSecretAccountId, mailImapPassword } -> do
        validateId "mail secret account id" mailSecretAccountId
        validateSecret "IMAP password" 8192 mailImapPassword
  where
    invalidScope scope =
        Text.null (Text.strip scope)
            || Text.length scope > 512
            || Text.any isControl scope

validateCredential :: MailAccount -> MailSecret -> Either Text ()
validateCredential account secret = do
    validateMailAccount account
    validateMailSecret secret
    when (account.mailAccountId /= secret.mailSecretAccountId) $
        Left "mail account and secret ids do not match"
    case (account.mailAccountProvider, secret) of
        (GmailProvider, MailOAuthSecret {}) -> pure ()
        (MicrosoftProvider, MailOAuthSecret {}) -> pure ()
        (ImapProvider, MailImapSecret {}) -> pure ()
        (GmailProvider, _) -> Left "Gmail accounts require OAuth credentials"
        (MicrosoftProvider, _) ->
            Left "Microsoft accounts require OAuth credentials"
        (ImapProvider, _) -> Left "custom IMAP accounts require a password"

validateOAuthMetadata :: MailAccount -> Either Text ()
validateOAuthMetadata account = do
    when (account.mailAccountImapSettings /= Nothing) $
        Left "OAuth account must not contain IMAP settings"
    case account.mailAccountOAuthClientId of
        Nothing
            | account.mailAccountState == MailNeedsReauthorization -> pure ()
            | otherwise -> Left "OAuth account is missing its OAuth client id"
        Just clientId ->
            validateMailOAuthClientId account.mailAccountProvider clientId

loadStoreUnlocked :: OsPath -> IO (Either Text MailStore)
loadStoreUnlocked home = do
    canonicalExists <- doesFileExist (mailStorePath home)
    if canonicalExists
        then decodeFileOrEmpty
            (mailStorePath home)
            (MailStore (MetadataFile 1 []) (SecretsFile 1 []))
        else do
            metadata <- decodeFileOrEmpty
                (mailAccountsPath home)
                (MetadataFile 1 [])
            secrets <- decodeFileOrEmpty
                (mailSecretsPath home)
                (SecretsFile 1 [])
            pure (MailStore <$> metadata <*> secrets)

credentialsFromStore :: MailStore -> Either Text [MailCredential]
credentialsFromStore store = do
    let accounts = store.storeMetadata.metadataAccounts
        secrets = store.storeSecrets.storedSecrets
    validateAccounts accounts
    traverse_ validateMailSecret secrets
    ensureDistinct "mail secret account ids" (map (.mailSecretAccountId) secrets)
    sortOn (.mailCredentialAccount.mailAccountId)
        <$> traverse (attachSecret secrets) accounts
  where
    attachSecret secrets account =
        case find ((== account.mailAccountId) . (.mailSecretAccountId)) secrets of
            Nothing -> Left "mail account has no stored credential"
            Just secret -> do
                validateCredential account secret
                pure MailCredential
                    { mailCredentialAccount = account
                    , mailCredentialSecret = secret
                    }

persistStore :: OsPath -> MailStore -> IO (Either Text ())
persistStore home rawStore =
    case credentialsFromStore store of
        Left err -> pure (Left err)
        Right _ ->
            writePrivateJson (mailStorePath home) store >>= \case
                Left err -> pure (Left err)
                Right () -> do
                    -- This mirror never drives runtime access. A failed mirror
                    -- update cannot create a metadata/credential mismatch.
                    _ <- writePrivateJson
                        (mailAccountsPath home)
                        store.storeMetadata
                    removeLegacySecrets home
                    pure (Right ())
  where
    store = removeOrphanSecrets rawStore

removeOrphanSecrets :: MailStore -> MailStore
removeOrphanSecrets store =
    let accountIds =
            map (.mailAccountId) store.storeMetadata.metadataAccounts
    in store
        { storeSecrets = store.storeSecrets
            { storedSecrets = filter
                ((`elem` accountIds) . (.mailSecretAccountId))
                store.storeSecrets.storedSecrets
            }
        }

removeLegacySecrets :: OsPath -> IO ()
removeLegacySecrets home = do
    let path = mailSecretsPath home
    exists <- doesFileExist path
    when exists do
        _ <- tryIO (removeFile path)
        pure ()

updateMetadata
    :: OsPath
    -> Text
    -> (MailAccount -> UTCTime -> MailAccount)
    -> IO (Either Text ())
updateMetadata home rawAccountId update
    | Text.null accountId = pure (Left "mail account id is required")
    | otherwise =
        withStoreLock home do
            decoded <- loadStoreUnlocked home
            case decoded of
                Left err -> pure (Left err)
                Right store
                    | not (any ((== accountId) . (.mailAccountId))
                        store.storeMetadata.metadataAccounts) ->
                            pure (Left "mail account was not found")
                    | otherwise -> do
                        now <- getCurrentTime
                        let metadata = store.storeMetadata
                        let accounts = map
                                (\account ->
                                    if account.mailAccountId == accountId
                                        then update account now
                                        else account)
                                metadata.metadataAccounts
                        case validateAccounts accounts of
                            Left err -> pure (Left err)
                            Right () -> persistStore home store
                                { storeMetadata =
                                    metadata { metadataAccounts = accounts }
                                }
  where
    accountId = Text.strip rawAccountId

withStoreLock :: OsPath -> IO value -> IO value
withStoreLock home action =
    withMVar storeProcessLock \_ ->
        withPrivateFileLock (storeLockPath home) action

storeProcessLock :: MVar ()
storeProcessLock = unsafePerformIO (newMVar ())
{-# NOINLINE storeProcessLock #-}

decodeFileOrEmpty
    :: Aeson.FromJSON value
    => OsPath
    -> value
    -> IO (Either Text value)
decodeFileOrEmpty path fallback = do
    exists <- doesFileExist path
    if not exists
        then pure (Right fallback)
        else
            tryIO action >>= \case
                Left exception ->
                    pure (Left
                        ("could not read mail account storage: "
                            <> boundedException exception))
                Right bytes ->
                    pure $ case Aeson.eitherDecode bytes of
                        Left _ -> Left "mail account storage is invalid"
                        Right value -> Right value
  where
    action = do
        setFileMode (unsafeToFilePath path) 0o600
        retryOnFileBusy (LBS.readFile (unsafeToFilePath path))

writePrivateJson
    :: Aeson.ToJSON value
    => OsPath
    -> value
    -> IO (Either Text ())
writePrivateJson path value =
    tryIO action >>= \case
        Left exception ->
            pure (Left
                ("could not write mail account storage: "
                    <> boundedException exception))
        Right () -> pure (Right ())
  where
    action = do
        createDirectoryIfMissing True (takeDirectory path)
        setFileMode (unsafeToFilePath (takeDirectory path)) 0o700
        writeLazyFileAtomically path 0o600 (Aeson.encode value)

upsertAccount :: MailAccount -> [MailAccount] -> [MailAccount]
upsertAccount account =
    (account :) . filter ((/= account.mailAccountId) . (.mailAccountId))

upsertSecret :: MailSecret -> [MailSecret] -> [MailSecret]
upsertSecret secret =
    (secret :) . filter ((/= secret.mailSecretAccountId) . (.mailSecretAccountId))

validateAccounts :: [MailAccount] -> Either Text ()
validateAccounts accounts = do
    traverse_ validateMailAccount accounts
    ensureDistinct "mail account ids" (map (.mailAccountId) accounts)
    ensureDistinct
        "mail account provider/email identities"
        (map accountIdentity accounts)
  where
    accountIdentity account =
        mailProviderSlug account.mailAccountProvider
            <> "\NUL"
            <> Text.toCaseFold (Text.strip account.mailAccountEmail)

ensureDistinct :: Text -> [Text] -> Either Text ()
ensureDistinct label values =
    unless (length values == length (nub values))
        (Left (label <> " contain duplicates"))

validateId :: Text -> Text -> Either Text ()
validateId label value =
    when (Text.null value || Text.length value > 256
        || Text.any (\character -> isControl character || isSpace character) value) $
        Left (label <> " is invalid")

validateSecret :: Text -> Int -> Text -> Either Text ()
validateSecret label maximumLength value =
    when (Text.null value || Text.length value > maximumLength
        || Text.any isControl value) $
        Left (label <> " is invalid")

validateErrorCode :: Text -> Either Text ()
validateErrorCode code =
    when (code `notElem` safeErrorCodes) $
        Left "mail account error code is invalid"

safeErrorCode :: Maybe Text -> Maybe Text
safeErrorCode = \case
    Nothing -> Nothing
    Just value
        | normalized `elem` safeErrorCodes -> Just normalized
        | otherwise -> Just "connection_failed"
      where
        normalized = Text.toCaseFold (Text.strip value)

safeErrorCodes :: [Text]
safeErrorCodes =
    [ "connection_failed"
    , "imap_auth_failed"
    , "imap_connect_failed"
    , "oauth_client_missing"
    , "oauth_refresh_failed"
    , "provider_auth_failed"
    , "imap_tls_failed"
    , "timeout"
    , "validation_failed"
    ]

boundedException :: Exception exception => exception -> Text
boundedException =
    Text.take 512 . Text.unwords . Text.words . formatException
