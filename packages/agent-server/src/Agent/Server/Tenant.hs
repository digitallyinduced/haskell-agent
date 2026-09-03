-- | Tenant registry and host-side tenant resource derivation.
module Agent.Server.Tenant
    ( TenantId
    , parseTenantId
    , renderTenantId
    , CredentialId
    , parseCredentialId
    , renderCredentialId
    , Principal(..)
    , AccessBoundary(..)
    , ResolvedTenant(..)
    , TenantRegistry
    , loadTenantRegistry
    , tenantRegistryCredentials
    , tenantRegistryTenants
    , lookupTenant
    , resolveTenantWorkspacePath
    , renderTenantDatabaseName
    , renderTenantRuntimeRole
    ) where

import Agent.Server.Auth
    ( TenantCredential
    , tenantCredential
    )
import Agent.Server.PrivateFile
    ( readPrivateFile
    , readPrivateTokenFile
    , validateTrustedPathAncestry
    )
import Agent.Server.Types
    ( AccessBoundary(..)
    , ApiError(..)
    , CredentialId
    , Principal(..)
    , TenantId
    , localTenantId
    , parseCredentialId
    , parseTenantId
    , renderCredentialId
    , renderTenantId
    )
import Control.Exception.Safe (tryIO)
import Control.Monad (unless)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson
    ( FromJSON(..)
    , Object
    , eitherDecodeStrict'
    , withObject
    , (.:)
    )
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.ByteArray (constEq)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (tails)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory
    ( canonicalizePath
    , createDirectoryIfMissing
    , doesDirectoryExist
    , makeAbsolute
    )
import System.FilePath
    ( (</>)
    , isAbsolute
    , makeRelative
    , normalise
    , splitDirectories
    , takeDirectory
    )
import System.Posix.Files
    ( deviceID
    , fileID
    , getFileStatus
    , isDirectory
    , setFileMode
    )

data RegistryFile = RegistryFile
    { registryFileVersion :: !Int
    , registryFileTenants :: ![TenantSpec]
    }

data TenantSpec = TenantSpec
    { tenantSpecId :: !TenantId
    , tenantSpecCredentials :: ![CredentialSpec]
    , tenantSpecWorkspaceRoot :: !FilePath
    }

data CredentialSpec = CredentialSpec
    { credentialSpecId :: !CredentialId
    , credentialSpecTokenFile :: !FilePath
    }

data ResolvedTenant = ResolvedTenant
    { resolvedTenantId :: !TenantId
    , resolvedTenantWorkspaceRoot :: !FilePath
    , resolvedTenantWorkspaceDevice :: !Integer
    , resolvedTenantWorkspaceInode :: !Integer
    , resolvedTenantHome :: !FilePath
    , resolvedTenantStateDirectory :: !FilePath
    , resolvedTenantDatabase :: !Text
    , resolvedTenantRuntimeRole :: !Text
    }
    deriving (Eq, Show)

data TenantRegistry = TenantRegistry
    { registryTenants :: !(Map TenantId ResolvedTenant)
    , registryCredentials :: ![TenantCredential]
    }

tenantRegistryCredentials :: TenantRegistry -> [TenantCredential]
tenantRegistryCredentials = (.registryCredentials)

tenantRegistryTenants :: TenantRegistry -> [ResolvedTenant]
tenantRegistryTenants = Map.elems . (.registryTenants)

lookupTenant :: TenantRegistry -> TenantId -> Maybe ResolvedTenant
lookupTenant registry tenantId =
    Map.lookup tenantId registry.registryTenants

-- | Load a versioned, owner-only registry.
--
-- Example:
--
-- @
-- {"version":1,"tenants":[{
--   "id":"018f6a14-7d52-7a52-9c00-66d5e7d70334",
--   "workspaceRoot":"/srv/agent/acme/workspace",
--   "credentials":[{
--     "id":"018f6a14-7d52-7a52-9c00-66d5e7d70335",
--     "tokenFile":"/run/credentials/acme-agent-token"
--   }]
-- }]}
-- @
loadTenantRegistry
    :: FilePath
    -- ^ Server-owned tenant state base.
    -> FilePath
    -- ^ Owner-only JSON registry.
    -> IO (Either Text TenantRegistry)
loadTenantRegistry rawStateBase registryPath = do
    prepareStateBase rawStateBase >>= \case
        Left err -> pure (Left err)
        Right stateBase ->
            readPrivateFile (1024 * 1024) registryPath >>= \case
                Left err ->
                    pure (Left ("could not read tenant registry: " <> err))
                Right bytes ->
                    case eitherDecodeStrict' bytes of
                        Left _ ->
                            pure
                                (Left
                                    "the tenant registry is not valid versioned JSON")
                        Right registry ->
                            resolveRegistry
                                stateBase
                                registryPath
                                registry

resolveRegistry
    :: FilePath
    -> FilePath
    -> RegistryFile
    -> IO (Either Text TenantRegistry)
resolveRegistry stateBase registryPath registry
    | registry.registryFileVersion /= 1 =
        pure (Left "the tenant registry version must be 1")
    | null specs =
        pure (Left "the tenant registry must contain at least one tenant")
    | length specs > maximumTenants =
        pure (Left "the tenant registry exceeds the tenant limit")
    | hasDuplicates (map (.tenantSpecId) specs) =
        pure (Left "the tenant registry contains duplicate tenant ids")
    | localTenantId `elem` map (.tenantSpecId) specs =
        pure (Left "the local-mode tenant id is reserved")
    | otherwise = do
        registryCanonical <- canonicalizeExistingFile registryPath
        resolved <- traverse (resolveTenant stateBase) specs
        pure do
            canonicalRegistry <- registryCanonical
            tenantsWithSecrets <- sequence resolved
            validateResolvedPaths
                stateBase
                canonicalRegistry
                tenantsWithSecrets
            let credentials =
                    [ ( spec.credentialSpecId
                      , tenant.resolvedTenantId
                      , token
                      )
                    | (tenant, secrets) <- tenantsWithSecrets
                    , (spec, _, token) <- secrets
                    ]
            unless
                (not (hasDuplicates [identifier | (identifier, _, _) <- credentials]))
                (Left "the tenant registry contains duplicate credential ids")
            validateDistinctTokens credentials
            Right TenantRegistry
                { registryTenants =
                    Map.fromList
                        [ (tenant.resolvedTenantId, tenant)
                        | (tenant, _) <- tenantsWithSecrets
                        ]
                , registryCredentials =
                    [ tenantCredential
                        Principal
                            { principalTenantId = tenantId
                            , principalCredentialId = Just credentialId
                            }
                        token
                    | (credentialId, tenantId, token) <- credentials
                    ]
                }
  where
    specs = registry.registryFileTenants

resolveTenant
    :: FilePath
    -> TenantSpec
    -> IO
        (Either
            Text
            (ResolvedTenant, [(CredentialSpec, FilePath, ByteString)]))
resolveTenant stateBase spec
    | null spec.tenantSpecCredentials =
        pure (Left "every tenant requires at least one credential")
    | length spec.tenantSpecCredentials > maximumCredentialsPerTenant =
        pure (Left "a tenant exceeds the credential limit")
    | otherwise = do
        workspace <-
            canonicalWorkspace spec.tenantSpecWorkspaceRoot
        secrets <-
            traverse
                loadCredential
                spec.tenantSpecCredentials
        pure do
            (root, workspaceDevice, workspaceInode) <- workspace
            loadedSecrets <- sequence secrets
            let identifier = Text.unpack (renderTenantId spec.tenantSpecId)
                tenantBase = stateBase </> identifier
            Right
                ( ResolvedTenant
                    { resolvedTenantId = spec.tenantSpecId
                    , resolvedTenantWorkspaceRoot = root
                    , resolvedTenantWorkspaceDevice = workspaceDevice
                    , resolvedTenantWorkspaceInode = workspaceInode
                    , resolvedTenantHome = tenantBase </> "home"
                    , resolvedTenantStateDirectory = tenantBase </> "state"
                    , resolvedTenantDatabase =
                        renderTenantDatabaseName spec.tenantSpecId
                    , resolvedTenantRuntimeRole =
                        renderTenantRuntimeRole spec.tenantSpecId
                    }
                , loadedSecrets
                )
  where
    loadCredential credential = do
        canonical <-
            canonicalizeExistingFile credential.credentialSpecTokenFile
        token <- readPrivateTokenFile credential.credentialSpecTokenFile
        pure do
            canonicalPath <- canonical
            secret <- firstCredentialError token
            validatedSecret <- validateTenantCredential secret
            Right (credential, canonicalPath, validatedSecret)

validateResolvedPaths
    :: FilePath
    -> FilePath
    -> [(ResolvedTenant, [(CredentialSpec, FilePath, ByteString)])]
    -> Either Text ()
validateResolvedPaths stateBase registryPath tenants
    | any overlapsTenantRoots tenantPairs =
        Left "tenant workspace roots must not overlap"
    | any (pathsOverlap stateBase) roots =
        Left "tenant workspace roots must not overlap server state"
    | containsPath stateBase registryPath =
        Left "the tenant registry must not be inside server state"
    | any (containsPath stateBase) credentialPaths =
        Left "tenant credential files must not be inside server state"
    | any (`containsPath` registryPath) roots =
        Left "the tenant registry must not be inside a tenant workspace"
    | any credentialInsideWorkspace credentialPaths =
        Left "tenant credential files must not be inside a tenant workspace"
    | otherwise = Right ()
  where
    roots =
        map (\(tenant, _) -> tenant.resolvedTenantWorkspaceRoot) tenants
    tenantPairs =
        [ (left, right)
        | left : remaining <- tails roots
        , right <- remaining
        ]
    credentialPaths =
        [ path
        | (_, credentials) <- tenants
        , (_, path, _) <- credentials
        ]
    overlapsTenantRoots (left, right) = pathsOverlap left right
    credentialInsideWorkspace path =
        any (`containsPath` path) roots

validateDistinctTokens
    :: [(CredentialId, TenantId, ByteString)]
    -> Either Text ()
validateDistinctTokens credentials
    | any equalPair
        [ (left, right)
        | left : remaining <- tails credentials
        , right <- remaining
        ] =
            Left "the tenant registry contains duplicate credentials"
    | otherwise = Right ()
  where
    equalPair ((_, _, left), (_, _, right)) =
        constEq
            (hash left :: Digest SHA256)
            (hash right :: Digest SHA256)

resolveTenantWorkspacePath
    :: ResolvedTenant
    -> Maybe FilePath
    -> IO (Either ApiError FilePath)
resolveTenantWorkspacePath tenant requested = do
    let root = tenant.resolvedTenantWorkspaceRoot
        raw = case requested of
            Nothing -> root
            Just path
                | isAbsolute path -> path
                | otherwise -> root </> path
    exists <- doesDirectoryExist raw
    if not exists
        then pure (Left invalidWorkspace)
        else do
            canonical <- tryIO (canonicalizePath =<< makeAbsolute raw)
            pure case canonical of
                Left _ -> Left invalidWorkspace
                Right path
                    | containsPath root path -> Right path
                    | otherwise -> Left ApiError
                        { apiErrorStatus = 403
                        , apiErrorCode = "workspace_not_allowed"
                        , apiErrorMessage =
                            "the working directory is outside the authenticated tenant workspace"
                        , apiErrorDetails = Nothing
                        }
  where
    invalidWorkspace = ApiError
        { apiErrorStatus = 422
        , apiErrorCode = "invalid_workspace"
        , apiErrorMessage =
            "the working directory must be an existing directory"
        , apiErrorDetails = Nothing
        }

renderTenantDatabaseName :: TenantId -> Text
renderTenantDatabaseName tenantId =
    "ha_t_" <> Text.filter (/= '-') (renderTenantId tenantId)

renderTenantRuntimeRole :: TenantId -> Text
renderTenantRuntimeRole tenantId =
    "ha_rt_" <> Text.filter (/= '-') (renderTenantId tenantId)

instance FromJSON RegistryFile where
    parseJSON = withObject "TenantRegistry" \value -> do
        rejectUnknownFields "TenantRegistry" ["version", "tenants"] value
        RegistryFile
            <$> value .: "version"
            <*> value .: "tenants"

instance FromJSON TenantSpec where
    parseJSON = withObject "Tenant" \value -> do
        rejectUnknownFields
            "Tenant"
            ["id", "credentials", "workspaceRoot"]
            value
        rawId <- value .: "id"
        tenantSpecId <-
            either (fail . Text.unpack) pure (parseTenantId rawId)
        tenantSpecCredentials <- value .: "credentials"
        tenantSpecWorkspaceRoot <- value .: "workspaceRoot"
        pure TenantSpec {..}

instance FromJSON CredentialSpec where
    parseJSON = withObject "TenantCredential" \value -> do
        rejectUnknownFields
            "TenantCredential"
            ["id", "tokenFile"]
            value
        rawId <- value .: "id"
        credentialSpecId <-
            either (fail . Text.unpack) pure (parseCredentialId rawId)
        credentialSpecTokenFile <- value .: "tokenFile"
        pure CredentialSpec {..}

rejectUnknownFields :: String -> [Key] -> Object -> Parser ()
rejectUnknownFields typeName allowed value =
    case filter (`notElem` allowed) (KeyMap.keys value) of
        [] -> pure ()
        unknown ->
            fail
                ("unknown field(s) in "
                    <> typeName
                    <> ": "
                    <> unwords (map show unknown))

prepareStateBase :: FilePath -> IO (Either Text FilePath)
prepareStateBase path = do
    prepared <- tryIO do
        createDirectoryIfMissing True path
        setFileMode path 0o700
        canonicalizePath =<< makeAbsolute path
    pure case prepared of
        Left _ -> Left "could not prepare the tenant state directory"
        Right canonical -> Right canonical

canonicalWorkspace
    :: FilePath
    -> IO (Either Text (FilePath, Integer, Integer))
canonicalWorkspace path = do
    exists <- doesDirectoryExist path
    if not exists
        then pure (Left "a tenant workspace root is not an existing directory")
        else do
            canonical <- tryIO (canonicalizePath =<< makeAbsolute path)
            case canonical of
                Left _ ->
                    pure (Left "could not canonicalize a tenant workspace root")
                Right result ->
                    validateTrustedPathAncestry
                        (takeDirectory result) >>= \case
                            Left err ->
                                pure
                                    (Left
                                        ("tenant workspace parent is not trusted: "
                                            <> err))
                            Right () -> do
                                inspected <- tryIO (getFileStatus result)
                                pure case inspected of
                                    Left _ ->
                                        Left
                                            "could not inspect a tenant workspace root"
                                    Right status
                                        | not (isDirectory status) ->
                                            Left
                                                "a tenant workspace root is not a directory"
                                        | otherwise ->
                                            Right
                                                ( result
                                                , fromIntegral
                                                    (deviceID status)
                                                , fromIntegral
                                                    (fileID status)
                                                )

canonicalizeExistingFile :: FilePath -> IO (Either Text FilePath)
canonicalizeExistingFile path = do
    canonical <- tryIO (canonicalizePath =<< makeAbsolute path)
    pure case canonical of
        Left _ -> Left "could not canonicalize a private file"
        Right result -> Right result

firstCredentialError
    :: Either Text ByteString
    -> Either Text ByteString
firstCredentialError =
    either
        (Left . ("could not read tenant credential: " <>))
        Right

validateTenantCredential :: ByteString -> Either Text ByteString
validateTenantCredential token
    | ByteString.length token < 32 =
        Left
            "tenant bearer tokens must contain at least 32 bytes"
    | otherwise = Right token

hasDuplicates :: Ord value => [value] -> Bool
hasDuplicates values =
    length values /= Set.size (Set.fromList values)

pathsOverlap :: FilePath -> FilePath -> Bool
pathsOverlap left right =
    containsPath left right || containsPath right left

containsPath :: FilePath -> FilePath -> Bool
containsPath root candidate =
    let relative = normalise (makeRelative root candidate)
        components = splitDirectories relative
    in not (isAbsolute relative)
        && case components of
            ".." : _ -> False
            _ -> True

maximumTenants :: Int
maximumTenants = 256

maximumCredentialsPerTenant :: Int
maximumCredentialsPerTenant = 16
