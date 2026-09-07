module Agent.Server.TenantSpec (spec) where

import Agent.Server.Tenant
    ( ResolvedTenant(..)
    , TenantRegistry
    , loadTenantRegistryWithTrustPolicy
    , resolveTenantWorkspacePath
    , tenantRegistryCredentials
    , tenantRegistryTenants
    )
import Agent.Server.PrivateFile (trustedPathPolicyWithin)
import Agent.Server.Types (ApiError(..))
import Data.Aeson
    ( Value
    , encode
    , object
    , (.=)
    )
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (tails)
import Data.Text qualified as Text
import System.Directory
    ( createDirectory
    , createDirectoryIfMissing
    , makeAbsolute
    )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import Test.Hspec

spec :: Spec
spec = describe "tenant registry" do
    it "derives disjoint host resources for each tenant" do
        withRegistryFixture \root registryPath -> do
            result <-
                loadFixtureRegistry root
                    (root </> "server-state")
                    registryPath
            registry <- either (fail . Text.unpack) pure result
            let tenants = tenantRegistryTenants registry
            length tenants `shouldBe` 2
            length (tenantRegistryCredentials registry) `shouldBe` 2
            map (.resolvedTenantDatabase) tenants
                `shouldSatisfy` allDistinct
            map (.resolvedTenantRuntimeRole) tenants
                `shouldSatisfy` allDistinct
            map (.resolvedTenantStateDirectory) tenants
                `shouldSatisfy` allDistinct

    it "rejects credential material shared by two tenants" do
        withSystemTempDirectory "agent-tenant-registry" \root -> do
            let workspaceA = root </> "workspace-a"
                workspaceB = root </> "workspace-b"
                token = root </> "shared-token"
                registryPath = root </> "registry.json"
            mapM_ createDirectory [workspaceA, workspaceB]
            writePrivate token
                "shared-secret-with-at-least-32-bytes\n"
            writePrivateJson registryPath $
                registryValue
                    [ tenantValue tenantA credentialA workspaceA token
                    , tenantValue tenantB credentialB workspaceB token
                    ]
            loadFixtureRegistry root
                (root </> "server-state")
                registryPath >>= \case
                    Left err ->
                        err `shouldSatisfy`
                            Text.isInfixOf "duplicate credentials"
                    Right _ ->
                        expectationFailure
                            "accepted a bearer shared by two tenants"

    it "rejects overlapping workspaces and workspace escape attempts" do
        withSystemTempDirectory "agent-tenant-registry" \root -> do
            let workspaceA = root </> "workspace"
                workspaceB = workspaceA </> "nested"
                tokenA = root </> "token-a"
                tokenB = root </> "token-b"
                registryPath = root </> "registry.json"
            createDirectoryIfMissing True workspaceB
            writePrivate tokenA
                "tenant-a-secret-with-at-least-32-bytes\n"
            writePrivate tokenB
                "tenant-b-secret-with-at-least-32-bytes\n"
            writePrivateJson registryPath $
                registryValue
                    [ tenantValue tenantA credentialA workspaceA tokenA
                    , tenantValue tenantB credentialB workspaceB tokenB
                    ]
            loadFixtureRegistry root
                (root </> "server-state")
                registryPath >>= \case
                    Left err ->
                        err `shouldSatisfy`
                            Text.isInfixOf "must not overlap"
                    Right _ ->
                        expectationFailure
                            "accepted overlapping tenant workspaces"

        withRegistryFixture \root registryPath -> do
            registry <-
                loadFixtureRegistry root
                    (root </> "server-state")
                    registryPath
                    >>= either (fail . Text.unpack) pure
            case tenantRegistryTenants registry of
                [] -> expectationFailure "loaded an empty tenant registry"
                tenant : _ ->
                    resolveTenantWorkspacePath tenant (Just root) >>= \case
                        Left err -> err.apiErrorStatus `shouldBe` 403
                        Right _ ->
                            expectationFailure
                                "resolved a path outside the tenant workspace"

    it "rejects a workspace beneath replaceable ancestry" do
        withSystemTempDirectory "agent-tenant-registry" \root -> do
            let writableParent = root </> "replaceable"
                workspace = writableParent </> "workspace"
                token = root </> "token"
                registryPath = root </> "registry.json"
            createDirectory writableParent
            createDirectory workspace
            setFileMode writableParent 0o777
            writePrivate token
                "tenant-secret-with-at-least-32-bytes\n"
            writePrivateJson registryPath $
                registryValue
                    [ tenantValue tenantA credentialA workspace token
                    ]
            loadFixtureRegistry root
                (root </> "server-state")
                registryPath >>= \case
                    Left err ->
                        err `shouldSatisfy`
                            Text.isInfixOf "workspace parent is not trusted"
                    Right _ ->
                        expectationFailure
                            "accepted a replaceable tenant workspace"

withRegistryFixture
    :: (FilePath -> FilePath -> IO value)
    -> IO value
withRegistryFixture action =
    withSystemTempDirectory "agent-tenant-registry" \temporaryRoot -> do
        root <- makeAbsolute temporaryRoot
        let workspaceA = root </> "workspace-a"
            workspaceB = root </> "workspace-b"
            tokenA = root </> "token-a"
            tokenB = root </> "token-b"
            registryPath = root </> "registry.json"
        mapM_ createDirectory [workspaceA, workspaceB]
        writePrivate tokenA
            "tenant-a-secret-with-at-least-32-bytes\n"
        writePrivate tokenB
            "tenant-b-secret-with-at-least-32-bytes\n"
        writePrivateJson registryPath $
            registryValue
                [ tenantValue tenantA credentialA workspaceA tokenA
                , tenantValue tenantB credentialB workspaceB tokenB
                ]
        action root registryPath

loadFixtureRegistry
    :: FilePath
    -> FilePath
    -> FilePath
    -> IO (Either Text.Text TenantRegistry)
loadFixtureRegistry trustedRoot =
    \stateRoot registryPath ->
        trustedPathPolicyWithin trustedRoot >>= \case
            Left err -> pure (Left err)
            Right trustPolicy ->
                loadTenantRegistryWithTrustPolicy
                    trustPolicy
                    stateRoot
                    registryPath

registryValue :: [Value] -> Value
registryValue tenants =
    object
        [ "version" .= (1 :: Int)
        , "tenants" .= tenants
        ]

tenantValue
    :: Text.Text
    -> Text.Text
    -> FilePath
    -> FilePath
    -> Value
tenantValue tenantId credentialId workspace token =
    object
        [ "id" .= tenantId
        , "workspaceRoot" .= workspace
        , "credentials" .=
            [ object
                [ "id" .= credentialId
                , "tokenFile" .= token
                ]
            ]
        ]

writePrivate :: FilePath -> String -> IO ()
writePrivate path contents = do
    writeFile path contents
    setFileMode path 0o600

writePrivateJson :: FilePath -> Value -> IO ()
writePrivateJson path value = do
    LazyByteString.writeFile path (encode value)
    setFileMode path 0o600

allDistinct :: Eq value => [value] -> Bool
allDistinct values =
    and
        [ left /= right
        | left : remaining <- tails values
        , right <- remaining
        ]

tenantA, tenantB, credentialA, credentialB :: Text.Text
tenantA = "018f6a14-7d52-7a52-9c00-66d5e7d70334"
tenantB = "018f6a14-7d52-7a52-9c00-66d5e7d70335"
credentialA = "018f6a14-7d52-7a52-9c00-66d5e7d70336"
credentialB = "018f6a14-7d52-7a52-9c00-66d5e7d70337"
