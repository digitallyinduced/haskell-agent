module Agent.Server.ConfigSpec (spec) where

import Agent.Server.Auth (AuthConfig(..), AuthMode(..))
import Agent.Server.Config
import Agent.Server.PrivateFile
    ( trustedPathPolicyWithin
    , validateTrustedPathWithPolicy
    )
import Control.Exception.Safe
    ( bracket
    , finally
    , onException
    )
import Data.Aeson
    ( encode
    , object
    , (.=)
    )
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text qualified as Text
import Control.Monad (forM_)
import System.Directory
    ( canonicalizePath
    , createDirectory
    , getTemporaryDirectory
    , removeFile
    )
import System.Environment
    ( lookupEnv
    , setEnv
    , unsetEnv
    , withArgs
    )
import System.FilePath ((</>))
import System.IO
    ( hClose
    , openBinaryTempFile
    )
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import Test.Hspec

spec :: Spec
spec = describe "server configuration" do
    it "keeps approval prompting enabled by default" do
        defaultServerConfig.serverYolo `shouldBe` False

    it "parses --yolo as explicit auto-approval" do
        config <- withArgs ["--yolo"] parseServerConfig
        config.serverYolo `shouldBe` True

    it "preserves explicit auto-approval during resolution" do
        withTokenEnvironmentUnset do
            resolved <-
                resolveServerConfig
                    defaultServerConfig { serverYolo = True }
            case resolved of
                Left err ->
                    expectationFailure (Text.unpack err)
                Right config ->
                    config.resolvedYolo `shouldBe` True

    it "withholds gateway integrations by default" do
        defaultServerConfig.serverGatewayIntegrations `shouldBe` False
        withTokenEnvironmentUnset do
            resolved <- resolveServerConfig defaultServerConfig
            case resolved of
                Left err ->
                    expectationFailure (Text.unpack err)
                Right resolvedConfig ->
                    resolvedConfig.resolvedGatewayIntegrations `shouldBe` False

    it "parses --gateway-integrations as an explicit opt-in" do
        config <- withArgs ["--gateway-integrations"] parseServerConfig
        config.serverGatewayIntegrations `shouldBe` True
        withTokenEnvironmentUnset do
            resolved <- resolveServerConfig config
            case resolved of
                Left err ->
                    expectationFailure (Text.unpack err)
                Right resolvedConfig ->
                    resolvedConfig.resolvedGatewayIntegrations `shouldBe` True

    it "reads a newline-terminated private token file through EOF" do
        withTokenEnvironmentUnset $
            withPrivateTokenFile "correct-token\n" \path -> do
                resolved <-
                    resolveServerConfig
                        defaultServerConfig
                            { serverTokenFile = Just path
                            }
                case resolved of
                    Left err ->
                        expectationFailure (Text.unpack err)
                    Right config ->
                        case config.resolvedAuth.authMode of
                            BearerTokenAuth token ->
                                token `shouldBe` "correct-token"
                            LoopbackHostAuth _ ->
                                expectationFailure
                                    "token file did not enable bearer mode"
                            TenantBearerAuth _ ->
                                expectationFailure
                                    "token file enabled tenant bearer mode"

    it "rejects token files beyond the bounded read limit" do
        withTokenEnvironmentUnset $
            withPrivateTokenFile
                (ByteString.replicate 4097 'x')
                \path -> do
                    resolved <-
                        resolveServerConfig
                            defaultServerConfig
                                { serverTokenFile = Just path
                                }
                    case resolved of
                        Left err ->
                            err `shouldSatisfy`
                                Text.isInfixOf "too large"
                        Right _ ->
                            expectationFailure
                                "an oversized token file was accepted"

    it "rejects a sandbox runner inside a tenant-writable root" do
        withTokenEnvironmentUnset $
            withSystemTempDirectory "agent-server-config" \root -> do
                let workspace = root <> "/workspace"
                    stateRoot = root <> "/state"
                    tokenPath = root <> "/token"
                    registryPath = root <> "/registry.json"
                    runnerPath = workspace <> "/runner"
                createDirectory workspace
                writeFile runnerPath "#!/bin/sh\nexit 0\n"
                setFileMode runnerPath 0o700
                writeFile tokenPath
                    "tenant-secret-with-at-least-thirty-two-bytes\n"
                setFileMode tokenPath 0o600
                LazyByteString.writeFile registryPath $
                    encode $
                        object
                            [ "version" .= (1 :: Int)
                            , "tenants" .=
                                [ object
                                    [ "id" .=
                                        ("018f6a14-7d52-7a52-9c00-66d5e7d70334"
                                            :: String)
                                    , "workspaceRoot" .= workspace
                                    , "credentials" .=
                                        [ object
                                            [ "id" .=
                                                ("018f6a14-7d52-7a52-9c00-66d5e7d70335"
                                                    :: String)
                                            , "tokenFile" .= tokenPath
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                setFileMode registryPath 0o600
                trustPolicy <-
                    trustedPathPolicyWithin root
                        >>= either (fail . Text.unpack) pure
                resolved <-
                    resolveServerConfigWithTrustPolicy
                        trustPolicy
                        defaultServerConfig
                            { serverTenantRegistry = Just registryPath
                            , serverTenantStateRoot = Just stateRoot
                            , serverSandboxRunner = Just runnerPath
                            }
                case resolved of
                    Left err ->
                        err `shouldSatisfy`
                            Text.isInfixOf "outside tenant-writable roots"
                    Right _ ->
                        expectationFailure
                            "accepted a tenant-writable sandbox runner"

    it "resolves tenants without tool execution without a sandbox runner" do
        withTokenEnvironmentUnset $
            withSingleTenantRegistry (Just "none") \root registryPath -> do
                trustPolicy <-
                    trustedPathPolicyWithin root
                        >>= either (fail . Text.unpack) pure
                resolved <-
                    resolveServerConfigWithTrustPolicy
                        trustPolicy
                        defaultServerConfig
                            { serverTenantRegistry = Just registryPath
                            , serverTenantStateRoot = Just (root </> "state")
                            }
                case resolved of
                    Left err -> expectationFailure (Text.unpack err)
                    Right config -> case config.resolvedServerMode of
                        MultiTenantMode multi ->
                            multi.multiTenantSandboxRunner `shouldBe` Nothing
                        LocalSingleUserMode ->
                            expectationFailure
                                "a tenant registry resolved to local mode"

    it "requires a sandbox runner for sandboxed tenants" do
        withTokenEnvironmentUnset $
            withSingleTenantRegistry Nothing \root registryPath -> do
                trustPolicy <-
                    trustedPathPolicyWithin root
                        >>= either (fail . Text.unpack) pure
                resolved <-
                    resolveServerConfigWithTrustPolicy
                        trustPolicy
                        defaultServerConfig
                            { serverTenantRegistry = Just registryPath
                            , serverTenantStateRoot = Just (root </> "state")
                            }
                case resolved of
                    Left err ->
                        err `shouldBe`
                            "tenants with sandboxed tool execution require --sandbox-runner"
                    Right _ ->
                        expectationFailure
                            "resolved a sandboxed tenant without a runner"

    it "resolves repeated --skill-root options to canonical trusted directories" do
        withTokenEnvironmentUnset $
            withSystemTempDirectory "agent-server-skills" \root -> do
                createDirectory (root </> "product")
                createDirectory (root </> "shared")
                config <-
                    withArgs
                        [ "--skill-root", root </> "product"
                        , "--skill-root", root </> "shared" </> ".." </> "shared"
                        ]
                        parseServerConfig
                trustPolicy <-
                    trustedPathPolicyWithin root
                        >>= either (fail . Text.unpack) pure
                expected <-
                    traverse canonicalizePath
                        [root </> "product", root </> "shared"]
                resolved <- resolveServerConfigWithTrustPolicy trustPolicy config
                case resolved of
                    Left err -> expectationFailure (Text.unpack err)
                    Right resolvedConfig ->
                        resolvedConfig.resolvedSkillRoots `shouldBe` expected

    it "rejects a skill root that is not a directory" do
        withTokenEnvironmentUnset $
            withSystemTempDirectory "agent-server-skills" \root -> do
                writeFile (root </> "SKILL.md") "not a directory"
                trustPolicy <-
                    trustedPathPolicyWithin root
                        >>= either (fail . Text.unpack) pure
                resolved <-
                    resolveServerConfigWithTrustPolicy
                        trustPolicy
                        defaultServerConfig
                            { serverSkillRoots = [root </> "SKILL.md"] }
                case resolved of
                    Left err ->
                        err `shouldSatisfy` Text.isInfixOf "not a directory"
                    Right _ ->
                        expectationFailure "accepted a file as a skill root"

    it "keeps skill roots apart from tenant-writable roots" do
        withTokenEnvironmentUnset $
            withSingleTenantRegistry (Just "none") \root registryPath -> do
                createDirectory (root </> "workspace" </> "skills")
                createDirectory (root </> "product-skills")
                trustPolicy <-
                    trustedPathPolicyWithin root
                        >>= either (fail . Text.unpack) pure
                let resolveWith skillRoots =
                        resolveServerConfigWithTrustPolicy
                            trustPolicy
                            defaultServerConfig
                                { serverTenantRegistry = Just registryPath
                                , serverTenantStateRoot = Just (root </> "state")
                                , serverSkillRoots = skillRoots
                                }
                -- Inside a tenant workspace, and containing one.
                forM_ [root </> "workspace" </> "skills", root] \skillRoot ->
                    resolveWith [skillRoot] >>= \case
                        Left err ->
                            err `shouldBe`
                                "skill roots must be outside tenant-writable roots"
                        Right _ ->
                            expectationFailure
                                ("accepted a tenant-writable skill root: " <> skillRoot)
                resolveWith [root </> "product-skills"] >>= \case
                    Left err -> expectationFailure (Text.unpack err)
                    Right resolvedConfig ->
                        length resolvedConfig.resolvedSkillRoots `shouldBe` 1

    it "confines an explicit build trust policy to its declared root" do
        withSystemTempDirectory "agent-server-trust" \outer -> do
            let trustedRoot = outer </> "trusted"
                sibling = outer </> "sibling"
            createDirectory trustedRoot
            createDirectory sibling
            trustPolicy <-
                trustedPathPolicyWithin trustedRoot
                    >>= either (fail . Text.unpack) pure
            validateTrustedPathWithPolicy trustPolicy sibling
                `shouldReturn`
                    Left "trusted path escapes its declared root"

    it "requires an explicit build trust boundary to be a directory" do
        withPrivateTokenFile "not-a-directory" \path -> do
            result <- trustedPathPolicyWithin path
            case result of
                Left err ->
                    err `shouldBe`
                        "trusted path boundary must be a directory"
                Right _ ->
                    expectationFailure
                        "accepted a regular file as a trust boundary"

-- | One tenant with an optional explicit @toolExecution@ mode.
withSingleTenantRegistry
    :: Maybe Text.Text
    -> (FilePath -> FilePath -> IO value)
    -> IO value
withSingleTenantRegistry toolExecution action =
    withSystemTempDirectory "agent-server-config" \root -> do
        let workspace = root </> "workspace"
            tokenPath = root </> "token"
            registryPath = root </> "registry.json"
        createDirectory workspace
        writeFile tokenPath
            "tenant-secret-with-at-least-thirty-two-bytes\n"
        setFileMode tokenPath 0o600
        LazyByteString.writeFile registryPath $
            encode $
                object
                    [ "version" .= (1 :: Int)
                    , "tenants" .=
                        [ object $
                            [ "id" .=
                                ("018f6a14-7d52-7a52-9c00-66d5e7d70334"
                                    :: String)
                            , "workspaceRoot" .= workspace
                            , "credentials" .=
                                [ object
                                    [ "id" .=
                                        ("018f6a14-7d52-7a52-9c00-66d5e7d70335"
                                            :: String)
                                    , "tokenFile" .= tokenPath
                                    ]
                                ]
                            ]
                            <> maybe
                                []
                                (\mode -> ["toolExecution" .= mode])
                                toolExecution
                        ]
                    ]
        setFileMode registryPath 0o600
        action root registryPath

withPrivateTokenFile
    :: ByteString.ByteString
    -> (FilePath -> IO value)
    -> IO value
withPrivateTokenFile contents action =
    bracket acquire removeFile action
  where
    acquire = do
        directory <- getTemporaryDirectory
        (path, handle) <-
            openBinaryTempFile directory "agent-server-token"
        (ByteString.hPut handle contents `finally` hClose handle)
            `onException` removeFile path
        pure path

withTokenEnvironmentUnset :: IO value -> IO value
withTokenEnvironmentUnset action =
    bracket
        (lookupEnv tokenEnvironment <* unsetEnv tokenEnvironment)
        restore
        (const action)
  where
    restore = \case
        Nothing -> unsetEnv tokenEnvironment
        Just value -> setEnv tokenEnvironment value

tokenEnvironment :: String
tokenEnvironment = "AGENT_SERVER_TOKEN"
