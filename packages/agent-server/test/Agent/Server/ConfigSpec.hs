module Agent.Server.ConfigSpec (spec) where

import Agent.Server.Auth (AuthConfig(..), AuthMode(..))
import Agent.Server.Config
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
import System.Directory
    ( createDirectory
    , getTemporaryDirectory
    , removeFile
    )
import System.Environment
    ( lookupEnv
    , setEnv
    , unsetEnv
    )
import System.IO
    ( hClose
    , openBinaryTempFile
    )
import System.IO.Temp (withTempDirectory)
import System.Posix.Files (setFileMode)
import Test.Hspec

spec :: Spec
spec = describe "server configuration" do
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
            withTempDirectory "." "agent-server-config" \root -> do
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
                resolved <-
                    resolveServerConfig
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
