module Agent.Server.ConfigSpec (spec) where

import Agent.Server.Auth (AuthConfig(..), AuthMode(..))
import Agent.Server.Config
import Control.Exception.Safe
    ( bracket
    , finally
    , onException
    )
import Data.ByteString.Char8 qualified as ByteString
import Data.Text qualified as Text
import System.Directory
    ( getTemporaryDirectory
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
