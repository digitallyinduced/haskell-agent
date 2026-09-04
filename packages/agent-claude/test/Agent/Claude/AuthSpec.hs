{-# LANGUAGE OverloadedStrings #-}

module Agent.Claude.AuthSpec (spec) where

import Agent.Claude.Auth
import Agent.Claude.Transport
import Control.Exception.Safe (bracket, finally, onException)
import Control.Monad (void)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text
import System.Directory
    ( createDirectory
    , getTemporaryDirectory
    , removeDirectoryRecursive
    , removeFile
    )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Posix.Files
    ( ownerExecuteMode
    , ownerReadMode
    , ownerWriteMode
    , setFileMode
    , unionFileModes
    )
import System.Posix.IO
    ( closeFd
    , createPipe
    , dup
    , dupTo
    , stdInput
    )
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
    describe "parseClaudeCodeAuthStatus" do
        it "accepts first-party claude.ai subscription status" do
            parseClaudeCodeAuthStatus "/bin/claude" (ByteString.pack validStatus)
                `shouldBe`
                    Right ClaudeCodeAuth
                        { executable = "/bin/claude"
                        , accountLabel = "person@example.com"
                        , subscriptionType = Just "max"
                        , transport = ClaudeCodeLocalSubscription
                        }

        it "rejects API-key and third-party provider authentication" do
            parseClaudeCodeAuthStatus "/bin/claude"
                (ByteString.pack
                    "{\"loggedIn\":true,\"authMethod\":\"apiKey\",\"apiProvider\":\"firstParty\",\"subscriptionType\":\"max\"}")
                `shouldSatisfy` isLeftContaining "claude.ai"
            parseClaudeCodeAuthStatus "/bin/claude"
                (ByteString.pack
                    "{\"loggedIn\":true,\"authMethod\":\"claude.ai\",\"apiProvider\":\"bedrock\",\"subscriptionType\":\"max\"}")
                `shouldSatisfy` isLeftContaining "third-party"

        it "requires login and a nonempty subscription type" do
            parseClaudeCodeAuthStatus "/bin/claude"
                (ByteString.pack
                    "{\"loggedIn\":false,\"authMethod\":\"claude.ai\",\"apiProvider\":\"firstParty\",\"subscriptionType\":\"max\"}")
                `shouldSatisfy` isLeftContaining "not logged in"
            parseClaudeCodeAuthStatus "/bin/claude"
                (ByteString.pack
                    "{\"loggedIn\":true,\"authMethod\":\"claude.ai\",\"apiProvider\":\"firstParty\",\"subscriptionType\":\"  \"}")
                `shouldSatisfy` isLeftContaining "subscription"

        it "uses a non-secret fallback account label" do
            parseClaudeCodeAuthStatus "/bin/claude"
                (ByteString.pack
                    "{\"loggedIn\":true,\"authMethod\":\"claude.ai\",\"apiProvider\":\"firstParty\",\"subscriptionType\":\"pro\"}")
                `shouldBe`
                    Right ClaudeCodeAuth
                        { executable = "/bin/claude"
                        , accountLabel = "Claude Code (pro)"
                        , subscriptionType = Just "pro"
                        , transport = ClaudeCodeLocalSubscription
                        }

        it "uses Claude's orgName metadata when no email is present" do
            parseClaudeCodeAuthStatus "/bin/claude"
                (ByteString.pack
                    "{\"loggedIn\":true,\"authMethod\":\"claude.ai\",\"apiProvider\":\"firstParty\",\"orgName\":\"Example Org\",\"subscriptionType\":\"team\"}")
                `shouldBe`
                    Right ClaudeCodeAuth
                        { executable = "/bin/claude"
                        , accountLabel = "Example Org"
                        , subscriptionType = Just "team"
                        , transport = ClaudeCodeLocalSubscription
                        }

    describe "loadClaudeCodeAuth" do
        it "does not inherit caller stdin for the auth subprocess" $
            withScratchDirectory "agent-claude-auth-stdin" \root -> do
                let executable = root </> "fake-claude"
                writeFile executable fakeAuthScriptReadsStdin
                setFileMode executable $
                    ownerReadMode
                        `unionFileModes` ownerWriteMode
                        `unionFileModes` ownerExecuteMode
                withEnvironmentVariables
                    [ ("CLAUDE_CODE_EXECUTABLE", Just executable)
                    , ("HASKELL_AGENT_GATEWAY_URL", Nothing)
                    , ("HASKELL_AGENT_GATEWAY_TOKEN", Nothing)
                    ]
                    do
                        result <-
                            withBlockedStandardInput $
                                timeout 2_000_000 loadClaudeCodeAuth
                        result `shouldBe`
                            Just
                                (Right ClaudeCodeAuth
                                    { executable
                                    , accountLabel = "stdin@example.com"
                                    , subscriptionType = Just "max"
                                    , transport = ClaudeCodeLocalSubscription
                                    })

        it "does not pass API or provider overrides to the auth subprocess" $
            withScratchDirectory "agent-claude-auth" \root -> do
                let executable = root </> "fake-claude"
                writeFile executable fakeAuthScript
                setFileMode executable $
                    ownerReadMode
                        `unionFileModes` ownerWriteMode
                        `unionFileModes` ownerExecuteMode
                withEnvironmentVariables
                    [ ("CLAUDE_CODE_EXECUTABLE", Just executable)
                    , ("ANTHROPIC_API_KEY", Just "must-not-leak")
                    , ("ANTHROPIC_AUTH_TOKEN", Just "must-not-leak")
                    , ("ANTHROPIC_BASE_URL", Just "https://example.invalid")
                    , ("CLAUDE_CODE_USE_BEDROCK", Just "1")
                    , ("CLAUDE_CODE_USE_VERTEX", Just "1")
                    , ("CLAUDE_CODE_USE_FOUNDRY", Just "1")
                    , ("CLAUDE_CODE_USE_ANTHROPIC_AWS", Just "1")
                    , ("CLAUDE_CODE_USE_ANTHROPIC_GOOGLE_CLOUD", Just "1")
                    , ("CLAUDE_CODE_USE_GATEWAY", Just "1")
                    , ("CLAUDE_CODE_USE_MANTLE", Just "1")
                    , ("CLAUDE_CODE_PROVIDER_MANAGED_BY_HOST", Just "1")
                    , ("CLAUDE_CODE_API_BASE_URL", Just "https://example.invalid")
                    , ("CLAUDE_CODE_OAUTH_TOKEN", Just "must-not-leak")
                    , ("CLAUDE_CODE_API_KEY_FILE_DESCRIPTOR", Just "9")
                    , ("CLAUDE_CODE_HOST_CREDS_FILE", Just "/tmp/credentials")
                    , ("CLAUDE_CODE_HFI_BEARER_TOKEN", Just "must-not-leak")
                    , ( "_CLAUDE_CODE_ASSUME_FIRST_PARTY_BASE_URL"
                      , Just "1"
                      )
                    , ("AGENT_PROXY_URL", Just "https://example.invalid")
                    , ("AWS_BEARER_TOKEN_BEDROCK", Just "must-not-leak")
                    , ("CLAUDE_TEST_PRESERVED", Just "yes")
                    ]
                    do
                        loadClaudeCodeAuth `shouldReturn`
                            Right ClaudeCodeAuth
                                { executable
                                , accountLabel = "auth@example.com"
                                , subscriptionType = Just "max"
                                , transport = ClaudeCodeLocalSubscription
                                }

        it "drains noisy stderr concurrently with the auth response" $
            withScratchDirectory "agent-claude-auth-noisy" \root -> do
                let executable = root </> "fake-claude"
                writeFile executable fakeNoisyAuthScript
                setFileMode executable $
                    ownerReadMode
                        `unionFileModes` ownerWriteMode
                        `unionFileModes` ownerExecuteMode
                withEnvironmentVariables
                    [("CLAUDE_CODE_EXECUTABLE", Just executable)]
                    do
                        loadClaudeCodeAuth `shouldReturn`
                            Right ClaudeCodeAuth
                                { executable
                                , accountLabel = "noisy@example.com"
                                , subscriptionType = Just "max"
                                , transport = ClaudeCodeLocalSubscription
                                }

        it "uses one deadline while joining both pipe drainers" $
            withScratchDirectory "agent-claude-auth-open-pipes" \root -> do
                let executable = root </> "fake-claude"
                writeFile executable fakeAuthScriptWithOpenPipes
                setFileMode executable $
                    ownerReadMode
                        `unionFileModes` ownerWriteMode
                        `unionFileModes` ownerExecuteMode
                withEnvironmentVariables
                    [("CLAUDE_CODE_EXECUTABLE", Just executable)]
                    do
                        result <- timeout 1_750_000 loadClaudeCodeAuth
                        result `shouldBe`
                            Just
                                (Left
                                    "Claude Code returned an unreadable authentication status.")

        it "uses explicit gateway credentials without requiring a local Claude login" $
            withScratchDirectory "agent-claude-gateway-auth" \root -> do
                let executable = root </> "fake-claude"
                writeFile executable "#!/bin/sh\nexit 77\n"
                setFileMode executable $
                    ownerReadMode
                        `unionFileModes` ownerWriteMode
                        `unionFileModes` ownerExecuteMode
                withEnvironmentVariables
                    [("CLAUDE_CODE_EXECUTABLE", Just executable)]
                    do
                        result <-
                            loadClaudeCodeGatewayAuth
                                ClaudeCodeGateway
                                    { gatewayBaseUrl = "https://gateway.example/"
                                    , gatewayToken = "gateway-secret"
                                    }
                        result `shouldBe`
                            Right ClaudeCodeAuth
                                { executable
                                , accountLabel = "Claude via gateway"
                                , subscriptionType = Nothing
                                , transport =
                                    ClaudeCodeGateway
                                        { gatewayBaseUrl = "https://gateway.example/"
                                        , gatewayToken = "gateway-secret"
                                        }
                                }
                        show result `shouldNotContain` "gateway-secret"

        it "does not activate gateway mode from ambient credentials" $
            withScratchDirectory "agent-claude-local-auth" \root -> do
                let executable = root </> "fake-claude"
                writeFile executable fakeAuthScript
                setFileMode executable $
                    ownerReadMode
                        `unionFileModes` ownerWriteMode
                        `unionFileModes` ownerExecuteMode
                withEnvironmentVariables
                    [ ("CLAUDE_CODE_EXECUTABLE", Just executable)
                    , ("CLAUDE_TEST_PRESERVED", Just "yes")
                    , ("HASKELL_AGENT_GATEWAY_URL", Just "https://gateway.example")
                    , ("HASKELL_AGENT_GATEWAY_TOKEN", Just "must-not-leak")
                    ]
                    do
                        loadClaudeCodeAuth `shouldReturn`
                            Right ClaudeCodeAuth
                                { executable
                                , accountLabel = "auth@example.com"
                                , subscriptionType = Just "max"
                                , transport = ClaudeCodeLocalSubscription
                                }

isLeftContaining :: Text.Text -> Either Text.Text a -> Bool
isLeftContaining needle = \case
    Left message -> needle `Text.isInfixOf` message
    Right _ -> False

validStatus :: String
validStatus =
    "{\"loggedIn\":true,\"authMethod\":\"claude.ai\",\
    \\"apiProvider\":\"firstParty\",\"email\":\"person@example.com\",\
    \\"subscriptionType\":\"max\",\"credential\":\"must-not-be-read\"}"

fakeAuthScript :: String
fakeAuthScript =
    unlines
        [ "#!/bin/sh"
        , "test \"$1\" = auth || exit 41"
        , "test \"$2\" = status || exit 42"
        , "test \"$3\" = --json || exit 43"
        , "test \"$CLAUDE_TEST_PRESERVED\" = yes || exit 44"
        , "if env | grep -E '^(ANTHROPIC_|CLAUDE_CODE_|_CLAUDE_CODE_ASSUME_FIRST_PARTY_BASE_URL=|AGENT_PROXY_URL=|AWS_BEARER_TOKEN_BEDROCK=)' >/dev/null; then"
        , "  echo 'billing override leaked into auth subprocess' >&2"
        , "  exit 45"
        , "fi"
        , "printf '%s\\n' '{\"loggedIn\":true,\"authMethod\":\"claude.ai\",\"apiProvider\":\"firstParty\",\"email\":\"auth@example.com\",\"subscriptionType\":\"max\"}'"
        ]

fakeAuthScriptReadsStdin :: String
fakeAuthScriptReadsStdin =
    unlines
        [ "#!/bin/sh"
        , "cat >/dev/null"
        , "printf '%s\\n' '{\"loggedIn\":true,\"authMethod\":\"claude.ai\",\"apiProvider\":\"firstParty\",\"email\":\"stdin@example.com\",\"subscriptionType\":\"max\"}'"
        ]

fakeNoisyAuthScript :: String
fakeNoisyAuthScript =
    unlines
        [ "#!/bin/sh"
        , "i=0"
        , "while [ \"$i\" -lt 2048 ]; do"
        , "  printf '%s' '0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef' >&2"
        , "  i=$((i + 1))"
        , "done"
        , "printf '%s\\n' '{\"loggedIn\":true,\"authMethod\":\"claude.ai\",\"apiProvider\":\"firstParty\",\"email\":\"noisy@example.com\",\"subscriptionType\":\"max\"}'"
        ]

fakeAuthScriptWithOpenPipes :: String
fakeAuthScriptWithOpenPipes =
    unlines
        [ "#!/bin/sh"
        , "(while :; do printf ' '; sleep 1; done) &"
        , "(while :; do printf x >&2; sleep 1; done) &"
        , "printf '%s\\n' '{\"loggedIn\":true,\"authMethod\":\"claude.ai\",\"apiProvider\":\"firstParty\",\"email\":\"open-pipes@example.com\",\"subscriptionType\":\"max\"}'"
        ]

withScratchDirectory :: String -> (FilePath -> IO a) -> IO a
withScratchDirectory prefix =
    bracket acquire removeDirectoryRecursive
  where
    acquire = do
        temporary <- getTemporaryDirectory
        (path, handle) <- openTempFile temporary prefix
        hClose handle
        removeFile path
        createDirectory path
        pure path

withEnvironmentVariables
    :: [(String, Maybe String)]
    -> IO a
    -> IO a
withEnvironmentVariables variables action = do
    previous <- mapM
        (\(name, _) -> do
            value <- lookupEnv name
            pure (name, value))
        variables
    mapM_ (uncurry install) variables
    action `finally` mapM_ (uncurry install) previous
  where
    install name = \case
        Nothing -> unsetEnv name
        Just value -> setEnv name value

-- | Replace stdin with a pipe whose writer stays open. A child that inherits
-- stdin will block in @cat@; a child created with a private closed stdin pipe
-- receives EOF immediately.
withBlockedStandardInput :: IO a -> IO a
withBlockedStandardInput action =
    bracket acquire release (const action)
  where
    acquire = do
        original <- dup stdInput
        (reader, writer) <- createPipe
        let cleanup = do
                closeFd original
                closeFd reader
                closeFd writer
        (do
            _ <- dupTo reader stdInput
            closeFd reader
            pure (original, writer)
            ) `onException` cleanup
    release (original, writer) =
        (void (dupTo original stdInput) `finally` closeFd original)
            `finally` closeFd writer
