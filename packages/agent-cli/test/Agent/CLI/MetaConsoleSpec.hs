module Agent.CLI.MetaConsoleSpec (spec) where

import Agent.CLI.MetaConsole
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , TurnInput(..)
    , emptyTurnOutput
    )
import Agent.MCP (McpProtocolPreference(..))
import Agent.Provider (Provider(..))
import Agent.Responses.Types
import Agent.ToolDispatch (functionToolCall)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Test.Hspec

spec :: Spec
spec = do
    describe "decodeMetaPlan" do
        it "decodes a fenced typed plan and maps Grok to xAI" do
            let result =
                    decodeMetaPlan $
                        Text.unlines
                            [ "```json"
                            , "{"
                            , "  \"summary\": \"Connect Grok and add docs\","
                            , "  \"actions\": ["
                            , "    {\"type\":\"connect_account\",\"provider\":\"GROK\"},"
                            , "    {\"type\":\"mcp_upsert\",\"name\":\"docs\",\"url\":\"https://mcp.example.test\",\"protocol\":\"modern\",\"oauthScopes\":[\"docs.read\"]}"
                            , "  ]"
                            , "}"
                            , "```"
                            ]
            result `shouldBe`
                Right MetaPlan
                    { metaSummary = "Connect Grok and add docs"
                    , metaActions =
                        [ MetaConnectAccount XAIProvider
                        , MetaUpsertMcp MetaMcpServer
                            { metaMcpName = "docs"
                            , metaMcpEnabled = True
                            , metaMcpUrl = Just "https://mcp.example.test"
                            , metaMcpCommand = Nothing
                            , metaMcpArgs = []
                            , metaMcpCwd = Nothing
                            , metaMcpStartupTimeoutSeconds = 30
                            , metaMcpRequestTimeoutSeconds = 60
                            , metaMcpProtocol = McpProtocolModern
                            , metaMcpOAuthScopes = Just ["docs.read"]
                            }
                        ]
                    }

        it "rejects unknown fields rather than silently accepting them" do
            decodeMetaPlan
                "{\"summary\":\"change\",\"actions\":[{\"type\":\"connect_account\",\"provider\":\"grok\",\"token\":\"do-not-accept\"}]}"
                `shouldSatisfy` \case
                    Left err -> "unknown field" `Text.isInfixOf` err
                    Right _ -> False

        it "prohibits model-generated MCP and LSP environment values" do
            decodeMetaPlan
                "{\"summary\":\"mcp env\",\"actions\":[{\"type\":\"mcp_upsert\",\"name\":\"docs\",\"command\":\"docs-mcp\",\"env\":{\"TOKEN\":\"secret\"}}]}"
                `shouldSatisfy` \case
                    Left err -> "unknown field" `Text.isInfixOf` err
                    Right _ -> False
            decodeMetaPlan
                "{\"summary\":\"lsp env\",\"actions\":[{\"type\":\"lsp_upsert\",\"name\":\"haskell\",\"command\":\"hls\",\"extensionToLanguage\":{\"hs\":\"haskell\"},\"env\":{\"TOKEN\":\"secret\"}}]}"
                `shouldSatisfy` \case
                    Left err -> "unknown field" `Text.isInfixOf` err
                    Right _ -> False

        it "allows OAuth scopes only on remote MCP servers" do
            decodeMetaPlan
                "{\"summary\":\"invalid oauth\",\"actions\":[{\"type\":\"mcp_upsert\",\"name\":\"local\",\"command\":\"mcp\",\"oauthScopes\":[\"read\"]}]}"
                `shouldSatisfy` \case
                    Left err -> "require a remote URL" `Text.isInfixOf` err
                    Right _ -> False

        it "rejects arbitrary or destructive session commands" do
            decodeMetaPlan
                "{\"summary\":\"leave\",\"actions\":[{\"type\":\"session_command\",\"command\":\"/quit\"}]}"
                `shouldSatisfy` \case
                    Left err -> "unsafe or unsupported" `Text.isInfixOf` err
                    Right _ -> False

        it "rejects plans that remove and modify the same server" do
            decodeMetaPlan
                (Text.concat
                    [ "{\"summary\":\"conflict\",\"actions\":["
                    , "{\"type\":\"mcp_remove\",\"name\":\"docs\"},"
                    , "{\"type\":\"mcp_set_enabled\",\"name\":\"docs\",\"enabled\":false}"
                    , "]}"
                    ])
                `shouldSatisfy` \case
                    Left err -> "removed and also modified" `Text.isInfixOf` err
                    Right _ -> False

        it "requires clarification to stand alone" do
            decodeMetaPlan
                (Text.concat
                    [ "{\"summary\":\"question\",\"actions\":["
                    , "{\"type\":\"clarify\",\"question\":\"Which account?\"},"
                    , "{\"type\":\"inform\",\"message\":\"Waiting\"}"
                    , "]}"
                    ])
                `shouldSatisfy` \case
                    Left err -> "only action" `Text.isInfixOf` err
                    Right _ -> False

        it "decodes the restricted harness and OAuth action language" do
            let result =
                    decodeMetaPlan
                        (Text.concat
                            [ "{\"summary\":\"configure harness\",\"actions\":["
                            , "{\"type\":\"session_command\",\"command\":\"/effort high\"},"
                            , "{\"type\":\"mcp_remove\",\"name\":\"old-mcp\"},"
                            , "{\"type\":\"mcp_set_enabled\",\"name\":\"docs\",\"enabled\":false},"
                            , "{\"type\":\"mcp_oauth_login\",\"name\":\"docs\"},"
                            , "{\"type\":\"set_mcp_init_strategy\",\"strategy\":\"progressive\"},"
                            , "{\"type\":\"set_web_fetch\",\"enabled\":true,\"allowedDomains\":[\"example.com\"],\"timeoutSeconds\":15},"
                            , "{\"type\":\"set_lsp_enabled\",\"enabled\":true},"
                            , "{\"type\":\"lsp_upsert\",\"name\":\"haskell\",\"command\":\"hls\",\"extensionToLanguage\":{\"hs\":\"haskell\"}},"
                            , "{\"type\":\"lsp_remove\",\"name\":\"old-lsp\"},"
                            , "{\"type\":\"set_max_concurrent_agents\",\"limit\":null},"
                            , "{\"type\":\"inform\",\"message\":\"Configuration is ready\"}"
                            , "]}"
                            ])
            result `shouldSatisfy` \case
                Right MetaPlan{metaActions} ->
                    length metaActions == 11
                        && MetaSessionCommand "/effort high" `elem` metaActions
                        && MetaLoginMcpOAuth "docs" `elem` metaActions
                        && MetaSetMaxConcurrentAgents Nothing `elem` metaActions
                Left _ -> False

    describe "redactMetaContext" do
        it "recursively hides env, client secrets, passwords, and tokens" do
            let context =
                    Aeson.object
                        [ "mcpServers" Aeson..=
                            Aeson.object
                                [ "docs" Aeson..=
                                    Aeson.object
                                        [ "env" Aeson..=
                                            Aeson.object ["API_TOKEN" Aeson..= ("abc" :: Text.Text)]
                                        , "oauth" Aeson..=
                                            Aeson.object
                                                [ "clientSecret" Aeson..= ("secret" :: Text.Text)
                                                , "clientId" Aeson..= ("public-id" :: Text.Text)
                                                ]
                                        ]
                                ]
                        , "nested" Aeson..=
                            [ Aeson.object
                                [ "accessToken" Aeson..= ("token" :: Text.Text)
                                , "password" Aeson..= ("pw" :: Text.Text)
                                ]
                            ]
                        ]
                rendered =
                    TextEncoding.decodeUtf8
                        (LBS.toStrict (Aeson.encode (redactMetaContext context)))
            rendered `shouldNotSatisfy` Text.isInfixOf "abc"
            rendered `shouldNotSatisfy` Text.isInfixOf "secret"
            rendered `shouldNotSatisfy` Text.isInfixOf "\"token\""
            rendered `shouldNotSatisfy` Text.isInfixOf "\"pw\""
            rendered `shouldSatisfy` Text.isInfixOf "public-id"
            countRedactions (redactMetaContext context) `shouldBe` 4

        it "redacts context again while building the model prompt" do
            let prompt =
                    metaConsolePrompt
                        (Aeson.object
                            [ "env" Aeson..=
                                Aeson.object ["PRIVATE" Aeson..= ("hidden" :: Text.Text)]
                            ])
                        "configure it"
            prompt `shouldNotSatisfy` Text.isInfixOf "hidden"
            prompt `shouldSatisfy` Text.isInfixOf "<redacted>"
            prompt `shouldSatisfy` Text.isInfixOf "Never emit env"

    describe "metaPlanPreviews" do
        it "describes mutations without MCP arguments or credentials" do
            let server = MetaMcpServer
                    { metaMcpName = "docs"
                    , metaMcpEnabled = True
                    , metaMcpUrl = Nothing
                    , metaMcpCommand = Just "npx"
                    , metaMcpArgs = ["--token", "sensitive"]
                    , metaMcpCwd = Nothing
                    , metaMcpStartupTimeoutSeconds = 30
                    , metaMcpRequestTimeoutSeconds = 60
                    , metaMcpProtocol = McpProtocolAuto
                    , metaMcpOAuthScopes = Nothing
                    }
                preview = metaActionPreview (MetaUpsertMcp server)
            preview `shouldSatisfy` Text.isInfixOf "'docs'"
            preview `shouldSatisfy` Text.isInfixOf "'npx'"
            preview `shouldNotSatisfy` Text.isInfixOf "sensitive"

    describe "runMetaConsoleWithCancel" do
        it "uses empty private state, strips tools, disables storage, and repairs JSON once" do
            let originalParams = case defaultResponseCreateParams of
                    ResponseCreateParams{..} -> ResponseCreateParams
                        { input = Just (ResponseInputText "stale")
                        , instructions = Just "coding instructions"
                        , previousResponseId = Just "previous"
                        , store = Just True
                        , tools = Just []
                        , ..
                        }
            paramsRef <- newIORef originalParams
            submissions <- newIORef ([] :: [[TurnInput]])
            states <- newIORef ([] :: [[ResponseItem]])
            previousValues <- newIORef ([] :: [Maybe Text.Text])
            privateParamsRef <- newIORef Nothing
            let factory privateParams =
                    Backend \state previous inputs _onEvent -> do
                        writeIORef privateParamsRef (Just privateParams)
                        modifyIORef' submissions (<> [inputs])
                        modifyIORef' states (<> [state])
                        modifyIORef' previousValues (<> [previous])
                        count <- length <$> readIORef submissions
                        let answer
                                | count == 1 = "not json"
                                | otherwise =
                                    "{\"summary\":\"ready\",\"actions\":[{\"type\":\"inform\",\"message\":\"Nothing to change\"}]}"
                        pure $ Right BackendResult
                            { backendOutput =
                                emptyTurnOutput
                                    ("meta-" <> Text.pack (show count))
                                    []
                                    (Just answer)
                            , backendState = state
                            }
            result <-
                runMetaConsoleWithCancel
                    (\_ action -> action)
                    factory
                    paramsRef
                    (Aeson.object ["env" Aeson..= ("hidden" :: Text.Text)])
                    "what is configured?"
            result `shouldBe`
                Right MetaPlan
                    { metaSummary = "ready"
                    , metaActions = [MetaInform "Nothing to change"]
                    }
            readIORef states `shouldReturn` [[], []]
            readIORef previousValues `shouldReturn` [Nothing, Nothing]
            sent <- readIORef submissions
            length sent `shouldBe` 2
            case sent of
                [ [UserMessage firstPrompt]
                    , [UserMessage repair]
                    ] -> do
                        firstPrompt `shouldNotSatisfy` Text.isInfixOf "hidden"
                        repair `shouldSatisfy` Text.isInfixOf "Prior response:"
                _ -> expectationFailure "expected two private user submissions"
            captured <- readIORef privateParamsRef
            fmap (.input) captured `shouldBe` Just Nothing
            fmap (.previousResponseId) captured `shouldBe` Just Nothing
            fmap (.tools) captured `shouldBe` Just Nothing
            fmap (.toolChoice) captured
                `shouldBe` Just (Just (ToolChoiceMode ToolChoiceNone))
            fmap (.store) captured `shouldBe` Just (Just False)
            fmap (.conversation) captured `shouldBe` Just Nothing
            fmap (.instructions) captured
                `shouldSatisfy` maybe False
                    (maybe False (Text.isInfixOf "configuration planner"))
            readIORef paramsRef `shouldReturn` originalParams

        it "rejects tool calls without running a repair request" do
            paramsRef <- newIORef defaultResponseCreateParams
            submissions <- newIORef (0 :: Int)
            let factory _ =
                    Backend \state _ _ _ -> do
                        modifyIORef' submissions (+ 1)
                        pure $ Right BackendResult
                            { backendOutput =
                                emptyTurnOutput
                                    "meta-tool"
                                    [functionToolCall
                                        "call-1" "shell_command" "{}"]
                                    Nothing
                            , backendState = state
                            }
            result <-
                runMetaConsoleWithCancel
                    (\_ action -> action)
                    factory
                    paramsRef
                    Aeson.Null
                    "install something"
            result `shouldBe` Left MetaUnexpectedToolCall
            readIORef submissions `shouldReturn` 1

countRedactions :: Aeson.Value -> Int
countRedactions = \case
    Aeson.Object object -> sum (map countRedactions (KeyMap.elems object))
    Aeson.Array values -> sum (fmap countRedactions values)
    Aeson.String "<redacted>" -> 1
    _ -> 0
