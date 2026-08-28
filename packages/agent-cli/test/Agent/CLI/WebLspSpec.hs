module Agent.CLI.WebLspSpec (spec) where

import Agent.CLI.Config
import Agent.CLI.Lsp
import Agent.CLI.WebFetch
import Agent.Loop (defaultLoopDispatch)
import Agent.OsPath (unsafeToFilePath)
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , dispatchToolCall
    , functionToolCall
    )
import Agent.Tools.Types
    ( AppTool(..)
    , appToolHandlers
    , defaultToolEnv
    , setToolSessionTmp
    )
import Control.Exception.Safe (bracket)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory
    ( getTemporaryDirectory
    , removeDirectoryRecursive
    )
import System.Directory.OsPath (createDirectoryIfMissing)
import qualified System.FilePath as FilePath
import System.OsPath
    ( takeDirectory
    , unsafeEncodeUtf
    )
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "web_fetch and LSP runtime support" do
    it "keeps both optional capabilities disabled by default" do
        defaultHarnessConfig.configWebFetch.webFetchEnabled
            `shouldBe` False
        defaultHarnessConfig.configLsp.lspEnabled
            `shouldBe` False

    it "loads and validates web_fetch and LSP configuration" $
        withTempDir "agent-web-lsp-config-" \homePath -> do
            let home = unsafeEncodeUtf homePath
            let path = harnessConfigPath home
            createDirectoryIfMissing True (takeDirectory path)
            LBS.writeFile (unsafeToFilePath path) configJson
            loadHarnessConfig home >>= \case
                Left err -> expectationFailure (Text.unpack err)
                Right config -> do
                    config.configWebFetch.webFetchEnabled `shouldBe` True
                    config.configWebFetch.webFetchAllowedDomains
                        `shouldBe` ["docs.rs", "example.com/docs"]
                    Map.keys config.configLsp.lspServers
                        `shouldBe` ["haskell"]

    it "rejects unsupported LSP transports and restart settings explicitly" $
        withTempDir "agent-web-lsp-config-" \homePath -> do
            let home = unsafeEncodeUtf homePath
                path = harnessConfigPath home
                writeServer extras = do
                    createDirectoryIfMissing True (takeDirectory path)
                    LBS.writeFile (unsafeToFilePath path) $
                        Aeson.encode $
                            object
                                [ "lsp" .= object
                                    [ "enabled" .= True
                                    , "servers" .= object
                                        [ "test" .= object
                                            ( [ "command" .= ("/bin/false" :: Text.Text)
                                              , "extensionToLanguage" .= object
                                                    [ ".hs" .= ("haskell" :: Text.Text)
                                                    ]
                                              ]
                                                <> extras
                                            )
                                        ]
                                    ]
                                ]
                failsWith needle =
                    loadHarnessConfig home >>= \case
                        Left err ->
                            err `shouldSatisfy` Text.isInfixOf needle
                        Right _ ->
                            expectationFailure
                                ("expected config rejection containing "
                                    <> Text.unpack needle)
            writeServer ["transport" .= ("socket" :: Text.Text)]
            failsWith "stdio only"
            writeServer ["restartOnCrash" .= True]
            failsWith "restartOnCrash"
            writeServer ["maxRestarts" .= (3 :: Int)]
            failsWith "maxRestarts"

    it "matches exact hosts and bounded path prefixes" do
        let allowed =
                either (error . Text.unpack) id $
                    traverse parseAllowedDomain
                        ["docs.rs", "example.com/docs"]
        domainAllowed allowed "www.docs.rs" "/crate" `shouldBe` True
        domainAllowed allowed "example.com" "/docs" `shouldBe` True
        domainAllowed allowed "example.com" "/docs/guide" `shouldBe` True
        domainAllowed allowed "example.com" "/docs-internal"
            `shouldBe` False
        domainAllowed allowed "sub.example.com" "/docs" `shouldBe` False

    it "blocks private and special-purpose IP address ranges" do
        map isNonPublicIPv4
            [ (10, 0, 0, 1)
            , (127, 0, 0, 1)
            , (169, 254, 169, 254)
            , (172, 16, 0, 1)
            , (192, 168, 1, 1)
            , (100, 64, 0, 1)
            , (192, 0, 2, 1)
            , (198, 51, 100, 1)
            , (203, 0, 113, 1)
            ]
            `shouldBe` replicate 9 True
        isNonPublicIPv4 (1, 1, 1, 1) `shouldBe` False
        isNonPublicIPv6 (0, 0, 0, 0, 0, 0, 0, 1) `shouldBe` True
        isNonPublicIPv6 (0x2001, 0x4860, 0, 0, 0, 0, 0, 0x8888)
            `shouldBe` False
        isNonPublicIPv6 (0x2001, 0x0db8, 0, 0, 0, 0, 0, 1)
            `shouldBe` True

    it "converts HTML structure while stripping active content" do
        let rendered =
                htmlToMarkdown
                    "<h1>Title</h1><script>steal()</script>\
                    \<p>Hello <b>world</b>.</p><ul><li>One</li></ul>"
        rendered `shouldSatisfy` Text.isInfixOf "# Title"
        rendered `shouldSatisfy` Text.isInfixOf "Hello world"
        rendered `shouldSatisfy` Text.isInfixOf "- One"
        rendered `shouldNotSatisfy` Text.isInfixOf "steal"

    it "encodes byte-accurate LSP Content-Length framing" do
        let value = object ["jsonrpc" .= ("2.0" :: Text.Text), "id" .= (1 :: Int)]
            frame = encodeLspFrame value
            (header, bodyWithDelimiter) = BS.breakSubstring "\r\n\r\n" frame
            body = BS.drop 4 bodyWithDelimiter
        header `shouldBe`
            ("Content-Length: " <> (fromStringBytes (show (BS.length body))))

    it "advertises lsp only after a successful initialize handshake" $
        withTempDir "agent-lsp-runtime-" \directory -> do
            let root = unsafeEncodeUtf directory
                script = directory FilePath.</> "mock-lsp.sh"
                trace = directory FilePath.</> "mock-lsp-requests.log"
                source = directory FilePath.</> "Test.hs"
            writeFile script mockLspScript
            writeFile source "module Test where\nmain = pure ()\n"
            env <- defaultToolEnv root
            setToolSessionTmp env (Just root)
            let server = LspServerConfig
                    { lspCommand = "/bin/sh"
                    , lspArgs = [Text.pack script, Text.pack trace]
                    , lspEnv = Map.empty
                    , lspExtensionToLanguage =
                        Map.singleton ".hs" "haskell"
                    , lspInitializationOptions = Nothing
                    , lspSettings = Nothing
                    , lspWorkspaceFolder = Nothing
                    , lspStartupTimeoutMilliseconds = 3000
                    , lspShutdownTimeoutMilliseconds = 3000
                    }
                config = LspConfig
                    { lspEnabled = True
                    , lspServers = Map.singleton "mock" server
                    }
            startup <- newLspRuntime config env
            startup.lspStartupWarnings `shouldBe` []
            case startup.lspStartupRuntime of
                Nothing -> expectationFailure "expected initialized LSP runtime"
                Just runtime -> do
                    let tool = lspRuntimeTool runtime
                        fileRequest operation =
                            object
                                [ "operation" .= (operation :: Text.Text)
                                , "file_path" .= Text.pack source
                                , "line" .= (0 :: Int)
                                , "character" .= (1 :: Int)
                                ]
                    tool.appToolName `shouldBe` "lsp"
                    definition <- callLsp tool
                        (fileRequest "goToDefinition")
                    definition.output `shouldSatisfy`
                        Text.isInfixOf ":1:2"
                    references <- callLsp tool
                        (fileRequest "findReferences")
                    references.output `shouldSatisfy`
                        Text.isInfixOf ":2:3"
                    hover <- callLsp tool (fileRequest "hover")
                    hover.output `shouldSatisfy`
                        Text.isInfixOf "hover docs"
                    implementations <- callLsp tool
                        (fileRequest "goToImplementation")
                    implementations.output `shouldSatisfy`
                        Text.isInfixOf ":3:4"
                    symbols <- callLsp tool $
                        object
                            [ "operation" .= ("documentSymbol" :: Text.Text)
                            , "file_path" .= Text.pack source
                            ]
                    symbols.output `shouldSatisfy`
                        Text.isInfixOf "- main"
                    workspaceSymbols <- callLsp tool $
                        object
                            [ "operation" .= ("workspaceSymbol" :: Text.Text)
                            , "query" .= ("Glob" :: Text.Text)
                            ]
                    workspaceSymbols.output `shouldSatisfy`
                        Text.isInfixOf "- Global"
                    closeLspRuntime runtime
                    requests <- Text.pack <$> readFile trace
                    mapM_
                        (\method ->
                            requests `shouldSatisfy`
                                Text.isInfixOf
                                    ("\"method\":\"" <> method <> "\""))
                        [ "textDocument/didOpen"
                        , "textDocument/didChange"
                        , "textDocument/definition"
                        , "textDocument/references"
                        , "textDocument/hover"
                        , "textDocument/implementation"
                        , "textDocument/documentSymbol"
                        , "workspace/symbol"
                        , "shutdown"
                        , "exit"
                        ]
                    requests `shouldSatisfy`
                        Text.isInfixOf
                            "\"id\":\"configuration-1\""
                    requests `shouldSatisfy`
                        Text.isInfixOf "\"result\":[null,null]"

    it "does not advertise lsp when every configured server fails" $
        withTempDir "agent-lsp-failed-" \directory -> do
            let root = unsafeEncodeUtf directory
            env <- defaultToolEnv root
            setToolSessionTmp env (Just root)
            let server = LspServerConfig
                    { lspCommand = "/definitely/missing/language-server"
                    , lspArgs = []
                    , lspEnv = Map.empty
                    , lspExtensionToLanguage =
                        Map.singleton ".hs" "haskell"
                    , lspInitializationOptions = Nothing
                    , lspSettings = Nothing
                    , lspWorkspaceFolder = Nothing
                    , lspStartupTimeoutMilliseconds = 100
                    , lspShutdownTimeoutMilliseconds = 100
                    }
                config = LspConfig
                    { lspEnabled = True
                    , lspServers = Map.singleton "missing" server
                    }
            startup <- newLspRuntime config env
            startup.lspStartupWarnings `shouldSatisfy` (not . null)
            case startup.lspStartupRuntime of
                Nothing -> pure ()
                Just runtime -> do
                    closeLspRuntime runtime
                    expectationFailure
                        "failed language server must not advertise lsp"

configJson :: LBS.ByteString
configJson =
    Aeson.encode $
        object
            [ "webFetch" .= object
                [ "enabled" .= True
                , "allowedDomains" .=
                    [ "docs.rs" :: Text.Text
                    , "example.com/docs"
                    ]
                ]
            , "lsp" .= object
                [ "enabled" .= True
                , "servers" .= object
                    [ "haskell" .= object
                        [ "command"
                            .= ("haskell-language-server-wrapper" :: Text.Text)
                        , "extensionToLanguage" .= object
                            [ ".hs" .= ("haskell" :: Text.Text)
                            ]
                        ]
                    ]
                ]
            ]

mockLspScript :: String
mockLspScript =
    unlines
        [ "trace=$1"
        , ": > \"$trace\""
        , "read_frame() {"
        , "  length=''"
        , "  while IFS= read -r line; do"
        , "    line=$(printf '%s' \"$line\" | tr -d '\\r')"
        , "    [ -z \"$line\" ] && break"
        , "    case \"$line\" in"
        , "      Content-Length:*) length=${line#Content-Length: } ;;"
        , "    esac"
        , "  done"
        , "  [ -n \"$length\" ] || exit 1"
        , "  body=$(dd bs=1 count=\"$length\" 2>/dev/null)"
        , "  printf '%s\\n' \"$body\" >> \"$trace\""
        , "}"
        , "send_frame() {"
        , "  body=$1"
        , "  printf 'Content-Length: %s\\r\\n\\r\\n%s' \"${#body}\" \"$body\""
        , "}"
        , "read_frame"
        , "send_frame '{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"capabilities\":{}}}'"
        , "read_frame"
        , "read_frame"
        , "read_frame"
        , "send_frame '{\"jsonrpc\":\"2.0\",\"id\":\"configuration-1\",\"method\":\"workspace/configuration\",\"params\":{\"items\":[{},{}]}}'"
        , "read_frame"
        , "send_frame '{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"uri\":\"file:///mock/Test.hs\",\"range\":{\"start\":{\"line\":0,\"character\":1}}}}'"
        , "read_frame"
        , "read_frame"
        , "send_frame '{\"jsonrpc\":\"2.0\",\"id\":3,\"result\":[{\"uri\":\"file:///mock/Test.hs\",\"range\":{\"start\":{\"line\":1,\"character\":2}}}]}'"
        , "read_frame"
        , "read_frame"
        , "send_frame '{\"jsonrpc\":\"2.0\",\"id\":4,\"result\":{\"contents\":{\"kind\":\"markdown\",\"value\":\"hover docs\"}}}'"
        , "read_frame"
        , "read_frame"
        , "send_frame '{\"jsonrpc\":\"2.0\",\"id\":5,\"result\":{\"uri\":\"file:///mock/Test.hs\",\"range\":{\"start\":{\"line\":2,\"character\":3}}}}'"
        , "read_frame"
        , "read_frame"
        , "send_frame '{\"jsonrpc\":\"2.0\",\"id\":6,\"result\":[{\"name\":\"main\",\"range\":{\"start\":{\"line\":0,\"character\":0}}}]}'"
        , "read_frame"
        , "send_frame '{\"jsonrpc\":\"2.0\",\"id\":7,\"result\":[{\"name\":\"Global\",\"location\":{\"uri\":\"file:///mock/Test.hs\",\"range\":{\"start\":{\"line\":0,\"character\":0}}}}]}'"
        , "read_frame"
        , "send_frame '{\"jsonrpc\":\"2.0\",\"id\":8,\"result\":null}'"
        , "read_frame"
        ]

callLsp :: AppTool -> Aeson.Value -> IO ToolCallResult
callLsp tool arguments =
    dispatchToolCall
        defaultLoopDispatch
        (appToolHandlers [tool])
        (functionToolCall
            "lsp-test"
            "lsp"
            (Text.decodeUtf8
                (LBS.toStrict (Aeson.encode arguments))))

fromStringBytes :: String -> BS.ByteString
fromStringBytes = BS.pack . map (fromIntegral . fromEnum)

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
    root <- getTemporaryDirectory
    bracket
        (mkdtemp (root FilePath.</> prefix))
        removeDirectoryRecursive
        action
