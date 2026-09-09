module Agent.Server.SandboxSpec
    ( spec
    , fakeSandboxRunner
    ) where

import Agent.Server.Sandbox
    ( TenantSandbox
    , closeTenantSandbox
    , composeSandboxTools
    , openTenantSandbox
    )
import Agent.Server.Sandbox.Worker
    ( SandboxWorkerConfig(..)
    , runSandboxWorker
    )
import Agent.Server.Tenant
    ( ResolvedTenant(..)
    , parseTenantId
    )
import Agent.Dialect (DialectId(CodexDialect))
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallResult(..)
    , ToolDispatchConfig(..)
    , ToolDispatchOutcome(..)
    , dispatchToolCallDetailed
    , functionToolCall
    , noArgsTool
    )
import Agent.Tools.Types
    ( ApprovalRule(..)
    , AppTool(..)
    , AppToolGroup(..)
    , appToolSupportsAsync
    , toolAllowsWithoutPrompt
    , toolAutoApproves
    , jsonAppTool
    , withAsyncToolCalls
    , defaultToolEnv
    , setToolSessionTmp
    , ToolEnv(..)
    )
import Agent.Tools.OutputArtifact (OutputArtifact(..), artifactTools, writeOutputArtifact, writeOutputArtifactDetailed)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
    ( Async
    , async
    , cancel
    , wait
    , waitCatch
    , withAsync
    )
import Control.Exception.Safe
    ( bracket
    , finally
    , tryAny
    )
import Control.Monad
    ( forever
    , unless
    , void
    , forM_
    )
import Data.Aeson
    ( FromJSON(..)
    , Value
    , decodeStrict'
    , encode
    , object
    , withObject
    , (.:)
    , (.=)
    )
import Data.Aeson.Types qualified as AesonTypes
import Data.Aeson.Key qualified as Key
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Char8 qualified as ByteString8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import GHC.IO.Handle
    ( hDuplicate
    , hDuplicateTo
    )
import System.Directory
    ( createDirectory
    , doesFileExist
    , listDirectory
    )
import System.Environment (getExecutablePath)
import System.FilePath ((</>))
import System.OsPath (unsafeEncodeUtf)
import System.IO
    ( BufferMode(NoBuffering)
    , Handle
    , SeekMode(AbsoluteSeek)
    , hClose
    , hFlush
    , hIsEOF
    , hSeek
    , hSetBinaryMode
    , hSetBuffering
    , stderr
    , stdin
    , stdout
    )
import System.IO.Temp
    ( withSystemTempDirectory
    , withSystemTempFile
    )
import System.Posix.IO
    ( createPipe
    , fdToHandle
    )
import System.Posix.Files
    ( deviceID
    , fileID
    , getFileStatus
    )
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "tenant sandbox protocol" do
    it "reads host-resident artifacts without starting the sandbox" do
        env <- defaultToolEnv (unsafeEncodeUtf ".")
        handle <- writeOutputArtifact env "{\"amount\":19900}" >>= either (fail . Text.unpack) pure
        let tools = composeSandboxTools (error "native reads must not start sandbox")
                validSessionId "/workspace" CodexDialect [ExecutionToolGroup (artifactTools env Nothing)]
        forM_ [("read_tool_output", ""), ("search_tool_output", ",\"pattern\":\"amount\"")] \(name, extra) -> do
            outcome <- dispatchToolCallDetailed testDispatchConfig (map (.appToolHandler) tools)
                (functionToolCall "read" name ("{\"handle\":\"" <> handle <> "\"" <> extra <> "}"))
            outcome.toolDispatchSucceeded `shouldBe` True
            outcome.toolDispatchResult.output `shouldSatisfy` Text.isInfixOf "19900"

    it "does not forward invalid artifact handles into the sandbox" do
        env <- defaultToolEnv (unsafeEncodeUtf ".")
        let tools = composeSandboxTools (error "invalid handles must not start sandbox")
                validSessionId "/workspace" CodexDialect [ExecutionToolGroup (artifactTools env Nothing)]
        forM_ ["read_tool_output", "export_tool_output"] \name -> do
            outcome <- dispatchToolCallDetailed testDispatchConfig (map (.appToolHandler) tools)
                (functionToolCall "invalid" name "{\"handle\":\"../secret\"}")
            outcome.toolDispatchSucceeded `shouldBe` False

    it "reads host disk fallback without forwarding to the guest" $
        withSystemTempDirectory "artifact-host-routing" \directory -> do
            base <- defaultToolEnv (unsafeEncodeUtf directory)
            setToolSessionTmp base (Just (unsafeEncodeUtf directory))
            let env = base { toolOutputMemoryCap = 0 }
            handle <- writeOutputArtifact env "host disk output" >>= either (fail . Text.unpack) pure
            let tools = composeSandboxTools (error "host disk must not start sandbox")
                    validSessionId "/workspace" CodexDialect [ExecutionToolGroup (artifactTools env Nothing)]
            outcome <- dispatchToolCallDetailed testDispatchConfig (map (.appToolHandler) tools)
                (functionToolCall "read" "read_tool_output" ("{\"handle\":\"" <> handle <> "\"}"))
            outcome.toolDispatchSucceeded `shouldBe` True
            outcome.toolDispatchResult.output `shouldSatisfy` Text.isInfixOf "host disk output"

    it "receives exact binary export bytes in a private guest file" $
        withUploadWorker \workspace stateRoot generation requestOutput responseInput worker -> do
            let bytes = ByteString8.pack ['\NUL', '\255', '\195', '\169'] <> ByteString8.replicate 50000 'x'
            writeJsonLine requestOutput (uploadRequest workspace generation (ByteString8.length bytes))
            writeUploadChunk requestOutput 0 (ByteString8.take 32768 bytes)
            writeUploadChunk requestOutput 32768 (ByteString8.drop 32768 bytes)
            writeUploadChunk requestOutput (ByteString8.length bytes) ""
            response <- within "upload result" (readJsonLine responseInput)
            parseField "ok" response `shouldReturn` True
            encodedPath <- parseField "output" response
            path <- maybe (fail "upload path missing") pure
                (decodeStrict' (TextEncoding.encodeUtf8 encodedPath) :: Maybe FilePath)
            path `shouldSatisfy` ((stateRoot <> "/sessions/") `isPrefixOf`)
            ByteString8.readFile path `shouldReturn` bytes
            hClose requestOutput
            within "upload shutdown" (wait worker) `shouldReturn` Right ()

    it "uploads host exports through the broker and rewrites only the guest path" $
        withFakeSandbox "upload" \tenant sandbox _ -> do
            env <- defaultToolEnv (unsafeEncodeUtf tenant.resolvedTenantWorkspaceRoot)
            setToolSessionTmp env (Just (unsafeEncodeUtf tenant.resolvedTenantHome))
            let bytes = ByteString8.pack ['\NUL', '\255', '\195', '\169']
                    <> ByteString8.replicate 50000 'x'
            forM_ [(60000, True), (40001, False)] \(cap, complete) -> do
                let limited = env { toolOutputArtifactCap = cap }
                artifact <- writeOutputArtifactDetailed limited bytes >>= either (fail . Text.unpack) pure
                let handle = artifact.artifactHandle
                let tools = composeSandboxTools sandbox validSessionId
                        tenant.resolvedTenantWorkspaceRoot CodexDialect
                        [ExecutionToolGroup (artifactTools limited Nothing)]
                outcome <- within "broker export" $
                    dispatchToolCallDetailed testDispatchConfig (map (.appToolHandler) tools)
                        (functionToolCall "export" "export_tool_output"
                            ("{\"handle\":\"" <> handle <> "\"}"))
                outcome.toolDispatchSucceeded `shouldBe` True
                metadata <- maybe (fail "export metadata missing") pure
                    (decodeStrict' (TextEncoding.encodeUtf8 outcome.toolDispatchResult.output) :: Maybe Value)
                (parseField "path" metadata :: IO Text) `shouldReturn` "/state/exported-output"
                (parseField "complete" metadata :: IO Bool) `shouldReturn` complete
                (parseField "stored_bytes" metadata :: IO Int)
                    `shouldReturn` min cap (ByteString8.length bytes)
                ByteString8.readFile (tenant.resolvedTenantStateDirectory </> "captured-upload")
                    `shouldReturn` ByteString8.take cap bytes

    it "forwards missing host artifact handles to the guest broker" $
        withFakeSandbox "normal" \tenant sandbox _ -> do
            env <- defaultToolEnv (unsafeEncodeUtf tenant.resolvedTenantWorkspaceRoot)
            setToolSessionTmp env (Just (unsafeEncodeUtf tenant.resolvedTenantHome))
            let tools = composeSandboxTools sandbox validSessionId
                    tenant.resolvedTenantWorkspaceRoot CodexDialect
                    [ExecutionToolGroup (artifactTools env Nothing)]
            forM_ [("read_tool_output", ""), ("search_tool_output", ",\"pattern\":\"amount\""),
                    ("export_tool_output", "")] \(name, extra) -> do
                let arguments = "{\"handle\":\"output-guest-only\"" <> extra <> "}"
                outcome <- within "guest artifact fallback" $
                    dispatchToolCallDetailed testDispatchConfig (map (.appToolHandler) tools)
                        (functionToolCall "guest-artifact" name arguments)
                outcome.toolDispatchSucceeded `shouldBe` True
                outcome.toolDispatchResult.output `shouldBe` arguments

    it "discards an upload whose terminal size is incomplete" $
        withUploadWorker \workspace stateRoot generation requestOutput _ worker -> do
            writeJsonLine requestOutput (uploadRequest workspace generation 6)
            writeUploadChunk requestOutput 0 "abc"
            writeUploadChunk requestOutput 3 ""
            within "rejected upload" (wait worker)
                `shouldReturn` Left "artifact upload is incomplete"
            listDirectory (stateRoot </> "sessions" </> Text.unpack validSessionId
                </> "tmp" </> "tool-output-artifacts") `shouldReturn` []

    it "rejects out-of-sequence upload frames and removes their partial files" $
        withUploadWorker \workspace stateRoot generation requestOutput _ worker -> do
            writeJsonLine requestOutput (uploadRequest workspace generation 6)
            writeUploadChunk requestOutput 0 "abc"
            writeUploadChunk requestOutput 1 "def"
            within "rejected sequence" (wait worker)
                `shouldReturn` Left "artifact upload frame sequence mismatch"
            listDirectory (stateRoot </> "sessions" </> Text.unpack validSessionId
                </> "tmp" </> "tool-output-artifacts") `shouldReturn` []

    it "rejects uploads above the total storage cap" $
        withUploadWorker \workspace _ generation requestOutput _ worker -> do
            writeJsonLine requestOutput (uploadRequest workspace generation (64 * 1024 * 1024 + 1))
            within "rejected size" (wait worker)
                `shouldReturn` Left "artifact upload exceeds storage limit"

    it "auto-approves only sandbox execution tools without reclassifying mutations" do
        let execution = testSandboxTool { appToolApproval = AlwaysPrompt }
            host = testHostServiceTool { appToolApproval = AlwaysPrompt }
            tools = composeSandboxTools
                (error "approval must not start the sandbox")
                validSessionId
                "/workspace"
                CodexDialect
                [ExecutionToolGroup [execution], HostToolGroup [host]]
        map toolAutoApproves tools `shouldBe` [True, False]
        mapM (\tool ->
            toolAllowsWithoutPrompt tool
                (functionToolCall "call-approval" tool.appToolName "{}"))
            tools `shouldReturn` [False, False]
        toolAutoApproves execution `shouldBe` False

    it "preserves per-call classification for auto-approved sandbox tools" do
        let execution = testSandboxTool
                { appToolApproval = ClassifyReadOnly
                    (\call -> pure (call.arguments == "read"))
                }
        proxied <- composedTool (composeSandboxTools
            (error "classification must not start the sandbox")
            validSessionId "/workspace" CodexDialect
            [ExecutionToolGroup [execution]])
        toolAutoApproves proxied `shouldBe` True
        toolAllowsWithoutPrompt proxied
            (functionToolCall "call-read" proxied.appToolName "read")
            `shouldReturn` True
        toolAllowsWithoutPrompt proxied
            (functionToolCall "call-write" proxied.appToolName "write")
            `shouldReturn` False

    it "preserves async capability on proxied execution tools" do
        case composeSandboxTools
            (error "sandbox handler is not evaluated")
            validSessionId
            "/workspace"
            CodexDialect
            [ExecutionToolGroup [withAsyncToolCalls testSandboxTool]] of
                proxied : _ -> appToolSupportsAsync proxied `shouldBe` True
                [] -> expectationFailure "expected proxied execution tool"

    it "routes structured workspace paths without rewriting command text" do
        withFakeSandbox "normal" \tenant sandbox _ -> do
            routed <-
                composedTool
                    (composeSandboxTools
                        sandbox
                        validSessionId
                        tenant.resolvedTenantWorkspaceRoot
                        CodexDialect
                        [ExecutionToolGroup [testSandboxTool]])
            let hostPath = Text.pack tenant.resolvedTenantWorkspaceRoot
                arguments =
                    "{\"path\":\"" <> hostPath <> "/src\","
                        <> "\"command\":\"printf "
                        <> hostPath
                        <> "/src\"}"
            outcome <-
                dispatchToolCallDetailed
                    testDispatchConfig
                    [routed.appToolHandler]
                    (functionToolCall "call-1" "list_dir" arguments)
            outcome.toolDispatchSucceeded `shouldBe` True
            decodeStrict'
                (TextEncoding.encodeUtf8
                    outcome.toolDispatchResult.output)
                `shouldBe`
                    Just
                        (object
                            [ "path" .= ("/workspace/src" :: Text)
                            , "command" .=
                                ("printf " <> hostPath <> "/src")
                            ])

    it "invalidates a generation-mismatched runner before the next call" do
        withFakeSandbox "bad-generation" \tenant sandbox modePath -> do
            first <- dispatchSandbox tenant sandbox
            first.toolDispatchSucceeded `shouldBe` False
            writeFile modePath "normal\n"
            second <- dispatchSandbox tenant sandbox
            second.toolDispatchSucceeded `shouldBe` True

    it "invalidates and restarts a cancelled stuck runner" do
        withFakeSandbox "hang" \tenant sandbox modePath -> do
            pending <- async (dispatchSandbox tenant sandbox)
            threadDelay 100000
            cancel pending
            _ <- waitCatch pending
            writeFile modePath "normal\n"
            second <- dispatchSandbox tenant sandbox
            second.toolDispatchSucceeded `shouldBe` True

    it "rejects active SVG image responses" do
        withFakeSandbox "svg" \tenant sandbox _ -> do
            outcome <- dispatchSandbox tenant sandbox
            outcome.toolDispatchSucceeded `shouldBe` False

    it "rejects a runner which attests the wrong tenant" do
        withTenantFixture "bad-ready" \tenant _ -> do
            runner <- getExecutablePath
            openTenantSandbox runner tenant >>= \case
                Left err -> expectationFailure (Text.unpack err)
                Right sandbox ->
                    finally
                        (do
                            outcome <- dispatchSandbox tenant sandbox
                            outcome.toolDispatchSucceeded `shouldBe` False
                            outcome.toolDispatchResult.output
                                `shouldSatisfy`
                                    Text.isInfixOf "wrong tenant")
                        (closeTenantSandbox sandbox)

    it "keeps bounded sanitized runner diagnostics private" do
        withTenantFixture "stderr-failure" \tenant _ -> do
            runner <- getExecutablePath
            openTenantSandbox runner tenant >>= \case
                Left err -> expectationFailure (Text.unpack err)
                Right sandbox ->
                    finally
                        (do
                            (outcome, captured) <-
                                captureStandardError
                                    (dispatchSandbox tenant sandbox)
                            outcome.toolDispatchSucceeded `shouldBe` False
                            outcome.toolDispatchResult.output
                                `shouldNotSatisfy`
                                    Text.isInfixOf "stderr-final-marker"
                            captured
                                `shouldSatisfy`
                                    Text.isInfixOf
                                        "stderr-final-marker injected control"
                            Text.count "\n" captured `shouldBe` 1)
                        (closeTenantSandbox sandbox)

    it "keeps host services local while routing execution tools to the guest" do
        withFakeSandbox "normal" \tenant sandbox _ ->
            do
                let routed =
                        composeSandboxTools
                            sandbox
                            validSessionId
                            tenant.resolvedTenantWorkspaceRoot
                            CodexDialect
                            [ HostToolGroup [testHostServiceTool]
                            , ExecutionToolGroup [testSandboxTool]
                            ]
                    handlers = map (.appToolHandler) routed
                hostOutcome <-
                    dispatchToolCallDetailed
                        testDispatchConfig
                        handlers
                        (functionToolCall "host-call" "mcp_test" "{}")
                hostOutcome.toolDispatchSucceeded `shouldBe` True
                hostOutcome.toolDispatchResult.output
                    `shouldBe` "host MCP handler ran"
                guestOutcome <-
                    dispatchToolCallDetailed
                        testDispatchConfig
                        handlers
                        (functionToolCall "guest-call" "list_dir" "{}")
                guestOutcome.toolDispatchSucceeded `shouldBe` True
                guestOutcome.toolDispatchResult.output `shouldBe` "{}"

    it "uses the real guest filesystem tool set" do
        withSystemTempDirectory "agent-sandbox-worker" \root -> do
            let workspace = root </> "workspace"
                stateRoot = root </> "state"
            createDirectory workspace
            createDirectory stateRoot
            writeFile (workspace </> "example.txt") "sandboxed\n"
            tenantId <- either (fail . Text.unpack) pure
                (parseTenantId validTenantId)
            (workerInput, requestOutput) <- pipeHandles
            (responseInput, workerOutput) <- pipeHandles
            let config = SandboxWorkerConfig
                    { workerProtocolVersion = 1
                    , workerTenantId = tenantId
                    , workerWorkspace = workspace
                    , workerStateRoot = stateRoot
                    , workerMaximumSessions = 4
                    }
            withAsync
                (runSandboxWorker config workerInput workerOutput)
                \worker -> (`finally` closeHandles
                    [ requestOutput
                    , workerInput
                    , responseInput
                    , workerOutput
                    ]) do
                        ready <- within "sandbox worker readiness"
                            (readJsonLine responseInput)
                        generation <-
                            (parseField "generation" ready :: IO Text)
                        let request =
                                object
                                    [ "type" .= ("tool" :: Text)
                                    , "version" .= (1 :: Int)
                                    , "tenantId" .= validTenantId
                                    , "generation" .= generation
                                    , "requestId" .= validRequestId
                                    , "sessionId" .= validSessionId
                                    , "cwd" .= workspace
                                    , "dialect" .= ("codex" :: Text)
                                    , "call" .= object
                                        [ "id" .= ("worker-call" :: Text)
                                        , "name" .= ("list_dir" :: Text)
                                        , "arguments" .=
                                            ("{\"target_directory\":\".\"}"
                                                :: Text)
                                        , "kind" .= ("function" :: Text)
                                        , "argumentsEncrypted" .= False
                                        ]
                                    ]
                        writeJsonLine requestOutput request
                        result <- within "sandbox worker result"
                            (readJsonLine responseInput)
                        parseField "ok" result `shouldReturn` True
                        resultText <- parseField "output" result
                        resultText `shouldSatisfy`
                            Text.isInfixOf "example.txt"
                        hClose requestOutput
                        within "sandbox worker shutdown" (wait worker)
                            `shouldReturn` Right ()
uploadRequest :: FilePath -> Text -> Int -> Value
uploadRequest workspace generation size = object
    [ "type" .= ("artifact_upload" :: Text)
    , "version" .= (1 :: Int)
    , "tenantId" .= validTenantId
    , "generation" .= generation
    , "requestId" .= validRequestId
    , "sessionId" .= validSessionId
    , "cwd" .= workspace
    , "dialect" .= ("codex" :: Text)
    , "call" .= object
        [ "id" .= ("upload" :: Text), "name" .= ("export_tool_output" :: Text)
        , "arguments" .= TextEncoding.decodeUtf8 (LazyByteString.toStrict (encode (object ["size" .= size])))
        , "kind" .= ("function" :: Text), "argumentsEncrypted" .= False
        ]
    ]

writeUploadChunk :: Handle -> Int -> ByteString8.ByteString -> IO ()
writeUploadChunk output offset bytes = writeJsonLine output (object
    [ "requestId" .= validRequestId
    , "offset" .= offset
    , "data" .= TextEncoding.decodeUtf8 (Base64.encode bytes)
    ])

withUploadWorker
    :: (FilePath -> FilePath -> Text -> Handle -> Handle -> Async (Either Text ()) -> IO a)
    -> IO a
withUploadWorker action =
    withSystemTempDirectory "agent-artifact-upload" \root -> do
        let workspace = root </> "workspace"
            stateRoot = root </> "state"
        createDirectory workspace
        createDirectory stateRoot
        tenantId <- either (fail . Text.unpack) pure (parseTenantId validTenantId)
        (workerInput, requestOutput) <- pipeHandles
        (responseInput, workerOutput) <- pipeHandles
        let config = SandboxWorkerConfig 1 tenantId workspace stateRoot 4
        withAsync (runSandboxWorker config workerInput workerOutput) \worker ->
            (`finally` closeHandles [requestOutput, workerInput, responseInput, workerOutput]) do
                ready <- within "upload worker readiness" (readJsonLine responseInput)
                generation <- parseField "generation" ready
                action workspace stateRoot generation requestOutput responseInput worker

dispatchSandbox
    :: ResolvedTenant
    -> TenantSandbox
    -> IO ToolDispatchOutcome
dispatchSandbox tenant sandbox = do
    routed <-
        composedTool
            (composeSandboxTools
                sandbox
                validSessionId
                tenant.resolvedTenantWorkspaceRoot
                CodexDialect
                [ExecutionToolGroup [testSandboxTool]])
    dispatchToolCallDetailed
        testDispatchConfig
        [routed.appToolHandler]
        (functionToolCall
            "call-1"
            "list_dir"
            "{\"path\":\".\"}")

testSandboxTool :: AppTool
testSandboxTool =
    jsonAppTool
        "list_dir"
        "test"
        []
        AlwaysReadOnly
        (noArgsTool "list_dir" (pure (Right "host handler ran")))

testHostServiceTool :: AppTool
testHostServiceTool =
    jsonAppTool
        "mcp_test"
        "test"
        []
        AlwaysReadOnly
        (noArgsTool "mcp_test" (pure (Right "host MCP handler ran")))

composedTool :: [AppTool] -> IO AppTool
composedTool = \case
    [tool] -> pure tool
    tools -> fail ("expected one composed tool, got " <> show (length tools))

testDispatchConfig :: ToolDispatchConfig
testDispatchConfig = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "unknown tool: " <> name
    , toolDispatchFormatResult = either id id
    , toolDispatchFormatException = \name _ ->
        "tool exception: " <> name
    , toolDispatchOnException = \_ _ -> pure ()
    , toolDispatchOnOutput = \_ _ -> pure ()
    , toolDispatchFinalizeOutput = \_ output -> pure output
    }

withFakeSandbox
    :: String
    -> (ResolvedTenant
        -> TenantSandbox
        -> FilePath
        -> IO value)
    -> IO value
withFakeSandbox mode action =
    withTenantFixture mode \tenant modePath -> do
        runner <- getExecutablePath
        bracket
            (openTenantSandbox runner tenant
                >>= either (fail . Text.unpack) pure)
            closeTenantSandbox
            (\sandbox -> action tenant sandbox modePath)

withTenantFixture
    :: String
    -> (ResolvedTenant -> FilePath -> IO value)
    -> IO value
withTenantFixture mode action =
    withSystemTempDirectory "agent-tenant-sandbox" \root -> do
        tenantId <- either (fail . Text.unpack) pure
            (parseTenantId validTenantId)
        let workspace = root </> "workspace"
            home = root </> "home"
            stateRoot = root </> "state"
            modePath = stateRoot </> "fake-mode"
        createDirectory workspace
        createDirectory stateRoot
        workspaceStatus <- getFileStatus workspace
        let tenant = ResolvedTenant
                { resolvedTenantId = tenantId
                , resolvedTenantWorkspaceRoot = workspace
                , resolvedTenantWorkspaceDevice =
                    fromIntegral (deviceID workspaceStatus)
                , resolvedTenantWorkspaceInode =
                    fromIntegral (fileID workspaceStatus)
                , resolvedTenantHome = home
                , resolvedTenantStateDirectory = stateRoot
                , resolvedTenantDatabase = "ha_test_tenant"
                , resolvedTenantRuntimeRole = "ha_test_runtime"
                }
        writeFile modePath (mode <> "\n")
        action tenant modePath

data FakeRequest = FakeRequest
    { fakeTenantId :: !Text
    , fakeGeneration :: !Text
    , fakeRequestId :: !Text
    , fakeArguments :: !Text
    }

instance FromJSON FakeRequest where
    parseJSON = withObject "FakeRequest" \payload ->
        FakeRequest
            <$> payload .: "tenantId"
            <*> payload .: "generation"
            <*> payload .: "requestId"
            <*> (payload .: "call" >>= withObject "FakeCall" (.: "arguments"))

fakeSandboxRunner :: [String] -> IO ()
fakeSandboxRunner arguments = do
    hSetBinaryMode stdin True
    hSetBinaryMode stdout True
    hSetBinaryMode stderr True
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    let tenantId = requiredOption "--tenant-id" arguments
        stateRoot = requiredOption "--state-root" arguments
        modePath = stateRoot </> "fake-mode"
    modeExists <- doesFileExist modePath
    mode <-
        if modeExists
            then Text.strip . Text.pack <$> readFile modePath
            else pure "normal"
    if mode == "stderr-failure"
        then do
            ByteString8.hPutStr
                stderr
                (ByteString8.replicate (1024 * 1024) 'x')
            ByteString8.hPutStr
                stderr
                "\nstderr-final-marker\tinjected\rcontrol\n"
            ByteString8.hPutStr stdout "{not-json}\n"
        else do
            let readyTenant =
                    if mode == "bad-ready"
                        then "018f6a14-7d52-7a52-9c00-66d5e7d70000"
                        else Text.pack tenantId
            writeJsonLine stdout $
                object
                    [ "type" .= ("ready" :: Text)
                    , "version" .= (1 :: Int)
                    , "tenantId" .= readyTenant
                    , "generation" .= fakeGenerationId
                    , "workspace" .= ("/workspace" :: Text)
                    , "state" .= ("/state" :: Text)
                    ]
            unless (mode == "bad-ready") (fakeLoop mode stateRoot)

fakeLoop :: Text -> FilePath -> IO ()
fakeLoop mode stateRoot = do
    eof <- hIsEOF stdin
    unless eof do
        line <- ByteString8.hGetLine stdin
        case (decodeStrict' line :: Maybe FakeRequest) of
            Nothing -> LazyByteString.hPut stdout "{\n" >> hFlush stdout
            Just request ->
                if mode == "hang"
                    then forever (threadDelay 1000000)
                    else do
                        resultOutput <- if mode == "upload"
                            then do
                                bytes <- receiveFakeUpload request 0
                                ByteString8.writeFile (stateRoot </> "captured-upload") bytes
                                pure "\"/state/exported-output\""
                            else pure request.fakeArguments
                        writeJsonLine stdout $
                            object
                                [ "type" .= ("result" :: Text)
                                , "version" .= (1 :: Int)
                                , "tenantId" .= request.fakeTenantId
                                , "generation" .=
                                    if mode == "bad-generation"
                                        then badGenerationId
                                        else request.fakeGeneration
                                , "requestId" .= request.fakeRequestId
                                , "ok" .= True
                                , "output" .= resultOutput
                                , "images" .=
                                    if mode == "svg"
                                        then
                                            [ object
                                                [ "url" .=
                                                    ("data:image/svg+xml;base64,PHN2Zz4="
                                                        :: Text)
                                                , "detail" .=
                                                    (Nothing :: Maybe Text)
                                                ]
                                            ]
                                        else ([] :: [Value])
                                ]
        fakeLoop mode stateRoot

receiveFakeUpload :: FakeRequest -> Int -> IO ByteString8.ByteString
receiveFakeUpload request offset = do
    frame <- readJsonLine stdin
    (parseField "requestId" frame :: IO Text) `shouldReturn` request.fakeRequestId
    (parseField "offset" frame :: IO Int) `shouldReturn` offset
    encoded <- parseField "data" frame
    bytes <- either fail pure (Base64.decode (TextEncoding.encodeUtf8 encoded))
    ByteString8.length bytes `shouldSatisfy` (<= 32768)
    if ByteString8.null bytes
        then do
            metadata <- maybe (fail "upload size missing") pure
                (decodeStrict' (TextEncoding.encodeUtf8 request.fakeArguments) :: Maybe Value)
            (parseField "size" metadata :: IO Int) `shouldReturn` offset
            pure ""
        else (bytes <>) <$> receiveFakeUpload request (offset + ByteString8.length bytes)

requiredOption :: String -> [String] -> String
requiredOption name arguments =
    case dropWhile (/= name) arguments of
        _ : value : _ -> value
        _ -> error ("missing fake runner option " <> name)

pipeHandles :: IO (Handle, Handle)
pipeHandles = do
    (readFd, writeFd) <- createPipe
    readHandle <- fdToHandle readFd
    writeHandle <- fdToHandle writeFd
    mapM_ configure [readHandle, writeHandle]
    pure (readHandle, writeHandle)
  where
    configure handle = do
        hSetBinaryMode handle True
        hSetBuffering handle NoBuffering

readJsonLine :: Handle -> IO Value
readJsonLine handle = do
    line <- ByteString8.hGetLine handle
    maybe (fail "invalid JSON from sandbox worker") pure
        (decodeStrict' line)

writeJsonLine :: Handle -> Value -> IO ()
writeJsonLine handle value = do
    LazyByteString.hPut handle (encode value)
    LazyByteString.hPut handle "\n"
    hFlush handle

parseField :: FromJSON value => Text -> Value -> IO value
parseField field value =
    case
        AesonTypes.parseEither
            (withObject "protocol message" (.: Key.fromText field))
            value
    of
        Left err -> fail err
        Right parsed -> pure parsed

closeHandles :: [Handle] -> IO ()
closeHandles = mapM_ (\handle -> void (tryAny (hClose handle)))

captureStandardError :: IO value -> IO (value, Text)
captureStandardError action =
    withSystemTempFile "agent-server-stderr" \_ capturedHandle ->
        bracket (hDuplicate stderr) hClose \originalStderr -> do
            hDuplicateTo capturedHandle stderr
            value <-
                action `finally` do
                    hFlush stderr
                    hDuplicateTo originalStderr stderr
            hFlush capturedHandle
            hSeek capturedHandle AbsoluteSeek 0
            captured <- ByteString8.hGetContents capturedHandle
            pure (value, TextEncoding.decodeLatin1 captured)

within :: String -> IO value -> IO value
within description action =
    timeout (10 * 1000 * 1000) action >>= \case
        Nothing -> fail (description <> " timed out")
        Just value -> pure value

validTenantId :: Text
validTenantId = "018f6a14-7d52-7a52-9c00-66d5e7d70334"

validSessionId :: Text
validSessionId = "018f6a14-7d52-7a52-9c00-66d5e7d70335"

validRequestId :: Text
validRequestId = "018f6a14-7d52-7a52-9c00-66d5e7d70336"

fakeGenerationId :: Text
fakeGenerationId = "018f6a14-7d52-7a52-9c00-66d5e7d70337"

badGenerationId :: Text
badGenerationId = "018f6a14-7d52-7a52-9c00-66d5e7d70338"
