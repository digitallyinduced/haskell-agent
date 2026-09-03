module Agent.Server.SandboxSpec
    ( spec
    , fakeSandboxRunner
    ) where

import Agent.Server.Sandbox
    ( TenantSandbox
    , closeTenantSandbox
    , openTenantSandbox
    , routeSandboxTool
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
    ( ToolCallResult(..)
    , ToolDispatchConfig(..)
    , ToolDispatchOutcome(..)
    , dispatchToolCallDetailed
    , functionToolCall
    , noArgsTool
    )
import Agent.Tools.Types
    ( ApprovalRule(..)
    , AppTool(..)
    , ToolPlacement(..)
    , jsonAppTool
    , withToolPlacement
    )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
    ( async
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
import Data.ByteString.Char8 qualified as ByteString8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import System.Directory
    ( createDirectory
    , doesFileExist
    )
import System.Environment (getExecutablePath)
import System.FilePath ((</>))
import System.IO
    ( BufferMode(NoBuffering)
    , Handle
    , hClose
    , hFlush
    , hIsEOF
    , hSetBinaryMode
    , hSetBuffering
    , stdin
    , stdout
    )
import System.IO.Temp (withSystemTempDirectory)
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
    it "routes structured workspace paths without rewriting command text" do
        withFakeSandbox "normal" \tenant sandbox _ -> do
            routed <-
                either (fail . Text.unpack) pure $
                    routeSandboxTool
                        sandbox
                        validSessionId
                        tenant.resolvedTenantWorkspaceRoot
                        CodexDialect
                        testSandboxTool
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

    it "rejects tools without an explicit placement" do
        withFakeSandbox "normal" \tenant sandbox _ ->
            case routeSandboxTool
                sandbox
                validSessionId
                tenant.resolvedTenantWorkspaceRoot
                CodexDialect
                testSandboxTool
                    { appToolPlacement = UnclassifiedTool }
            of
                Left _ -> pure ()
                Right _ ->
                    expectationFailure
                        "accepted an unclassified tool for sandbox routing"

    it "dispatches the real guest filesystem tool set" do
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
dispatchSandbox
    :: ResolvedTenant
    -> TenantSandbox
    -> IO ToolDispatchOutcome
dispatchSandbox tenant sandbox = do
    routed <-
        either (fail . Text.unpack) pure $
            routeSandboxTool
                sandbox
                validSessionId
                tenant.resolvedTenantWorkspaceRoot
                CodexDialect
                testSandboxTool
    dispatchToolCallDetailed
        testDispatchConfig
        [routed.appToolHandler]
        (functionToolCall
            "call-1"
            "list_dir"
            "{\"path\":\".\"}")

testSandboxTool :: AppTool
testSandboxTool =
    withToolPlacement SandboxTool $
        jsonAppTool
            "list_dir"
            "test"
            []
            AlwaysReadOnly
            (noArgsTool "list_dir" (pure (Right "host handler ran")))

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
    hSetBuffering stdout NoBuffering
    let tenantId = requiredOption "--tenant-id" arguments
        stateRoot = requiredOption "--state-root" arguments
        modePath = stateRoot </> "fake-mode"
    modeExists <- doesFileExist modePath
    mode <-
        if modeExists
            then Text.strip . Text.pack <$> readFile modePath
            else pure "normal"
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
    unless (mode == "bad-ready") (fakeLoop mode)

fakeLoop :: Text -> IO ()
fakeLoop mode = do
    eof <- hIsEOF stdin
    unless eof do
        line <- ByteString8.hGetLine stdin
        case (decodeStrict' line :: Maybe FakeRequest) of
            Nothing -> LazyByteString.hPut stdout "{\n" >> hFlush stdout
            Just request ->
                if mode == "hang"
                    then forever (threadDelay 1000000)
                    else
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
                                , "output" .= request.fakeArguments
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
        fakeLoop mode

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
