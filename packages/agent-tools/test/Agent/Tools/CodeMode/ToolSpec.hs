module Agent.Tools.CodeMode.ToolSpec (spec) where

import Agent.Tools.CodeMode.Backend (CodeModeBackend(..))
import Agent.Tools.CodeMode.Tool
    ( CodeModeToolSet(..)
    , CodeModeNestedSpec(..)
    , CodeModeNestedInvoke
    , ToolMode(..)
    , newCodeModeToolSetWithBackend
    , newCodeModeToolSetWithRepair
    , ExecPragma(..)
    , parseExecSource
    , parseExecSourceFor
    , projectNestedResult
    )
import Data.Aeson (Value(..), object, (.=), encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TextEncoding
import Data.Either (isLeft)
import Agent.Loop (defaultLoopDispatch)
import Agent.ToolArgs (objectArgsExact, reqInt)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch
    ( ToolCallResult(..), ToolOutcome(..), customToolCall, dispatchToolCall, typedTool
    , toolCallResultImages
    , toolCallResultFiles, ToolResultFile(..)
    )
import Agent.Tools.CodeMode.Host (codeModeWorkerPath, ImageDetailVisibility(..))
import Agent.Tools.CodeMode.Haskell.Host
    (HaskellRepairRequest(..), haskellCellEnvironment, haskellCellGuidance)
import Agent.Tools.Types (AppTool(..), ApprovalRule(..), ToolExecutionPolicy(..), ToolOutputMetadata(..), ToolOutputFormat(..), jsonAppToolWithExecution)
import Control.Exception.Safe (bracket)
import Control.Monad (forM_)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (getTemporaryDirectory, removePathForcibly, withCurrentDirectory)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = do
  describe "structured nested result projection" do
    let payload = object ["count" .= (42 :: Int)]
        metadata = Just (ToolOutputMetadata Nothing McpToolOutput)
        encoded = TextEncoding.decodeUtf8 . LBS.toStrict . encode
        resultFor output = do
            result <- dispatchToolCall defaultLoopDispatch
                [doubleTool.appToolHandler]
                (customToolCall "projection" "double" "{\"value\":21}")
            pure result { output = output, toolResultOutcome = Just ToolSucceeded }
        native envelope result = ToolCallResultWithStructured
            result.callId result.output result.callKind result.toolResultMode
            result.toolResultImages result.toolResultOutcome envelope []
    it "preserves JSON-looking local file content as text" do
        result <- resultFor (encoded payload)
        projectNestedResult Nothing result `shouldBe` String (encoded payload)
    it "decodes explicitly marked MCP JSON output" do
        result <- resultFor (encoded payload)
        projectNestedResult metadata result `shouldBe` payload
    it "prefers structured content over redundant textual content" do
        let envelope = object
                [ "structuredContent" .= payload
                , "content" .= [object ["type" .= ("text" :: Text), "text" .= ("summary" :: Text)]]
                ]
        result <- resultFor "summary\nSaved artifact: report.csv"
        projectNestedResult metadata (native envelope result) `shouldBe` payload
    it "does not unwrap envelope-shaped payload properties a second time" do
        let inner = object ["content" .= ([] :: [Value]), "structuredContent" .= payload]
            envelope = object ["content" .= ([] :: [Value]), "structuredContent" .= inner]
        result <- resultFor (encoded inner)
        projectNestedResult metadata result `shouldBe` inner
        projectNestedResult metadata (native envelope result) `shouldBe` inner
    it "retains multimodal and explicit error envelopes" do
        forM_
            [ object ["structuredContent" .= payload, "content" .=
                [object ["type" .= ("image" :: Text), "data" .= ("encoded" :: Text)]]]
            , object ["isError" .= True, "structuredContent" .= payload]
            ] \envelope -> do
                result <- resultFor (encoded envelope)
                projectNestedResult metadata result `shouldBe` envelope
    it "preserves failed dispatch and artifact-suffixed output without guessing" do
        result <- resultFor (encoded payload <> "\nSaved artifact: report.csv")
        projectNestedResult metadata result `shouldBe` String result.output
        let failed = result { output = encoded payload, toolResultOutcome = Just ToolFailed }
        projectNestedResult metadata failed `shouldBe` String failed.output
    it "refreshes sampled hints without retaining values or changing captured bindings" do
        calls <- newIORef (0 :: Int)
        let marked = doubleTool { appToolOutputMetadata = metadata }
            specs = [CodeModeNestedSpec marked Nothing]
            invoke _ _ = do
                modifyIORef' calls (+ 1)
                let observed = object ["secret" .= ("do-not-retain" :: Text)]
                    envelope = object ["content" .= ([] :: [Value]), "structuredContent" .= observed]
                Right . native envelope <$> resultFor (encoded observed)
        withToolSet HaskellBackend invoke specs \tools -> do
            _ <- runExec tools "Tools.double (Tools.ToolArguments_double { Tools.value = 21 }) >>= json"
            Right refreshed <- tools.codeModeRefreshToolSet specs
            let description = Text.intercalate "\n" (map (.appToolDescription) refreshed)
            description `shouldSatisfy` Text.isInfixOf "Observed return shape"
            description `shouldSatisfy` (not . Text.isInfixOf "do-not-retain")
            Text.intercalate "\n" (map (.appToolDescription) tools.codeModeTools)
                `shouldSatisfy` (not . Text.isInfixOf "Observed return shape")
            let declared = marked { appToolOutputMetadata =
                    Just (ToolOutputMetadata (Just (object ["type" .= ("object" :: Text)])) McpToolOutput) }
            Right declaredSurface <- tools.codeModeRefreshToolSet [CodeModeNestedSpec declared Nothing]
            Text.intercalate "\n" (map (.appToolDescription) declaredSurface)
                `shouldSatisfy` (not . Text.isInfixOf "Observed return shape")
            readIORef calls `shouldReturn` 1
  describe "Haskell runtime resources" do
    it "executes outside the checkout when Cabal data files are not installed" $
      bracket
        (getTemporaryDirectory >>= \root -> mkdtemp (root </> "haskell-code-mode-resource-test-"))
        removePathForcibly \directory ->
        bracket (lookupEnv "agent_tools_datadir")
            (maybe (unsetEnv "agent_tools_datadir") (setEnv "agent_tools_datadir")) \_ -> do
          setEnv "agent_tools_datadir" directory
          withCurrentDirectory directory $
            withToolSet HaskellBackend (\_ _ -> pure (Left "unexpected tool call")) [] \tools -> do
              result <- runExec tools "text \"embedded support ready\""
              result.output `shouldSatisfy` Text.isInfixOf "embedded support ready"
  integrationSpec
  describe "documented Haskell cell environment" do
    it "executes the exact documented JSON traversal example" $
      withToolSet HaskellBackend (\_ _ -> pure (Left "unexpected tool call")) [] \tools -> do
        let description = Text.intercalate "\n" (map (.appToolDescription) tools.codeModeTools)
            example = Text.takeWhile (/= '`') $
                Text.drop (Text.length "```haskell\n") $
                snd (Text.breakOn "```haskell\n" haskellCellGuidance)
        description `shouldSatisfy` Text.isInfixOf haskellCellEnvironment
        description `shouldSatisfy` Text.isInfixOf haskellCellGuidance
        result <- runExec tools example
        result.output `shouldSatisfy` Text.isInfixOf "[\"tea\",\"rice\"]"
    it "supports the documented qualified JSON, numeric, indexing and sorting imports" $
      withToolSet HaskellBackend (\_ _ -> pure (Left "unexpected tool call")) [] \tools -> do
        result <- runExec tools $ Text.unlines
            [ "do"
            , "  let key = Key.fromText (\"count\" :: Text)"
            , "      value = Object (KeyMap.singleton key (Number 7))"
            , "  count <- either fail pure (AesonTypes.parseEither (withObject \"count\" (\\o -> o .: key)) value :: Either String Int)"
            , "  let exact = Scientific.toBoundedInteger 7 :: Maybe Int"
            , "      indexed = Map.fromList [(\"count\" :: Text, count)]"
            , "  json (toJSON (List.sortOn id (Vector.toList (Vector.fromList [Map.lookup \"count\" indexed, exact]))))"
            ]
        result.output `shouldSatisfy` Text.isInfixOf "[7,7]"
    it "constructs generated records and reads them with record dot, not selectors" $
      withToolSet HaskellBackend (\_ _ -> pure (Left "unexpected tool call")) [nested doubleTool] \tools -> do
        result <- runExec tools
            "do\n  let args = Tools.ToolArguments_double { Tools.value = 21 }\n  json (toJSON args.value)"
        result.output `shouldSatisfy` Text.isInfixOf "21"
        rejected <- runExec tools
            "do\n  let args = Tools.ToolArguments_double { Tools.value = 21 }\n  json (toJSON (Tools.value args))"
        rejected.output `shouldSatisfy` Text.isInfixOf "did not execute"
  describe "Haskell compiler repair dispatch" do
    it "repairs before dispatch and preserves nested approval denial" do
        repairs <- newIORef (0 :: Int)
        calls <- newIORef (0 :: Int)
        let repair request = do
                request.repairEnvironment `shouldSatisfy` Text.isInfixOf haskellCellEnvironment
                request.repairEnvironment `shouldSatisfy` Text.isInfixOf haskellCellGuidance
                request.repairBindings `shouldSatisfy` Text.isInfixOf "double ::"
                request.repairBindings `shouldSatisfy` (not . Text.isInfixOf "P.toJSON")
                request.repairBindings `shouldSatisfy` (not . Text.isInfixOf "callTool")
                modifyIORef' repairs (+ 1)
                pure (Just (doubleSource HaskellBackend))
            invoke _ _ = modifyIORef' calls (+ 1) >> pure (Left "denied by approval")
        worker <- codeModeWorkerPath
        bracket
            (newCodeModeToolSetWithRepair (Just repair) HaskellBackend CodeOnlyToolMode
                ImageDetailVisible worker invoke [nested doubleTool] >>= requireRight)
            (.closeCodeModeToolSet) \tools -> do
                readIORef repairs `shouldReturn` 0
                result <- runExec tools "text (42 :: Int)"
                result.output `shouldSatisfy` Text.isInfixOf "denied by approval"
                readIORef repairs `shouldReturn` 1
                readIORef calls `shouldReturn` 1
  describe "backend-specific exec source" do
    it "accepts raw Haskell expressions" do
        case parseExecSourceFor HaskellBackend "text \"ready\"" of
            Right (source, pragma) -> do
                source `shouldBe` "text \"ready\""
                pragma.yieldTimeMs `shouldBe` Nothing
                pragma.maxOutputTokens `shouldBe` Nothing
            Left err -> expectationFailure (show err)

    it "parses a Haskell comment pragma" do
        case parseExecSourceFor HaskellBackend
                "-- @exec: {\"yield_time_ms\": 0, \"max_output_tokens\": 200}\ndo\n  text \"ready\"" of
            Right (source, pragma) -> do
                source `shouldBe` "do\n  text \"ready\""
                pragma.yieldTimeMs `shouldBe` Just 0
                pragma.maxOutputTokens `shouldBe` Just 200
            Left err -> expectationFailure (show err)

    it "does not reinterpret a JavaScript pragma as Haskell metadata" do
        let source = "// @exec: {\"yield_time_ms\": 0}\ntext \"ready\""
        case parseExecSourceFor HaskellBackend source of
            Right (actual, pragma) -> do
                actual `shouldBe` source
                pragma.yieldTimeMs `shouldBe` Nothing
            Left err -> expectationFailure (show err)

    it "keeps the existing JavaScript parser" do
        case parseExecSource "// @exec: {\"yield_time_ms\": 42}\ntext(1);" of
            Right (source, pragma) -> do
                source `shouldBe` "text(1);"
                pragma.yieldTimeMs `shouldBe` Just 42
            Left err -> expectationFailure (show err)

    it "rejects empty Haskell expressions and directives without an action" do
        isLeft (parseExecSourceFor HaskellBackend "") `shouldBe` True
        isLeft (parseExecSourceFor HaskellBackend "-- @exec: {}") `shouldBe` True

    it "rejects negative Haskell pragma limits" do
        isLeft (parseExecSourceFor HaskellBackend
            "-- @exec: {\"yield_time_ms\": -1}\npure ()") `shouldBe` True

integrationSpec :: Spec
integrationSpec = describe "registered code-mode toolsets" do
    forM_ [JavaScriptBackend, HaskellBackend] \backend ->
      describe (show backend) do
        it "invokes generated tools through the supplied dispatcher" do
            calls <- newIORef (0 :: Int)
            let invoke tool call = do
                    modifyIORef' calls (+ 1)
                    Right <$> dispatchToolCall defaultLoopDispatch [tool.appToolHandler] call
            withToolSet backend invoke [nested doubleTool] \tools -> do
                result <- runExec tools (doubleSource backend)
                result.output `shouldSatisfy` Text.isInfixOf "42"
                readIORef calls `shouldReturn` 1
                map (.appToolName) tools.codeModeTools `shouldBe` ["exec", "wait"]

        it "propagates a denied nested invocation without running its handler" do
            calls <- newIORef (0 :: Int)
            let invoke _ _ = modifyIORef' calls (+ 1) >> pure (Left "denied by approval")
            withToolSet backend invoke [nested doubleTool] \tools -> do
                result <- runExec tools (doubleSource backend)
                result.output `shouldSatisfy` Text.isInfixOf "denied by approval"
                result.output `shouldSatisfy` Text.isInfixOf "Script failed"
                readIORef calls `shouldReturn` 1

        it "refreshes wrappers while preserving captured descriptions and dispatch tables" do
            descriptions <- newIORef []
            let invoke tool call = do
                    modifyIORef' descriptions (<> [tool.appToolDescription])
                    Right <$> dispatchToolCall defaultLoopDispatch [tool.appToolHandler] call
            withToolSet backend invoke [] \original -> do
                added <- original.codeModeRefreshToolSet [nested doubleTool] >>= requireRight
                let addedSet = original { codeModeTools = added }
                before <- runExec original (doubleSource backend)
                before.output `shouldSatisfy` Text.isInfixOf "Script failed"
                readIORef descriptions `shouldReturn` []
                _ <- runExec addedSet (doubleSource backend)
                replaced <- original.codeModeRefreshToolSet
                    [nested (doubleTool { appToolDescription = "Replacement operation." })] >>= requireRight
                _ <- runExec addedSet (doubleSource backend)
                _ <- runExec (original { codeModeTools = replaced }) (doubleSource backend)
                readIORef descriptions `shouldReturn`
                    ["Double an integer.", "Double an integer.", "Replacement operation."]
                map (.appToolDescription) added `shouldSatisfy` any (Text.isInfixOf "Double an integer.")
                map (.appToolDescription) replaced `shouldSatisfy` any (Text.isInfixOf "Replacement operation.")
                removed <- original.codeModeRefreshToolSet [] >>= requireRight
                after <- runExec (original { codeModeTools = removed }) (doubleSource backend)
                after.output `shouldSatisfy` Text.isInfixOf "Script failed"
                readIORef descriptions `shouldReturn`
                    ["Double an integer.", "Double an integer.", "Replacement operation."]

        it "forwards native image envelopes through generatedImage" do
            withToolSet backend (\_ _ -> pure (Left "unexpected nested call")) [] \tools -> do
                result <- runExec tools $ case backend of
                    JavaScriptBackend -> "generatedImage({image_url: 'data:image/png;base64,aGVsbG8='});"
                    HaskellBackend -> "generatedImage (object [\"image_url\" .= (\"data:image/png;base64,aGVsbG8=\" :: Text)])"
                length (toolCallResultImages result) `shouldBe` 1

        it "preserves native files from nested callbacks without returning their bytes to the cell" do
            let file = ToolResultFile "report.pdf" "application/pdf" "native-pdf"
                invoke _ call = do
                    result <- dispatchToolCall defaultLoopDispatch [doubleTool.appToolHandler] call
                    pure $ Right $ ToolCallResultWithFiles
                        result.callId "document attached" result.callKind result.toolResultMode
                        [] (Just ToolSucceeded) [file]
            withToolSet backend invoke [nested doubleTool] \tools -> do
                result <- runExec tools (doubleSource backend)
                toolCallResultFiles result `shouldBe` [file]
                result.output `shouldSatisfy` Text.isInfixOf "document attached"
                result.output `shouldSatisfy` (not . Text.isInfixOf "native-pdf")

    it "typechecks an entire Haskell cell before any nested effect" do
        calls <- newIORef (0 :: Int)
        let invoke _ _ = modifyIORef' calls (+ 1) >> pure (Left "must not execute")
        withToolSet HaskellBackend invoke [nested doubleTool] \tools -> do
            forM_ ["text (123 :: Int)", "text ("] \invalidClause -> do
                result <- runExec tools
                    ("do\n  _ <- Tools.double (Tools.ToolArguments_double { Tools.value = 21 })\n  " <> invalidClause)
                result.output `shouldSatisfy` Text.isInfixOf "Script failed"
                readIORef calls `shouldReturn` 0

    it "publishes declared output types after refresh and retains mismatches without another call" do
        calls <- newIORef (0 :: Int)
        let schema = object
                [ "type" .= ("object" :: Text)
                , "properties" .= object ["count" .= object ["type" .= ("integer" :: Text)]]
                , "required" .= (["count"] :: [Text])
                , "additionalProperties" .= False
                ]
            tool = doubleTool
                { appToolOutputMetadata = Just (ToolOutputMetadata (Just schema) McpToolOutput) }
            invoke actual call = do
                modifyIORef' calls (+ 1)
                Right <$> dispatchToolCall defaultLoopDispatch [actual.appToolHandler] call
        withToolSet HaskellBackend invoke [] \original -> do
            refreshed <- original.codeModeRefreshToolSet [nested tool] >>= requireRight
            map (.appToolDescription) refreshed `shouldSatisfy`
                any (Text.isInfixOf "ToolOutput_double")
            result <- runExec (original { codeModeTools = refreshed })
                "do\n  result <- Tools.double (Tools.ToolArguments_double { Tools.value = 21 })\n  case result.decodedResult of\n    Left _ -> json result.rawResult\n    Right _ -> text \"unexpected successful decode\""
            result.output `shouldSatisfy` Text.isInfixOf "42"
            result.output `shouldSatisfy` (not . Text.isInfixOf "Script failed")
            readIORef calls `shouldReturn` 1

    it "keeps an active Haskell cell on its captured tools across refresh" do
        descriptions <- newIORef []
        refresh <- newIORef (pure ())
        let invoke tool call = do
                modifyIORef' descriptions (<> [tool.appToolDescription])
                joinAction <- atomicModifyIORef' refresh (\action -> (pure (), action))
                joinAction
                Right <$> dispatchToolCall defaultLoopDispatch [tool.appToolHandler] call
        withToolSet HaskellBackend invoke [nested doubleTool] \original -> do
            nextTools <- newIORef original.codeModeTools
            writeIORef refresh do
                replacement <- original.codeModeRefreshToolSet
                    [nested (doubleTool { appToolDescription = "Replacement operation." })] >>= requireRight
                writeIORef nextTools replacement
            result <- runExec original ("do\n  " <> doubleSource HaskellBackend
                <> "\n  " <> doubleSource HaskellBackend)
            result.output `shouldSatisfy` Text.isInfixOf "42"
            readIORef descriptions `shouldReturn` ["Double an integer.", "Double an integer."]
            replacement <- readIORef nextTools
            next <- runExec (original { codeModeTools = replacement }) (doubleSource HaskellBackend)
            next.output `shouldSatisfy` Text.isInfixOf "42"
            readIORef descriptions `shouldReturn`
                ["Double an integer.", "Double an integer.", "Replacement operation."]

doubleTool :: AppTool
doubleTool = jsonAppToolWithExecution "double" "Double an integer."
    [PropertySchema "value" PropertyInteger True Nothing]
    AlwaysReadOnly ParallelSafe
    (typedTool "double" (objectArgsExact ["value"] \obj -> reqInt obj "value")
        \value -> pure (Right (Text.pack (show (value * 2)))))

doubleSource :: CodeModeBackend -> Text
doubleSource JavaScriptBackend = "text(await tools.double({value: 21}));"
doubleSource HaskellBackend =
    "Tools.double (Tools.ToolArguments_double { Tools.value = 21 }) >>= json"

nested :: AppTool -> CodeModeNestedSpec
nested tool = CodeModeNestedSpec tool Nothing

withToolSet :: CodeModeBackend -> CodeModeNestedInvoke -> [CodeModeNestedSpec] -> (CodeModeToolSet -> IO ()) -> IO ()
withToolSet backend invoke specs action = do
    worker <- codeModeWorkerPath
    bracket
        (newCodeModeToolSetWithBackend backend CodeOnlyToolMode ImageDetailVisible worker invoke specs >>= requireRight)
        (.closeCodeModeToolSet) action

requireRight :: Either Text a -> IO a
requireRight = either (fail . Text.unpack) pure

runExec :: CodeModeToolSet -> Text -> IO ToolCallResult
runExec tools source = dispatchToolCall defaultLoopDispatch
    (map (.appToolHandler) tools.codeModeTools)
    (customToolCall "test-exec" "exec" source)
