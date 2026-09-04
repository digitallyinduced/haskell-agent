module Agent.Tools.CodeMode.HostSpec (spec) where

import Agent.Loop (defaultLoopDispatch)
import qualified Agent.Json.Decode as Json
import Agent.ToolArgs (objectArgsExact, reqInt)
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    , ToolHandler
    , customToolCall
    , dispatchToolCall
    , textTool
    , typedTool
    )
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.Tools.CodeMode.Host
import Agent.Tools.CodeMode.Protocol (CodeModeToolMetadata(..))
import Agent.Tools.CodeMode.Tool
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , freeformApplyPatchAppToolWithExecution
    , jsonAppToolWithExecution
    )
import Control.Concurrent
    ( newEmptyMVar
    , putMVar
    , readMVar
    , takeMVar
    , threadDelay
    , tryPutMVar
    )
import Control.Concurrent.Async (async, cancel, wait, withAsync)
import Control.Exception.Safe (bracket, finally, uninterruptibleMask_)
import Control.Monad (void)
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.List
import Data.IORef
import qualified Data.Text as Text
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "code-mode Bun host" do
    it "defaults to two retained workers" do
        let config = defaultCodeModeConfig
                "data/code-mode/worker.mjs"
                (\_ _ -> pure $ Left "no tools")
        config.workerPoolSize `shouldBe` 2

    it "resolves the bundled worker independently of the current directory" do
        worker <- bundledCodeModeWorkerPath
        doesFileExist worker `shouldReturn` True
        codeModeWorkerPath `shouldReturn` worker

    it "materializes the embedded worker when Cabal's data override is stale" do
        withEnvironmentOverride
            "agent_core_datadir"
            "/definitely/missing/agent-core-data" do
                worker <- bundledCodeModeWorkerPath
                doesFileExist worker `shouldReturn` True
                contents <- readFile worker
                contents `shouldSatisfy`
                    (\source ->
                        "code-mode-cell" `Data.List.isInfixOf` source
                            && "exec_main.mjs" `Data.List.isInfixOf` source)

    it "parses exec pragmas with the current strict contract" do
        parseExecSource
            ("// @exec: {\"yield" <> "_time_ms\": 10, \
                \\"max_output_tokens\": 20}\ntext(\"hi\");")
            `shouldBe`
                Right
                    ( "text(\"hi\");"
                    , ExecPragma
                        { yieldTimeMs = Just 10
                        , maxOutputTokens = Just 20
                        }
                    )
        parseExecSource "// @exec:\ntext(\"hi\");"
            `shouldSatisfy` \case
                Left errorText ->
                    "must be a JSON object" `Text.isInfixOf` errorText
                Right _ -> False
        parseExecSource
            "// @exec: {\"yield_time_ms\": 10, \"surprise\": true}\ntext(\"hi\");"
            `shouldBe`
                Right
                    ( "text(\"hi\");"
                    , ExecPragma (Just 10) Nothing
                    )
        parseExecSource
            ("// @exec: {\"yield" <> "_time_ms\": -1}\ntext(\"hi\");")
            `shouldSatisfy` \case
                Left errorText ->
                    "non-negative safe integers" `Text.isInfixOf` errorText
                Right _ -> False

    it "executes a fresh cell and delegates tool effects" do
        let handler name arguments
                | name == "math.double"
                , Object object <- arguments
                , Just (Number number) <- KeyMap.lookup "value" object =
                    pure $ Right $ Number (number * 2)
                | otherwise = pure $ Left "unexpected tool call"
            config = defaultCodeModeConfig
                "data/code-mode/worker.mjs"
                handler
        host <- newCodeModeHost config
        result <- execCodeCell
            host
            "text(await tools.math.double({ value: 21 }));"
            ["math.double"]
            3000
        closeCodeModeHost host
        result `shouldBe`
            Right CodeModeFinished
                { cellId = "1"
                , cellValue = Aeson.object
                    [ "content" Aeson..=
                        [ Aeson.object
                            [ "type" Aeson..= ("text" :: String)
                            , "text" Aeson..= ("42" :: String)
                            ]
                        ]
                    ]
                }

    it "stops a cell and its nested tool when execution is cancelled" do
        started <- newEmptyMVar
        stopped <- newEmptyMVar
        let handler _ _ =
                finally
                    (putMVar started () >> threadDelay maxBound >> pure (Right Null))
                    (putMVar stopped ())
            config = defaultCodeModeConfig
                "data/code-mode/worker.mjs"
                handler
        host <- newCodeModeHost config
        withAsync
            (execCodeCell
                host
                "await tools.slow({});"
                ["slow"]
                60000)
            \running -> do
                readMVar started
                cancel running
                timeout 1000000 (readMVar stopped) `shouldReturn` Just ()
                terminateCodeCell host "1" `shouldReturn`
                    Left (CodeModeUnknownCell "1")
        closeCodeModeHost host

    it "cancels concurrent nested tools before waiting for either cleanup" do
        firstStarted <- newEmptyMVar
        secondStarted <- newEmptyMVar
        firstCancelling <- newEmptyMVar
        secondCancelling <- newEmptyMVar
        releaseCleanup <- newEmptyMVar
        let blockedHandler started cancelling =
                finally
                    ( putMVar started ()
                        >> threadDelay maxBound
                        >> pure (Right Null)
                    )
                    ( uninterruptibleMask_ do
                        putMVar cancelling ()
                        readMVar releaseCleanup
                    )
            handler name _
                | name == "first" =
                    blockedHandler firstStarted firstCancelling
                | name == "second" =
                    blockedHandler secondStarted secondCancelling
                | otherwise = pure (Left "unexpected tool call")
            config = defaultCodeModeConfig
                "data/code-mode/worker.mjs"
                handler
        bracket (newCodeModeHost config) closeCodeModeHost \host ->
            (do
                started <- execCodeCell
                    host
                    "await Promise.all([tools.first({}), tools.second({})]);"
                    ["first", "second"]
                    1
                started `shouldBe`
                    Right CodeModeRunning
                        { cellId = "1"
                        , cellOutput = emptyContent
                        }
                timeout
                    5000000
                    (readMVar firstStarted >> readMVar secondStarted)
                    `shouldReturn` Just ()
                withAsync (terminateCodeCell host "1") \terminating -> do
                    cancelled <- timeout
                        500000
                        (readMVar firstCancelling >> readMVar secondCancelling)
                    _ <- tryPutMVar releaseCleanup ()
                    result <- wait terminating
                    result `shouldBe`
                        Right CodeModeTerminated
                            { cellId = "1"
                            , cellValue = emptyContent
                            }
                    cancelled `shouldBe` Just ())
                `finally` void (tryPutMVar releaseCleanup ())

    it "keeps JavaScript globals isolated when reusing a pooled worker" do
        let config = defaultCodeModeConfig
                "data/code-mode/worker.mjs"
                (\_ _ -> pure $ Left "no tools")
        host <- newCodeModeHost config
        first <- execCodeCell
            host
            "globalThis.cellSecret = 42; text(globalThis.cellSecret);"
            []
            3000
        second <- execCodeCell
            host
            "text(typeof globalThis.cellSecret);"
            []
            3000
        first `shouldBe`
            Right CodeModeFinished
                { cellId = "1"
                , cellValue = textContent "42"
                }
        second `shouldBe`
            Right CodeModeFinished
                { cellId = "2"
                , cellValue = textContent "undefined"
                }
        closeCodeModeHost host

    it "rejects a second observer without racing cell output queues" do
        let config = defaultCodeModeConfig
                "data/code-mode/worker.mjs"
                (\_ _ -> pure $ Left "no tools")
        host <- newCodeModeHost config
        started <- execCodeCell
            host
            "await new Promise(() => {});"
            []
            1
        started `shouldSatisfy` \case
            Right CodeModeRunning { cellId = "1" } -> True
            _ -> False
        first <- async (waitCodeCell host "1" 200)
        threadDelay 20000
        waitCodeCell host "1" 200 `shouldReturn`
            Left (CodeModeBusyObserver "1")
        _ <- wait first
        _ <- terminateCodeCell host "1"
        closeCodeModeHost host

    it "returns queued explicit yields when a cell is terminated" do
        let config = defaultCodeModeConfig
                "data/code-mode/worker.mjs"
                (\_ _ -> pure $ Left "no tools")
        host <- newCodeModeHost config
        started <- execCodeCell
            host
            "text(\"first\"); yield_control(); text(\"second\"); yield_control(); await new Promise(() => {});"
            []
            3000
        started `shouldBe`
            Right CodeModeRunning
                { cellId = "1"
                , cellOutput = textContent "first"
                }
        threadDelay 20000
        terminated <- terminateCodeCell host "1"
        terminated `shouldBe`
            Right CodeModeTerminated
                { cellId = "1"
                , cellValue = textContent "second"
                }
        closeCodeModeHost host

    it "returns an already-observed natural completion instead of termination" do
        handlerStarted <- newEmptyMVar
        releaseHandler <- newEmptyMVar
        let handler name _
                | name == "gate" = do
                    putMVar handlerStarted ()
                    readMVar releaseHandler
                    pure (Right Null)
                | otherwise = pure (Left "unexpected tool call")
            config = defaultCodeModeConfig
                "data/code-mode/worker.mjs"
                handler
        bracket (newCodeModeHost config) closeCodeModeHost \host -> do
            started <- execCodeCell
                host
                "await tools.gate({}); store(\"completion-marker\", true); text(\"done\");"
                ["gate"]
                1
            started `shouldBe`
                Right CodeModeRunning
                    { cellId = "1"
                    , cellOutput = emptyContent
                    }
            timeout 5000000 (readMVar handlerStarted) `shouldReturn` Just ()
            putMVar releaseHandler ()
            timeout 5000000 (waitForCompletionMarker host 100)
                `shouldReturn` Just True
            terminated <- terminateCodeCell host "1"
            terminated `shouldBe`
                Right CodeModeFinished
                    { cellId = "1"
                    , cellValue = textContent "done"
                    }

    it "does not expose Node globals" do
        let config = defaultCodeModeConfig
                "data/code-mode/worker.mjs"
                (\_ _ -> pure $ Left "no tools")
        host <- newCodeModeHost config
        result <- execCodeCell
            host
            "text([typeof process, typeof require, typeof fs, typeof net, typeof child_process, typeof fetch, typeof console]);"
            []
            3000
        closeCodeModeHost host
        result `shouldBe`
            Right CodeModeFinished
                { cellId = "1"
                , cellValue = Aeson.object
                    [ "content" Aeson..=
                        [ Aeson.object
                            [ "type" Aeson..= ("text" :: String)
                            , "text" Aeson..=
                                ("[\"undefined\",\"undefined\",\"undefined\",\"undefined\",\"undefined\",\"undefined\",\"undefined\"]" :: String)
                            ]
                        ]
                    ]
                }

    it "retains a yielded cell until it is terminated" do
        let config = defaultCodeModeConfig
                "data/code-mode/worker.mjs"
                (\_ _ -> pure $ Left "no tools")
        host <- newCodeModeHost config
        started <- execCodeCell
            host
            "text(\"partial\"); await new Promise(() => {});"
            []
            20
        started `shouldBe`
            Right CodeModeRunning
                { cellId = "1"
                , cellOutput = textContent "partial"
                }
        terminated <- terminateCodeCell host "1"
        terminated `shouldBe`
            Right CodeModeTerminated
                { cellId = "1"
                , cellValue = emptyContent
                }
        closeCodeModeHost host

    it "returns output accumulated after the last timed yield on termination" do
        handlerStarted <- newEmptyMVar
        releaseHandler <- newEmptyMVar
        notification <- newEmptyMVar
        let handler name _
                | name == "gate" = do
                    putMVar handlerStarted ()
                    readMVar releaseHandler
                    pure (Right Null)
                | otherwise = pure (Left "unexpected tool call")
            config =
                (defaultCodeModeConfig "data/code-mode/worker.mjs" handler)
                    { notifyHandler = putMVar notification
                    }
        bracket (newCodeModeHost config) closeCodeModeHost \host -> do
            started <- execCodeCell
                host
                "await tools.gate({}); text(\"late\"); notify(\"late-content-observed\"); await new Promise(() => {});"
                ["gate"]
                1
            started `shouldBe`
                Right CodeModeRunning
                    { cellId = "1"
                    , cellOutput = emptyContent
                    }
            timeout 5000000 (readMVar handlerStarted) `shouldReturn` Just ()
            putMVar releaseHandler ()
            observed <- timeout 5000000 (takeMVar notification)
            observed `shouldBe` Just "late-content-observed"
            terminated <- terminateCodeCell host "1"
            terminated `shouldBe`
                Right CodeModeTerminated
                    { cellId = "1"
                    , cellValue = textContent "late"
                    }

    it "exposes helper content and ignores JavaScript completion values" do
        let config = defaultCodeModeConfig
                "data/code-mode/worker.mjs"
                (\_ _ -> pure $ Left "no tools")
        host <- newCodeModeHost config
        result <- execCodeCell
            host
            "text(\"hello\"); 3;"
            []
            3000
        closeCodeModeHost host
        case result of
            Right CodeModeFinished { cellValue = Object output } -> do
                KeyMap.lookup "value" output `shouldBe` Nothing
                KeyMap.lookup "content" output `shouldSatisfy` maybe False
                    \case
                        Array content -> not (null content)
                        _ -> False
            other -> expectationFailure
                ("unexpected helper result: " <> show other)

    it "does not keep a completed cell alive for unawaited timers" do
        let config = defaultCodeModeConfig
                "data/code-mode/worker.mjs"
                (\_ _ -> pure $ Left "no tools")
        host <- newCodeModeHost config
        started <- getCurrentTime
        result <- execCodeCell
            host
            "setTimeout(() => {}, 60000); text(\"done\");"
            []
            3000
        finished <- getCurrentTime
        closeCodeModeHost host
        result `shouldBe`
            Right CodeModeFinished
                { cellId = "1"
                , cellValue = Aeson.object
                    [ "content" Aeson..=
                        [ Aeson.object
                            [ "type" Aeson..= ("text" :: String)
                            , "text" Aeson..= ("done" :: String)
                            ]
                        ]
                    ]
                }
        realToFrac (diffUTCTime finished started) `shouldSatisfy`
            (< (2 :: Double))

    it "returns numeric timer ids and can terminate CPU-bound cells" do
        let config = defaultCodeModeConfig
                "data/code-mode/worker.mjs"
                (\_ _ -> pure $ Left "no tools")
        host <- newCodeModeHost config
        timer <- execCodeCell
            host
            "text(typeof setTimeout(() => {}, 60000));"
            []
            3000
        timer `shouldBe`
            Right CodeModeFinished
                { cellId = "1"
                , cellValue = textContent "number"
                }
        started <- getCurrentTime
        running <- execCodeCell host "while (true) {}" [] 20
        running `shouldBe`
            Right CodeModeRunning
                { cellId = "2"
                , cellOutput = emptyContent
                }
        terminated <- terminateCodeCell host "2"
        finished <- getCurrentTime
        terminated `shouldBe`
            Right CodeModeTerminated
                { cellId = "2"
                , cellValue = emptyContent
                }
        realToFrac (diffUTCTime finished started) `shouldSatisfy`
            (< (2 :: Double))
        closeCodeModeHost host

    it "yields only accumulated content and later returns new output" do
        let config = defaultCodeModeConfig
                "data/code-mode/worker.mjs"
                (\_ _ -> pure $ Left "no tools")
        host <- newCodeModeHost config
        started <- execCodeCell
            host
            "text(\"before\"); yield_control(); await new Promise(resolve => setTimeout(resolve, 30)); text(\"after\");"
            []
            3000
        started `shouldBe`
            Right CodeModeRunning
                { cellId = "1"
                , cellOutput = textContent "before"
                }
        finished <- waitCodeCell host "1" 3000
        finished `shouldBe`
            Right CodeModeFinished
                { cellId = "1"
                , cellValue = textContent "after"
                }
        closeCodeModeHost host

    it "persists successful store writes between cells" do
        let config = defaultCodeModeConfig
                "data/code-mode/worker.mjs"
                (\_ _ -> pure $ Left "no tools")
        host <- newCodeModeHost config
        written <- execCodeCell
            host
            "store(\"answer\", { value: 42 });"
            []
            3000
        written `shouldBe`
            Right CodeModeFinished
                { cellId = "1"
                , cellValue = emptyContent
                }
        loaded <- execCodeCell
            host
            "text(load(\"answer\"));"
            []
            3000
        loaded `shouldBe`
            Right CodeModeFinished
                { cellId = "2"
                , cellValue = textContent "{\"value\":42}"
                }
        closeCodeModeHost host

    it "retains partial output and store writes from failed cells" do
        let config = defaultCodeModeConfig
                "data/code-mode/worker.mjs"
                (\_ _ -> pure $ Left "no tools")
        host <- newCodeModeHost config
        failed <- execCodeCell
            host
            "text(\"before failure\"); store(\"candidate\", true); throw new Error(\"boom\");"
            []
            3000
        failed `shouldSatisfy` \case
            Right CodeModeFailed
                { cellId = "1"
                , cellValue
                , cellError
                } ->
                    cellValue == textContent "before failure"
                        && "boom" `Text.isInfixOf` cellError
            _ -> False
        loaded <- execCodeCell
            host
            "text(load(\"candidate\"));"
            []
            3000
        loaded `shouldBe`
            Right CodeModeFinished
                { cellId = "2"
                , cellValue = textContent "true"
                }
        closeCodeModeHost host

    it "turns asynchronous timer callback failures into cell failures" do
        let config = defaultCodeModeConfig
                "data/code-mode/worker.mjs"
                (\_ _ -> pure $ Left "no tools")
        host <- newCodeModeHost config
        failed <- execCodeCell
            host
            "await new Promise(resolve => setTimeout(resolve, 10)); setTimeout(() => { throw new Error(\"timer boom\"); }, 0); await new Promise(() => {});"
            []
            3000
        failed `shouldSatisfy` \case
            Right CodeModeFailed
                { cellValue = value
                , cellError = errorText
                } ->
                    value == emptyContent
                        && "timer boom" `Text.isInfixOf` errorText
            _ -> False
        closeCodeModeHost host

    it "validates generated image metadata before emitting image content" do
        let config = defaultCodeModeConfig
                "data/code-mode/worker.mjs"
                (\_ _ -> pure $ Left "no tools")
        host <- newCodeModeHost config
        failed <- execCodeCell
            host
            "generatedImage({ image_url: \"data:image/png;base64,AA==\", output_hint: 42 });"
            []
            3000
        failed `shouldSatisfy` \case
            Right CodeModeFailed
                { cellValue = value
                , cellError = errorText
                } ->
                    value == emptyContent
                        && "output_hint" `Text.isInfixOf` errorText
            _ -> False
        closeCodeModeHost host

    it "passes descriptions to ALL_TOOLS and treats source as a module" do
        let config = defaultCodeModeConfig
                "data/code-mode/worker.mjs"
                (\_ _ -> pure $ Left "no tools")
        host <- newCodeModeHost config
        metadata <- execCodeCellWithTools
            host
            "text(ALL_TOOLS);"
            [ CodeModeToolMetadata
                { toolMetadataName = "inspect"
                , toolMetadataDescription = "Inspect a value."
                }
            ]
            3000
        metadata `shouldBe`
            Right CodeModeFinished
                { cellId = "1"
                , cellValue =
                    textContent
                        "[{\"name\":\"inspect\",\"description\":\"Inspect a value.\"}]"
                }
        topLevelReturn <- execCodeCell host "return 1;" [] 3000
        topLevelReturn `shouldSatisfy` \case
            Right CodeModeFailed { cellError = errorText } ->
                "return" `Text.isInfixOf` Text.toLower errorText
            _ -> False
        closeCodeModeHost host

    it "routes notify through the host hook without adding normal output" do
        notifications <- newIORef []
        let config =
                (defaultCodeModeConfig
                    "data/code-mode/worker.mjs"
                    (\_ _ -> pure $ Left "no tools"))
                    { notifyHandler = \message ->
                        modifyIORef' notifications (<> [message])
                    }
        host <- newCodeModeHost config
        result <- execCodeCell
            host
            "notify(\"working\");"
            []
            3000
        result `shouldBe`
            Right CodeModeFinished
                { cellId = "1"
                , cellValue = emptyContent
                }
        readIORef notifications `shouldReturn` ["working"]
        closeCodeModeHost host

    it "rejects oversized source before starting a worker" do
        let config =
                (defaultCodeModeConfig
                    "data/code-mode/worker.mjs"
                    (\_ _ -> pure $ Left "no tools"))
                    { maxSourceBytes = 4 }
        host <- newCodeModeHost config
        result <- execCodeCell host "text(\"too large\");" [] 3000
        result `shouldSatisfy` \case
            Left (CodeModeResourceError message) ->
                "configured 4-byte limit" `Text.isInfixOf` message
            _ -> False
        closeCodeModeHost host

    it "routes nested tools through the approval-aware invoke" do
        approvals <- newIORef (0 :: Int)
        worker <- codeModeWorkerPath
        let doubleTool = jsonAppToolWithExecution
                "double"
                "Double an integer."
                [ PropertySchema "value" PropertyInteger True Nothing ]
                AlwaysReadOnly
                ParallelSafe
                (typedTool "double" doubleArgsDecoder \(args :: DoubleArgs) ->
                    pure (Right (Text.pack (show (args.value * 2)))))
            invoke call = do
                modifyIORef' approvals (+ 1)
                result <- dispatchToolCall
                    defaultLoopDispatch
                    [toolHandlerOf doubleTool]
                    call
                pure (Right result)
        created <- newCodeModeToolSet
            CodeOnlyToolMode
            ImageDetailVisible
            worker
            invoke
            [plainNested doubleTool]
        toolSet <- either
            (\err -> expectationFailure (show err) >> fail "unreachable")
            pure
            created
        toolSet.codeModeNestedToolNames `shouldBe` ["double"]
        result <- runRegisteredExec toolSet
            "text(await tools.double({\"value\": 21}));"
        result `shouldSatisfy` Text.isInfixOf "42"
        readIORef approvals `shouldReturn` 1
        toolSet.closeCodeModeToolSet

    it "passes freeform nested tool input without JSON encoding" do
        received <- newIORef Nothing
        worker <- codeModeWorkerPath
        let patch = "*** Begin Patch\n*** End Patch"
            patchTool = freeformApplyPatchAppToolWithExecution
                "apply_patch"
                "Apply a patch."
                AlwaysReadOnly
                TurnSequential
                (textTool "apply_patch" \input -> do
                    writeIORef received (Just input)
                    pure (Right "applied"))
            invoke call = do
                call.callKind `shouldBe` CustomCallKind
                result <- dispatchToolCall
                    defaultLoopDispatch
                    [toolHandlerOf patchTool]
                    call
                pure (Right result)
        created <- newCodeModeToolSet
            CodeOnlyToolMode
            ImageDetailVisible
            worker
            invoke
            [plainNested patchTool]
        toolSet <- either
            (\err -> expectationFailure (show err) >> fail "unreachable")
            pure
            created
        result <- runRegisteredExec toolSet
            "text(await tools.apply_patch(\"*** Begin Patch\\n*** End Patch\"));"
        result `shouldSatisfy` Text.isInfixOf "applied"
        readIORef received `shouldReturn` Just patch
        toolSet.closeCodeModeToolSet

    it "expands nested declarations only in code-only mode" do
        worker <- codeModeWorkerPath
        let lookupTool = jsonAppToolWithExecution
                "lookup"
                "Look up a value."
                [ PropertySchema "key" PropertyString True Nothing ]
                AlwaysReadOnly
                ParallelSafe
                (typedTool "lookup" emptyObjectDecoder \() -> pure (Right "value"))
            invoke _ = pure
                (Right (ToolCallResult "nested" "value" FunctionCallKind))
        codeOnly <- newCodeModeToolSet
            CodeOnlyToolMode ImageDetailVisible worker invoke
            [plainNested lookupTool]
        mixed <- newCodeModeToolSet
            CodeToolMode ImageDetailVisible worker invoke
            [plainNested lookupTool]
        let descriptionOf
                :: Either Text.Text CodeModeToolSet -> IO Text.Text
            descriptionOf built = case built of
                Right toolSet ->
                    case toolSet.codeModeTools of
                        execTool_ : _ -> do
                            toolSet.closeCodeModeToolSet
                            pure execTool_.appToolDescription
                        [] -> fail "missing exec tool"
                Left err -> fail (show err)
        onlyDescription <- descriptionOf codeOnly
        mixedDescription <- descriptionOf mixed
        onlyDescription `shouldSatisfy`
            Text.isInfixOf "### `lookup`"
        onlyDescription `shouldSatisfy`
            Text.isInfixOf "declare const tools: { lookup(args:"
        mixedDescription `shouldSatisfy`
            (not . Text.isInfixOf "### `lookup`")

    it "fails closed before advertising a missing worker" do
        let invoke _ = pure
                (Right (ToolCallResult "nested" "value" FunctionCallKind))
        unavailable <- newCodeModeToolSet
            CodeOnlyToolMode
            ImageDetailVisible
            "data/code-mode/missing-worker.mjs"
            invoke
            []
        case unavailable of
            Left message ->
                message `shouldBe`
                    "code-mode worker script was not found: data/code-mode/missing-worker.mjs"
            Right toolSet -> do
                toolSet.closeCodeModeToolSet
                expectationFailure
                    "expected missing code-mode worker to fail closed"

    it "keeps the first tool when code-mode names normalize to a duplicate" do
        worker <- codeModeWorkerPath
        let mkTool name = jsonAppToolWithExecution
                name
                ("Tool " <> name <> ".")
                []
                AlwaysReadOnly
                ParallelSafe
                (typedTool name emptyObjectDecoder \() ->
                    pure (Right ("from " <> name)))
            invoke call = do
                result <- dispatchToolCall
                    defaultLoopDispatch
                    [toolHandlerOf (mkTool "look-up"), toolHandlerOf (mkTool "look_up")]
                    call
                pure (Right result)
        created <- newCodeModeToolSet
            CodeOnlyToolMode ImageDetailVisible worker invoke
            [plainNested (mkTool "look-up"), plainNested (mkTool "look_up")]
        toolSet <- either
            (\err -> expectationFailure (show err) >> fail "unreachable")
            pure
            created
        toolSet.codeModeNestedToolNames `shouldBe` ["look_up"]
        result <- runRegisteredExec toolSet
            "text(await tools.look_up({}));"
        result `shouldSatisfy` Text.isInfixOf "from look-up"
        toolSet.closeCodeModeToolSet

    it "preserves namespaced tool identity through nested dispatch" do
        worker <- codeModeWorkerPath
        let lookupTool = jsonAppToolWithExecution
                "lookup"
                "Look up a value."
                []
                AlwaysReadOnly
                ParallelSafe
                (typedTool "lookup" emptyObjectDecoder \() ->
                    pure (Right "namespaced result"))
            invoke call = do
                call.name `shouldBe` "lookup"
                result <- dispatchToolCall
                    defaultLoopDispatch
                    [toolHandlerOf lookupTool]
                    call
                pure (Right result)
        created <- newCodeModeToolSet
            CodeOnlyToolMode ImageDetailVisible worker invoke
            [ CodeModeNestedSpec
                { nestedSpecTool = lookupTool
                , nestedSpecNamespace = Just CodeModeNamespace
                    { namespaceName = "catalog"
                    , namespaceDescription = "Catalog tools."
                    }
                }
            ]
        toolSet <- either
            (\err -> expectationFailure (show err) >> fail "unreachable")
            pure
            created
        toolSet.codeModeNestedToolNames `shouldBe` ["catalog__lookup"]
        result <- runRegisteredExec toolSet
            "text(await tools.catalog__lookup({}));"
        result `shouldSatisfy` Text.isInfixOf "namespaced result"
        toolSet.closeCodeModeToolSet

plainNested :: AppTool -> CodeModeNestedSpec
plainNested tool = CodeModeNestedSpec
    { nestedSpecTool = tool
    , nestedSpecNamespace = Nothing
    }

toolHandlerOf :: AppTool -> ToolHandler
toolHandlerOf tool = tool.appToolHandler

-- Run the registered exec tool end to end through its dispatch handler.
runRegisteredExec :: CodeModeToolSet -> Text.Text -> IO Text.Text
runRegisteredExec toolSet source =
    case toolSet.codeModeTools of
        execTool_ : _ -> do
            result <- dispatchToolCall
                defaultLoopDispatch
                [execTool_.appToolHandler]
                (customToolCall "exec-call" "exec" source)
            pure result.output
        [] -> fail "missing exec tool"

newtype DoubleArgs = DoubleArgs { value :: Int }

doubleArgsDecoder :: Json.Decoder DoubleArgs
doubleArgsDecoder = objectArgsExact ["value"] \object_ ->
        DoubleArgs <$> reqInt object_ "value"

emptyObjectDecoder :: Json.Decoder ()
emptyObjectDecoder = Json.object (pure ())

emptyContent :: Value
emptyContent = Aeson.object
    [ "content" Aeson..= ([] :: [Value]) ]

textContent :: Text.Text -> Value
textContent value = Aeson.object
    [ "content" Aeson..=
        [ Aeson.object
            [ "type" Aeson..= ("text" :: String)
            , "text" Aeson..= value
            ]
        ]
    ]

waitForCompletionMarker :: CodeModeHost -> Int -> IO Bool
waitForCompletionMarker _ 0 = do
    expectationFailure "completion-marker was never observed"
    pure False
waitForCompletionMarker host attempts = do
    initial <- execCodeCell
        host
        "text(load(\"completion-marker\") === true ? \"ready\" : \"waiting\");"
        []
        3000
    case initial of
        Right CodeModeFinished { cellValue } ->
            handleMarker cellValue
        Right CodeModeRunning { cellId = identifier } -> do
            completed <- waitCodeCell host identifier 3000
            case completed of
                Right CodeModeFinished { cellValue } ->
                    handleMarker cellValue
                unexpected -> do
                    _ <- terminateCodeCell host identifier
                    expectationFailure $
                        "completion-marker probe did not finish: "
                            <> show unexpected
                    pure False
        unexpected -> do
            expectationFailure $
                "completion-marker probe did not start: " <> show unexpected
            pure False
  where
    handleMarker value
        | value == textContent "ready" = pure True
        | value == textContent "waiting" = do
            threadDelay 10000
            waitForCompletionMarker host (attempts - 1)
        | otherwise = do
            expectationFailure $
                "completion-marker probe returned unexpected value: "
                    <> show value
            pure False

withEnvironmentOverride :: String -> String -> IO a -> IO a
withEnvironmentOverride name value action =
    bracket acquire restore (const action)
  where
    acquire = do
        previous <- lookupEnv name
        setEnv name value
        pure previous
    restore = \case
        Nothing -> unsetEnv name
        Just previous -> setEnv name previous

