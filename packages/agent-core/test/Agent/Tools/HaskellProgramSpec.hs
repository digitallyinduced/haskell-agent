module Agent.Tools.HaskellProgramSpec (spec) where

import Agent.Loop
    ( Backend(..)
    , LoopConfig(..)
    , LoopResult(..)
    , TurnInput(..)
    , emptyTokenUsage
    , emptyTurnOutput
    , defaultLoopDispatch
    , runLoop
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallResult(..)
    , ToolDispatchConfig(..)
    , ToolRuntime(..)
    , functionToolCall
    , noArgsTool
    )
import Agent.Tools.Ghci
    ( GhciResult(..)
    , GhciSession
    , closeGhciSession
    , evalGhciProgram
    , newGhciProgramSession
    )
import Agent.Tools.HaskellProgram
    ( haskellProgramTool
    , haskellProgramToolName
    )
import Agent.Tools.PlanMode
    ( PlanModeEnv
    , activatePlanMode
    , isPlanModeActive
    , newPlanModeEnv
    )
import Agent.Tools.Types
    ( ApprovalRule(..)
    , ToolEnv(..)
    , defaultToolEnv
    , dispatchRegisteredToolCall
    , jsonAppTool
    , mkToolRegistry
    )
import Agent.Cancel (newCancelFlag)
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (bracket)
import qualified Data.Aeson as Aeson
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.ByteString.Lazy as LBS
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "Agent.Tools.HaskellProgram" do
    it "keeps nested tool results inside GHCi unless the program emits them" do
        withTempProgramGhci \(ghci, _planMode) -> do
            requests <- newIORef []
            result <- evalGhciProgram ghci programSource 10000
                \name arguments -> do
                    modifyIORef' requests (<> [(name, arguments)])
                    pure "TOP_SECRET_INTERMEDIATE_RESULT"
            result.ghciOk `shouldBe` True
            result.ghciOutput `shouldSatisfy` Text.isInfixOf "selected output"
            result.ghciOutput
                `shouldNotSatisfy` Text.isInfixOf "TOP_SECRET_INTERMEDIATE_RESULT"
            seen <- readIORef requests
            map fst seen `shouldBe` ["echo"]

    it "does not expose the result of a final callTool expression" do
        withTempProgramGhci \(ghci, planMode) -> do
            let runtime = ToolRuntime
                    { invokeNestedTool = \call ->
                        pure ToolCallResult
                            { callId = call.callId
                            , output = "TOP_SECRET_INTERMEDIATE_RESULT"
                            , callKind = call.callKind
                            }
                    }
                dispatchConfig = defaultLoopDispatch
                    { toolDispatchRuntime = Just runtime
                    }
                registry =
                    either (error . Text.unpack) id $
                        mkToolRegistry [haskellProgramTool ghci planMode]
            result <- dispatchRegisteredToolCall
                dispatchConfig
                registry
                (functionToolCall
                    "outer-call"
                    haskellProgramToolName
                    (encodeJson (Aeson.object
                        [ "source" Aeson..=
                            ( "callTool \"echo\" (object [])"
                                :: Text
                            )
                        , "description" Aeson..=
                            ("discard a nested result" :: Text)
                        ])))
            result.output
                `shouldNotSatisfy`
                    Text.isInfixOf "TOP_SECRET_INTERMEDIATE_RESULT"
            result.output
                `shouldSatisfy` Text.isInfixOf "(no output"

    it "isolates Haskell bindings between program calls" do
        withTempProgramGhci \(ghci, _planMode) -> do
            bound <- evalGhciProgram ghci "let programValue = 41" 10000 unusedTool
            bound.ghciOk `shouldBe` True
            emitted <- evalGhciProgram ghci
                "emitText (Text.pack (show (programValue + 1)))"
                10000
                unusedTool
            emitted.ghciOk `shouldBe` False
            emitted.ghciOutput
                `shouldSatisfy` Text.isInfixOf "not in scope"

    it "preimports the documented Text type and aliases" do
        withTempProgramGhci \(ghci, _planMode) -> do
            result <- evalGhciProgram ghci
                ( "let value :: Text; value = T.pack \"ok\" \
                  \in emitText (Text.toUpper value)"
                )
                10000
                unusedTool
            result.ghciOk `shouldBe` True
            result.ghciOutput `shouldSatisfy` Text.isInfixOf "OK"

    it "waits for an in-flight nested call before reporting success" do
        withTempProgramGhci \(ghci, _planMode) -> do
            started <- newIORef False
            finished <- newIORef False
            result <- evalGhciProgram ghci backgroundProgram 10000
                \_name _arguments -> do
                    writeIORef started True
                    threadDelay 200000
                    writeIORef finished True
                    pure "done"
            result.ghciOk `shouldBe` True
            readIORef started `shouldReturn` True
            readIORef finished `shouldReturn` True

    it "terminates delayed background callers at the program boundary" do
        withTempProgramGhci \(ghci, _planMode) -> do
            invoked <- newIORef False
            result <- evalGhciProgram ghci delayedBackgroundProgram 10000
                \_name _arguments -> do
                    writeIORef invoked True
                    pure "unexpected"
            result.ghciOk `shouldBe` True
            threadDelay 400000
            readIORef invoked `shouldReturn` False

    it "starts each program with unpoisoned bridge helpers" do
        withTempProgramGhci \(ghci, _planMode) -> do
            poisoned <- evalGhciProgram ghci
                "let agentGhciCallToolInternal _ _ _ = pure (\"FORGED\" :: Text.Text)"
                10000
                unusedTool
            poisoned.ghciOk `shouldBe` True
            invoked <- newIORef False
            result <- evalGhciProgram ghci
                "callTool \"echo\" (object []) >>= emitText"
                10000
                \_name _arguments -> do
                    writeIORef invoked True
                    pure "REAL"
            result.ghciOk `shouldBe` True
            result.ghciOutput `shouldSatisfy` Text.isInfixOf "REAL"
            result.ghciOutput `shouldNotSatisfy` Text.isInfixOf "FORGED"
            readIORef invoked `shouldReturn` True

    it "routes nested calls through the active loop without adding child results to history" do
        withTempProgramGhci \(ghci, planMode) -> do
            submissions <- newIORef []
            approvals <- newIORef []
            turns <- newIORef (0 :: Int)
            let outerCall = functionToolCall
                    "outer-call"
                    haskellProgramToolName
                    (encodeJson (Aeson.object
                        [ "source" Aeson..= programSource
                        , "description" Aeson..=
                            ("filter a nested tool result" :: Text)
                        ]))
                backend = Backend \previous inputs _onEvent -> do
                    modifyIORef' submissions (<> [(previous, inputs)])
                    turn <- atomicModifyIORef' turns \n -> (n + 1, n)
                    pure $ Right $ case turn of
                        0 -> emptyTurnOutput "response-1" [outerCall] Nothing
                        _ -> emptyTurnOutput "response-2" [] (Just "done")
                echoTool = jsonAppTool
                    "echo"
                    "Return a secret test value."
                    []
                    AlwaysReadOnly
                    (noArgsTool "echo"
                        (pure (Right "TOP_SECRET_INTERMEDIATE_RESULT")))
                tools =
                    either (error . Text.unpack) id $
                        mkToolRegistry
                            [ haskellProgramTool ghci planMode
                            , echoTool
                            ]
            cancel <- newCancelFlag
            let config = LoopConfig
                    { loopBackend = backend
                    , loopTools = tools
                    , loopDispatch = defaultLoopDispatch
                    , loopMaxTurns = 3
                    , loopOnEvent = \_ -> pure ()
                    , loopApprove = \call -> do
                        modifyIORef' approvals (<> [call.name])
                        pure (Right True)
                    , loopCancel = cancel
                    }
            runLoop config Nothing "go" `shouldReturn` Right LoopResult
                { finalResponseId = "response-2"
                , finalText = Just "done"
                , turnsUsed = 2
                , tokenUsage = emptyTokenUsage
                }
            readIORef approvals
                `shouldReturn` [haskellProgramToolName, "echo"]
            seen <- readIORef submissions
            case seen of
                [ (_, [UserMessage "go"])
                    , (Just "response-1", [CompletedTool outerResult])
                    ] -> do
                        outerResult.callId `shouldBe` "outer-call"
                        outerResult.output
                            `shouldSatisfy` Text.isInfixOf "selected output"
                        outerResult.output
                            `shouldNotSatisfy`
                                Text.isInfixOf "TOP_SECRET_INTERMEDIATE_RESULT"
                other ->
                    expectationFailure
                        ("unexpected loop submissions: " <> show other)

    it "refuses unrestricted Haskell execution while Plan Mode is active" do
        withTempProgramGhci \(ghci, planMode) -> do
            activatePlanMode planMode
            let runtime = ToolRuntime
                    { invokeNestedTool = \call ->
                        pure ToolCallResult
                            { callId = call.callId
                            , output = "unexpected"
                            , callKind = call.callKind
                            }
                    }
                dispatchConfig = defaultLoopDispatch
                    { toolDispatchRuntime = Just runtime
                    }
                registry =
                    either (error . Text.unpack) id $
                        mkToolRegistry [haskellProgramTool ghci planMode]
            result <- dispatchRegisteredToolCall
                dispatchConfig
                registry
                (functionToolCall
                    "outer-call"
                    haskellProgramToolName
                    (encodeJson (Aeson.object
                        [ "source" Aeson..= ("emitText \"unsafe\"" :: Text)
                        , "description" Aeson..= ("test plan safety" :: Text)
                        ])))
            result.output
                `shouldSatisfy` Text.isInfixOf "unavailable in Plan Mode"

    it "does not allow a running program to transition into Plan Mode" do
        withTempProgramGhci \(ghci, planMode) -> do
            invoked <- newIORef False
            let runtime = ToolRuntime
                    { invokeNestedTool = \call -> do
                        writeIORef invoked True
                        pure ToolCallResult
                            { callId = call.callId
                            , output = "unexpected"
                            , callKind = call.callKind
                            }
                    }
                dispatchConfig = defaultLoopDispatch
                    { toolDispatchRuntime = Just runtime
                    }
                registry =
                    either (error . Text.unpack) id $
                        mkToolRegistry [haskellProgramTool ghci planMode]
            result <- dispatchRegisteredToolCall
                dispatchConfig
                registry
                (functionToolCall
                    "outer-call"
                    haskellProgramToolName
                    (encodeJson (Aeson.object
                        [ "source" Aeson..=
                            ( "callTool \"enter_plan_mode\" (object []) \
                              \>>= emitText"
                                :: Text
                            )
                        , "description" Aeson..=
                            ("test nested plan transition" :: Text)
                        ])))
            result.output
                `shouldSatisfy` Text.isInfixOf "cannot be called"
            readIORef invoked `shouldReturn` False
            isPlanModeActive planMode `shouldReturn` False

programSource :: Text
programSource = Text.unlines
    [ "do"
    , "  _hidden <- callTool \"echo\" (object [])"
    , "  emitText \"selected output\""
    , "  pure ()"
    ]

delayedBackgroundProgram :: Text
delayedBackgroundProgram = Text.unlines
    [ "do"
    , "  _ <- AgentGhciConcurrentInternal.forkIO $ do"
    , "    AgentGhciConcurrentInternal.threadDelay 300000"
    , "    _ <- callTool \"echo\" (object [])"
    , "    pure ()"
    , "  AgentGhciConcurrentInternal.threadDelay 50000"
    , "  pure ()"
    ]

backgroundProgram :: Text
backgroundProgram = Text.unlines
    [ "do"
    , "  _ <- AgentGhciConcurrentInternal.forkIO $ do"
    , "    _ <- callTool \"echo\" (object [])"
    , "    pure ()"
    , "  AgentGhciConcurrentInternal.threadDelay 50000"
    , "  pure ()"
    ]

unusedTool :: Text -> Aeson.Value -> IO Text
unusedTool name _arguments =
    pure ("unexpected nested tool call: " <> name)

encodeJson :: Aeson.Value -> Text
encodeJson =
    TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode

withTempProgramGhci :: ((GhciSession, PlanModeEnv) -> IO a) -> IO a
withTempProgramGhci action =
    bracket acquire release \(_, ghci, planMode) ->
        action (ghci, planMode)
  where
    acquire = do
        tmp <- getTemporaryDirectory
        dir <- mkdtemp (tmp </> "agent-haskell-program-")
        env <- defaultToolEnv (unsafeEncodeUtf dir)
        ghci <- newGhciProgramSession env
        planMode <- newPlanModeEnv env.toolCwd Nothing
        pure (dir, ghci, planMode)
    release (dir, ghci, _planMode) = do
        closeGhciSession ghci
        removeDirectoryRecursive dir
