module Agent.CLI.ClaudeSpec (spec) where

import Agent.CLI.Claude
import Agent.Claude
    ( ClaudeCodePermissionRequest(..)
    , ClaudeCodePermissionResult(..)
    )
import Agent.ToolDispatch (ToolCall(..), functionToolCall)
import Agent.Tools.PlanMode
    ( PlanDecision(..)
    , PlanModeHooks(..)
    , PlanModeEnv
    , isPlanModeActive
    , newPlanModeEnv
    , readPlanMarkdown
    )
import Control.Exception.Safe (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef
import Data.Text (Text)
import System.Directory
    ( createDirectory
    , getTemporaryDirectory
    , removeFile
    , removePathForcibly
    )
import System.IO (hClose, openTempFile)
import System.OsPath (OsPath, unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec =
    describe "Claude host permissions" do
        it "classifies native reads and observational shell commands" do
            nativeClaudeToolReadOnly
                (functionToolCall "read" "Read"
                    "{\"file_path\":\"README.md\"}")
                `shouldBe` True
            nativeClaudeToolReadOnly
                (functionToolCall "status" "Bash"
                    "{\"command\":\"git status --short\"}")
                `shouldBe` True
            nativeClaudeToolReadOnly
                (functionToolCall "write" "Bash"
                    "{\"command\":\"rm -rf build\"}")
                `shouldBe` False

        it "routes native permissions through the late-bound host policy" do
            observed <- newIORef []
            (slot, plan) <- testRuntime \call readOnly -> do
                modifyIORef' observed (<> [(call.name, readOnly)])
                pure (Right True)
            result <-
                handleClaudePermissionRequest slot
                    (permissionRequest "Read"
                        (Aeson.object
                            ["file_path" Aeson..= ("README.md" :: Text)]))
            result `shouldBe`
                ClaudeCodePermissionAllow
                    { updatedInput = Nothing
                    , updatedPermissions = []
                    }
            readIORef observed `shouldReturn` [("Read", Just True)]
            isPlanModeActive plan `shouldReturn` False

        it "routes registered tools through the host approval pipeline" do
            observed <- newIORef []
            (slot, _) <- testRuntime \call readOnly -> do
                modifyIORef' observed (<> [(call.name, readOnly)])
                pure (Right False)
            let call =
                    functionToolCall
                        "registered"
                        "database_query"
                        "{\"sql\":\"select 1\"}"
            approveClaudeRegisteredTool slot call
                `shouldReturn` Right False
            readIORef observed
                `shouldReturn` [("database_query", Nothing)]

        it "fails closed until the late-bound runtime is installed" do
            slot <- newClaudeSessionRuntimeSlot
            approveClaudeRegisteredTool slot
                (functionToolCall "registered" "database_query" "{}")
                `shouldReturn`
                    Left "The host approval pipeline is not ready."
            handleClaudePermissionRequest slot
                (permissionRequest "FutureNativeTool" (Aeson.object []))
                `shouldReturn`
                    ClaudeCodePermissionDeny
                        { message =
                            "The host approval pipeline is not ready; the tool was denied."
                        , interrupt = False
                        }

        it "treats unknown native tools as mutating and preserves denials" do
            observed <- newIORef []
            (slot, _) <- testRuntime \call readOnly -> do
                modifyIORef' observed (<> [(call.name, readOnly)])
                pure (Left "host policy denied the unknown tool")
            handleClaudePermissionRequest slot
                (permissionRequest "FutureNativeTool" (Aeson.object []))
                `shouldReturn`
                    ClaudeCodePermissionDeny
                        { message = "host policy denied the unknown tool"
                        , interrupt = False
                        }
            readIORef observed
                `shouldReturn` [("FutureNativeTool", Just False)]

        it "returns native AskUserQuestion answers in updatedInput" do
            (slot, _) <- testRuntime \_ _ -> pure (Right True)
            let input =
                    Aeson.object
                        [ "questions" Aeson..=
                            [ Aeson.object
                                [ "question" Aeson..=
                                    ("Choose a color?" :: Text)
                                , "options" Aeson..=
                                    [ Aeson.object
                                        [ "label" Aeson..= ("Red" :: Text)
                                        , "description" Aeson..= ("" :: Text)
                                        ]
                                    , Aeson.object
                                        [ "label" Aeson..= ("Blue" :: Text)
                                        , "description" Aeson..= ("" :: Text)
                                        ]
                                    ]
                                ]
                            ]
                        ]
            result <-
                handleClaudePermissionRequest slot
                    (permissionRequest "AskUserQuestion" input)
            case result of
                ClaudeCodePermissionAllow
                    { updatedInput = Just (Aeson.Object updated) } ->
                        KeyMap.lookup "answers" updated `shouldBe`
                            Just
                                (Aeson.object
                                    [ "Choose a color?" Aeson..=
                                        ("Blue" :: Text)
                                    ])
                _ ->
                    expectationFailure
                        ("expected an updated allow result, got " <> show result)

        it "synchronizes Claude's native EnterPlanMode with host policy" do
            (slot, plan) <- testRuntime \_ _ -> pure (Right True)
            handleClaudePermissionRequest slot
                (permissionRequest "EnterPlanMode"
                    (Aeson.object
                        ["explanation" Aeson..= ("Inspect first" :: Text)]))
                `shouldReturn`
                    ClaudeCodePermissionAllow
                        { updatedInput = Nothing
                        , updatedPermissions = []
                        }
            isPlanModeActive plan `shouldReturn` True

        it "bypasses only the host's already-approved MCP envelope" do
            observed <- newIORef (0 :: Int)
            (slot, _) <- testRuntime \_ _ -> do
                modifyIORef' observed (+ 1)
                pure (Right True)
            handleClaudePermissionRequest slot
                (permissionRequest
                    "mcp__haskell-agent__database_query"
                    (Aeson.object []))
                `shouldReturn`
                    ClaudeCodePermissionAllow
                        { updatedInput = Nothing
                        , updatedPermissions = []
                        }
            readIORef observed `shouldReturn` 0
            _ <- handleClaudePermissionRequest slot
                (permissionRequest
                    "mcp__another-server__database_query"
                    (Aeson.object []))
            readIORef observed `shouldReturn` 1

        it "rejects ExitPlanMode when planning is inactive" do
            (slot, plan) <- testRuntime \_ _ -> pure (Right True)
            handleClaudePermissionRequest slot
                (permissionRequest "ExitPlanMode" (Aeson.object []))
                `shouldReturn`
                    ClaudeCodePermissionDeny
                        { message = "Plan mode is not active."
                        , interrupt = False
                        }
            isPlanModeActive plan `shouldReturn` False

        it "persists a supplied native plan before presenting it" $
            withPlanDirectory \directory -> do
                (slot, plan) <-
                    testRuntimeAt directory \_ _ -> pure (Right True)
                _ <- handleClaudePermissionRequest slot
                    (permissionRequest "EnterPlanMode" (Aeson.object []))
                handleClaudePermissionRequest slot
                    (permissionRequest "ExitPlanMode"
                        (Aeson.object
                            ["plan" Aeson..= ("# Safe plan" :: Text)]))
                    `shouldReturn`
                        ClaudeCodePermissionAllow
                            { updatedInput = Nothing
                            , updatedPermissions = []
                            }
                readPlanMarkdown plan `shouldReturn` "# Safe plan"
                isPlanModeActive plan `shouldReturn` False

testRuntime
    :: (ToolCall
        -> Maybe Bool
        -> IO (Either Text Bool))
    -> IO (ClaudeSessionRuntimeSlot, PlanModeEnv)
testRuntime approve = do
    testRuntimeAt (unsafeEncodeUtf "/tmp") approve

testRuntimeAt
    :: OsPath
    -> (ToolCall
        -> Maybe Bool
        -> IO (Either Text Bool))
    -> IO (ClaudeSessionRuntimeSlot, PlanModeEnv)
testRuntimeAt directory approve = do
    let hooks = PlanModeHooks
            { planConfirmEnter = \_ -> pure True
            , planDecideExit = \_ -> pure PlanApprove
            , planAskQuestion = \_ _ -> pure (Just "Blue")
            }
    plan <- newPlanModeEnv directory (Just hooks)
    slot <- newClaudeSessionRuntimeSlot
    installClaudeSessionRuntime slot ClaudeSessionRuntime
        { approveNativeTool = approve
        , approveRegisteredTool = \call -> approve call Nothing
        , planMode = plan
        }
    pure (slot, plan)

withPlanDirectory :: (OsPath -> IO a) -> IO a
withPlanDirectory action =
    bracket acquire removePathForcibly (action . unsafeEncodeUtf)
  where
    acquire = do
        temporary <- getTemporaryDirectory
        (path, handle) <- openTempFile temporary "agent-claude-plan-"
        hClose handle
        removeFile path
        createDirectory path
        pure path

permissionRequest :: Text -> Aeson.Value -> ClaudeCodePermissionRequest
permissionRequest toolName input =
    ClaudeCodePermissionRequest
        { toolName
        , input
        , permissionSuggestions = []
        , blockedPath = Nothing
        , decisionReason = Nothing
        , title = Nothing
        , displayName = Nothing
        , description = Nothing
        , toolUseId = Just "tool-use"
        , agentId = Nothing
        , raw = input
        }
