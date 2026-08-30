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
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef
import Data.Text (Text)
import System.OsPath (unsafeEncodeUtf)
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

testRuntime
    :: (ToolCall
        -> Maybe Bool
        -> IO (Either Text Bool))
    -> IO (ClaudeSessionRuntimeSlot, PlanModeEnv)
testRuntime approve = do
    let hooks = PlanModeHooks
            { planConfirmEnter = \_ -> pure True
            , planDecideExit = \_ -> pure PlanApprove
            , planAskQuestion = \_ _ -> pure (Just "Blue")
            }
    plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp") (Just hooks)
    slot <- newClaudeSessionRuntimeSlot
    installClaudeSessionRuntime slot ClaudeSessionRuntime
        { approveNativeTool = approve
        , planMode = plan
        }
    pure (slot, plan)

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
