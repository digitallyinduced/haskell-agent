module Agent.Tools.PlanModeSpec (spec) where

import Agent.OsPath (toText)
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolDispatchConfig(..)
    , dispatchToolCall
    , functionToolCall
    )
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.Tools.PlanMode
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , jsonToolParameters
    )
import Control.Exception.Safe (bracket)
import qualified Data.Aeson as Aeson
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

fromFilePath = unsafeEncodeUtf

spec :: Spec
spec = describe "Agent.Tools.PlanMode" do
    describe "request_user_input_async" do
        it "advertises Codex titles and optional string choices without generic approval" do
            withTempPlan \env -> do
                let tool = requestUserInputAsyncTool env
                case tool.appToolApproval of
                    AlwaysReadOnly -> pure ()
                    _ -> expectationFailure "async clarification should not prompt for tool approval"
                jsonToolParameters tool `shouldBe` Just
                    [ PropertySchema "questions" (PropertyArray (PropertyObject
                        [ PropertySchema "title" PropertyString True (Just "The question to ask.")
                        , PropertySchema "options" (PropertyArray PropertyString) False
                            (Just "Optional suggested answers. Omit for a free-text question.")
                        ])) True (Just "One or more questions.")
                    ]

        it "publishes questions without entering the blocking input hooks" do
            withTempPlanHooks (testHooks \_ _ -> fail "must not ask synchronously") \env -> do
                setPlanModeInputWaitHooks env (fail "must not begin waiting") (fail "must not end waiting")
                delivered <- newIORef ([] :: [Text])
                setAsyncQuestionDelivery env \message ->
                    modifyIORef' delivered (<> [message]) >> pure (Right ())
                runAsyncTool env "{\"questions\":[{\"title\":\"Color?\",\"options\":[\"Blue\",\"Red\"]},{\"title\":\"Any details?\"}]}"
                    `shouldReturn` "{\"accepted\":true}"
                readIORef delivered `shouldReturn`
                    ["Color?\n- Blue\n- Red\n\nAny details?\n\nReply in the chat; I can continue independent work meanwhile."]
                isPlanModeActive env `shouldReturn` False

        it "does not acknowledge delivery when the host cannot publish questions" do
            withTempPlan \env -> do
                runAsyncTool env "{\"questions\":[{\"title\":\"Color?\"}]}"
                    `shouldReturn` "ERR Asynchronous questions are unavailable in this host."
                setAsyncQuestionDelivery env \_ -> pure (Left "disconnected")
                runAsyncTool env "{\"questions\":[{\"title\":\"Color?\"}]}"
                    `shouldReturn` "ERR disconnected"

        it "rejects invalid questions before delivery" do
            withTempPlan \env -> do
                setAsyncQuestionDelivery env \_ -> fail "must not publish invalid input"
                mapM_ (\arguments -> runAsyncTool env arguments >>= (`shouldSatisfy` Text.isPrefixOf "ERR "))
                    [ "{}"
                    , "{\"questions\":[]}"
                    , "{\"questions\":[{\"title\":\"   \"}]}"
                    , "{\"questions\":[{\"title\":\"Color?\",\"options\":[]}]}"
                    , "{\"questions\":[{\"title\":\"Color?\",\"options\":[\"\"]}]}"
                    , "{\"questions\":[{\"title\":\"Color?\",\"options\":[3]}]}"
                    ]

    it "uses its dedicated confirmation instead of generic tool approval" do
        withTempPlan \env ->
            case (enterPlanModeTool env).appToolApproval of
                AlwaysReadOnly -> pure ()
                _ -> expectationFailure "enter_plan_mode should be read-only"

    it "activates and deactivates plan mode" do
        withTempPlan \env -> do
            isPlanModeActive env `shouldReturn` False
            activatePlanMode env
            isPlanModeActive env `shouldReturn` True
            deactivatePlanMode env
            isPlanModeActive env `shouldReturn` False

    it "brackets questions with the configured input-wait hooks" do
        events <- newIORef ([] :: [Text])
        let record event = modifyIORef' events (<> [event])
            hooks = testHooks \_ _ -> do
                record "question"
                pure (Just "answer")
        withTempPlanHooks hooks \env -> do
            setPlanModeInputWaitHooks
                env
                (record "begin wait")
                (record "end wait")
            env.planHooks.planAskQuestion "Question?" []
                `shouldReturn` Just "answer"
            readIORef events `shouldReturn`
                ["begin wait", "question", "end wait"]

    it "writes and reads plan.md under the fallback directory" do
        withTempPlan \env -> do
            writePlanMarkdown env "# Hello\n" `shouldReturn` Right ()
            content <- readPlanMarkdown env
            content `shouldBe` "# Hello\n"
            path <- planFilePath env
            path `shouldSatisfy` Text.isSuffixOf "plan.md" . toText

    it "recognizes plan.md edit targets" do
        isPlanFileEditTarget (fromFilePath "/tmp/sess/plan.md")
            (fromFilePath "/tmp/sess/plan.md") `shouldBe` True
        isPlanFileEditTarget (fromFilePath "/tmp/sess/plan.md")
            (fromFilePath "plan.md") `shouldBe` True
        isPlanFileEditTarget (fromFilePath "/tmp/sess/plan.md")
            (fromFilePath "/tmp/other/plan.md") `shouldBe` False
        isPlanFileEditTarget (fromFilePath "/tmp/sess/plan.md")
            (fromFilePath "/tmp/sess/other.hs") `shouldBe` False

    describe "ask_user_question" do
        it "advertises the structured Grok Build questions schema" do
            withTempPlan \env -> do
                let parameters =
                        fromMaybe [] (jsonToolParameters (askUserQuestionTool env))
                map (.propertyName) parameters `shouldBe` ["questions"]
                case parameters of
                    [PropertySchema
                        { propertyType =
                            PropertyArray (PropertyObject questionProperties)
                        , required = True
                        }] -> do
                            map (.propertyName) questionProperties
                                `shouldBe` ["question", "options", "multi_select"]
                            map (.required) questionProperties
                                `shouldBe` [True, True, False]
                            case questionProperties of
                                [ _
                                    , PropertySchema
                                        { propertyType =
                                            PropertyArray
                                                (PropertyObject optionProperties)
                                        }
                                    , PropertySchema
                                        { propertyType = PropertyBoolean }
                                    ] -> do
                                        map (.propertyName) optionProperties
                                            `shouldBe`
                                                [ "label"
                                                , "description"
                                                , "preview"
                                                ]
                                        map (.required) optionProperties
                                            `shouldBe` [True, True, False]
                                _ -> expectationFailure
                                    "unexpected structured question schema"
                    _ -> expectationFailure
                        "expected one required questions-array parameter"

        it "asks structured questions sequentially outside plan mode" do
            seen <- newIORef []
            answers <- newIORef
                ["Postgres", "Auth, Logging", "Submit answers"]
            let hooks = testHooks \question choices -> do
                    modifyIORef' seen (<> [(question, choices)])
                    atomicModifyIORef' answers \case
                        answer : rest -> (rest, Just answer)
                        [] -> ([], Nothing)
            withTempPlanHooks hooks \env -> do
                isPlanModeActive env `shouldReturn` False
                output <- runAskTool env $
                    "{\"questions\":["
                        <> "{\"question\":\"Which database?\",\"options\":["
                        <> "{\"label\":\"Postgres\",\"description\":\"Reliable relational database\","
                        <> "\"preview\":\"CREATE TABLE users (...);\"},"
                        <> "{\"label\":\"SQLite\",\"description\":\"Simple embedded database\"}"
                        <> "]},"
                        <> "{\"question\":\"Which features?\",\"options\":["
                        <> "{\"label\":\"Auth\",\"description\":\"User authentication\"},"
                        <> "{\"label\":\"Logging\",\"description\":\"Audit logs\"}"
                        <> "],\"multi_select\":true}"
                        <> "]}"
                output `shouldBe`
                    "User has answered your questions: "
                        <> "\"Which database?\"=\"Postgres\", "
                        <> "\"Which features?\"=\"Auth, Logging\". "
                        <> "You can now continue with the user's answers in mind."
                readIORef seen `shouldReturn`
                    [ ( "Which database?"
                      , [ "Postgres — Reliable relational database — "
                            <> "Preview: CREATE TABLE users (...);"
                        , "SQLite — Simple embedded database"
                        , "Type a custom reply"
                        ]
                      )
                    , ( "Which features?"
                      , [ "Auth — User authentication"
                        , "Logging — Audit logs"
                        , "Done selecting"
                        , "← Back to previous question"
                        , "Type a custom reply"
                        ]
                      )
                    , ( "Review your answers before sending them:\n\n"
                            <> "1. Which database?\n   Postgres\n\n"
                            <> "2. Which features?\n   Auth, Logging"
                      , ["Submit answers", "← Back to last question"]
                      )
                    ]

        it "maps a displayed structured choice back to its label" do
            let displayed =
                    "Postgres — Reliable relational database — "
                        <> "Preview: CREATE TABLE users (...);"
                hooks = testHooks \_ choices ->
                    pure $ Just $
                        if "Submit answers" `elem` choices
                            then "Submit answers"
                            else displayed
            withTempPlanHooks hooks \env ->
                runAskTool env
                    ( "{\"questions\":[{\"question\":\"Which database?\","
                        <> "\"options\":[{\"label\":\"Postgres\","
                        <> "\"description\":\"Reliable relational database\","
                        <> "\"preview\":\"CREATE TABLE users (...);\"}]}]}"
                    )
                    `shouldReturn`
                        "User has answered your questions: "
                            <> "\"Which database?\"=\"Postgres\". "
                            <> "You can now continue with the user's answers in mind."

        it "can go back and replace earlier, multi-select, and final answers" do
            answers <- newIORef
                [ "Postgres"
                , "Auth"
                , "← Back to previous question"
                , "SQLite"
                , "Logging"
                , "Done selecting"
                , "← Back to last question"
                , "Auth"
                , "Done selecting"
                , "Submit answers"
                ]
            let hooks = testHooks \_ _ ->
                    atomicModifyIORef' answers \case
                        answer : rest -> (rest, Just answer)
                        [] -> ([], Nothing)
            withTempPlanHooks hooks \env -> do
                output <- runAskTool env $
                    "{\"questions\":["
                        <> "{\"question\":\"Which database?\",\"options\":["
                        <> "{\"label\":\"Postgres\",\"description\":\"\"},"
                        <> "{\"label\":\"SQLite\",\"description\":\"\"}]},"
                        <> "{\"question\":\"Which feature?\",\"multi_select\":true,"
                        <> "\"options\":["
                        <> "{\"label\":\"Auth\",\"description\":\"\"},"
                        <> "{\"label\":\"Logging\",\"description\":\"\"}]}"
                        <> "]}"
                output `shouldBe`
                    "User has answered your questions: "
                        <> "\"Which database?\"=\"SQLite\", "
                        <> "\"Which feature?\"=\"Auth\". "
                        <> "You can now continue with the user's answers in mind."
                readIORef answers `shouldReturn` []

        it "keeps accepting the legacy single-question input" do
            seen <- newIORef []
            let hooks = testHooks \question choices -> do
                    modifyIORef' seen (<> [(question, choices)])
                    pure $ Just $
                        if "Submit answers" `elem` choices
                            then "Submit answers"
                            else "Postgres"
            withTempPlanHooks hooks \env -> do
                output <- runAskTool env
                    ( "{\"question\":\"Which database?\","
                        <> "\"options\":\"Postgres, SQLite\"}"
                    )
                output `shouldBe`
                    "User has answered your questions: "
                        <> "\"Which database?\"=\"Postgres\". "
                        <> "You can now continue with the user's answers in mind."
                readIORef seen `shouldReturn`
                    [ ( "Which database?"
                      , ["Postgres", "SQLite", "Type a custom reply"]
                      )
                    , ( "Review your answers before sending them:\n\n"
                            <> "1. Which database?\n   Postgres"
                      , ["Submit answers", "← Back to last question"]
                      )
                    ]

        it "offers a custom reply last and returns the entered text" do
            seen <- newIORef []
            answers <- newIORef
                [ "Type a custom reply"
                , "Use CockroachDB instead"
                , "Submit answers"
                ]
            let hooks = testHooks \question choices -> do
                    modifyIORef' seen (<> [(question, choices)])
                    atomicModifyIORef' answers \case
                        answer : rest -> (rest, Just answer)
                        [] -> ([], Nothing)
            withTempPlanHooks hooks \env -> do
                output <- runAskTool env
                    ( "{\"questions\":[{\"question\":\"Which database?\","
                        <> "\"options\":[{\"label\":\"Postgres\","
                        <> "\"description\":\"Relational database\"}]}]}"
                    )
                output `shouldBe`
                    "User has answered your questions: "
                        <> "\"Which database?\"=\"Use CockroachDB instead\". "
                        <> "You can now continue with the user's answers in mind."
                readIORef seen `shouldReturn`
                    [ ( "Which database?"
                      , [ "Postgres — Relational database"
                        , "Type a custom reply"
                        ]
                      )
                    , ("Which database?\nEnter a custom reply:", [])
                    , ( "Review your answers before sending them:\n\n"
                            <> "1. Which database?\n   Use CockroachDB instead"
                      , ["Submit answers", "← Back to last question"]
                      )
                    ]

        it "adds structured answers for Claude's native callback" do
            let hooks = testHooks \_ _ -> pure (Just "Blue")
                input =
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
            withTempPlanHooks hooks \env ->
                answerAskUserQuestionInput env input `shouldReturn`
                    Right
                        (Aeson.object
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
                            , "answers" Aeson..=
                                Aeson.object
                                    [ "Choose a color?" Aeson..=
                                        ("Blue" :: Text)
                                    ]
                            ])

withTempPlan :: (PlanModeEnv -> IO a) -> IO a
withTempPlan = withTempPlanHooks testHooksDefault

withTempPlanHooks :: PlanModeHooks -> (PlanModeEnv -> IO a) -> IO a
withTempPlanHooks hooks action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (tmp </> "agent-plan-XXXXXX"))
        removeDirectoryRecursive
        (\dir -> newPlanModeEnv (fromFilePath dir) (Just hooks) >>= action)

testHooksDefault :: PlanModeHooks
testHooksDefault = testHooks \_ _ -> pure Nothing

testHooks :: (Text -> [Text] -> IO (Maybe Text)) -> PlanModeHooks
testHooks ask = PlanModeHooks
    { planConfirmEnter = \_ -> pure True
    , planDecideExit = \_ -> pure PlanApprove
    , planAskQuestion = ask
    }

runAskTool :: PlanModeEnv -> Text -> IO Text
runAskTool env arguments = do
    result <- dispatchToolCall testDispatchConfig
        [(askUserQuestionTool env).appToolHandler]
        (functionToolCall "ask-1" "ask_user_question" arguments)
    pure result.output

runAsyncTool :: PlanModeEnv -> Text -> IO Text
runAsyncTool env arguments = do
    result <- dispatchToolCall testDispatchConfig
        [(requestUserInputAsyncTool env).appToolHandler]
        (functionToolCall "async-1" "request_user_input_async" arguments)
    pure result.output

testDispatchConfig :: ToolDispatchConfig
testDispatchConfig = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "unknown:" <> name
    , toolDispatchFormatResult = either ("ERR " <>) id
    , toolDispatchFormatException = \name _ -> "EX " <> name
    , toolDispatchOnException = \_ _ -> pure ()
    , toolDispatchOnOutput = \_ _ -> pure ()
    , toolDispatchFinalizeOutput = \_call output -> pure output
    }
