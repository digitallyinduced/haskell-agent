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
import Agent.Tools.PlanMode.Document
import Agent.Tools.PlanMode.Tracker
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , jsonToolParameters
    )
import Control.Exception.Safe (bracket)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
    ( createFileLink
    , getTemporaryDirectory
    , removeDirectoryRecursive
    )
import System.FilePath ((</>))
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Files
    ( accessModes
    , fileMode
    , getFileStatus
    , intersectFileModes
    )
import System.Posix.Temp (mkdtemp)
import Test.Hspec

fromFilePath = unsafeEncodeUtf

spec :: Spec
spec = describe "Agent.Tools.PlanMode" do
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

    it "writes and reads an authoritative plan snapshot" do
        withTempPlan \env -> do
            writePlanMarkdown env "# Hello\n" `shouldReturn` Right ()
            content <- readPlanMarkdown env
            content `shouldBe` "# Hello\n"
            readPlanSnapshot env `shouldReturn`
                PlanPresent PlanSnapshot
                    { planSnapshotMarkdown = "# Hello\n"
                    , planSnapshotBytes = "# Hello\n"
                    , planSnapshotDigest =
                        PlanDigest
                            "90f8ec5669cd34183b9b0fdf8b94f5efb4c3672876330f4aa76088c2b4ad17be"
                    }
            path <- planFilePath env
            path `shouldSatisfy` Text.isSuffixOf "plan.md" . toText

    it "authorizes only the exact resolved plan path" do
        isPlanFileEditTarget (fromFilePath "/tmp/sess/plan.md")
            (fromFilePath "/tmp/sess/plan.md") `shouldBe` True
        isPlanFileEditTarget (fromFilePath "/tmp/sess/plan.md")
            (fromFilePath "plan.md") `shouldBe` False
        isPlanFileEditTarget (fromFilePath "/tmp/sess/plan.md")
            (fromFilePath "/tmp/other/plan.md") `shouldBe` False
        isPlanFileEditTarget (fromFilePath "/tmp/sess/plan.md")
            (fromFilePath "/tmp/sess/other.hs") `shouldBe` False

    describe "authoritative plan file" do
        it "distinguishes absence and seeds without truncating existing content" do
            withTempPlan \env -> do
                readPlanSnapshot env `shouldReturn` PlanAbsent
                first <- ensurePlanMarkdown env
                fmap (.planSnapshotMarkdown) first `shouldBe` Right ""
                writePlanMarkdown env "keep me" `shouldReturn` Right ()
                second <- ensurePlanMarkdown env
                fmap (.planSnapshotMarkdown) second `shouldBe` Right "keep me"

        it "writes private mode-0600 files" do
            withTempPlan \env -> do
                writePlanMarkdown env "private" `shouldReturn` Right ()
                path <- planFilePath env
                status <- getFileStatus (Text.unpack (toText path))
                fileMode status `intersectFileModes` accessModes
                    `shouldBe` 0o600

        it "reports invalid UTF-8 instead of treating the plan as empty" do
            withTempPlan \env -> do
                path <- planFilePath env
                BS.writeFile (Text.unpack (toText path)) (BS.pack [0xff, 0xfe])
                readPlanSnapshot env >>= \case
                    PlanUnreadable (PlanFileInvalidUtf8 _ _) -> pure ()
                    result -> expectationFailure
                        ("expected invalid UTF-8, got " <> show result)

        it "refuses a symlink leaf for reads and writes" do
            withTempPlan \env -> do
                path <- planFilePath env
                let pathText = Text.unpack (toText path)
                    target = take (length pathText - length ("plan.md" :: String))
                        pathText
                            </> "outside.md"
                writeFile target "outside"
                createFileLink target pathText
                readPlanSnapshot env `shouldReturn`
                    PlanUnreadable (PlanFileSymlink path)
                writeResult <- writePlanMarkdown env "replacement"
                writeResult `shouldSatisfy` isLeft
                readFile target `shouldReturn` "outside"

    describe "structured plan documents" do
        it "recognizes section aliases and extracts actionable verification" do
            let document = parsePlanDocument $
                    "# Summary\nKeep behavior compatible.\n\n"
                        <> "## Design\nUse a pure state machine.\n\n"
                        <> "## Affected Areas\n- agent-core\n\n"
                        <> "## Tests\n"
                        <> "- cabal repl agent-core:agent-core-test\n"
                        <> "```sh\n"
                        <> "tmux capture-pane -p\n"
                        <> "```\n"
            map (.planSectionKind) document.planDocumentSections
                `shouldBe`
                    [ PlanContext
                    , PlanApproach
                    , PlanChanges
                    , PlanVerification
                    ]
            document.planDocumentVerification `shouldBe`
                [ "cabal repl agent-core:agent-core-test"
                , "tmux capture-pane -p"
                ]
            document.planDocumentWarnings `shouldBe` []

        it "keeps headings inside fenced code out of the structure" do
            let document = parsePlanDocument $
                    "## Approach\nRead first.\n"
                        <> "```markdown\n## Verification\n- not real\n```\n"
                        <> "## Changes\n- parser\n"
            map (.planSectionKind) document.planDocumentSections
                `shouldBe` [PlanApproach, PlanChanges]
            map (.planWarningCode) document.planDocumentWarnings
                `shouldContain` [PlanMissingVerification]

        it "emits advisory warnings without rejecting native Markdown" do
            let document = parsePlanDocument "#Summary\nJust prose."
            map (.planWarningCode) document.planDocumentWarnings
                `shouldMatchList`
                    [ PlanMissingApproach
                    , PlanMissingChangeScope
                    , PlanMissingVerification
                    , PlanMalformedSectionHeading
                    ]

        it "warns when verification has no actionable checks" do
            let document = parsePlanDocument $
                    "## Approach\nDo it.\n"
                        <> "## Changes\nCore.\n"
                        <> "## Verification\nLooks correct.\n"
            map (.planWarningCode) document.planDocumentWarnings
                `shouldBe` [PlanVerificationNotActionable]

    describe "pure plan tracker" do
        it "keeps exit pending write-restricted and rejects stale replies" do
            let digest = PlanDigest "digest"
                active = activatePlanTracker initialPlanTracker
                exited = beginPlanExit "request-1" digest active
            fmap planTrackerRestrictsWrites exited `shouldBe` Right True
            (exited >>= resolvePlanApproval
                    (PlanGeneration 99)
                    digest
                    RevisePlan)
                `shouldBe` Left PlanTrackerStaleResolution

        it "returns to active on revise and retains approved continuation" do
            let digest = PlanDigest "digest"
                continuation = ApprovedPlanContinuation
                    { approvedPlanDigest = digest
                    , approvedPlanVerification = ["cabal repl"]
                    , approvedPlanContinuation = "Implement now."
                    }
                begin = beginPlanExit
                    "request-1"
                    digest
                    (activatePlanTracker initialPlanTracker)
            revised <- expectRight $
                begin >>= resolvePlanApproval
                    (PlanGeneration 1)
                    digest
                    RevisePlan
            revised.trackerPhase `shouldBe` TrackerActive
            approved <- expectRight $
                beginPlanExit "request-2" digest revised
                    >>= resolvePlanApproval
                        (PlanGeneration 2)
                        digest
                        (ApprovePlan continuation)
            approved.trackerPhase `shouldBe` TrackerInactive
            approved.trackerApprovedContinuation
                `shouldBe` Just continuation
            markPlanContinuationDelivered approved
                `shouldSatisfy`
                    ((== Nothing) . (.trackerApprovedContinuation))

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
            answers <- newIORef ["Postgres", "Auth, Logging"]
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
                        ]
                      )
                    , ( "Which features?"
                      , [ "Auth — User authentication"
                        , "Logging — Audit logs"
                        , "Done selecting"
                        ]
                      )
                    ]

        it "maps a displayed structured choice back to its label" do
            let displayed =
                    "Postgres — Reliable relational database — "
                        <> "Preview: CREATE TABLE users (...);"
                hooks = testHooks \_ _ -> pure (Just displayed)
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

        it "keeps accepting the legacy single-question input" do
            seen <- newIORef []
            let hooks = testHooks \question choices -> do
                    writeIORef seen [(question, choices)]
                    pure (Just "Postgres")
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
                    [("Which database?", ["Postgres", "SQLite"])]

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

expectRight :: Show error => Either error value -> IO value
expectRight = \case
    Right value -> pure value
    Left err -> do
        expectationFailure ("expected Right, got Left " <> show err)
        fail "unreachable after expectation failure"

testDispatchConfig :: ToolDispatchConfig
testDispatchConfig = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "unknown:" <> name
    , toolDispatchFormatResult = either ("ERR " <>) id
    , toolDispatchFormatException = \name _ -> "EX " <> name
    , toolDispatchOnException = \_ _ -> pure ()
    , toolDispatchOnOutput = \_ _ -> pure ()
    , toolDispatchFinalizeOutput = \_call output -> pure output
    }
