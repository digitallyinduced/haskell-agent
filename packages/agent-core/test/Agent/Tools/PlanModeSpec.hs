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
import Agent.Tools.PlanMode.Persistence
import Agent.Tools.PlanMode.Tracker
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolBatchPhase(..)
    , jsonToolParameters
    )
import Control.Exception.Safe (bracket, tryAny)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft, isRight)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
    ( createFileLink
    , doesFileExist
    , getTemporaryDirectory
    , removeFile
    , removeDirectoryRecursive
    )
import System.FilePath ((</>))
import System.OsPath (OsPath, unsafeEncodeUtf)
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

    it "marks entry and exit as mode-changing batch barriers" do
        withTempPlan \env -> do
            (enterPlanModeTool env).appToolBatchPhase
                `shouldBe` ToolBatchModeBarrier
            (exitPlanModeTool env).appToolBatchPhase
                `shouldBe` ToolBatchTerminal

    it "activates and deactivates plan mode" do
        withTempPlan \env -> do
            readPlanModeState env `shouldReturn` PlanInactive
            isPlanModeActive env `shouldReturn` False
            activatePlanMode env
            readPlanModeState env `shouldReturn` PlanActive
            isPlanModeActive env `shouldReturn` True
            deactivatePlanMode env
            isPlanModeActive env `shouldReturn` False

    it "exposes non-breaking session attachment accessors" do
        withTempPlan \env -> do
            readPlanSessionDir env `shouldReturn` Nothing
            attachPlanSessionDir env env.planFallbackDir
                `shouldReturn` Right ()
            readPlanSessionDir env `shouldReturn`
                Just env.planFallbackDir
            setPlanSessionDir env Nothing
            readPlanSessionDir env `shouldReturn` Nothing

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

        it "restores only when no newer revision has won" do
            let snapshot = initialPlanTracker
                active = activatePlanTracker snapshot
                reminded = notePlanReminder active
            planTrackerCurrentRevision active `shouldBe` 1
            planTrackerReminderCount reminded `shouldBe` 1
            restorePlanTrackerIfRevision
                active.trackerRevision
                snapshot
                reminded
                `shouldBe`
                    Left
                        (PlanTrackerRevisionConflict
                            active.trackerRevision
                            reminded.trackerRevision)
            restored <- expectRight $
                restorePlanTrackerIfRevision
                    reminded.trackerRevision
                    snapshot
                    reminded
            restored.trackerPhase `shouldBe` TrackerInactive
            restored.trackerRevision `shouldBe`
                reminded.trackerRevision + 1
            restored.trackerEverActivated `shouldBe` True

    describe "durable plan tracker" do
        it "round-trips the versioned tracker including pending digest fields" do
            let tracker = expectBeginExit
                    "approval-1"
                    validDigest
                    (notePlanReminder
                        (activatePlanTracker initialPlanTracker))
            encoded <- expectRight (encodePlanTracker tracker)
            decodePlanTracker (LBS.toStrict encoded)
                `shouldBe` Right tracker

        it "treats a missing legacy sidecar as inactive" do
            withTempPlan \env -> do
                let directory = env.planFallbackDir
                    statePath = planTrackerStateFilePath directory
                attachPlanSessionDir env directory `shouldReturn` Right ()
                readPlanModeState env `shouldReturn` PlanInactive
                readPlanTracker env `shouldReturn` initialPlanTracker
                doesFileExist (pathText statePath) `shouldReturn` False

        it "persists pre-attach state and restores reminders and revisions" do
            withTempPlan \env -> do
                let directory = env.planFallbackDir
                activatePlanMode env
                updateResult <-
                    updatePlanTracker env (Right . notePlanReminder)
                updateResult `shouldSatisfy` isRight
                expected <- readPlanTracker env
                attachPlanSessionDir env directory `shouldReturn` Right ()
                stateStatus <-
                    getFileStatus
                        (pathText (planTrackerStateFilePath directory))
                fileMode stateStatus `intersectFileModes` accessModes
                    `shouldBe` 0o600
                resumed <- newPlanModeEnv directory Nothing
                attachPlanSessionDir resumed directory `shouldReturn` Right ()
                readPlanModeState resumed `shouldReturn` PlanActive
                readPlanTracker resumed `shouldReturn` expected

        it "persists and restores an exit-pending approval digest" do
            withTempPlan \env -> do
                let directory = env.planFallbackDir
                attachPlanSessionDir env directory `shouldReturn` Right ()
                activatePlanMode env
                updateResult <- updatePlanTracker env
                    (beginExitText "approval-1" validDigest)
                updateResult `shouldSatisfy` isRight
                expected <- readPlanTracker env
                resumed <- newPlanModeEnv directory Nothing
                attachPlanSessionDir resumed directory `shouldReturn` Right ()
                readPlanTracker resumed `shouldReturn` expected
                isPlanModeActive resumed `shouldReturn` True

        it "normalizes transient restart fields and advances the revision" do
            withTempPlan \env -> do
                let directory = env.planFallbackDir
                    persisted =
                        bufferPlanActivation
                            (activatePlanTracker initialPlanTracker)
                writePlanTrackerState directory persisted `shouldReturn` Right ()
                attachPlanSessionDir env directory `shouldReturn` Right ()
                restored <- readPlanTracker env
                restored.trackerBufferedActivation `shouldBe` False
                restored.trackerRevision `shouldBe`
                    persisted.trackerRevision + 1
                readPlanTrackerState directory
                    `shouldReturn` Right (Just restored)

        it "returns corrupt state as a startup error" do
            withTempPlan \env -> do
                let directory = env.planFallbackDir
                    statePath = planTrackerStateFilePath directory
                BS.writeFile (pathText statePath) "{not-json"
                result <- attachPlanSessionDir env directory
                result `shouldSatisfy` either
                    (Text.isInfixOf "invalid plan-mode state JSON")
                    (const False)
                readPlanSessionDir env `shouldReturn` Nothing
                readPlanModeState env `shouldReturn` PlanInactive

        it "refuses a symbolic-link state file" do
            withTempPlan \env -> do
                let directory = env.planFallbackDir
                    statePath = planTrackerStateFilePath directory
                    target = pathText directory </> "outside-state.json"
                writeFile target "{}"
                createFileLink target (pathText statePath)
                result <- attachPlanSessionDir env directory
                result `shouldSatisfy`
                    either
                        (Text.isInfixOf "symbolic-link")
                        (const False)

        it "loads a successfully attached directory only once" do
            withTempPlan \env -> do
                let directory = env.planFallbackDir
                    statePath = planTrackerStateFilePath directory
                attachPlanSessionDir env directory `shouldReturn` Right ()
                activatePlanMode env
                before <- readPlanTracker env
                BS.writeFile (pathText statePath) "{broken"
                attachPlanSessionDir env directory `shouldReturn` Right ()
                readPlanTracker env `shouldReturn` before

        it "keeps a restrictive transition active when persistence fails" do
            withTempPlan \env -> do
                let directory = env.planFallbackDir
                    statePath = planTrackerStateFilePath directory
                    target = pathText directory </> "outside-state.json"
                attachPlanSessionDir env directory `shouldReturn` Right ()
                writeFile target "{}"
                createFileLink target (pathText statePath)
                activation <- tryAny (activatePlanMode env)
                activation `shouldSatisfy` isLeft
                isPlanModeActive env `shouldReturn` True

        it "does not relax active mode when persistence fails" do
            withTempPlan \env -> do
                let directory = env.planFallbackDir
                    statePath = planTrackerStateFilePath directory
                    target = pathText directory </> "outside-state.json"
                attachPlanSessionDir env directory `shouldReturn` Right ()
                activatePlanMode env
                removeFile (pathText statePath)
                writeFile target "{}"
                createFileLink target (pathText statePath)
                deactivation <- tryAny (deactivatePlanMode env)
                deactivation `shouldSatisfy` isLeft
                isPlanModeActive env `shouldReturn` True

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

validDigest :: PlanDigest
validDigest = PlanDigest (Text.replicate 64 "a")

beginExitText
    :: Text
    -> PlanDigest
    -> PlanTracker
    -> Either Text PlanTracker
beginExitText requestKey digest tracker =
    case beginPlanExit requestKey digest tracker of
        Left err -> Left (Text.pack (show err))
        Right value -> Right value

expectBeginExit :: Text -> PlanDigest -> PlanTracker -> PlanTracker
expectBeginExit requestKey digest tracker =
    case beginPlanExit requestKey digest tracker of
        Left err -> error ("unexpected plan exit failure: " <> show err)
        Right value -> value

pathText :: OsPath -> FilePath
pathText = Text.unpack . toText

testDispatchConfig :: ToolDispatchConfig
testDispatchConfig = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "unknown:" <> name
    , toolDispatchFormatResult = either ("ERR " <>) id
    , toolDispatchFormatException = \name _ -> "EX " <> name
    , toolDispatchOnException = \_ _ -> pure ()
    , toolDispatchOnOutput = \_ _ -> pure ()
    , toolDispatchFinalizeOutput = \_call output -> pure output
    }
