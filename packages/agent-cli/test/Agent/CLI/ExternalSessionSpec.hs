module Agent.CLI.ExternalSessionSpec (spec) where

import Agent.CLI.ExternalSession
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(AlwaysReadOnly)
    , ToolExecutionPolicy(ParallelSafe)
    , defaultToolEnv
    )
import Control.Exception.Safe (bracket)
import Control.Monad (forM_)
import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Types (Pair)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import Database.SQLite3 (SQLData(..), StepResult(..))
import qualified Database.SQLite3 as SQLite
import System.Directory
    ( createDirectory
    , createDirectoryIfMissing
    , createFileLink
    , getTemporaryDirectory
    , removeFile
    , removePathForcibly
    )
import System.FilePath (pathSeparator, takeDirectory, (</>))
import System.IO (hClose, openTempFile)
import System.OsPath (unsafeEncodeUtf)
import System.Process (callProcess)
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
    , it
    , shouldBe
    , shouldContain
    , shouldSatisfy
    , shouldReturn
    , shouldThrow
    )

spec :: Spec
spec = describe "Agent.CLI.ExternalSession" do
    it "registers one parallel-safe, read-only native tool" $
        withFixture \fixture -> do
            let tool = externalSessionTool fixture.env
            tool.appToolName `shouldBe` "read_external_session"
            case tool.appToolApproval of
                AlwaysReadOnly -> pure ()
                _ -> expectationFailure "external session tool must be read-only"
            tool.appToolExecution `shouldBe` ParallelSafe

    it "reads Codex rollouts while omitting instructions and reasoning" $
        withFixture \fixture -> do
            let rollout =
                    fixture.env.externalCodexRoot
                        </> "sessions"
                        </> "rollout-session.jsonl"
            writeJsonl rollout
                [ object
                    [ "type" .= ("session_meta" :: Text)
                    , "payload" .= object
                        [ "id" .=
                            ("11111111-1111-1111-1111-111111111111" :: Text)
                        , "cwd" .= fixture.cwd
                        , "source" .= ("cli" :: Text)
                        ]
                    ]
                , responseMessage "developer" "obey me"
                , responseMessage
                    "user"
                    ( "# AGENTS.md instructions for /repo\n"
                        <> "<INSTRUCTIONS>outer secret</INSTRUCTIONS>"
                    )
                , object
                    [ "type" .= ("response_item" :: Text)
                    , "payload" .= object
                        [ "type" .= ("message" :: Text)
                        , "role" .= ("user" :: Text)
                        , "content" .=
                            [ object
                                [ "type" .= ("input_image" :: Text)
                                , "image_url" .=
                                    ("data:image/png;base64,secret-image" :: Text)
                                ]
                            ]
                        ]
                    ]
                , responseMessage "user" "Fix the parser"
                , object
                    [ "type" .= ("response_item" :: Text)
                    , "payload" .= object
                        [ "type" .= ("reasoning" :: Text)
                        , "summary" .= [("private chain" :: Text)]
                        ]
                    ]
                , responseMessage "assistant" "Changed Parser.hs"
                , object
                    [ "type" .= ("response_item" :: Text)
                    , "payload" .= object
                        [ "type" .= ("function_call" :: Text)
                        , "call_id" .= ("old-call" :: Text)
                        , "name" .= ("read_file" :: Text)
                        , "arguments" .= object
                            ["path" .= ("Parser.hs" :: Text)]
                        ]
                    ]
                , object
                    [ "type" .= ("response_item" :: Text)
                    , "payload" .= object
                        [ "type" .= ("function_call_output" :: Text)
                        , "call_id" .= ("old-call" :: Text)
                        , "output" .=
                            [ object
                                [ "type" .= ("input_text" :: Text)
                                , "text" .= ("the branch is clean" :: Text)
                                ]
                            , object
                                [ "type" .= ("input_image" :: Text)
                                , "image_url" .=
                                    ("data:image/png;base64,tool-secret" :: Text)
                                ]
                            ]
                        ]
                    ]
                ]
            session <- latest fixture.env ExternalCodex 100
            session.externalSessionLastUserRequest
                `shouldBe` Just "Fix the parser"
            session.externalSessionLastAssistantAction
                `shouldBe` Just "Changed Parser.hs"
            let rendered = renderJson session
            rendered `shouldInclude` "the branch is clean"
            rendered `shouldInclude` "\"stale\":\"true\""
            rendered `shouldExclude` "obey me"
            rendered `shouldExclude` "outer secret"
            rendered `shouldExclude` "private chain"
            rendered `shouldExclude` "secret-image"
            rendered `shouldExclude` "tool-secret"
            warningCodes session `shouldContain` ["image_content_omitted"]

    it "redacts nested mixed binary results without discarding generic JSON" $
        withFixture \fixture -> do
            let rollout =
                    fixture.env.externalCodexRoot
                        </> "sessions"
                        </> "mixed-result.jsonl"
            writeJsonl rollout
                [ codexMetadata fixture.cwd "mixed-result"
                , responseMessage "user" "Inspect the mixed tool result"
                , object
                    [ "type" .= ("response_item" :: Text)
                    , "payload" .= object
                        [ "type" .= ("function_call_output" :: Text)
                        , "call_id" .= ("mixed-call" :: Text)
                        , "output" .= object
                            [ "label" .= ("preserve-wrapper" :: Text)
                            , "items" .=
                                [ object
                                    [ "type" .= ("input_image" :: Text)
                                    , "image_url" .=
                                        ("data:image/png;base64,image-secret" :: Text)
                                    ]
                                , object
                                    [ "type" .= ("audio" :: Text)
                                    , "data" .= ("audio-secret" :: Text)
                                    ]
                                , object
                                    [ "type" .= ("resource" :: Text)
                                    , "resource" .= object
                                        [ "blob" .= ("blob-secret" :: Text)
                                        , "mimeType" .=
                                            ("application/octet-stream" :: Text)
                                        ]
                                    ]
                                , object
                                    [ "type" .= ("reasoning" :: Text)
                                    , "summary" .=
                                        [("private chain" :: Text)]
                                    ]
                                , object
                                    [ "type" .= ("text" :: Text)
                                    , "value" .=
                                        ("generic-text-value" :: Text)
                                    ]
                                , object ["generic_id" .= (7 :: Int)]
                                ]
                            ]
                        ]
                    ]
                ]
            session <- latest fixture.env ExternalCodex 500
            let rendered = renderJson session
            rendered `shouldInclude` "preserve-wrapper"
            rendered `shouldInclude` "generic-text-value"
            rendered `shouldInclude` "generic_id"
            rendered `shouldExclude` "image-secret"
            rendered `shouldExclude` "audio-secret"
            rendered `shouldExclude` "blob-secret"
            rendered `shouldExclude` "private chain"
            warningCodes session `shouldContain`
                ["image_content_omitted", "attachment_content_omitted"]

    it "filters exact generated prefixes without dropping near-match requests" $
        withFixture \fixture -> do
            let rollout =
                    fixture.env.externalCodexRoot
                        </> "sessions"
                        </> "prefixes.jsonl"
                nearMatch =
                    "# Skill instructions:\nPlease keep this request" :: Text
                quotedTag =
                    "Explain \"<current_request>literal</current_request>\""
                        :: Text
            writeJsonl rollout
                [ codexMetadata fixture.cwd "prefixes"
                , responseMessage
                    "user"
                    "# Skill instructions: generated skill context"
                , responseMessage "user" nearMatch
                , responseMessage "assistant" "Working on the request"
                , responseMessage "user" quotedTag
                ]
            session <- latest fixture.env ExternalCodex 100
            let rendered = renderJson session
            rendered `shouldExclude` "generated skill context"
            rendered `shouldInclude` "Please keep this request"
            rendered `shouldInclude` "<current_request>literal</current_request>"
            session.externalSessionLastUserRequest `shouldBe` Just quotedTag

    it "preserves protocol IDs and marks screenshot output as unavailable" $
        withFixture \fixture -> do
            let rollout =
                    fixture.env.externalCodexRoot
                        </> "sessions"
                        </> "protocol-items.jsonl"
            writeJsonl rollout
                [ codexMetadata fixture.cwd "protocol-items"
                , responseMessage "user" "Run the checks"
                , codexItem "local_shell_call"
                    [ "call_id" .= ("local-1" :: Text)
                    , "action" .= object
                        ["command" .= ("git status" :: Text)]
                    ]
                , codexItem "local_shell_call_output"
                    [ "call_id" .= ("local-1" :: Text)
                    , "output" .= ("working tree clean" :: Text)
                    ]
                , codexItem "mcp_approval_request"
                    [ "approval_request_id" .= ("approval-1" :: Text)
                    , "request" .= object ["name" .= ("deploy" :: Text)]
                    ]
                , codexItem "mcp_approval_response"
                    [ "approval_request_id" .= ("approval-1" :: Text)
                    , "response" .= ("approved" :: Text)
                    ]
                , codexItem "computer_call"
                    [ "call_id" .= ("computer-1" :: Text)
                    , "action" .= object ["type" .= ("screenshot" :: Text)]
                    ]
                , codexItem "computer_call_output"
                    [ "call_id" .= ("computer-1" :: Text)
                    , "output" .= object
                        [ "type" .= ("computer_screenshot" :: Text)
                        , "image_url" .=
                            ("data:image/png;base64,screenshot-secret" :: Text)
                        ]
                    ]
                ]
            session <- latest fixture.env ExternalCodex 200
            let calls =
                    concatMap (.externalTurnToolCalls)
                        session.externalSessionTurns
                results =
                    concatMap (.externalTurnToolResults)
                        session.externalSessionTurns
            map (.historicalCallId) calls `shouldContain`
                ["local-1", "approval-1", "computer-1"]
            map (.historicalResultCallId) results `shouldContain`
                ["local-1", "approval-1", "computer-1"]
            let rendered = renderJson session
            rendered `shouldInclude` "working tree clean"
            rendered `shouldExclude` "screenshot-secret"
            warningCodes session `shouldContain` ["image_content_omitted"]

    it "applies Codex compaction and rollback records" $
        withFixture \fixture -> do
            let rollout =
                    fixture.env.externalCodexRoot
                        </> "sessions"
                        </> "compacted.jsonl"
            writeJsonl rollout
                [ codexMetadata fixture.cwd "compacted"
                , responseMessage "user" "Stale pre-compact"
                , object
                    [ "type" .= ("compacted" :: Text)
                    , "payload" .= object
                        [ "replacement_history" .=
                            [ messagePayload "user" "Replacement goal"
                            , messagePayload
                                "assistant"
                                "Replacement action"
                            ]
                        ]
                    ]
                , responseMessage "user" "Rolled-back request"
                , responseMessage "assistant" "Rolled-back answer"
                , object
                    [ "type" .= ("event_msg" :: Text)
                    , "payload" .= object
                        [ "type" .= ("thread_rolled_back" :: Text)
                        , "num_turns" .= (1 :: Int)
                        ]
                    ]
                , responseMessage "user" "Active follow-up"
                ]
            session <- latest fixture.env ExternalCodex 100
            let rendered = renderJson session
                turnText =
                    Text.intercalate "\n" $
                        map (.externalTurnText) session.externalSessionTurns
            rendered `shouldInclude` "Replacement goal"
            rendered `shouldInclude` "Replacement action"
            rendered `shouldInclude` "Active follow-up"
            turnText `shouldExclude` "Stale pre-compact"
            turnText `shouldExclude` "Rolled-back request"
            turnText `shouldExclude` "Rolled-back answer"
            session.externalSessionLastUserRequest
                `shouldBe` Just "Active follow-up"

    it "treats a valid Codex database as authoritative and rejects outside rollouts" $
        withFixture \fixture -> do
            let databasePath =
                    fixture.env.externalCodexRoot </> "state_1.sqlite"
                fallback =
                    fixture.env.externalCodexRoot
                        </> "sessions"
                        </> "fallback.jsonl"
                outside = fixture.root </> "outside-rollout.jsonl"
            createDirectoryIfMissing True fixture.env.externalCodexRoot
            writeJsonl fallback
                [ codexMetadata fixture.cwd "fallback"
                , responseMessage "user" "must not be discovered"
                ]
            writeJsonl outside
                [ codexMetadata fixture.cwd "outside"
                , responseMessage "user" "outside secret"
                ]
            withDatabase databasePath \database ->
                SQLite.exec database
                    "CREATE TABLE threads (id TEXT, rollout_path TEXT, \
                    \source TEXT, cwd TEXT, archived INTEGER, title TEXT, \
                    \first_user_message TEXT, created_at REAL, \
                    \updated_at REAL)"
            discoverExternalSessions fixture.env ExternalCodex 0
                `shouldReturn` []
            withDatabase databasePath \database ->
                executeSql database
                    "INSERT INTO threads VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
                    [ SQLText "outside"
                    , SQLText (Text.pack outside)
                    , SQLText "cli"
                    , SQLText (Text.pack fixture.cwd)
                    , SQLInteger 0
                    , SQLText "Outside"
                    , SQLText "Outside"
                    , SQLInteger 1
                    , SQLInteger 2
                    ]
            discoverExternalSessions fixture.env ExternalCodex 0
                `shouldReturn` []

    it "reads compressed Codex rollouts through the native zstd path" $
        withFixture \fixture -> do
            let plain =
                    fixture.env.externalCodexRoot
                        </> "sessions"
                        </> "compressed.jsonl"
                compressed = plain <> ".zst"
            writeJsonl plain
                [ codexMetadata fixture.cwd "compressed"
                , responseMessage "user" "Resume compressed work"
                , responseMessage "assistant" "Recovered compressed history"
                ]
            callProcess fixture.env.externalZstdExecutable
                ["-q", "-f", plain, "-o", compressed]
            removeFile plain
            session <- latest fixture.env ExternalCodex 100
            session.externalSessionLastUserRequest
                `shouldBe` Just "Resume compressed work"
            session.externalSessionLastAssistantAction
                `shouldBe` Just "Recovered compressed history"

    it "follows only Claude's active parent chain and bounds tool arguments" $
        withFixture \fixture -> do
            let slug = map
                    (\character ->
                        if character == pathSeparator then '-' else character)
                    fixture.cwd
                transcript =
                    fixture.env.externalClaudeRoot
                        </> "projects"
                        </> slug
                        </> "claude-session.jsonl"
                common
                    :: Text
                    -> Text
                    -> Maybe Text
                    -> Text
                    -> Value
                    -> Value
                common recordType uuid parent timestamp message =
                    object
                        [ "type" .= (recordType :: Text)
                        , "uuid" .= (uuid :: Text)
                        , "parentUuid" .= parent
                        , "sessionId" .= ("claude-session" :: Text)
                        , "cwd" .= fixture.cwd
                        , "timestamp" .= (timestamp :: Text)
                        , "message" .= message
                        ]
            writeJsonl transcript
                [ common
                    "user"
                    "u1"
                    (Nothing :: Maybe Text)
                    "2026-01-01T00:00:00Z"
                    (object
                        [ "content" .=
                            ( "Instructions supplied by the outer agent harness:\n"
                                <> "<harness_instructions>outer secret</harness_instructions>\n"
                                <> "Current request:\n"
                                <> "<current_request>Add a test</current_request>"
                                :: Text
                            )
                        ])
                , common
                    "attachment"
                    "attachment"
                    (Just "u1")
                    "2026-01-01T00:00:30Z"
                    (object ["content" .= ("attachment metadata" :: Text)])
                , common
                    "assistant"
                    "a1"
                    (Just "attachment")
                    "2026-01-01T00:01:00Z"
                    (object
                        [ "content" .=
                            [ object
                                [ "type" .= ("thinking" :: Text)
                                , "thinking" .= ("private chain" :: Text)
                                ]
                            , object
                                [ "type" .= ("tool_use" :: Text)
                                , "id" .= ("call-1" :: Text)
                                , "name" .= ("Read" :: Text)
                                , "input" .= object
                                    ["file_path" .= Text.replicate 200 "A"]
                                ]
                            , object
                                [ "type" .= ("image" :: Text)
                                , "source" .= object
                                    [ "type" .= ("base64" :: Text)
                                    , "data" .= ("claude-image-secret" :: Text)
                                    ]
                                ]
                            , object
                                [ "type" .= ("text" :: Text)
                                , "text" .= ("Reading the spec" :: Text)
                                ]
                            ]
                        ])
                , common
                    "assistant"
                    "stale"
                    (Just "u1")
                    "2025-01-01T00:01:00Z"
                    (object ["content" .= ("stale branch" :: Text)])
                , object
                    [ "type" .= ("custom-title" :: Text)
                    , "sessionId" .= ("claude-session" :: Text)
                    , "timestamp" .= ("2026-01-01T00:03:00Z" :: Text)
                    , "customTitle" .= ("Renamed Claude task" :: Text)
                    ]
                ]
            session <- latest fixture.env ExternalClaude 40
            session.externalSessionCandidate.candidateTitle
                `shouldBe` "Renamed Claude task"
            session.externalSessionLastUserRequest `shouldBe` Just "Add a test"
            let rendered = renderJson session
            rendered `shouldInclude` "Reading the spec"
            rendered `shouldExclude` "stale branch"
            rendered `shouldExclude` "outer secret"
            rendered `shouldExclude` "attachment metadata"
            rendered `shouldExclude` "private chain"
            rendered `shouldExclude` "claude-image-secret"
            warningCodes session `shouldContain` ["image_content_omitted"]
            let calls =
                    concatMap (.externalTurnToolCalls)
                        session.externalSessionTurns
            case calls of
                call : _ ->
                    Text.length call.historicalCallArguments
                        `shouldSatisfy` (<= 40)
                [] -> expectationFailure "expected a recovered Claude tool call"

    it "does not follow symlinked Claude transcripts during discovery" $
        withFixture \fixture -> do
            let slug = map
                    (\character ->
                        if character == pathSeparator then '-' else character)
                    fixture.cwd
                project =
                    fixture.env.externalClaudeRoot </> "projects" </> slug
                outside = fixture.root </> "outside-claude.jsonl"
                linked = project </> "linked.jsonl"
            writeJsonl outside
                [ object
                    [ "type" .= ("user" :: Text)
                    , "uuid" .= ("outside-user" :: Text)
                    , "sessionId" .= ("outside-session" :: Text)
                    , "cwd" .= fixture.cwd
                    , "message" .= object
                        ["content" .= ("outside secret" :: Text)]
                    ]
                ]
            createDirectoryIfMissing True project
            createFileLink outside linked
            discoverExternalSessions fixture.env ExternalClaude 0
                `shouldReturn` []

    it "reads Cursor's native SQLite store and ignores system metadata" $
        withFixture \fixture -> do
            let sessionId = "33333333-3333-3333-3333-333333333333"
                directory =
                    fixture.env.externalCursorRoot
                        </> "chats"
                        </> "project"
                        </> Text.unpack sessionId
                metadata = directory </> "meta.json"
                store = directory </> "store.db"
            createDirectoryIfMissing True directory
            LBS.writeFile metadata $ encode $ object
                [ "id" .= sessionId
                , "cwd" .= fixture.cwd
                , "title" .= ("Cursor task" :: Text)
                , "updatedAt" .= (2 :: Int)
                ]
            withDatabase store \database -> do
                SQLite.exec database
                    "CREATE TABLE blobs (key TEXT, value TEXT)"
                executeSql database
                    "INSERT INTO blobs VALUES (?, ?)"
                    [ SQLText "conversation"
                    , SQLText $ renderJson $ object
                        [ "messages" .=
                            [ object
                                [ "role" .= ("system" :: Text)
                                , "text" .= ("unsafe instruction" :: Text)
                                ]
                            , object
                                [ "role" .= ("user" :: Text)
                                , "content" .=
                                    [ object
                                        [ "type" .= ("text" :: Text)
                                        , "text" .=
                                            ("Continue Cursor work" :: Text)
                                        ]
                                    , object
                                        [ "type" .= ("image" :: Text)
                                        , "data" .=
                                            ("cursor-image-secret" :: Text)
                                        ]
                                    ]
                                ]
                            , object
                                [ "role" .= ("assistant" :: Text)
                                , "text" .= ("Updated Main.hs" :: Text)
                                ]
                            ]
                        , "metadata" .= object
                            [ "role" .= ("user" :: Text)
                            , "content" .=
                                ("instruction-like metadata" :: Text)
                            ]
                        ]
                    ]
            session <- showReference fixture.env ExternalCursor sessionId 100
            let rendered = renderJson session
            rendered `shouldInclude` "Continue Cursor work"
            rendered `shouldInclude` "Updated Main.hs"
            rendered `shouldExclude` "unsafe instruction"
            rendered `shouldExclude` "instruction-like metadata"
            rendered `shouldExclude` "cursor-image-secret"
            warningCodes session `shouldContain` ["image_content_omitted"]

    it "decodes Grok session directories and bounds streamed histories" $
        withFixture \fixture -> do
            let encoded = concatMap
                    (\character ->
                        if character == pathSeparator then "%2F" else [character])
                    fixture.cwd
                directory =
                    fixture.env.externalGrokRoot
                        </> "sessions"
                        </> encoded
                        </> "grok-session"
            createDirectoryIfMissing True directory
            LBS.writeFile (directory </> "summary.json") $ encode $ object
                [ "info" .= object
                    [ "id" .= ("grok-session" :: Text)
                    , "cwd" .= fixture.cwd
                    ]
                , "session_summary" .= ("Grok task" :: Text)
                , "updated_at" .= ("2026-01-01T00:01:00Z" :: Text)
                ]
            let assistants =
                    [ object
                        [ "type" .= ("assistant" :: Text)
                        , "content" .=
                            ("Grok answer " <> Text.pack (show index))
                        ]
                    | index <- [0 .. 249 :: Int]
                    ]
            writeJsonl (directory </> "chat_history.jsonl") $
                [ object
                    [ "type" .= ("system" :: Text)
                    , "content" .= ("unsafe instruction" :: Text)
                    ]
                , object
                    [ "type" .= ("user" :: Text)
                    , "content" .=
                        [ object
                            [ "type" .= ("text" :: Text)
                            , "text" .= ("Continue Grok work" :: Text)
                            ]
                        , object
                            [ "type" .= ("image" :: Text)
                            , "data" .= ("grok-image-secret" :: Text)
                            ]
                        ]
                    ]
                , object
                    [ "type" .= ("reasoning" :: Text)
                    , "content" .= ("private chain" :: Text)
                    ]
                ] <> assistants
            session <- latest fixture.env ExternalGrok 40
            length session.externalSessionTurns `shouldBe` 200
            session.externalSessionLastUserRequest
                `shouldBe` Just "Continue Grok work"
            session.externalSessionLastAssistantAction
                `shouldBe` Just "Grok answer 249"
            let rendered = renderJson session
            rendered `shouldExclude` "unsafe instruction"
            rendered `shouldExclude` "private chain"
            rendered `shouldExclude` "grok-image-secret"
            warningCodes session
                `shouldContain` ["image_content_omitted", "turns_truncated"]

    it "returns candidates instead of guessing ambiguous title matches" $
        withFixture \fixture -> do
            forM_ [("one" :: Text), "two"] \sessionId -> do
                let directory =
                        fixture.env.externalGrokRoot
                            </> "sessions"
                            </> "repo"
                            </> Text.unpack sessionId
                createDirectoryIfMissing True directory
                LBS.writeFile (directory </> "summary.json") $ encode $ object
                    [ "info" .= object
                        [ "id" .= sessionId
                        , "cwd" .= fixture.cwd
                        ]
                    , "session_summary" .= ("Shared migration task" :: Text)
                    ]
            outcome <- runExternalSession fixture.env ResumeRequest
                { resumeProvider = ExternalGrok
                , resumeOperation = ResumeShow
                , resumeReference = Just "migration"
                , resumeWithinMinutes = 0
                , resumeMaxToolChars = 100
                }
            outcome `shouldSatisfy` \case
                ResumeAmbiguous "migration" candidates ->
                    length candidates == 2
                _ -> False

    it "routes explicit paths through the filesystem access gate" $
        withFixture \fixture -> do
            let outside = fixture.root </> "outside.jsonl"
            writeJsonl outside [responseMessage "user" "outside"]
            runExternalSession fixture.env ResumeRequest
                { resumeProvider = ExternalCodex
                , resumeOperation = ResumeShow
                , resumeReference = Just (Text.pack outside)
                , resumeWithinMinutes = 0
                , resumeMaxToolChars = 100
                }
                `shouldThrow` \case
                    ExternalSessionAccessDenied{} -> True
                    _ -> False

data Fixture = Fixture
    { root :: !FilePath
    , cwd :: !FilePath
    , env :: !ExternalSessionEnv
    }

withFixture :: (Fixture -> IO value) -> IO value
withFixture action =
    withTempDirectory "external-session" \root -> do
        let cwd = root </> "repo"
            scratch = root </> "scratch"
        createDirectory cwd
        createDirectory scratch
        toolEnv <- defaultToolEnv (unsafeEncodeUtf cwd)
        let env = ExternalSessionEnv
                { externalToolEnv = toolEnv
                , externalCwd = cwd
                , externalScratchDirectory = scratch
                , externalHomeDirectory = root
                , externalCodexRoot = root </> "codex"
                , externalClaudeRoot = root </> "claude"
                , externalCursorRoot = root </> "cursor"
                , externalCursorDesktopStores = []
                , externalGrokRoot = root </> "grok"
                , externalZstdExecutable = "zstd"
                , externalNow = fail "externalNow was unexpectedly evaluated"
                }
        action Fixture{root, cwd, env}

latest
    :: ExternalSessionEnv
    -> ExternalProvider
    -> Int
    -> IO ExternalSession
latest env provider maxToolChars =
    resolved =<< runExternalSession env ResumeRequest
        { resumeProvider = provider
        , resumeOperation = ResumeShow
        , resumeReference = Nothing
        , resumeWithinMinutes = 0
        , resumeMaxToolChars = maxToolChars
        }

showReference
    :: ExternalSessionEnv
    -> ExternalProvider
    -> Text
    -> Int
    -> IO ExternalSession
showReference env provider reference maxToolChars =
    resolved =<< runExternalSession env ResumeRequest
        { resumeProvider = provider
        , resumeOperation = ResumeShow
        , resumeReference = Just reference
        , resumeWithinMinutes = 0
        , resumeMaxToolChars = maxToolChars
        }

resolved :: ResumeOutcome -> IO ExternalSession
resolved = \case
    ResumeResolved session -> pure session
    outcome ->
        expectationFailure ("expected resolved session, got " <> show outcome)
            >> fail "unreachable"

responseMessage :: Text -> Text -> Value
responseMessage role text =
    object
        [ "type" .= ("response_item" :: Text)
        , "payload" .= messagePayload role text
        ]

messagePayload :: Text -> Text -> Value
messagePayload role text =
    object
        [ "type" .= ("message" :: Text)
        , "role" .= role
        , "content" .=
            [ object
                [ "type" .=
                    (if role == "assistant"
                        then "output_text"
                        else "input_text" :: Text)
                , "text" .= text
                ]
            ]
        ]

codexMetadata :: FilePath -> Text -> Value
codexMetadata cwd sessionId =
    object
        [ "type" .= ("session_meta" :: Text)
        , "payload" .= object
            [ "id" .= sessionId
            , "cwd" .= cwd
            , "source" .= ("cli" :: Text)
            ]
        ]

codexItem :: Text -> [Pair] -> Value
codexItem itemType fields =
    object
        [ "type" .= ("response_item" :: Text)
        , "payload" .= object (("type" .= itemType) : fields)
        ]

warningCodes :: ExternalSession -> [Text]
warningCodes = map (.externalWarningCode) . (.externalSessionWarnings)

shouldInclude :: Text -> Text -> IO ()
shouldInclude value fragment =
    value `shouldSatisfy` Text.isInfixOf fragment

shouldExclude :: Text -> Text -> IO ()
shouldExclude value fragment =
    value `shouldSatisfy` (not . Text.isInfixOf fragment)

renderJson :: ValueOrJson value => value -> Text
renderJson =
    TextEncoding.decodeUtf8With lenientDecode . LBS.toStrict . encodeJson

class ValueOrJson value where
    encodeJson :: value -> LBS.ByteString

instance ValueOrJson Value where
    encodeJson = encode

instance ValueOrJson ExternalSession where
    encodeJson = encode

writeJsonl :: FilePath -> [Value] -> IO ()
writeJsonl path values = do
    createDirectoryIfMissing True (takeDirectory path)
    LBS.writeFile path $
        mconcat [encode value <> "\n" | value <- values]

withDatabase
    :: FilePath
    -> (SQLite.Database -> IO value)
    -> IO value
withDatabase path =
    bracket (SQLite.open (Text.pack path)) SQLite.close

executeSql :: SQLite.Database -> Text -> [SQLData] -> IO ()
executeSql database sql parameters =
    SQLite.withStatement database sql \statement -> do
        SQLite.bind statement parameters
        SQLite.step statement >>= \case
            Done -> pure ()
            Row -> expectationFailure "unexpected SQLite result row"

withTempDirectory :: String -> (FilePath -> IO value) -> IO value
withTempDirectory template action = do
    base <- getTemporaryDirectory
    bracket
        (do
            (path, handle) <- openTempFile base template
            hClose handle
            removeFile path
            createDirectory path
            pure path)
        removePathForcibly
        action
