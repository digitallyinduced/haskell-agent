module Agent.CLI.ResumeSpec (spec) where

import Agent.CLI.Picker (PickerKey(..))
import Agent.CLI.Resume
import Agent.Runtime.Session
    ( LegacySubagentTarget(..)
    , SessionMeta(..)
    , SessionResumeStats(..)
    , SessionTurn(..)
    , TranscriptEffect(..)
    )
import Agent.Dialect (DialectId(..))
import Agent.OpenAI.Compaction (userTextItem)
import Agent.Provider (Provider(..))
import Agent.Responses.Types
    ( CompactionItem(..)
    , MessageContent(..)
    , ResponseContentPart(..)
    , ResponseItem(..)
    , ResponseMessage(..)
    , ResponseRole(..)
    )
import Agent.Store.Postgres.Session (ConversationSearchResult(..))
import qualified Agent.Store.Postgres.Session as Store
import Agent.Store.Postgres (openStore, closeStore, trustedPool, storeConfig, defaultManagedPostgresConfig)
import Agent.Store.Postgres.Managed (stopManagedPostgres)
import Agent.Store.Types (renderStoreError)
import Control.Exception.Safe (bracket)
import Control.Monad (forM_, void)
import Data.Either (isLeft)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import qualified System.Directory as Directory
import qualified System.Directory.OsPath as OsDirectory
import qualified System.FilePath as FilePath
import System.Posix.Temp (mkdtemp)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Time.Clock (addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Text as Text
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

fromFilePath = unsafeEncodeUtf

spec :: Spec
spec = do
    describe "loadLatestResumeSession" do
        it "selects an exact canonical directory across pages, excluding archived and deleted sessions" do
            temporary <- Directory.getTemporaryDirectory
            bracket
                (mkdtemp (temporary FilePath.</> "rs"))
                Directory.removeDirectoryRecursive
                \base -> do
                    postgresBin <- fromMaybe "" <$> lookupEnv "AGENT_POSTGRES_BIN"
                    let project = base FilePath.</> "project"
                        sibling = base FilePath.</> "project-other"
                        alias = base FilePath.</> "alias"
                        child = project FilePath.</> "child"
                        cwd = fromFilePath project
                        config = defaultManagedPostgresConfig (base FilePath.</> "db") postgresBin
                        timestamp = posixSecondsToUTCTime 1000
                        metadata key path age = Store.SessionMetadata
                            { sessionMetadataKey = key
                            , sessionMetadataVersion = 1
                            , sessionMetadataCreatedAt = timestamp
                            , sessionMetadataUpdatedAt = addUTCTime age timestamp
                            , sessionMetadataProvider = "xai"
                            , sessionMetadataConnection = "xai"
                            , sessionMetadataGatewayIdentity = Nothing
                            , sessionMetadataModel = "grok-4.6"
                            , sessionMetadataTransportModel = Just "grok-4.6"
                            , sessionMetadataDialect = "grok-build"
                            , sessionMetadataLegacyTarget = Nothing
                            , sessionMetadataCwd = Text.pack path
                            , sessionMetadataEffort = "medium"
                            , sessionMetadataTitle = key
                            , sessionMetadataTitleIsManual = False
                            , sessionMetadataTitleRefreshIndex = 0
                            , sessionMetadataTitleUserTurns = 0
                            , sessionMetadataLastResponseId = Nothing
                            , sessionMetadataInputTokens = 0
                            , sessionMetadataOutputTokens = 0
                            , sessionMetadataCachedTokens = 0
                            , sessionMetadataLastRecap = Nothing
                            , sessionMetadataLastTurnSummary = Nothing
                            , sessionMetadataLastRecapMainTurns = 0
                            , sessionMetadataHeadless = False
                            }
                    mapM_ (Directory.createDirectoryIfMissing True) [project, sibling, child]
                    Directory.createDirectoryLink project alias
                    bracket
                        (openStore config >>= either (fail . Text.unpack . renderStoreError) pure)
                        (\store -> closeStore store >> void (stopManagedPostgres (storeConfig store)))
                        \store -> do
                            let pool = trustedPool store
                                insert meta = Store.createSession pool meta
                                    `shouldReturn` Right True
                            loadLatestResumeSession pool cwd Nothing InteractiveSessions >>= (`shouldSatisfy` isLeft)
                            insert ((metadata "headless-only" base 100) { Store.sessionMetadataHeadless = True })
                            loadLatestResumeSession pool (fromFilePath base) Nothing InteractiveSessions >>= (`shouldSatisfy` isLeft)
                            loadLatestResumeSession pool (fromFilePath base) Nothing AllSessions `shouldReturn` Right "headless-only"
                            insert (metadata "older" project 0)
                            insert (metadata "newest-b" project 10)
                            insert ((metadata "newest-a" project 10)
                                { Store.sessionMetadataConnection = "organization-gateway"
                                , Store.sessionMetadataGatewayIdentity = Just "another-identity"
                                })
                            insert (metadata "archived" project 40)
                            Store.setSessionArchived pool "archived" True (addUTCTime 100 timestamp) `shouldReturn` Right True
                            insert (metadata "deleted" project 50)
                            Store.deleteSession pool "deleted" (addUTCTime 100 timestamp) `shouldReturn` Right True
                            insert (metadata "child" child 60)
                            -- More than one page of newer conversations from a
                            -- similarly named sibling must not displace the match.
                            forM_ [1 .. 101 :: Int] \index ->
                                insert (metadata ("sibling-" <> Text.pack (show index)) sibling 70)
                            insert ((metadata "headless" project 100) { Store.sessionMetadataHeadless = True })
                            let unreadable = base FilePath.</> "unreadable"
                                resolveDirectory path
                                    | path == fromFilePath unreadable = ioError (userError "directory unavailable")
                                    | otherwise = OsDirectory.canonicalizePath path
                            insert (metadata "unreadable-directory" unreadable 150)
                            loadLatestResumeSessionWithCanonicalize resolveDirectory pool cwd Nothing InteractiveSessions `shouldReturn` Right "newest-a"
                            loadLatestResumeSessionWithCanonicalize resolveDirectory pool (fromFilePath unreadable) Nothing InteractiveSessions >>= (`shouldSatisfy` isLeft)
                            loadLatestResumeSession pool cwd Nothing InteractiveSessions `shouldReturn` Right "newest-a"
                            loadLatestResumeSession pool cwd Nothing AllSessions `shouldReturn` Right "headless"
                            loadLatestResumeSession pool (fromFilePath alias) Nothing InteractiveSessions `shouldReturn` Right "newest-a"
                            loadLatestResumeSession pool (fromFilePath base) Nothing InteractiveSessions >>= (`shouldSatisfy` isLeft)
                            loadLatestResumeSession pool cwd (Just "current-identity") InteractiveSessions `shouldReturn` Right "newest-a"
                            insert ((metadata "unsupported" project 200) { Store.sessionMetadataVersion = 999 })
                            loadLatestResumeSession pool cwd Nothing InteractiveSessions
                                `shouldReturn` Left "unsupported session schema version 999 for session unsupported (expected 1)"

    describe "publishResumeHistoryAfterBoundary" do
        it "does not publish fullscreen history across a gateway boundary" do
            published <- newIORef False
            publishResumeHistoryAfterBoundary
                (Left "gateway boundary mismatch")
                (writeIORef published True)
                `shouldReturn` Left "gateway boundary mismatch"
            readIORef published `shouldReturn` False

        it "publishes fullscreen history after boundary validation" do
            published <- newIORef False
            publishResumeHistoryAfterBoundary
                (Right ())
                (writeIORef published True)
                `shouldReturn` Right ()
            readIORef published `shouldReturn` True

    describe "resumeEntriesFrom" do
        it "uses untitled when the title is empty" do
            case resumeEntriesFrom [(sampleMeta "abc" "", [])] of
                [entry] -> do
                    entry.resumeTitle `shouldBe` "(untitled)"
                    entry.resumeId `shouldBe` "abc"
                other ->
                    expectationFailure ("expected one entry, got " <> show (length other))

        it "captures metadata and loaded transcript details" do
            case resumeEntriesFrom [(sampleMeta "abc" "first", [sampleTurn])] of
                [entry] -> do
                    entry.resumeProject `shouldBe` "tmp-repo"
                    entry.resumeCwd `shouldBe` "/tmp/repo"
                    entry.resumeLoaded `shouldBe` True
                    entry.resumeMessageCount `shouldBe` 2
                    entry.resumeTurnCount `shouldBe` 1
                    entry.resumeToolCount `shouldBe` 0
                    entry.resumePrompt `shouldBe` "hello"
                other ->
                    expectationFailure ("expected one entry, got " <> show (length other))

        it "builds cheap metadata-only entries for the fullscreen list" do
            let entry = resumeEntryFromMeta (sampleMeta "abc" "first")
            entry.resumeLoaded `shouldBe` False
            entry.resumeTurnCount `shouldBe` 0

        it "keeps full-session counts and first prompt when the preview is truncated" do
            let recent = [sampleTurn { turnUserText = "later question" }]
                stats = SessionResumeStats
                    { resumeStatsTurnCount = 80
                    , resumeStatsMessageCount = 160
                    , resumeStatsToolCount = 12
                    , resumeStatsFirstPrompt = Just "hello"
                    }
                entry =
                    resumeEntryFromPage
                        (sampleMeta "abc" "first")
                        stats
                        recent
            entry.resumeLoaded `shouldBe` True
            entry.resumeTurnCount `shouldBe` 80
            entry.resumeMessageCount `shouldBe` 160
            entry.resumeToolCount `shouldBe` 12
            entry.resumePrompt `shouldBe` "hello"
            entry.resumeTranscript
                `shouldSatisfy` any (Text.isInfixOf "later question")

    describe "filterResumeSessionsForBoundary" do
        let direct = sampleMeta "direct" "direct"
            gateway identity sessionId =
                (sampleMeta sessionId sessionId)
                    { metaConnection = "organization-gateway"
                    , metaGatewayIdentity = Just identity
                    }
            legacyGateway =
                (gateway "gateway-a" "legacy")
                    { metaGatewayIdentity = Nothing }
            sessions =
                [ direct
                , gateway "gateway-a" "allowed"
                , gateway "gateway-b" "other"
                , legacyGateway
                ]

        it "offers every session while disconnected" do
            map (.metaId)
                (filterResumeSessionsForBoundary Nothing sessions)
                `shouldBe` ["direct", "allowed", "other", "legacy"]

        it "offers every session while connected to a gateway" do
            map (.metaId)
                (filterResumeSessionsForBoundary
                    (Just "gateway-a")
                    sessions)
                `shouldBe` ["direct", "allowed", "other", "legacy"]

        it "allows resume metadata from another gateway credential" do
            validateResumeMetaForBoundary
                (Just "gateway-a")
                (gateway "gateway-b" "other")
                `shouldBe` Right ()

    describe "resumeNeedsGeneratedContext" do
        it "requeues context after compact, clear, and new boundaries" do
            map
                (\marker ->
                    resumeNeedsGeneratedContext
                        [sampleTurn
                            { turnUserText = marker
                            , turnEffect =
                                if marker == "/compact"
                                    then TranscriptReplace
                                    else TranscriptReset
                            }
                        ])
                ["/compact", "/clear", "/new"]
                `shouldBe` [True, True, True]

        it "requeues context after a typed automatic-compaction checkpoint" do
            let checkpoint =
                    CompactionItemValue CompactionItem
                        { itemId = Nothing
                        , encryptedContent = Nothing
                        }
            resumeNeedsGeneratedContext
                [sampleTurn
                    { turnItems = [checkpoint]
                    , turnEffect = TranscriptReplace
                    }
                ]
                `shouldBe` True

        it "repairs old compacted sessions until regenerated context persists" do
            let boundary = sampleTurn
                    { turnUserText = "/compact"
                    , turnEffect = TranscriptReplace
                    }
                ordinary = sampleTurn
                    { turnUserText = "continue"
                    , turnItems = [userTextItem "ordinary input"]
                    }
                regenerated = ordinary
                    { turnItems =
                        [ userTextItem
                            "## Skills\nThe following reusable skills are available in this session.\n"
                        ]
                    }
            resumeNeedsGeneratedContext [boundary, ordinary]
                `shouldBe` True
            resumeNeedsGeneratedContext [boundary, regenerated]
                `shouldBe` False
            let memoryOnly = ordinary
                    { turnItems =
                        [ userTextItem
                            "<structured-memory>\n## Available structured memory\n</structured-memory>"
                        ]
                    }
            resumeNeedsGeneratedContext [boundary, memoryOnly]
                `shouldBe` False

        it "does not mistake ephemeral harness context for a reload" do
            let boundary = sampleTurn
                    { turnUserText = "/compact"
                    , turnEffect = TranscriptReplace
                    }
                ephemeral text = sampleTurn
                    { turnUserText = "continue"
                    , turnItems = [userTextItem text]
                    }
            map
                (\text ->
                    resumeNeedsGeneratedContext [boundary, ephemeral text])
                [ "Plan mode is active. Do not make any edits or writes to the system except for the plan file."
                , "# Skill instructions: one-off"
                , "<subagent_notification>done</subagent_notification>"
                ]
                `shouldBe` [True, True, True]

        it "does not requeue context without a transcript boundary" do
            resumeNeedsGeneratedContext [sampleTurn] `shouldBe` False

        it "does not infer a boundary from current assistant text" do
            let ordinarySummaryHeading = sampleTurn
                    { turnItems =
                        [ MessageItem ResponseMessage
                            { messageId = Nothing
                            , content = MessageContentParts
                                [ OutputTextPart
                                    "Compacted conversation summary:\nordinary reply"
                                    Nothing
                                    Nothing
                                ]
                            , role = RoleAssistant
                            , status = Nothing
                            , phase = Nothing
                            , passthrough = Nothing
                            }
                        ]
                    , turnEffect = TranscriptAppend
                    }
            resumeNeedsGeneratedContext [ordinarySummaryHeading]
                `shouldBe` False

    describe "resolveSessionInitialContext" do
        it "requests generated context for a fresh session" do
            let initial = resolveSessionInitialContext False False Nothing
            initial.initialContextItems `shouldBe` []
            initial.initialContextResumeNeedsFresh `shouldBe` False
            initial.initialContextPrevious `shouldBe` Nothing
            initial.initialContextNeeded `shouldBe` True
            initial.initialContextMayRestoreSnapshot `shouldBe` False

        it "reuses a compatible response chain without regenerating context" do
            let meta =
                    (sampleMeta "abc" "first")
                        { metaLastResponseId = Just "response-1"
                        }
                initial =
                    resolveSessionInitialContext
                        False
                        False
                        (Just (meta, [sampleTurn]))
            initial.initialContextPrevious `shouldBe` Just "response-1"
            initial.initialContextNeeded `shouldBe` False
            initial.initialContextMayRestoreSnapshot `shouldBe` False

        it "drops the response chain when the provider target changes" do
            let meta =
                    (sampleMeta "abc" "first")
                        { metaLastResponseId = Just "response-1"
                        }
                initial =
                    resolveSessionInitialContext
                        False
                        True
                        (Just (meta, [sampleTurn]))
            initial.initialContextPrevious `shouldBe` Nothing
            initial.initialContextNeeded `shouldBe` False
            initial.initialContextMayRestoreSnapshot `shouldBe` False

        it "requests generated context after a transcript reset" do
            let boundary =
                    sampleTurn
                        { turnEffect = TranscriptReset
                        }
                initial =
                    resolveSessionInitialContext
                        False
                        False
                        (Just (sampleMeta "abc" "first", [boundary]))
            initial.initialContextResumeNeedsFresh `shouldBe` True
            initial.initialContextNeeded `shouldBe` True
            initial.initialContextMayRestoreSnapshot `shouldBe` False

        it "marks a saved prompt snapshot as a possible fast resume" do
            let meta =
                    (sampleMeta "abc" "first")
                        { metaLastResponseId = Nothing
                        , metaPromptSnapshot =
                            Just (error "snapshot value should remain lazy")
                        }
                initial =
                    resolveSessionInitialContext
                        False
                        False
                        (Just (meta, []))
            initial.initialContextNeeded `shouldBe` True
            initial.initialContextMayRestoreSnapshot `shouldBe` True

    describe "applyResumeKey" do
        let entries =
                resumeEntriesFrom
                    [ (sampleMeta "one" "first", [])
                    , (sampleMeta "two" "second", [])
                    ]
            state0 = initialResumeState entries

        it "confirms the first session" do
            case applyResumeKey PickerKeyConfirm state0 of
                Left (Just entry) -> entry.resumeId `shouldBe` "one"
                other -> expectationFailure ("unexpected " <> show other)

        it "moves down and confirms" do
            case applyResumeKey PickerKeyDown state0 of
                Right down ->
                    case applyResumeKey PickerKeyConfirm down of
                        Left (Just entry) -> entry.resumeId `shouldBe` "two"
                        other -> expectationFailure ("unexpected " <> show other)
                Left other -> expectationFailure ("unexpected " <> show other)

        it "filters by typed characters" do
            let typed =
                    foldl
                        (\s c -> case applyResumeKey (PickerKeyChar c) s of
                            Right s' -> s'
                            Left _ -> s)
                        state0
                        ("sec" :: String)
            map (.resumeId) (visibleResume typed) `shouldBe` ["two"]

    describe "ResumeBrowser" do
        let now = posixSecondsToUTCTime (3 * 60 * 60)
            entries =
                resumeEntriesFrom
                    [ (sampleMeta "one" "first", [])
                    , (sampleMeta "two" "second", [])
                    ]
            browser0 = initialResumeBrowser now entries

        it "uses explicit search state and filters across session metadata" do
            browser0.resumeBrowserSearching `shouldBe` False
            let searched =
                    insertResumeSearch "sec" (beginResumeSearch browser0)
            searched.resumeBrowserSearching `shouldBe` True
            map (.resumeId) (visibleResumeBrowser searched)
                `shouldBe` ["two"]
            endResumeSearch searched
                `shouldSatisfy` (not . (.resumeBrowserSearching))

        it "keeps PostgreSQL search results visible after applying a query" do
            let applied =
                    applyResumeSearchResults
                        "database migration"
                        (take 1 entries)
                        browser0
            applied.resumeBrowserAppliedQuery
                `shouldBe` Just "database migration"
            map (.resumeId) (visibleResumeBrowser applied)
                `shouldBe` ["one"]

        it "moves, expands, and removes the selected session" do
            let moved = moveResumeBrowser 1 browser0
                expanded = toggleResumeExpanded moved
                removed = removeResumeEntry "two" expanded
            fmap (.resumeId) (selectedResumeBrowser moved)
                `shouldBe` Just "two"
            expanded.resumeBrowserExpanded `shouldBe` Just "two"
            map (.resumeId) (visibleResumeBrowser removed)
                `shouldBe` ["one"]
            removed.resumeBrowserExpanded `shouldBe` Nothing

        it "cycles through provider sources and back to all" do
            resumeSourceLabel browser0.resumeBrowserSource `shouldBe` "All"
            resumeSourceLabel (cycleResumeSource browser0).resumeBrowserSource
                `shouldBe` "xAI"
            resumeSourceLabel
                (cycleResumeSource (cycleResumeSource browser0)).resumeBrowserSource
                `shouldBe` "All"

    describe "resumeSearchEntries" do
        it "deduplicates ranked turn matches and attaches the best excerpt" do
            let at = posixSecondsToUTCTime 120
                results =
                    [ ConversationSearchResult
                        "two" 3 at
                        "migrate state to postgres"
                        (Just "the database migration is complete")
                        0.9
                    , ConversationSearchResult
                        "two" 1 at
                        "older postgres mention"
                        Nothing
                        0.4
                    , ConversationSearchResult
                        "one" 2 at
                        "search previous sessions"
                        Nothing
                        0.3
                    ]
                entries =
                    resumeSearchEntries
                        [ sampleMeta "one" "first"
                        , sampleMeta "two" "second"
                        ]
                        results
            map (.resumeId) entries `shouldBe` ["two", "one"]
            fmap (.resumeMatch) entries
                `shouldBe`
                    [ Just
                        "user: migrate state to postgres  ·  assistant: the database migration is complete"
                    , Just "user: search previous sessions"
                    ]

    describe "groupResumeEntries" do
        it "groups matching cwd basenames while preserving first-seen order" do
            let other =
                    (sampleMeta "two" "second")
                        { metaCwd = fromFilePath "/tmp/other"
                        }
                again =
                    (sampleMeta "three" "third")
                        { metaCwd = fromFilePath "/tmp/repo"
                        }
                groups =
                    groupResumeEntries $
                        resumeEntriesFrom
                            [ (sampleMeta "one" "first", [])
                            , (other, [])
                            , (again, [])
                            ]
            map fst groups `shouldBe` ["tmp-repo", "tmp-other"]
            map (map (.resumeId) . snd) groups
                `shouldBe` [["one", "three"], ["two"]]

    describe "resumeRelativeAge" do
        it "formats recent session ages" do
            let now = posixSecondsToUTCTime (3 * 24 * 60 * 60)
            resumeRelativeAge now (addUTCTime (-17 * 60 * 60) now)
                `shouldBe` "17h ago"
            resumeRelativeAge now (addUTCTime (-2 * 24 * 60 * 60) now)
                `shouldBe` "2d ago"

    describe "renderResumeFrame" do
        it "lists titles" do
            let frame =
                    renderResumeFrame False $
                        initialResumeState
                            (resumeEntriesFrom
                                [(sampleMeta "one" "first", [sampleTurn])])
            frame `shouldSatisfy` Text.isInfixOf "first"
            frame `shouldSatisfy` Text.isInfixOf "resume"
            frame `shouldSatisfy` Text.isInfixOf "transcript"
            frame `shouldSatisfy` Text.isInfixOf "user: hello"

        it "keeps the selected title and transcript in separate columns" do
            let frame =
                    renderResumeFrameFor False 10 80 $
                        initialResumeState
                            (resumeEntriesFrom
                                [(sampleMeta "one" "first", [sampleTurn])])
            frame `shouldSatisfy` Text.isInfixOf "sessions"
            frame `shouldSatisfy` Text.isInfixOf " │ "
            frame `shouldSatisfy` Text.isInfixOf "assistant: hi"
            length (Text.lines frame) `shouldBe` 9

sampleTurn :: SessionTurn
sampleTurn =
    SessionTurn
        { turnAt = posixSecondsToUTCTime 0
        , turnUserText = "hello"
        , turnAssistantText = Just "hi"
        , turnError = Nothing
        , turnResponseId = Nothing
        , turnItems = []
        , turnDisplayItems = []
        , turnUsage = Nothing
        , turnEffect = TranscriptAppend
        , turnProviderTelemetry = []
        }

sampleMeta :: Text.Text -> Text.Text -> SessionMeta
sampleMeta sid title =
    SessionMeta
        { metaVersion = 1
        , metaId = sid
        , metaCreatedAt = posixSecondsToUTCTime 0
        , metaUpdatedAt = posixSecondsToUTCTime 0
        , metaProvider = XAIProvider
        , metaConnection = "xai"
        , metaGatewayIdentity = Nothing
        , metaModel = "grok-4.6"
        , metaTransportModel = Just "grok-4.6"
        , metaDialect = GrokBuildDialect
        , metaLegacySubagentTarget = Just LegacySubagentTarget
            { legacyTargetProvider = XAIProvider
            , legacyTargetConnection = "xai"
            , legacyTargetEffectiveModel = "grok-4.6"
            , legacyTargetDialect = GrokBuildDialect
            }
        , metaCwd = fromFilePath "/tmp/repo"
        , metaEffort = "high"
        , metaTitle = title
        , metaTitleIsManual = False
        , metaTitleRefreshIndex = 0
        , metaTitleUserTurns = 0
        , metaLastResponseId = Nothing
        , metaInputTokens = 0
        , metaOutputTokens = 0
        , metaCachedTokens = 0
        , metaLastRecap = Nothing
        , metaLastTurnSummary = Nothing
        , metaLastRecapMainTurns = 0
        , metaPromptSnapshot = Nothing
        , metaHeadless = False
        }
