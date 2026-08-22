module Agent.CLI.ResumeSpec (spec) where

import Agent.CLI.Picker (PickerKey(..))
import Agent.CLI.Resume
import Agent.CLI.Session (SessionMeta(..), SessionTurn(..))
import System.OsPath (unsafeEncodeUtf)
import Agent.Provider (Provider(..))
import Data.Time.Clock (addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Text as Text
import Test.Hspec

fromFilePath = unsafeEncodeUtf

spec :: Spec
spec = do
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
        , turnUsage = Nothing
        }

sampleMeta :: Text.Text -> Text.Text -> SessionMeta
sampleMeta sid title =
    SessionMeta
        { metaVersion = 1
        , metaId = sid
        , metaCreatedAt = posixSecondsToUTCTime 0
        , metaUpdatedAt = posixSecondsToUTCTime 0
        , metaProvider = XAIProvider
        , metaModel = "grok-4.6"
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
        }
