module Agent.CLI.ResumeSpec (spec) where

import Agent.CLI.Picker (PickerKey(..))
import Agent.CLI.Resume
import Agent.CLI.Session (SessionMeta(..), SessionTurn(..))
import Agent.Provider (Provider(..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Text as Text
import Test.Hspec

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
        , metaCwd = "/tmp/repo"
        , metaEffort = "high"
        , metaTitle = title
        , metaLastResponseId = Nothing
        , metaInputTokens = 0
        , metaOutputTokens = 0
        , metaCachedTokens = 0
        }
