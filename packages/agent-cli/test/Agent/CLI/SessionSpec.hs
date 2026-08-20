module Agent.CLI.SessionSpec (spec) where

import Agent.CLI.Session
import Agent.OpenAI.Responses.Types
import Agent.Provider (Provider(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , getTemporaryDirectory
    , removeDirectoryRecursive
    )
import System.FilePath ((</>))
import System.Posix.Temp (mkdtemp)
import Control.Exception (bracket)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.Session" do
    describe "sessionsRoot" do
        it "is ~/.haskell-agent/sessions" do
            sessionsRoot "/home/marc"
                `shouldBe` "/home/marc" </> ".haskell-agent" </> "sessions"

    describe "sessionTitleFromPrompt" do
        it "collapses whitespace and truncates long prompts" do
            sessionTitleFromPrompt "  hello   world  " `shouldBe` "hello world"
            let long = Text.replicate 100 "a"
            Text.length (sessionTitleFromPrompt long) `shouldBe` 72

    describe "createSession/appendTurn/loadSession" do
        it "round-trips meta and transcript items" $
            withTempDir "agent-sessions-" \root -> do
                handle <- createSession root XAIProvider "grok-4" "/tmp/work" "low" Nothing
                doesDirectoryExist handle.sessionDir `shouldReturn` True
                doesFileExist handle.sessionMetaPath `shouldReturn` True
                handle.sessionMeta.metaTitle `shouldBe` "untitled"

                let item = MessageItem ResponseMessage
                        { messageId = Nothing
                        , content = MessageContentParts
                            [InputTextPart "hi" Nothing KeyMap.empty]
                        , role = RoleUser
                        , status = Nothing
                        , phase = Nothing
                        , extraFields = KeyMap.empty
                        }
                    turn = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "hi there"
                        , turnAssistantText = Just "hello"
                        , turnResponseId = Just "resp-1"
                        , turnItems = [item]
                        }
                handle' <- appendTurn handle turn
                handle'.sessionMeta.metaTitle `shouldBe` "hi there"
                handle'.sessionMeta.metaLastResponseId `shouldBe` Just "resp-1"

                loaded <- loadSession root handle.sessionMeta.metaId
                case loaded of
                    Left err -> expectationFailure err
                    Right (meta, turns) -> do
                        meta.metaId `shouldBe` handle.sessionMeta.metaId
                        meta.metaProvider `shouldBe` XAIProvider
                        meta.metaModel `shouldBe` "grok-4"
                        meta.metaCwd `shouldBe` "/tmp/work"
                        case turns of
                            [loadedTurn] -> do
                                loadedTurn.turnUserText `shouldBe` "hi there"
                                loadedTurn.turnItems `shouldBe` [item]
                            other ->
                                expectationFailure
                                    ("expected one turn, got " <> show (length other))

                listed <- listSessions root
                map (.metaId) listed `shouldBe` [handle.sessionMeta.metaId]

    describe "json codec" do
        it "encodes and decodes SessionTurn" do
            let turn = SessionTurn
                    { turnAt = fixedTime
                    , turnUserText = "q"
                    , turnAssistantText = Nothing
                    , turnResponseId = Nothing
                    , turnItems = []
                    }
            Aeson.eitherDecode (Aeson.encode turn) `shouldBe` Right turn

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2026 8 19) (secondsToDiffTime 0)

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
    tmp <- getTemporaryDirectory
    bracket (mkdtemp (tmp </> prefix)) removeDirectoryRecursive action

