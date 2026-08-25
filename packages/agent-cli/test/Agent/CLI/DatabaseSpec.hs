module Agent.CLI.DatabaseSpec (spec) where

import Agent.CLI.Database
import Agent.CLI.Database.Storage
import Agent.CLI.Database.Store (deriveDatabaseScopes)
import Agent.CLI.Options (StorageCommand(..))
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolDispatchConfig(..)
    , functionToolCall
    )
import Agent.Tools.Types
    ( appToolHandlers
    )
import Agent.ToolDispatch (dispatchToolCall)
import Control.Exception.Safe (displayException)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "databaseTools" do
        it "dispatches a read-only query to the selected scope" do
            seen <- newIORef Nothing
            let env = testEnv
                    { databaseRunQuery = \scope sql -> do
                        writeIORef seen (Just (scope, sql))
                        pure (Right (object ["rows" .= [object ["title" .= ("test" :: Text)]]]))
                    }
            result <- dispatchToolCall dispatchConfig
                (appToolHandlers (databaseTools env))
                (functionToolCall "call-1" "database_query"
                    "{\"scope\":\"user\",\"sql\":\"select * from todos\"}")
            readIORef seen `shouldReturn`
                Just (DatabaseUserScope, "select * from todos")
            result.output `shouldContainText` "\"rows\""

        it "rejects empty mutating SQL before calling storage" do
            called <- newIORef False
            let env = testEnv
                    { databaseRunExecute = \_ _ _ -> do
                        writeIORef called True
                        pure (Right (object []))
                    }
            result <- dispatchToolCall dispatchConfig
                (appToolHandlers (databaseTools env))
                (functionToolCall "call-2" "database_execute"
                    "{\"scope\":\"repository\",\"sql\":\"  \",\"purpose\":\"todos\"}")
            readIORef called `shouldReturn` False
            result.output `shouldContainText` "must not be empty"

        it "rejects unknown scopes during argument decoding" do
            result <- dispatchToolCall dispatchConfig
                (appToolHandlers (databaseTools testEnv))
                (functionToolCall "call-3" "database_schema"
                    "{\"scope\":\"organization\"}")
            result.output `shouldContainText` "expected user, repository, or checkout"

        it "dispatches full-text search over past conversations" do
            seen <- newIORef Nothing
            let env = testEnv
                    { databaseSearchConversations = \query limit -> do
                        writeIORef seen (Just (query, limit))
                        pure (Right (Aeson.toJSON
                            [ object
                                [ "session_id" .= ("session-1" :: Text)
                                , "turn_index" .= (4 :: Int)
                                , "occurred_at" .= ("2026-08-24T09:30:00Z" :: Text)
                                , "user_text" .= ("How should we store this?" :: Text)
                                , "assistant_text" .=
                                    Just ("Use PostgreSQL.\nKeep scopes explicit." :: Text)
                                , "rank" .= (0.8 :: Double)
                                ]
                            ]))
                    }
            result <- dispatchToolCall dispatchConfig
                (appToolHandlers (databaseTools env))
                (functionToolCall "call-4" "conversation_search"
                    "{\"query\":\"postgres memory\",\"limit\":5}")
            readIORef seen `shouldReturn` Just ("postgres memory", 5)
            result.output `shouldBe`
                "Match 1\n\
                \Session: session-1\n\
                \Turn: 4\n\
                \Occurred at: 2026-08-24T09:30:00Z\n\
                \User:\n\
                \  How should we store this?\n\
                \Assistant:\n\
                \  Use PostgreSQL.\n\
                \  Keep scopes explicit."

        it "renders an empty conversation search result as text" do
            let env = testEnv
                    { databaseSearchConversations = \_ _ ->
                        pure (Right (Aeson.toJSON ([] :: [Aeson.Value])))
                    }
            result <- dispatchToolCall dispatchConfig
                (appToolHandlers (databaseTools env))
                (functionToolCall "call-5" "conversation_search"
                    "{\"query\":\"nothing\"}")
            result.output `shouldBe` "(no matching conversations)"

    describe "runStorageCommand" do
        it "dispatches each administrative command to its store action" do
            let env = StorageCommandEnv
                    { storageStatusAction = pure (Right "status")
                    , storageStartAction = pure (Right "start")
                    , storageStopAction = pure (Right "stop")
                    , storageMigrateAction = pure (Right "migrate")
                    , storageDoctorAction = pure (Right "doctor")
                    }
            runStorageCommand env StorageStatus `shouldReturn` Right "status"
            runStorageCommand env StorageStart `shouldReturn` Right "start"
            runStorageCommand env StorageStop `shouldReturn` Right "stop"
            runStorageCommand env StorageMigrate `shouldReturn` Right "migrate"
            runStorageCommand env StorageDoctor `shouldReturn` Right "doctor"

    describe "deriveDatabaseScopes" do
        it "is stable for one state directory and checkout" do
            first <- deriveDatabaseScopes
                "/tmp/haskell-agent-state"
                "/tmp/haskell-agent-checkout"
            second <- deriveDatabaseScopes
                "/tmp/haskell-agent-state"
                "/tmp/haskell-agent-checkout"
            first `shouldBe` second

        it "isolates different checkouts" do
            first <- deriveDatabaseScopes
                "/tmp/haskell-agent-state"
                "/tmp/haskell-agent-checkout-a"
            second <- deriveDatabaseScopes
                "/tmp/haskell-agent-state"
                "/tmp/haskell-agent-checkout-b"
            first `shouldNotBe` second

testEnv :: DatabaseToolsEnv
testEnv = DatabaseToolsEnv
    { databaseDescribeScope = \_ -> pure (Right (object []))
    , databaseRunQuery = \_ _ -> pure (Right (object []))
    , databaseRunExecute = \_ _ _ -> pure (Right (object []))
    , databaseSearchConversations = \_ _ ->
        pure (Right (Aeson.toJSON ([] :: [Aeson.Value])))
    }

dispatchConfig :: ToolDispatchConfig
dispatchConfig = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "unknown: " <> name
    , toolDispatchFormatResult = either ("error: " <>) id
    , toolDispatchFormatException = \name exception ->
        name <> ": " <> Text.pack (displayException exception)
    , toolDispatchOnException = \_ _ -> pure ()
    , toolDispatchOnOutput = \_ _ -> pure ()
    }

shouldContainText :: Text -> Text -> Expectation
shouldContainText actual expected =
    actual `shouldSatisfy` Text.isInfixOf expected
