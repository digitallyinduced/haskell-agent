module Agent.CLI.BtwSpec (spec) where

import Agent.CLI.Btw
import Agent.Error (ApiError(..))
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , TurnInput(..)
    , emptyTurnOutput
    )
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Responses.Types
import Agent.ToolDispatch (functionToolCall)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "sideQuestionPrompt" do
        it "marks inherited history as reference-only and forbids tools" do
            let prompt = sideQuestionPrompt "what does this do?"
            prompt `shouldSatisfy` Text.isInfixOf "inherited reference context"
            prompt `shouldSatisfy` Text.isInfixOf "Do not call tools"
            prompt `shouldSatisfy` Text.isSuffixOf "what does this do?\n"

    describe "trimDanglingToolSuffix" do
        it "keeps complete tool pairs" do
            let items =
                    [ userItem "before"
                    , functionCallItem "call-1"
                    , functionOutputItem "call-1"
                    ]
            trimDanglingToolSuffix items `shouldBe` items

        it "drops reasoning and an unmatched live tool call" do
            let prefix = [userItem "before"]
                items =
                    prefix
                        <> [ reasoningItem
                           , functionCallItem "call-live"
                           ]
            trimDanglingToolSuffix items `shouldBe` prefix

        it "does not truncate an old unmatched call before a later message" do
            let items =
                    [ userItem "before"
                    , functionCallItem "old-call"
                    , userItem "continued later"
                    ]
            trimDanglingToolSuffix items `shouldBe` items

    describe "runBtwWithCancel" do
        it "uses private state and preserves parent params including tools" do
            let originalItems = turnInputsToItems [UserMessage "earlier context"]
                tool = KnownResponseTool ToolWebSearch
                    (TaggedObject "web_search" KeyMap.empty)
                originalParams = case defaultResponseCreateParams of
                    ResponseCreateParams{..} -> ResponseCreateParams
                        { input = Just (ResponseInputText "stale input")
                        , model = Just "test-model"
                        , previousResponseId = Just "stale-response"
                        , tools = Just [tool]
                        , ..
                        }
            mainParams <- newIORef originalParams
            mainTranscript <- newIORef originalItems
            seenPrevious <- newIORef (Just "not-called")
            seenInputs <- newIORef []
            seenPrivateParams <- newIORef Nothing
            seenPrivateTranscript <- newIORef []
            let factory privateParams =
                    Backend \privateTranscript previous inputs _onEvent -> do
                        writeIORef seenPrevious previous
                        writeIORef seenInputs inputs
                        writeIORef seenPrivateParams . Just =<< readIORef privateParams
                        writeIORef seenPrivateTranscript privateTranscript
                        pure $ Right BackendResult
                            { backendOutput =
                                emptyTurnOutput
                                    "side-response" [] (Just "side answer")
                            , backendState = privateTranscript
                            }
            result <- runBtwWithCancel (\_ action -> action)
                factory mainParams mainTranscript "why?"
            result `shouldBe` Right "side answer"
            readIORef seenPrevious `shouldReturn` Nothing
            seen <- readIORef seenInputs
            seen `shouldSatisfy` \case
                [UserMessage text] ->
                    "Side question boundary." `Text.isPrefixOf` text
                        && "why?" `Text.isSuffixOf` Text.strip text
                _ -> False
            readIORef seenPrivateTranscript `shouldReturn` originalItems
            readIORef mainTranscript `shouldReturn` originalItems
            readIORef mainParams `shouldReturn` originalParams
            captured <- readIORef seenPrivateParams
            fmap (.tools) captured `shouldBe` Just (Just [tool])
            fmap (.input) captured `shouldBe` Just Nothing
            fmap (.previousResponseId) captured `shouldBe` Just Nothing
            fmap (.toolChoice) captured
                `shouldBe` Just (Just (ToolChoiceMode ToolChoiceNone))

        it "rejects tool calls without a follow-up submission" do
            params <- newIORef defaultResponseCreateParams
            transcript <- newIORef []
            submissions <- newIORef (0 :: Int)
            let factory _ = Backend \state _ _ _ -> do
                    modifyIORef' submissions (+ 1)
                    pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "side-response"
                            [functionToolCall "call-1" "shell_command" "{}"] Nothing
                        , backendState = state
                        }
            result <- runBtwWithCancel (\_ action -> action)
                factory params transcript "do something"
            result `shouldBe` Left BtwUnexpectedToolCall
            readIORef submissions `shouldReturn` 1

        it "surfaces transport and empty-response failures" do
            params <- newIORef defaultResponseCreateParams
            transcript <- newIORef []
            let transport _ = Backend \_ _ _ _ ->
                    pure (Left (ConnectionError "offline"))
                empty _ = Backend \state _ _ _ ->
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "side-response" [] (Just "  ")
                        , backendState = state
                        }
            runBtwWithCancel (\_ action -> action)
                transport params transcript "why?"
                `shouldReturn` Left (BtwTransport (ConnectionError "offline"))
            runBtwWithCancel (\_ action -> action)
                empty params transcript "why?"
                `shouldReturn` Left BtwEmptyResponse

    describe "formatBtwError" do
        it "uses the shared provider error rendering" do
            let rendered =
                    formatBtwError
                        (BtwTransport (ConnectionError "socket closed"))
            rendered `shouldSatisfy`
                Text.isInfixOf "operation could not be completed"
            rendered `shouldSatisfy`
                Text.isInfixOf "socket closed"
            rendered `shouldNotSatisfy`
                Text.isInfixOf "ConnectionError"

userItem :: Text.Text -> ResponseItem
userItem text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts [InputTextPart text Nothing KeyMap.empty]
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , extraFields = KeyMap.empty
    }

functionCallItem :: Text.Text -> ResponseItem
functionCallItem callId = FunctionCallItem FunctionCall
    { itemId = Nothing
    , callId
    , name = "shell_command"
    , arguments = "{}"
    , status = Nothing
    , extraFields = KeyMap.empty
    }

functionOutputItem :: Text.Text -> ResponseItem
functionOutputItem callId = FunctionCallOutputItem FunctionCallOutput
    { itemId = Nothing
    , callId
    , output = Aeson.String "ok"
    , status = Nothing
    , extraFields = KeyMap.empty
    }

reasoningItem :: ResponseItem
reasoningItem = ReasoningItemValue ReasoningItem
    { itemId = Nothing
    , summary = []
    , content = Nothing
    , encryptedContent = Nothing
    , status = Nothing
    , extraFields = KeyMap.empty
    }
