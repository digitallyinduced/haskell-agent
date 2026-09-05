module Agent.CLI.BtwSpec (spec) where

import Agent.CLI.Btw
import Agent.Error (ApiError(..))
import Agent.Json (rawJsonFromEncoding)
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , BackendSnapshot(..)
    , TurnInput(..)
    , emptyBackendSnapshot
    , emptyTurnOutput
    )
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Responses.Types
import Agent.ToolDispatch (functionToolCall)
import qualified Data.Aeson as Aeson
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

        it "removes an old unmatched call before a later message" do
            let items =
                    [ userItem "before"
                    , functionCallItem "old-call"
                    , userItem "continued later"
                    ]
            trimDanglingToolSuffix items
                `shouldBe` [userItem "before", userItem "continued later"]

        it "removes an orphan output without a preceding call" do
            let items =
                    [ functionOutputItem "orphan"
                    , userItem "continued later"
                    ]
            trimDanglingToolSuffix items `shouldBe` [userItem "continued later"]

        it "keeps complete native computer-call pairs" do
            let items =
                    [ userItem "before"
                    , computerCallItem "computer-1"
                    , computerOutputItem "computer-1"
                    ]
            trimDanglingToolSuffix items `shouldBe` items

        it "drops reasoning and an unmatched native computer call" do
            let prefix = [userItem "before"]
                items = prefix <> [reasoningItem, computerCallItem "computer-live"]
            trimDanglingToolSuffix items `shouldBe` prefix

        it "removes an orphan native computer output" do
            let items =
                    [ computerOutputItem "computer-orphan"
                    , userItem "continued later"
                    ]
            trimDanglingToolSuffix items `shouldBe` [userItem "continued later"]

        it "does not pair an output that precedes its call" do
            let items =
                    [ functionOutputItem "torn"
                    , functionCallItem "torn"
                    , userItem "continued later"
                    ]
            trimDanglingToolSuffix items `shouldBe` [userItem "continued later"]

    describe "runBtwWithCancel" do
        it "uses private state and preserves parent params including tools" do
            let originalItems = turnInputsToItems [UserMessage "earlier context"]
                tool = knownResponseTool ToolWebSearch
                originalParams = case defaultResponseCreateParams of
                    ResponseCreateParams{..} -> ResponseCreateParams
                        { input = Just (ResponseInputText "stale input")
                        , model = Just "test-model"
                        , previousResponseId = Just "stale-response"
                        , tools = Just [tool]
                        , ..
                        }
            seenPrevious <- newIORef (Just "not-called")
            seenInputs <- newIORef []
            seenPrivateParams <- newIORef Nothing
            seenPrivateTranscript <- newIORef emptyBackendSnapshot
            let factory privateParams =
                    Backend \privateTranscript previous inputs _onEvent -> do
                        writeIORef seenPrevious previous
                        writeIORef seenInputs inputs
                        writeIORef seenPrivateParams (Just privateParams)
                        writeIORef seenPrivateTranscript privateTranscript
                        pure $ Right BackendResult
                            { backendOutput =
                                emptyTurnOutput
                                    "side-response" [] (Just "side answer")
                            , backendState = privateTranscript
                            }
            result <- runBtwWithCancel (\_ action -> action)
                factory (sideCallSnapshot originalParams originalItems) "why?"
            result `shouldBe` Right "side answer"
            readIORef seenPrevious `shouldReturn` Nothing
            seen <- readIORef seenInputs
            seen `shouldSatisfy` \case
                [UserMessage text] ->
                    "Side question boundary." `Text.isPrefixOf` text
                        && "why?" `Text.isSuffixOf` Text.strip text
                _ -> False
            (.backendItems) <$> readIORef seenPrivateTranscript
                `shouldReturn` originalItems
            captured <- readIORef seenPrivateParams
            fmap (.tools) captured `shouldBe` Just (Just [tool])
            fmap (.input) captured `shouldBe` Just Nothing
            fmap (.previousResponseId) captured `shouldBe` Just Nothing
            fmap (.toolChoice) captured
                `shouldBe` Just (Just (ToolChoiceMode ToolChoiceNone))

        it "rejects tool calls without a follow-up submission" do
            submissions <- newIORef (0 :: Int)
            let factory _ = Backend \state _ _ _ -> do
                    modifyIORef' submissions (+ 1)
                    pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "side-response"
                            [functionToolCall "call-1" "shell_command" "{}"] Nothing
                        , backendState = state
                        }
            result <- runBtwWithCancel (\_ action -> action)
                factory
                (sideCallSnapshot defaultResponseCreateParams [])
                "do something"
            result `shouldBe` Left BtwUnexpectedToolCall
            readIORef submissions `shouldReturn` 1

        it "surfaces transport and empty-response failures" do
            let transport _ = Backend \_ _ _ _ ->
                    pure (Left (ConnectionError "offline"))
                empty _ = Backend \state _ _ _ ->
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "side-response" [] (Just "  ")
                        , backendState = state
                        }
            runBtwWithCancel (\_ action -> action)
                transport (sideCallSnapshot defaultResponseCreateParams [])
                "why?"
                `shouldReturn` Left (BtwTransport (ConnectionError "offline"))
            runBtwWithCancel (\_ action -> action)
                empty (sideCallSnapshot defaultResponseCreateParams [])
                "why?"
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
    , content = MessageContentParts [InputTextPart text Nothing]
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
    }

functionCallItem :: Text.Text -> ResponseItem
functionCallItem callId = FunctionCallItem FunctionCall
    { itemId = Nothing
    , callId
    , name = "shell_command"
    , namespace = Nothing
    , provider = Nothing
    , arguments = "{}"
    , encryptedFunctionArgs = Nothing
    , status = Nothing
    , async = Nothing
    }

computerCallItem :: Text.Text -> ResponseItem
computerCallItem computerCallId = ComputerCallItem ComputerCall
    { computerCallItemId = Nothing
    , computerCallId
    , computerActions = []
    , pendingSafetyChecks = []
    , computerCallStatus = Nothing
    , computerCallExtra = mempty
    }

computerOutputItem :: Text.Text -> ResponseItem
computerOutputItem computerOutputCallId =
    ComputerCallOutputItem ComputerCallOutput
        { computerOutputItemId = Nothing
        , computerOutputCallId
        , screenshotDataUrl = ""
        , acknowledgedChecks = []
        , computerOutputStatus = Nothing
        , computerOutputExtra = mempty
        }

functionOutputItem :: Text.Text -> ResponseItem
functionOutputItem callId = FunctionCallOutputItem FunctionCallOutput
    { localOutcome = Nothing
    , itemId = Nothing
    , callId
    , name = Nothing
    , namespace = Nothing
    , provider = Nothing
    , output = rawJsonFromEncoding (Aeson.toEncoding ("ok" :: Text.Text))
    , status = Nothing
    , async = Nothing
    }

reasoningItem :: ResponseItem
reasoningItem = ReasoningItemValue ReasoningItem
    { itemId = Nothing
    , summary = []
    , content = Nothing
    , encryptedContent = Nothing
    , status = Nothing
    }
