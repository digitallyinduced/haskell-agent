module Agent.XAI.LoopBackendSpec (spec) where

import Agent.Error (ApiError(..))
import Agent.Loop
import Agent.OpenAI.Responses.Types
import Agent.ToolDispatch
import Agent.XAI.LoopBackend
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = do
    describe "xaiBackendWith" do
        it "replays the local transcript on the tool follow-up" do
            seen <- newIORef []
            events <- newIORef []
            remaining <- newIORef
                [ testResponse "resp-1"
                    [ functionCallItem "c1" "read_file" "{\"target_file\":\"README.md\"}" ]
                , testResponse "resp-2" [assistantItem "done"]
                ]
            backend <- xaiBackendWith (scriptedSend seen remaining) baseParams

            first <- backend.submitTurn Nothing [UserMessage "read it"]
                (modifyIORef' events . (:))
            first `shouldBe` Right TurnOutput
                { responseId = "resp-1"
                , toolCalls =
                    [ functionToolCall "c1" "read_file" "{\"target_file\":\"README.md\"}" ]
                , assistantText = Nothing
                }

            second <- backend.submitTurn (Just "resp-1")
                [CompletedTool (functionResult "c1" "file contents")]
                (const (pure ()))
            second `shouldBe` Right TurnOutput
                { responseId = "resp-2"
                , toolCalls = []
                , assistantText = Just "done"
                }

            requests <- readIORef seen
            map inputItems requests `shouldBe`
                [ [ userItem "read it" ]
                , [ userItem "read it"
                  , functionCallItem "c1" "read_file" "{\"target_file\":\"README.md\"}"
                  , functionOutputItem "c1" "file contents"
                  ]
                ]
            reverse <$> readIORef events `shouldReturn` [TextDelta "call"]

        it "leaves the transcript unchanged when the transport fails" do
            seen <- newIORef []
            remaining <- newIORef
                [ testResponse "resp-1" [assistantItem "hi"] ]
            backend <- xaiBackendWith
                (\request onEvent -> do
                    n <- length <$> readIORef seen
                    if n == 0
                        then do
                            modifyIORef' seen (++ [request])
                            pure (Left (ConnectionError "boom"))
                        else scriptedSend seen remaining request onEvent)
                baseParams

            failed <- backend.submitTurn Nothing [UserMessage "hi"] (const (pure ()))
            failed `shouldBe` Left (ConnectionError "boom")

            recovered <- backend.submitTurn Nothing [UserMessage "hi"] (const (pure ()))
            recovered `shouldBe` Right TurnOutput
                { responseId = "resp-1"
                , toolCalls = []
                , assistantText = Just "hi"
                }
            map inputItems <$> readIORef seen `shouldReturn`
                [ [userItem "hi"]
                , [userItem "hi"]
                ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

baseParams :: ResponseCreateParams
baseParams = defaultResponseCreateParams { model = Just "grok-4.5" }

scriptedSend
    :: IORef [ResponseCreateParams]
    -> IORef [Response]
    -> ResponseCreateParams
    -> (ResponseStreamEvent -> IO ())
    -> IO (Either ApiError Response)
scriptedSend seen remaining request onEvent = do
    modifyIORef' seen (++ [request])
    next <- atomicModifyIORef' remaining \case
        [] -> ([], Nothing)
        response : rest -> (rest, Just response)
    case next of
        Nothing -> pure (Left (ConnectionError "scripted backend exhausted"))
        Just response -> do
            onEvent (deltaEvent EventOutputTextDelta "call")
            pure (Right response)

functionResult :: Text -> Text -> ToolCallResult
functionResult callId output = ToolCallResult
    { callId
    , output
    , callKind = FunctionCallKind
    }

functionCallItem :: Text -> Text -> Text -> ResponseItem
functionCallItem callId name arguments = FunctionCallItem FunctionCall
    { itemId = Nothing
    , callId
    , name
    , arguments
    , status = Just ItemCompleted
    , extraFields = KeyMap.empty
    }

functionOutputItem :: Text -> Text -> ResponseItem
functionOutputItem callId output = FunctionCallOutputItem FunctionCallOutput
    { itemId = Nothing
    , callId
    , output = Aeson.String output
    , status = Nothing
    , extraFields = KeyMap.empty
    }

userItem :: Text -> ResponseItem
userItem text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts [InputTextPart text Nothing KeyMap.empty]
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , extraFields = KeyMap.empty
    }

assistantItem :: Text -> ResponseItem
assistantItem text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts [OutputTextPart text Nothing Nothing KeyMap.empty]
    , role = RoleAssistant
    , status = Just ItemCompleted
    , phase = Nothing
    , extraFields = KeyMap.empty
    }

deltaEvent :: StreamEventType -> Text -> ResponseStreamEvent
deltaEvent otherEventType delta = OtherResponseStreamEvent
    { otherEventType
    , sequenceNumber = Nothing
    , eventExtraFields = KeyMap.fromList [(Key.fromText "delta", Aeson.String delta)]
    }

testResponse :: Text -> [ResponseItem] -> Response
testResponse responseId output = case Aeson.fromJSON $ Aeson.object
    [ "id" Aeson..= responseId
    , "created_at" Aeson..= (0 :: Int)
    , "model" Aeson..= ("grok-4.5" :: Text)
    , "status" Aeson..= ("completed" :: Text)
    , "output" Aeson..= output
    ] of
    Aeson.Success response -> response
    Aeson.Error err -> error err

inputItems :: ResponseCreateParams -> [ResponseItem]
inputItems request = case request.input of
    Just (ResponseInputItems items) -> items
    _ -> []
