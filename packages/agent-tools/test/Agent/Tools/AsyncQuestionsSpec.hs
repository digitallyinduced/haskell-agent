module Agent.Tools.AsyncQuestionsSpec (spec) where

import Agent.ToolDispatch
    ( ToolCallResult(..), ToolDispatchConfig(..), dispatchToolCall, functionToolCall )
import Agent.Tools.AsyncQuestions
import Agent.Tools.Types (AppTool(..), ApprovalRule(..), jsonToolParameters)
import qualified Data.Aeson as Aeson
import Data.Either (isLeft)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Test.Hspec

spec :: Spec
spec = describe "Agent.Tools.AsyncQuestions" do
    it "delivers structured questions without a plan environment" do
        delivered <- newIORef []
        let deliver request = modifyIORef' delivered (<> [request]) >> pure (Right ())
        runTool deliver validArguments `shouldReturn` "{\"accepted\":true}"
        readIORef delivered `shouldReturn` [expectedRequest]

    it "uses the shared schema without a generic approval prompt" do
        let tool = requestUserInputAsyncTool (\_ -> pure (Right ()))
        jsonToolParameters tool `shouldBe` Just requestUserInputAsyncParameters
        case tool.appToolApproval of
            AlwaysReadOnly -> pure ()
            _ -> expectationFailure "questions should not request tool approval"

    it "acknowledges delivery, never a fabricated user answer" do
        executeRequestUserInputAsync (\request -> do
            request `shouldBe` expectedRequest
            pure (Right ())) expectedRequest
            `shouldReturn` Right "{\"accepted\":true}"
        executeRequestUserInputAsync (\_ -> pure (Left "disconnected")) expectedRequest
            `shouldReturn` Left "disconnected"
        runTool (\_ -> pure (Left "disconnected")) validArguments
            `shouldReturn` "ERR disconnected"

    it "shares validation with Aeson-based hosts" do
        Aeson.eitherDecodeStrict' (Text.encodeUtf8 validArguments)
            `shouldBe` Right expectedRequest
        mapM_ (\arguments -> do
            (Aeson.eitherDecodeStrict' (Text.encodeUtf8 arguments)
                :: Either String RequestUserInputAsyncArgs) `shouldSatisfy` isLeft
            runTool (\_ -> fail "must not deliver invalid questions") arguments
                >>= (`shouldSatisfy` Text.isPrefixOf "ERR "))
            [ "{}"
            , "{\"questions\":null}"
            , "{\"questions\":[]}"
            , "{\"questions\":[{}]}"
            , "{\"questions\":[{\"title\":\"   \"}]}"
            , "{\"questions\":[{\"title\":\"Color?\",\"options\":[]}]}"
            , "{\"questions\":[{\"title\":\"Color?\",\"options\":[\" \"]}]}"
            , "{\"questions\":[{\"title\":\"Color?\",\"options\":[42]}]}"
            ]

    it "treats absent or null suggestions as a free-text question" do
        let arguments = "{\"questions\":[{\"title\":\"Details?\",\"options\":null}]}"
        Aeson.eitherDecodeStrict' (Text.encodeUtf8 arguments)
            `shouldBe` Right (RequestUserInputAsyncArgs [AsyncQuestion "Details?" []])
        runTool (\request -> do
            request `shouldBe` RequestUserInputAsyncArgs [AsyncQuestion "Details?" []]
            pure (Right ())) arguments `shouldReturn` "{\"accepted\":true}"

validArguments :: Text
validArguments = "{\"questions\":[{\"title\":\"Color?\",\"options\":[\"Blue\",\"Red\"]},{\"title\":\"Details?\"}]}"

expectedRequest :: RequestUserInputAsyncArgs
expectedRequest = RequestUserInputAsyncArgs
    [AsyncQuestion "Color?" ["Blue", "Red"], AsyncQuestion "Details?" []]

runTool :: (RequestUserInputAsyncArgs -> IO (Either Text ())) -> Text -> IO Text
runTool deliver arguments = do
    result <- dispatchToolCall config [(requestUserInputAsyncTool deliver).appToolHandler]
        (functionToolCall "question-1" "request_user_input_async" arguments)
    pure result.output
  where
    config = ToolDispatchConfig
        { toolDispatchUnknownTool = \name -> "unknown:" <> name
        , toolDispatchFormatResult = either ("ERR " <>) id
        , toolDispatchFormatException = \name _ -> "EX " <> name
        , toolDispatchOnException = \_ _ -> pure ()
        , toolDispatchOnOutput = \_ _ -> pure ()
        , toolDispatchFinalizeOutput = \_ output -> pure output
        }
