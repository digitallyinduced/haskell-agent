module Agent.Responses.ResponseMergeSpec (spec) where

import Agent.Responses.Codec (decodeResponse)
import Agent.Responses.ResponseMerge
import Agent.Responses.Types
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = describe "typed response merging" do
    it "fills empty terminal output from streamed items" do
        let call = functionCall "call-1"
            merged = mergeCompletedResponseOutput [call] baseResponse
        merged.output `shouldBe` [call]

    it "keeps terminal items and appends missing streamed items" do
        let call1 = functionCall "call-1"
            call2 = functionCall "call-2"
            terminal = withOutput [call1] baseResponse
        (mergeCompletedResponseOutput [call1, call2] terminal).output
            `shouldBe` [call1, call2]

    it "overlays known lifecycle fields" do
        let created = withModel "request-model" baseResponse
            completed = withIdentityAndStatus "" ResponseCompleted baseResponse
        mergeResponseFragments [created, completed]
            `shouldSatisfy` \case
                Just response ->
                    response.model == "request-model"
                        && response.status == ResponseCompleted
                Nothing -> False

    it "overlays lifecycle fragments associatively" do
        let first = withModel "one" baseResponse
            second = withIdentityAndStatus "" ResponseCompleted baseResponse
            third = withOutput [functionCall "call-3"] baseResponse
        mergeResponseFragments [first, second, third]
            `shouldBe`
                (mergeResponseFragments [first, second]
                    >>= \merged -> mergeResponseFragments [merged, third])

    it "keeps unrelated earlier lifecycle fields" do
        let earlier = withModel "request-model" baseResponse
            later = withIdentityAndStatus "resp-final" ResponseCompleted
                baseResponse
        mergeResponseFragments [earlier, later]
            `shouldSatisfy` \case
                Just response ->
                    response.responseId == "resp-final"
                        && response.model == "request-model"
                Nothing -> False

    it "merging an identifiable streamed item is idempotent" do
        let call = functionCall "call-1"
            once = mergeCompletedResponseOutput [call] baseResponse
        mergeCompletedResponseOutput [call] once `shouldBe` once

baseResponse :: Response
baseResponse =
    either error id $ decodeResponse $ BS.pack
        "{\"id\":\"resp-1\",\"created_at\":0,\"model\":\"\",\
        \\"status\":\"in_progress\",\"output\":[]}"

functionCall :: Text -> ResponseItem
functionCall callId = FunctionCallItem FunctionCall
    { itemId = Nothing
    , callId
    , name = "echo"
    , namespace = Nothing
    , provider = Nothing
    , arguments = "{}"
    , encryptedFunctionArgs = Nothing
    , status = Nothing
    }

withOutput :: [ResponseItem] -> Response -> Response
withOutput value response = response { output = value }

withModel :: Text -> Response -> Response
withModel value response = response { model = value }

withIdentityAndStatus :: Text -> ResponseStatus -> Response -> Response
withIdentityAndStatus identifier responseStatus response =
    response { responseId = identifier, status = responseStatus }
