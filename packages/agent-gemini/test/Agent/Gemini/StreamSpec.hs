module Agent.Gemini.StreamSpec (spec) where

import Agent.Gemini.Stream
import Agent.Error (ApiError(..))
import Agent.Gemini.Types (GenerateContentResponse)
import Control.Monad (foldM, forM_)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Test.Hspec

spec :: Spec
spec = describe "Gemini SSE decoder" do
    it "handles all split points and bytewise CRLF, UTF-8, multiline data, and EOF" do
        let body = Text.encodeUtf8 $
                "\r\n: keepalive\r\n\r\nid: 1\r\nevent: ignored\r\n\r\n"
                <> "event: also-ignored\r\ndata:{\"responseId\":\"hé🙂\",\r\n"
                <> "data: \"candidates\":[]}\r\n\r\n"
                <> "data: \x2003[DONE]\x2003\r\n\r\n"
                <> "data: {\"responseId\":\"last\",\"candidates\":[]}"
            expected = parseSseResponses $
                "data: {\"responseId\":\"hé🙂\",\"candidates\":[]}\n\n"
                <> "data: {\"responseId\":\"last\",\"candidates\":[]}\n\n"
        expected `shouldSatisfy` either (const False) ((== 2) . length)
        forM_ [0 .. BS.length body] \offset ->
            decodeChunks [BS.take offset body, "", BS.drop offset body]
                `shouldBe` expected
        decodeChunks (map BS.singleton (BS.unpack body)) `shouldBe` expected

    it "rejects malformed JSON" do
        parseSseResponses "data: {not-json}\n\n" `shouldSatisfy` isLeft

    it "rejects malformed JSON at every split and on EOF instead of skipping it" do
        forM_ ["data: {not-json}", "data: {not-json}\n\ndata: {\"candidates\":[]}\n\n"] \body ->
            forM_ [0 .. BS.length body] \offset ->
                decodeChunks [BS.take offset body, BS.drop offset body]
                    `shouldSatisfy` \case
                        Left (JsonDecodeError message preview) ->
                            "Invalid Gemini SSE JSON: " `Text.isPrefixOf` message
                                && preview == "{not-json}"
                        _ -> False

    it "waits for the block boundary to reject UTF-8, including non-data lines" do
        (decoder, events) <- expectRight $
            feedSseDecoder newSseDecoder ": \xc3"
        events `shouldBe` []
        let invalid result = case result of
                Left (JsonDecodeError message _) ->
                    "Invalid UTF-8 in Gemini SSE event: " `Text.isPrefixOf` message
                _ -> False
        fmap snd (feedSseDecoder decoder "\x28\n\n") `shouldSatisfy` invalid
        finishSseDecoder decoder `shouldSatisfy` invalid

    it "keeps the Gemini limit error label" do
        fmap snd (feedSseDecoder newSseDecoder (BS.replicate (64 * 1024 * 1024 + 1) 97))
            `shouldBe` Left (JsonDecodeError "Gemini SSE event exceeds 67108864 bytes" "")

    it "does not silently accept an error envelope as an empty response" do
        parseSseResponses
            "data: {\"error\":{\"message\":\"stream failed\"}}\n\n"
            `shouldSatisfy` isLeft

decodeChunks :: [BS.ByteString] -> Either ApiError [GenerateContentResponse]
decodeChunks chunks = do
    (decoder, events) <- foldM step (newSseDecoder, []) chunks
    trailing <- finishSseDecoder decoder
    pure (events <> trailing)
  where
    step (decoder, events) chunk = do
        (next, completed) <- feedSseDecoder decoder chunk
        pure (next, events <> completed)

expectRight :: Show error => Either error value -> IO value
expectRight = either (fail . show) pure
