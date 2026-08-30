module Agent.Gemini.StreamSpec (spec) where

import Agent.Gemini.Stream
import Test.Hspec

spec :: Spec
spec = describe "Gemini SSE decoder" do
    it "handles CRLF and arbitrary chunk boundaries" do
        let payload = "data: {\"responseId\":\"r\",\"candidates\":[]}\r\n\r\n"
            chunks = ["data: {\"response", "Id\":\"r\",\"candidates\":[]}\r\n", "\r\n"]
            result = do
                (decoder, first) <- feedSseDecoder newSseDecoder (chunks !! 0)
                (decoder2, second) <- feedSseDecoder decoder (chunks !! 1)
                trailing <- finishSseDecoder decoder2
                pure (first <> second <> trailing)
        result `shouldBe` parseSseResponses payload
        result `shouldSatisfy` either (const False) ((== 1) . length)

    it "rejects malformed JSON" do
        parseSseResponses "data: {not-json}\n\n" `shouldSatisfy` isLeft

    it "does not silently accept an error envelope as an empty response" do
        parseSseResponses
            "data: {\"error\":{\"message\":\"stream failed\"}}\n\n"
            `shouldSatisfy` isLeft
  where
    isLeft (Left _) = True
    isLeft _ = False
