module Agent.Transport.SSESpec (spec) where

import Agent.Error (ApiError(..))
import Agent.Transport.SSE
import Control.Monad (foldM, forM_)
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (forAll, listOf, elements, chooseInt, (===))

spec :: Spec
spec = describe "shared SSE framing" do
    modifyMaxSuccess (const 500) $
        prop "matches independent whole-body framing for arbitrary bytes and chunks" $
            forAll (listOf (elements [0, 10, 13, 32, 58, 97, 195, 255])) \bytes ->
                forAll (listOf (chooseInt (0, 16))) \sizes ->
                    let body = BS.pack bytes
                    in frameChunks (splitChunks sizes body)
                        === Right (referenceBlocks body)

    it "handles every two-chunk split, bytewise input, and empty chunks" do
        let body = "\n:first\r\nx\r\n\r\n\r\nlast\r"
            expected = Right [":first\nx", "last"]
        forM_ [0 .. BS.length body] \offset ->
            frameChunks [BS.take offset body, "", BS.drop offset body]
                `shouldBe` expected
        frameChunks (map BS.singleton (BS.unpack body)) `shouldBe` expected

    it "emits only complete blocks during feed and flushes the final block" do
        (framer, events) <- expectRight $ feed "a\n\nb\nc"
        events `shouldBe` ["a"]
        finishSseFramer keep framer `shouldBe` Right ["b\nc"]

    it "flushes empty, line-terminated, unterminated, and CR-terminated tails" do
        forM_ ["", "\n", "\r", "\r\n\r\n"] \body ->
            frameChunks [body] `shouldBe` Right []
        forM_ ["a", "a\n", "a\r", "a\r\n", "a\n\r"] \body ->
            frameChunks [body] `shouldBe` Right ["a"]

    it "strips one trailing CR but does not treat bare CRs as delimiters" do
        frameChunks ["a\rb\r\r\n\n"] `shouldBe` Right ["a\rb\r"]

    it "skips empty blocks without invoking the event decoder" do
        let result = do
                (framer, events) <- feedSseFramer "Test"
                    (const (Left callbackError)) newSseFramer "\n\r\n"
                trailing <- finishSseFramer (const (Left callbackError)) framer
                pure (events <> trailing :: [BS.ByteString])
        result `shouldBe` Right []

    it "retains callback skipping and reports callback errors before later limit errors" do
        let decode block = if block == "skip" then Right Nothing else keep block
        fmap snd (feedSseFramer "Test" decode newSseFramer "skip\n\nok\n\n")
            `shouldBe` Right ["ok"]
        fmap snd (feedSseFramer "Test" (const (Left callbackError))
            newSseFramer ("bad\n\n" <> BS.replicate (maxSseEventBytes + 1) 97))
            `shouldBe` (Left callbackError :: Either ApiError [BS.ByteString])

    it "accepts the exact 64 MiB wire limit and resets it after each block" do
        maxSseEventBytes `shouldBe` 64 * 1024 * 1024
        let body = BS.replicate (maxSseEventBytes - 4) 97 <> "\r\n\r\n"
        (framer, lengths) <- expectRight $
            feedSseFramer "Test" keepLength newSseFramer body
        lengths `shouldBe` [maxSseEventBytes - 4]
        fmap snd (feedSseFramer "Test" keepLength framer body)
            `shouldBe` Right lengths

    it "counts newline delimiters against the limit and bounds error previews" do
        (framer, _) <- expectRight $
            feed (BS.replicate maxSseEventBytes 97)
        fmap snd (feedSseFramer "Test" keep framer "\n")
            `shouldBe` Left (limitError (Text.replicate 2000 "a"))
        fmap snd (feedSseFramer "Test" keep framer "b")
            `shouldBe` Left (limitError (Text.replicate 2000 "a"))
        -- EOF adds no synthetic newline bytes.
        finishSseFramer keepLength framer `shouldBe` Right [maxSseEventBytes]

    it "counts completed lines and CRLF bytes, not just the current line" do
        (framer, _) <- expectRight $
            feed (":comment\r\n" <> BS.replicate (maxSseEventBytes - 10) 97)
        fmap snd (feedSseFramer "Test" keep framer "\r")
            `shouldBe` Left (limitError (":comment" <> Text.replicate 1992 "a"))

    it "rejects an oversized first chunk with the existing empty preview" do
        fmap snd (feed (BS.replicate (maxSseEventBytes + 1) 97))
            `shouldBe` Left (limitError "")

keep :: BS.ByteString -> Either ApiError (Maybe BS.ByteString)
keep = Right . Just

keepLength :: BS.ByteString -> Either ApiError (Maybe Int)
keepLength = Right . Just . BS.length

feed :: BS.ByteString -> Either ApiError (SseFramer, [BS.ByteString])
feed = feedSseFramer "Test" keep newSseFramer

frameChunks :: [BS.ByteString] -> Either ApiError [BS.ByteString]
frameChunks chunks = do
    (framer, events) <- foldM step (newSseFramer, []) chunks
    trailing <- finishSseFramer keep framer
    pure (events <> trailing)
  where
    step (framer, events) chunk = do
        (next, completed) <- feedSseFramer "Test" keep framer chunk
        pure (next, events <> completed)

-- Deliberately independent of the incremental framer and its CR helper.
referenceBlocks :: BS.ByteString -> [BS.ByteString]
referenceBlocks = blocks . map stripCR . BS.split 10
  where
    stripCR line
        | not (BS.null line) && BS.last line == 13 = BS.init line
        | otherwise = line
    blocks [] = []
    blocks (line : rest) | BS.null line = blocks rest
    blocks lines =
        let (block, rest) = break BS.null lines
        in BS.intercalate "\n" block : blocks rest

splitChunks :: [Int] -> BS.ByteString -> [BS.ByteString]
splitChunks [] bytes = [bytes]
splitChunks (size : sizes) bytes =
    BS.take size bytes : splitChunks sizes (BS.drop size bytes)

callbackError :: ApiError
callbackError = JsonDecodeError "callback failed" ""

limitError :: Text.Text -> ApiError
limitError = JsonDecodeError "Test SSE event exceeds 67108864 bytes"

expectRight :: Show error => Either error value -> IO value
expectRight = either (fail . show) pure
