module Claude.Agent.SDK.Internal.Transport.OutputBufferSpec (spec) where

import Claude.Agent.SDK.Internal.Transport.OutputBuffer
    ( OutputReadResult(..)
    , emptyOutputBuffer
    , readOutputLine
    )
import Control.Exception.Safe (IOException, throwIO)
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.IORef
    ( atomicModifyIORef'
    , modifyIORef'
    , newIORef
    , readIORef
    )
import System.IO.Error (eofErrorType, mkIOError)
import Test.Hspec

spec :: Spec
spec = describe "OutputBuffer" do
    it "returns records that arrive in one read without reading again" do
        reader <- scriptedReader [Deliver "a\nbb\nccc\n"]
        outcomes <- readOutcomes 4 8_192 reader
        outcomes `shouldBe` [Line "a", Line "bb", Line "ccc", End]
        reader.requestedSizes `shouldReturn` [8_192, 8_192]

    it "joins a record that spans several reads" do
        reader <-
            scriptedReader
                [ Deliver "abc"
                , Deliver "def"
                , Deliver "g\nh"
                , Deliver "i\n"
                ]
        outcomes <- readOutcomes 3 8_192 reader
        outcomes `shouldBe` [Line "abcdefg", Line "hi", End]

    it "handles newlines at read boundaries and a trailing record" do
        reader <-
            scriptedReader [Deliver "abc\n", Deliver "\n", Deliver "def"]
        outcomes <- readOutcomes 5 8_192 reader
        outcomes `shouldBe` [Line "abc", Line "", Line "def", End, End]

    it "treats an EOF exception like an empty read" do
        reader <- scriptedReader [Deliver "abc", Fail eofError]
        outcomes <- readOutcomes 2 8_192 reader
        outcomes `shouldBe` [Line "abc", End]

    it "reports other read failures and keeps the buffered record" do
        reader <-
            scriptedReader
                [Deliver "abc", Fail (userError "boom"), Deliver "def\n"]
        outcomes <- readOutcomes 3 8_192 reader
        outcomes
            `shouldBe` [Failure "user error (boom)", Line "abcdef", End]

    it "rejects a record as soon as it is read past the limit" do
        reader <- scriptedReader [Deliver "abcdefgh\n"]
        outcomes <- readOutcomes 1 4 reader
        outcomes `shouldBe` [TooLarge]
        reader.requestedSizes `shouldReturn` [5]

    it "bounds every read to the remaining allowance plus one byte" do
        reader <- scriptedReader [Deliver "abcd", Deliver "efghij"]
        outcomes <- readOutcomes 1 10 reader
        outcomes `shouldBe` [Line "abcdefghij"]
        reader.requestedSizes `shouldReturn` [11, 7, 1]

    it "rejects an oversized record delivered whole and resumes after it" do
        reader <- scriptedReaderIgnoringSize [Deliver "abcdefgh\nok\n"]
        outcomes <- readOutcomes 3 4 reader
        outcomes `shouldBe` [TooLarge, Line "ok", End]

    it "joins a large record spread over many reads in order" do
        let record = ByteString.pack (take 1_000_003 (cycle [65 .. 90]))
        reader <-
            scriptedReader
                (map Deliver (chunksOf 1_000 (record <> "\n")))
        outcomes <- readOutcomes 2 2_000_000 reader
        case outcomes of
            [Line actual, End] ->
                (ByteString.length actual, actual == record)
                    `shouldBe` (ByteString.length record, True)
            other ->
                expectationFailure
                    ("unexpected outcomes: " <> show (map summarize other))

data Delivery
    = Deliver ByteString
    | Fail IOException

data Outcome
    = Line ByteString
    | End
    | TooLarge
    | Failure String
    deriving (Eq, Show)

data Reader = Reader
    { readChunk :: Int -> IO ByteString
    , requestedSizes :: IO [Int]
    }

-- | Serve deliveries in order, honouring the requested read size like a pipe.
scriptedReader :: [Delivery] -> IO Reader
scriptedReader = makeReader True

-- | Serve each delivery whole, even when it exceeds the requested size.
scriptedReaderIgnoringSize :: [Delivery] -> IO Reader
scriptedReaderIgnoringSize = makeReader False

makeReader :: Bool -> [Delivery] -> IO Reader
makeReader honorSize deliveries = do
    queue <- newIORef deliveries
    sizes <- newIORef []
    let readChunk size = do
            modifyIORef' sizes (size :)
            next <-
                atomicModifyIORef' queue \case
                    [] ->
                        ([], Right ByteString.empty)
                    Deliver bytes : rest
                        | honorSize && ByteString.length bytes > size ->
                            let (served, remaining) =
                                    ByteString.splitAt size bytes
                             in (Deliver remaining : rest, Right served)
                        | otherwise ->
                            (rest, Right bytes)
                    Fail exception : rest ->
                        (rest, Left exception)
            either throwIO pure next
    pure
        Reader
            { readChunk
            , requestedSizes = reverse <$> readIORef sizes
            }

readOutcomes :: Int -> Int -> Reader -> IO [Outcome]
readOutcomes count maximumBytes reader = do
    bufferRef <- newIORef emptyOutputBuffer
    replicateM
        count
        (outcome <$> readOutputLine bufferRef maximumBytes reader.readChunk)

outcome :: OutputReadResult -> Outcome
outcome = \case
    OutputReadLine line -> Line line
    OutputReadEnd -> End
    OutputReadTooLarge -> TooLarge
    OutputReadFailure exception -> Failure (show exception)

summarize :: Outcome -> Outcome
summarize = \case
    Line line -> Line (ByteString.take 32 line)
    other -> other

eofError :: IOException
eofError = mkIOError eofErrorType "read" Nothing Nothing

chunksOf :: Int -> ByteString -> [ByteString]
chunksOf size bytes
    | ByteString.null bytes = []
    | otherwise =
        let (chunk, rest) = ByteString.splitAt size bytes
         in chunk : chunksOf size rest
