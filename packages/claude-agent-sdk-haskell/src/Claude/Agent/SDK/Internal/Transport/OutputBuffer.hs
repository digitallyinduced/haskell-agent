module Claude.Agent.SDK.Internal.Transport.OutputBuffer
    ( OutputBuffer
    , OutputLine(..)
    , OutputReadResult(..)
    , appendOutputChunk
    , emptyOutputBuffer
    , finishOutputBuffer
    , outputPartialLength
    , readBufferedLine
    , takeOutputLine
    ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import Data.IORef
    ( IORef
    , readIORef
    , writeIORef
    )
import System.IO.Error (isEOFError)

data OutputLine
    = OutputLine !ByteString !OutputBuffer
    | OutputLineTooLarge !OutputBuffer

data OutputReadResult
    = OutputReadLine !ByteString
    | OutputReadEnd
    | OutputReadTooLarge
    | OutputReadFailure !IOException

data OutputBuffer = OutputBuffer
    { completeLines :: ![ByteString]
    , partialChunksReversed :: ![ByteString]
    , partialByteLength :: !Int
    }

emptyOutputBuffer :: OutputBuffer
emptyOutputBuffer =
    OutputBuffer
        { completeLines = []
        , partialChunksReversed = []
        , partialByteLength = 0
        }

-- | Add bytes received from stdout. Each chunk is split on newlines once.
-- Completed records retain their constituent slices until they are returned.
appendOutputChunk :: OutputBuffer -> ByteString -> OutputBuffer
appendOutputChunk buffer chunk
    | ByteString.null chunk = buffer
    | otherwise =
        case ByteString.split newline chunk of
            [suffix] ->
                appendPartial suffix buffer
            first : second : remaining ->
                let (middle, suffix) =
                        splitLast second remaining
                    firstLine =
                        materializeChunks $
                            addChunk
                                first
                                buffer.partialChunksReversed
                 in OutputBuffer
                        { completeLines =
                            -- readBufferedLine drains queued records before
                            -- receiving another chunk, so this left side is
                            -- empty in production. A list keeps the hot
                            -- dequeue path allocation-free.
                            buffer.completeLines
                                <> (firstLine : middle)
                        , partialChunksReversed =
                            addChunk suffix []
                        , partialByteLength =
                            ByteString.length suffix
                        }
            [] -> buffer
  where
    newline = 10

takeOutputLine
    :: Int
    -> OutputBuffer
    -> Maybe OutputLine
takeOutputLine maximumBytes buffer =
    case buffer.completeLines of
        [] ->
            Nothing
        line : remaining ->
            Just $
                if ByteString.length line > maximumBytes
                    then
                        OutputLineTooLarge
                            (buffer { completeLines = remaining })
                    else
                        OutputLine
                            line
                            (buffer { completeLines = remaining })

-- | Materialize the final unterminated record at EOF.
--
-- Precondition: 'takeOutputLine' returned 'Nothing'. 'readBufferedLine'
-- enforces this by draining every completed record before reading again.
finishOutputBuffer
    :: OutputBuffer
    -> (Maybe ByteString, OutputBuffer)
finishOutputBuffer buffer
    | buffer.partialByteLength == 0 =
        (Nothing, emptyOutputBuffer)
    | otherwise =
        ( Just $
            materializeChunks buffer.partialChunksReversed
        , emptyOutputBuffer
        )

outputPartialLength :: OutputBuffer -> Int
outputPartialLength = (.partialByteLength)

-- | Read one bounded line. An EOF exception is treated like an empty read,
-- matching Handle.hGetSome behavior across platforms. Non-EOF failures leave
-- the buffered partial record intact so a caller may retry.
readBufferedLine
    :: IORef OutputBuffer
    -> Int
    -> (Int -> IO ByteString)
    -> IO OutputReadResult
readBufferedLine bufferRef maximumBytes readChunk =
    go
  where
    go = do
        buffered <- readIORef bufferRef
        case takeOutputLine maximumBytes buffered of
            Just (OutputLine line remaining) -> do
                writeIORef bufferRef remaining
                pure (OutputReadLine line)
            Just (OutputLineTooLarge remaining) -> do
                writeIORef bufferRef remaining
                pure OutputReadTooLarge
            Nothing
                | outputPartialLength buffered > maximumBytes ->
                    pure OutputReadTooLarge
                | otherwise -> do
                    let remainingBytes =
                            maximumBytes
                                - outputPartialLength buffered
                        readSize =
                            min 8_192 (remainingBytes + 1)
                    result <-
                        try (readChunk readSize)
                            :: IO (Either IOException ByteString)
                    case result of
                        Right chunk
                            | ByteString.null chunk ->
                                finish buffered
                            | otherwise -> do
                                writeIORef
                                    bufferRef
                                    (appendOutputChunk buffered chunk)
                                go
                        Left exception
                            | isEOFError exception ->
                                finish buffered
                            | otherwise ->
                                pure (OutputReadFailure exception)

    finish buffered = do
        let (trailing, empty) = finishOutputBuffer buffered
        writeIORef bufferRef empty
        pure case trailing of
            Nothing -> OutputReadEnd
            Just line
                -- The pre-read size guard means an EOF exception cannot
                -- reach this branch with an oversized partial through the
                -- transport API. Keep the empty-read and EOF paths identical.
                | ByteString.length line > maximumBytes ->
                    OutputReadTooLarge
                | otherwise ->
                    OutputReadLine line

appendPartial :: ByteString -> OutputBuffer -> OutputBuffer
appendPartial bytes buffer =
    buffer
        { partialChunksReversed =
            addChunk bytes buffer.partialChunksReversed
        , partialByteLength =
            buffer.partialByteLength + ByteString.length bytes
        }

addChunk :: ByteString -> [ByteString] -> [ByteString]
addChunk bytes chunks
    | ByteString.null bytes = chunks
    | otherwise = bytes : chunks

materializeChunks :: [ByteString] -> ByteString
materializeChunks =
    ByteString.concat . reverse

splitLast :: a -> [a] -> ([a], a)
splitLast current = \case
    [] -> ([], current)
    next : remaining ->
        let (beforeLast, lastValue) =
                splitLast next remaining
         in (current : beforeLast, lastValue)
