-- | Bounded newline-delimited record reader for Claude Code's stdout.
module Claude.Agent.SDK.Internal.Transport.OutputBuffer
    ( OutputBuffer
    , OutputReadResult(..)
    , emptyOutputBuffer
    , readOutputLine
    ) where

import Control.Exception.Safe (IOException, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString8
import qualified Data.ByteString.Unsafe as ByteStringUnsafe
import Data.IORef (IORef, readIORef, writeIORef)
import System.IO.Error (isEOFError)

-- | Bytes received from stdout that have not been returned as a record yet.
--
-- Claude Code echoes every tool result as one NDJSON record, so a record can
-- span thousands of pipe reads: an image @Read@ embeds the whole file as
-- base64. The chunks of the current record are kept as a list and joined
-- once when its newline arrives, so a record costs one copy rather than one
-- copy per read. A record that arrives within a single read is returned as
-- a slice of that read without copying.
data OutputBuffer = OutputBuffer
    { recordChunksReversed :: ![ByteString]
    -- ^ Newline-free chunks of the current record, newest first.
    , recordLength :: !Int
    -- ^ Total size of 'recordChunksReversed'.
    , unscanned :: !ByteString
    -- ^ Bytes from the latest read that have not been scanned for a newline.
    }

emptyOutputBuffer :: OutputBuffer
emptyOutputBuffer =
    OutputBuffer
        { recordChunksReversed = []
        , recordLength = 0
        , unscanned = ByteString.empty
        }

data OutputReadResult
    = OutputReadLine !ByteString
    | OutputReadEnd
    | OutputReadTooLarge
    | OutputReadFailure !IOException

-- | Read one newline-terminated record of at most @maximumBytes@ bytes, not
-- counting the newline. The final record may be terminated by end of input.
--
-- @readChunk size@ returns at most @size@ bytes, or an empty string at end
-- of input, like 'ByteString.hGetSome'; an EOF exception is treated as an
-- empty read. The buffer is persisted after every read, so an interrupted
-- call never drops input, and a record is rejected as soon as it has been
-- read past the limit rather than after it has been buffered in full.
readOutputLine
    :: IORef OutputBuffer
    -> Int
    -> (Int -> IO ByteString)
    -> IO OutputReadResult
readOutputLine bufferRef maximumBytes readChunk =
    go
  where
    go = do
        buffer <- readIORef bufferRef
        case ByteString8.elemIndex '\n' buffer.unscanned of
            Just newlineIndex -> do
                -- The index is inside the scanned bytes, so both slices are
                -- in bounds. Keep this branch strict: it runs once per
                -- record and must not allocate more than the old reader.
                let !recordEnd =
                        ByteStringUnsafe.unsafeTake
                            newlineIndex
                            buffer.unscanned
                    !remaining =
                        ByteStringUnsafe.unsafeDrop
                            (newlineIndex + 1)
                            buffer.unscanned
                writeIORef
                    bufferRef
                    (emptyOutputBuffer { unscanned = remaining })
                if buffer.recordLength + newlineIndex > maximumBytes
                    then pure OutputReadTooLarge
                    else pure $! OutputReadLine (joinRecord buffer recordEnd)
            Nothing -> do
                let !pending = bufferScannedBytes buffer
                if pending.recordLength > maximumBytes
                    then pure OutputReadTooLarge
                    else do
                        let readSize =
                                min
                                    8_192
                                    (maximumBytes - pending.recordLength + 1)
                        result <- try (readChunk readSize)
                        case result of
                            Right chunk
                                | ByteString.null chunk ->
                                    finish pending
                                | otherwise -> do
                                    writeIORef
                                        bufferRef
                                        (pending { unscanned = chunk })
                                    go
                            Left exception
                                | isEOFError exception ->
                                    finish pending
                                | otherwise ->
                                    pure (OutputReadFailure exception)

    finish pending = do
        writeIORef bufferRef emptyOutputBuffer
        if pending.recordLength == 0
            then pure OutputReadEnd
            else pure $! OutputReadLine (joinRecord pending ByteString.empty)

-- | Move the newline-free 'unscanned' bytes into the current record.
bufferScannedBytes :: OutputBuffer -> OutputBuffer
bufferScannedBytes buffer
    | ByteString.null buffer.unscanned =
        buffer
    | otherwise =
        OutputBuffer
            { recordChunksReversed =
                buffer.unscanned : buffer.recordChunksReversed
            , recordLength =
                buffer.recordLength + ByteString.length buffer.unscanned
            , unscanned = ByteString.empty
            }

-- | Assemble the current record from its buffered chunks and its final
-- slice, copying only when the record spans more than one read.
joinRecord :: OutputBuffer -> ByteString -> ByteString
joinRecord buffer recordEnd =
    case buffer.recordChunksReversed of
        [] ->
            recordEnd
        chunks ->
            ByteString.concat (reverse (recordEnd : chunks))
