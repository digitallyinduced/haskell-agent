-- | Append-friendly text accumulation.
--
-- Chunks are retained in reverse arrival order so appending does not copy the
-- accumulated prefix. Convert to strict 'Text' only at an output boundary.
-- The streaming benchmark also compares this representation with
-- @text-builder@ under the same 'IORef' update pattern.
module Agent.TextBuffer
    ( TextBuffer
    , appendTextBuffer
    , compactTextBuffer
    , emptyTextBuffer
    , textBufferChunkCount
    , textBufferFromText
    , textBufferLength
    , textBufferNull
    , textBufferToText
    ) where

import Data.Text (Text)
import qualified Data.Text as Text

data TextBuffer = TextBuffer
    { bufferLength :: !Int
    , bufferChunkCount :: !Int
    , bufferChunks :: ![Text]
    }

instance Eq TextBuffer where
    left == right = textBufferToText left == textBufferToText right

instance Show TextBuffer where
    showsPrec precedence = showsPrec precedence . textBufferToText

emptyTextBuffer :: TextBuffer
emptyTextBuffer = TextBuffer 0 0 []

textBufferFromText :: Text -> TextBuffer
textBufferFromText text
    | Text.null text = emptyTextBuffer
    | otherwise = TextBuffer (Text.length text) 1 [text]

appendTextBuffer :: Text -> TextBuffer -> TextBuffer
appendTextBuffer chunk buffer
    | Text.null chunk = buffer
appendTextBuffer chunk (TextBuffer size count chunks) =
    TextBuffer
        (size + Text.length chunk)
        (count + 1)
        (chunk : chunks)

textBufferToText :: TextBuffer -> Text
textBufferToText (TextBuffer _ _ chunks) =
    Text.concat (reverse chunks)

textBufferNull :: TextBuffer -> Bool
textBufferNull (TextBuffer size _ _) =
    size == 0

-- | Number of 'Text' code units retained by the buffer. This is constant time
-- and is intended for queue/backpressure accounting.
textBufferLength :: TextBuffer -> Int
textBufferLength = (.bufferLength)

-- | Number of append chunks retained by the buffer.
textBufferChunkCount :: TextBuffer -> Int
textBufferChunkCount = (.bufferChunkCount)

-- | Collapse accumulated chunks after streaming completes.
compactTextBuffer :: TextBuffer -> TextBuffer
compactTextBuffer buffer@(TextBuffer _ _ chunks) =
    case chunks of
        [] -> buffer
        [_] -> buffer
        _ -> textBufferFromText (textBufferToText buffer)
