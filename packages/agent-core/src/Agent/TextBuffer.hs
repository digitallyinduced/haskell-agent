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
    , textBufferFromText
    , textBufferNull
    , textBufferToText
    ) where

import Data.Text (Text)
import qualified Data.Text as Text

data TextBuffer = TextBuffer ![Text]

instance Eq TextBuffer where
    left == right = textBufferToText left == textBufferToText right

instance Show TextBuffer where
    showsPrec precedence = showsPrec precedence . textBufferToText

emptyTextBuffer :: TextBuffer
emptyTextBuffer = TextBuffer []

textBufferFromText :: Text -> TextBuffer
textBufferFromText text
    | Text.null text = emptyTextBuffer
    | otherwise = TextBuffer [text]

appendTextBuffer :: Text -> TextBuffer -> TextBuffer
appendTextBuffer chunk buffer
    | Text.null chunk = buffer
appendTextBuffer chunk (TextBuffer chunks) =
    TextBuffer (chunk : chunks)

textBufferToText :: TextBuffer -> Text
textBufferToText (TextBuffer chunks) =
    Text.concat (reverse chunks)

textBufferNull :: TextBuffer -> Bool
textBufferNull (TextBuffer chunks) =
    null chunks

-- | Collapse accumulated chunks after streaming completes.
compactTextBuffer :: TextBuffer -> TextBuffer
compactTextBuffer buffer@(TextBuffer chunks) =
    case chunks of
        [] -> buffer
        [_] -> buffer
        _ -> textBufferFromText (textBufferToText buffer)
