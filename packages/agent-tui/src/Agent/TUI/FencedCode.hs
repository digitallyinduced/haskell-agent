-- | Shared Markdown fenced-code parsing.
--
-- The fullscreen renderer and copy-code commands use this module so their
-- block numbering and fence matching cannot drift apart.
module Agent.TUI.FencedCode
    ( FenceMarker(..)
    , FencedBlock(..)
    , FenceChunk(..)
    , fenceOpener
    , isFenceCloser
    , fenceChunks
    , fencedBlocks
    ) where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text

data FenceMarker = FenceMarker
    { fenceCharacter :: !Char
    , fenceLength :: !Int
    }
    deriving (Eq, Show)

-- | Markdown source split into prose and fenced-code chunks in source order.
-- Delimiter lines are represented by the metadata on 'FenceBlock', rather than
-- repeated in the surrounding prose.
data FenceChunk
    = FenceText !Text
    | FenceBlock !FencedBlock
    deriving (Eq, Show)

data FencedBlock = FencedBlock
    { fencedIndex :: !Int
    , fencedMarker :: !FenceMarker
    , fencedInfo :: !Text
    , fencedBody :: !Text
    , fencedClosed :: !Bool
    }
    deriving (Eq, Show)

-- | Parse a fence opener, allowing the zero-to-three leading spaces permitted
-- by Markdown. Backtick fence info strings may not themselves contain a
-- backtick.
fenceOpener :: Text -> Maybe (FenceMarker, Text)
fenceOpener line = do
    stripped <- stripFenceIndent line
    character <- Text.uncons stripped >>= \(first, _) ->
        if first == '`' || first == '~'
            then Just first
            else Nothing
    let
        (markerText, suffix) = Text.span (== character) stripped
        marker = FenceMarker character (Text.length markerText)
        info = Text.strip suffix
    if marker.fenceLength < 3
        || (character == '`' && Text.any (== '`') info)
        then Nothing
        else Just (marker, info)

-- | Whether a line closes a particular opener. A closer must use the same
-- marker character, be at least as long, and contain only trailing whitespace.
isFenceCloser :: FenceMarker -> Text -> Bool
isFenceCloser marker line =
    case stripFenceIndent line of
        Nothing -> False
        Just stripped ->
            let (markerText, suffix) =
                    Text.span (== marker.fenceCharacter) stripped
            in Text.length markerText >= marker.fenceLength
                && Text.all isSpace suffix

-- | Split Markdown into prose and fenced blocks in source order. Unterminated
-- blocks are included with 'fencedClosed' set to 'False'. Prose and body text
-- retain their original line endings.
fenceChunks :: Text -> [FenceChunk]
fenceChunks = go 1 [] . sourceLines
  where
    go _ prose [] = proseChunk prose
    go index prose (line : rest) =
        case fenceOpener line.lineText of
            Nothing -> go index (line : prose) rest
            Just (marker, info) ->
                let
                    (bodyLines, closingAndRest) =
                        break (isFenceCloser marker . (.lineText)) rest
                    (closed, remaining) =
                        case closingAndRest of
                            [] -> (False, [])
                            _closing : after -> (True, after)
                    block = FencedBlock
                        { fencedIndex = index
                        , fencedMarker = marker
                        , fencedInfo = info
                        , fencedBody =
                            foldMap
                                (\bodyLine ->
                                    bodyLine.lineText <> bodyLine.lineEnding)
                                bodyLines
                        , fencedClosed = closed
                        }
                in proseChunk prose
                    <> [FenceBlock block]
                    <> go (index + 1) [] remaining

    proseChunk [] = []
    proseChunk reversedLines =
        [FenceText (foldMap sourceLineText (reverse reversedLines))]

-- | Extract all fenced blocks in source order.
fencedBlocks :: Text -> [FencedBlock]
fencedBlocks =
    foldr
        (\chunk rest ->
            case chunk of
                FenceText _ -> rest
                FenceBlock block -> block : rest)
        []
        . fenceChunks

data SourceLine = SourceLine
    { lineText :: !Text
    , lineEnding :: !Text
    }

sourceLines :: Text -> [SourceLine]
sourceLines input
    | Text.null input = []
    | otherwise =
        let (line, suffix) = Text.breakOn "\n" input
        in if Text.null suffix
            then [SourceLine line ""]
            else SourceLine line "\n" : sourceLines (Text.drop 1 suffix)

sourceLineText :: SourceLine -> Text
sourceLineText line = line.lineText <> line.lineEnding

stripFenceIndent :: Text -> Maybe Text
stripFenceIndent line =
    let (spaces, stripped) = Text.span (== ' ') line
    in if Text.length spaces <= 3
        then Just stripped
        else Nothing
