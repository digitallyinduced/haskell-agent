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

import Control.Applicative ((<|>))
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
    (_, stripped) <- stripFenceIndent line
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
        Just (_, stripped) ->
            let (markerText, suffix) =
                    Text.span (== marker.fenceCharacter) stripped
            in Text.length markerText >= marker.fenceLength
                && Text.all isSpace suffix

-- | Split Markdown into prose and fenced blocks in source order. Unterminated
-- blocks are included with 'fencedClosed' set to 'False'. Prose and body text
-- retain their original line endings.
fenceChunks :: Text -> [FenceChunk]
fenceChunks = go 1 [] [] . sourceLines
  where
    go _ prose _ [] = proseChunk prose
    go index prose previous (line : rest) =
        case fenceOpenerInContext previous line.lineText of
            Nothing -> go index (line : prose) (line : previous) rest
            Just opener ->
                let
                    (bodyLines, closingAndRest) =
                        break
                            (isContextualFenceCloser opener . (.lineText))
                            rest
                    (closed, closingLine, remaining) =
                        case closingAndRest of
                            [] -> (False, [], [])
                            closing : after -> (True, [closing], after)
                    block = FencedBlock
                        { fencedIndex = index
                        , fencedMarker = opener.openMarker
                        , fencedInfo = opener.openInfo
                        , fencedBody =
                            foldMap
                                (\bodyLine ->
                                    stripBodyIndent
                                        opener.openIndent
                                        bodyLine.lineText
                                        <> bodyLine.lineEnding)
                                bodyLines
                        , fencedClosed = closed
                        }
                    consumed = line : bodyLines <> closingLine
                in proseChunk prose
                    <> [FenceBlock block]
                    <> go
                        (index + 1)
                        []
                        (reverse consumed <> previous)
                        remaining

    proseChunk [] = []
    proseChunk reversedLines =
        [FenceText (foldMap sourceLineText (reverse reversedLines))]

data ContextualFence = ContextualFence
    { openMarker :: !FenceMarker
    , openInfo :: !Text
    , openContainerIndent :: !Int
    , openIndent :: !Int
    }

fenceOpenerInContext :: [SourceLine] -> Text -> Maybe ContextualFence
fenceOpenerInContext previous line =
    if not (startsWithFenceMarker line)
        then Nothing
        else
            let lineIndent = leadingSpaceCount line
            in (do
                    containerIndent <-
                        listContainerIndent lineIndent previous
                    contextualFence containerIndent line)
                <|> contextualFence 0 line

contextualFence :: Int -> Text -> Maybe ContextualFence
contextualFence containerIndent line = do
    insideContainer <- dropSpaceIndent containerIndent line
    (fenceIndent, stripped) <- stripFenceIndent insideContainer
    (marker, info) <- fenceOpener stripped
    pure ContextualFence
        { openMarker = marker
        , openInfo = info
        , openContainerIndent = containerIndent
        , openIndent = containerIndent + fenceIndent
        }

isContextualFenceCloser :: ContextualFence -> Text -> Bool
isContextualFenceCloser opener line =
    maybe False
        (isFenceCloser opener.openMarker)
        (dropSpaceIndent opener.openContainerIndent line)

-- | Find the nearest surrounding list item whose continuation indentation
-- contains the prospective fence. Intervening non-blank lines must remain
-- within that item, which prevents an old list from making an unrelated
-- top-level four-space-indented line look like a fence.
listContainerIndent :: Int -> [SourceLine] -> Maybe Int
listContainerIndent lineIndent = go maxBound
  where
    go _ [] = Nothing
    go minimumIndent (line : rest)
        | Text.null (Text.strip line.lineText) =
            go minimumIndent rest
        | Just contentIndent <- listItemContentIndent line.lineText
        , contentIndent <= lineIndent
        , contentIndent <= minimumIndent =
            Just contentIndent
        | otherwise =
            let minimumIndent' =
                    min minimumIndent (leadingSpaceCount line.lineText)
            in if minimumIndent' == 0
                then Nothing
                else go minimumIndent' rest

listItemContentIndent :: Text -> Maybe Int
listItemContentIndent line =
    bulletIndent <|> orderedIndent
  where
    leading = leadingSpaceCount line
    stripped = Text.drop leading line
    bulletIndent = do
        (marker, afterMarker) <- Text.uncons stripped
        if marker `elem` ['-', '+', '*']
            then contentIndentAfterMarker leading 1 afterMarker
            else Nothing
    orderedIndent = do
        let (digits, afterDigits) = Text.span isAsciiDigit stripped
        if Text.null digits || Text.length digits > 9
            then Nothing
            else do
                (marker, afterMarker) <- Text.uncons afterDigits
                if marker == '.' || marker == ')'
                    then
                        contentIndentAfterMarker
                            leading
                            (Text.length digits + 1)
                            afterMarker
                    else Nothing

contentIndentAfterMarker :: Int -> Int -> Text -> Maybe Int
contentIndentAfterMarker leading markerWidth afterMarker =
    let spaces = leadingSpaceCount afterMarker
    in if spaces >= 1 && spaces <= 4
        then Just (leading + markerWidth + spaces)
        else Nothing

isAsciiDigit :: Char -> Bool
isAsciiDigit character = character >= '0' && character <= '9'

leadingSpaceCount :: Text -> Int
leadingSpaceCount = Text.length . Text.takeWhile (== ' ')

startsWithFenceMarker :: Text -> Bool
startsWithFenceMarker line =
    case Text.uncons (Text.dropWhile (== ' ') line) of
        Just (marker, _) -> marker == '`' || marker == '~'
        Nothing -> False

dropSpaceIndent :: Int -> Text -> Maybe Text
dropSpaceIndent count line =
    let (spaces, _) = Text.span (== ' ') line
    in if Text.length spaces >= count
        then Just (Text.drop count line)
        else Nothing

stripBodyIndent :: Int -> Text -> Text
stripBodyIndent count line =
    let available = leadingSpaceCount line
    in Text.drop (min count available) line

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

stripFenceIndent :: Text -> Maybe (Int, Text)
stripFenceIndent line =
    let (spaces, stripped) = Text.span (== ' ') line
    in if Text.length spaces <= 3
        then Just (Text.length spaces, stripped)
        else Nothing
