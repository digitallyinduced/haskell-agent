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
    , FenceStreamState
    , FenceSection(..)
    , emptyFenceStreamState
    , feedFenceStream
    , fenceStreamSections
    , FenceSectionLayout(..)
    , feedFenceStreamWithProse
    , fenceStreamPendingProse
    , fenceStreamLayout
    , fenceStreamRetainedBytes
    ) where

import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
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
    deriving (Eq, Show)

-- | Completed prose sections retain the renderer's fence-chunk and section
-- indices. Only an empty, newline-terminated line makes prose cacheable.
data FenceSection
    = FenceProseSection !Int !Int !Bool !Text
    | FenceCodeSection !Int !FencedBlock
    deriving (Eq, Show)

-- | Logical retained size, including source context and unfinished sections.
-- Count shared text conservatively and never construct a rendering snapshot.
-- Integer arithmetic lets callers saturate at their own accounting limit.
-- This is a logical estimate, not exact heap residency: Text slices may share
-- backing arrays, and their complete allocation sizes are not inspected.
fenceStreamRetainedBytes :: FenceStreamState -> Integer
fenceStreamRetainedBytes state =
    256
        + foldl' (\size section -> size + sectionBytes section) 0 state.streamSections
        + foldl' (\size line -> size + 128 + textBytes line.lineText + textBytes line.lineEnding)
            0 state.streamPrevious
        + textBytes state.streamPendingLine
        + maybe 0 (\opener -> 128 + textBytes opener.openInfo) state.streamOpener
        + foldl' (\size text -> size + textBytes text) 0 state.streamLines
  where
    textBytes text = 64 + 4 * toInteger (Text.length text)
    sectionBytes (FenceProseSection _ _ _ text) = 128 + textBytes text
    sectionBytes (FenceCodeSection _ block) =
        128 + textBytes block.fencedInfo + textBytes block.fencedBody

-- | Append-only parser state. A partial final line is never committed: even a
-- seemingly complete closing fence can become ordinary code when more arrives.
-- Rendering previews that line without changing the retained parser state.
data FenceStreamState = FenceStreamState
    { streamSections :: !(Seq FenceSection)
    , streamPrevious :: ![SourceLine]
    , streamPendingLine :: !Text
    , streamOpener :: !(Maybe ContextualFence)
    , streamLines :: ![Text]
    , streamChunkIndex :: !Int
    , streamCodeIndex :: !Int
    , streamSectionIndex :: !Int
    , streamHasProse :: !Bool
    }
    deriving (Eq, Show)

emptyFenceStreamState :: FenceStreamState
emptyFenceStreamState =
    FenceStreamState Seq.empty [] "" Nothing [] 1 1 1 False

feedFenceStream :: FenceStreamState -> Text -> FenceStreamState
feedFenceStream state input =
    case Text.breakOn "\n" input of
        (fragment, suffix)
            | Text.null suffix ->
                state{streamPendingLine = state.streamPendingLine <> fragment}
            | otherwise ->
                let line = SourceLine (state.streamPendingLine <> fragment) "\n"
                    next = consumeStreamLine state{streamPendingLine = ""} line
                in feedFenceStream next (Text.drop 1 suffix)

-- | Structural events for a downstream prose parser. Only newline-complete
-- prose lines are committed; the final line remains a reversible preview.
feedFenceStreamWithProse
    :: FenceStreamState -> Text -> (FenceStreamState, [(Int, Int, Text)])
feedFenceStreamWithProse state input =
    case Text.breakOn "\n" input of
        (fragment, suffix)
            | Text.null suffix ->
                (state{streamPendingLine = state.streamPendingLine <> fragment}, [])
            | otherwise ->
                let source = state.streamPendingLine <> fragment
                    line = SourceLine source "\n"
                    -- A prose line leaves the parser outside a fence. Reuse
                    -- the transition's decision instead of running contextual
                    -- opener recognition twice for every completed line.
                    events = case (state.streamOpener, next.streamOpener) of
                        (Nothing, Nothing) ->
                            [(state.streamChunkIndex, state.streamSectionIndex, source)]
                        _ -> []
                    next = consumeStreamLine state{streamPendingLine = ""} line
                    (final, remaining) = feedFenceStreamWithProse next (Text.drop 1 suffix)
                in (final, events <> remaining)

fenceStreamPendingProse :: FenceStreamState -> Maybe (Int, Int, Text)
fenceStreamPendingProse state
    | Nothing <- state.streamOpener
    , not (Text.null state.streamPendingLine)
    , Nothing <- fenceOpenerInContext state.streamPrevious state.streamPendingLine =
        Just (state.streamChunkIndex, state.streamSectionIndex, state.streamPendingLine)
    | otherwise = Nothing

-- | A view that does not flatten the growing prose section. Source-bearing
-- snapshots remain available for callers that actually require source text.
data FenceSectionLayout
    = FenceProseLayout !Int !Int !Bool
    | FenceCodeLayout !Int !FencedBlock
    deriving (Eq, Show)

fenceStreamLayout :: FenceStreamState -> [FenceSectionLayout]
fenceStreamLayout state =
    let preview
            | Text.null state.streamPendingLine = state
            | otherwise = consumeStreamLine state{streamPendingLine = ""}
                (SourceLine state.streamPendingLine "")
        completed = map sectionLayout (toList preview.streamSections)
        active = case preview.streamOpener of
            Just opener ->
                [FenceCodeLayout preview.streamChunkIndex (streamBlock preview opener False)]
            Nothing
                | not (null preview.streamLines) ->
                    [FenceProseLayout preview.streamChunkIndex preview.streamSectionIndex False]
                | otherwise -> []
    in completed <> active
  where
    sectionLayout (FenceProseSection chunk section stable _) =
        FenceProseLayout chunk section stable
    sectionLayout (FenceCodeSection chunk block) = FenceCodeLayout chunk block

consumeStreamLine :: FenceStreamState -> SourceLine -> FenceStreamState
consumeStreamLine state line =
    (consume state){streamPrevious = retainContext line state.streamPrevious}
  where
    consume current = case current.streamOpener of
        Just opener
            | isContextualFenceCloser opener line.lineText ->
                current
                    { streamSections = current.streamSections
                        |> FenceCodeSection current.streamChunkIndex
                            (streamBlock current opener True)
                    , streamOpener = Nothing
                    , streamLines = []
                    , streamChunkIndex = current.streamChunkIndex + 1
                    , streamCodeIndex = current.streamCodeIndex + 1
                    }
            | otherwise ->
                current{streamLines =
                    (stripBodyIndent opener.openIndent line.lineText
                        <> line.lineEnding) : current.streamLines}
        Nothing -> case fenceOpenerInContext current.streamPrevious line.lineText of
            Just opener ->
                current
                    { streamSections = appendProseTail current
                    , streamOpener = Just opener
                    , streamLines = []
                    , streamChunkIndex = current.streamChunkIndex
                        + if current.streamHasProse then 1 else 0
                    , streamSectionIndex = 1
                    , streamHasProse = False
                    }
            Nothing
                | Text.null line.lineText
                , line.lineEnding == "\n"
                , not (null current.streamLines) ->
                    current
                        { streamSections = current.streamSections
                            |> FenceProseSection current.streamChunkIndex
                                current.streamSectionIndex True
                                (Text.concat (reverse ("\n" : current.streamLines)))
                        , streamLines = []
                        , streamSectionIndex = current.streamSectionIndex + 1
                        , streamHasProse = True
                        }
                | otherwise ->
                    current
                        { streamLines = sourceLineText line : current.streamLines
                        , streamHasProse = True
                        }

-- The contextual lookup stops at a non-list, non-blank, unindented line.
-- Earlier source can no longer influence any future fence, so do not retain it.
retainContext :: SourceLine -> [SourceLine] -> [SourceLine]
retainContext line previous
    | leadingSpaceCount line.lineText == 0
    , not (Text.null (Text.strip line.lineText))
    , Nothing <- listItemContentIndent line.lineText = []
    | otherwise = line : previous

appendProseTail :: FenceStreamState -> Seq FenceSection
appendProseTail state
    | null state.streamLines = state.streamSections
    | otherwise = state.streamSections
        |> FenceProseSection state.streamChunkIndex state.streamSectionIndex
            False (Text.concat (reverse state.streamLines))

streamBlock :: FenceStreamState -> ContextualFence -> Bool -> FencedBlock
streamBlock state opener closed = FencedBlock
    { fencedIndex = state.streamCodeIndex
    , fencedMarker = opener.openMarker
    , fencedInfo = opener.openInfo
    , fencedBody = Text.concat (reverse state.streamLines)
    , fencedClosed = closed
    }

-- | Snapshot with the same interpretation as 'fenceChunks' on the source seen
-- so far, but without rescanning completed lines or completed prose sections.
fenceStreamSections :: FenceStreamState -> [FenceSection]
fenceStreamSections state =
    let preview
            | Text.null state.streamPendingLine = state
            | otherwise = consumeStreamLine
                state{streamPendingLine = ""}
                (SourceLine state.streamPendingLine "")
    in toList $ case preview.streamOpener of
        Nothing -> appendProseTail preview
        Just opener -> preview.streamSections
            |> FenceCodeSection preview.streamChunkIndex
                (streamBlock preview opener False)

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
    deriving (Eq, Show)

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
