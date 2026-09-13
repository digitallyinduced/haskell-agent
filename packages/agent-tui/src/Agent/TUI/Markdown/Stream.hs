-- | Width-independent, append-only Markdown syntax. Completed lines and table
-- cells are parsed once. The last line and a possible table header remain
-- provisional because later input can change their interpretation.
module Agent.TUI.Markdown.Stream
    ( MarkdownStreamState
    , emptyMarkdownStreamState
    , feedMarkdownStream
    , markdownStreamSnapshot
    , finishMarkdownStream
    , markdownStreamRetainedBytes
    , MarkdownSection(..)
    , MarkdownBlock(..)
    , MarkdownLineKind(..)
    , MarkdownCell(..)
    ) where

import Agent.TUI.FencedCode
import qualified Agent.TUI.Markdown.Block as Block
import Agent.TUI.Markdown.Inline
import Agent.TUI.TextWidth (displayTerminalText, graphemeCellWidth, graphemeClusters)
import Data.Char (isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text

data MarkdownLineKind
    = ProseLine
    | HeadingLine
    | BulletLine !Text
    | OrderedLine !Text !Text
    | QuoteLine
    | BlankLine
    | ThematicLine
    deriving (Eq, Show)

data MarkdownCell = MarkdownCell
    { cellInlines :: ![Inline]
    , cellNaturalWidth :: !Int
    , cellMinimumWidth :: !Int
    }
    deriving (Eq, Show)

data MarkdownBlock
    = MarkdownLine !MarkdownLineKind ![Inline]
    | MarkdownTable ![Block.TableAlignment] !(Seq [MarkdownCell])
    deriving (Eq, Show)

data MarkdownSection
    = MarkdownProseSection !Int !Int !Bool !Bool !(Seq MarkdownBlock)
    | MarkdownCodeSection !Int !FencedBlock
    deriving (Eq, Show)

data ProseState = ProseState
    { completedBlocks :: !(Seq MarkdownBlock)
    , candidateHeader :: !(Maybe (Text, MarkdownBlock))
    , activeTable :: !(Maybe ([Block.TableAlignment], Seq [MarkdownCell]))
    , allWhitespace :: !Bool
    }
    deriving (Eq, Show)

data PendingInline = PendingInline
    { pendingKey :: !(Int, Int)
    , pendingKind :: !MarkdownLineKind
    , pendingBody :: !Text
    , pendingParser :: !(Maybe InlineStreamState)
    }
    deriving (Eq, Show)

data MarkdownStreamState = MarkdownStreamState
    { fenceParser :: !FenceStreamState
    , proseParsers :: !(Map (Int, Int) ProseState)
    , pendingInline :: !(Maybe PendingInline)
    }
    deriving (Eq, Show)

emptyMarkdownStreamState :: MarkdownStreamState
emptyMarkdownStreamState = MarkdownStreamState emptyFenceStreamState Map.empty Nothing

-- | Logical retained storage for mailbox admission. Include both source and
-- syntax, even if some Text storage is shared. Inspect retained fields rather
-- than snapshots: rendering can allocate or reparse provisional syntax.
-- This remains valid when the associated UI block is missing or stale.
-- The charge is a logical estimate, not an exact measurement of heap residency
-- or the backing arrays shared by Text slices.
markdownStreamRetainedBytes :: MarkdownStreamState -> Integer
markdownStreamRetainedBytes state =
    128
        + fenceStreamRetainedBytes state.fenceParser
        + Map.foldl' (\size prose -> size + proseBytes prose) 0 state.proseParsers
        + maybe 0 pendingBytes state.pendingInline
  where
    textBytes text = 64 + 4 * toInteger (Text.length text)
    kindBytes (BulletLine marker) = textBytes marker
    kindBytes (OrderedLine marker spacing) = textBytes marker + textBytes spacing
    kindBytes _ = 0
    blockBytes (MarkdownLine kind inlines) =
        128 + kindBytes kind + inlinesBytes inlines
    blockBytes (MarkdownTable alignments rows) = 128 + tableBytes alignments rows
    tableBytes alignments rows =
        64 * toInteger (length alignments)
            + foldl' (\size cells ->
                size + 64 + foldl' (\rowSize cell -> rowSize + cellBytes cell) 0 cells) 0 rows
    inlinesBytes = foldl' (\size node -> size + inlineRetainedBytes node) 0
    cellBytes :: MarkdownCell -> Integer
    cellBytes cell = 128 + inlinesBytes cell.cellInlines
    proseBytes :: ProseState -> Integer
    proseBytes prose =
        192 + foldl' (\size block -> size + blockBytes block) 0 prose.completedBlocks
            + maybe 0 (\(source, node) -> textBytes source + blockBytes node) prose.candidateHeader
            + maybe 0 (uncurry tableBytes) prose.activeTable
    pendingBytes :: PendingInline -> Integer
    pendingBytes pending =
        128 + kindBytes pending.pendingKind + textBytes pending.pendingBody
            + maybe 0 inlineStreamRetainedBytes pending.pendingParser

emptyProseState :: ProseState
emptyProseState = ProseState Seq.empty Nothing Nothing True

feedMarkdownStream :: MarkdownStreamState -> Text -> MarkdownStreamState
feedMarkdownStream state input
    | Text.null input = state
    | otherwise =
        let (fences, lines_) = feedFenceStreamWithProse state.fenceParser input
            prose = case lines_ of
                [] -> state.proseParsers
                first : rest -> foldl' (consumeLine Nothing)
                    (consumeLine state.pendingInline state.proseParsers first) rest
            pending = case fenceStreamPendingProse fences of
                Nothing -> Nothing
                Just (chunk, section, source) ->
                    let (kind, body) = lineParts source
                        key = (chunk, section)
                        -- For short lines, constructing resumable scanner state
                        -- costs more than the batch parse. Retain block syntax
                        -- either way, and start the inline scanner once useful.
                        parser = if Text.compareLength body 128 /= GT
                            then Nothing
                            else Just $ case state.pendingInline of
                                -- Prose bodies are the complete source line: when
                                -- no newline arrived, the input is exactly its
                                -- suffix. Avoid comparing the growing old prefix.
                                Just previous
                                    | Just previousParser <- previous.pendingParser
                                    , previous.pendingKey == key
                                    , previous.pendingKind == ProseLine
                                    , kind == ProseLine
                                    , not (Text.any (== '\n') input) ->
                                        feedInlineStream previousParser input
                                Just previous
                                    | Just previousParser <- previous.pendingParser
                                    , null lines_
                                    , previous.pendingKey == key
                                    , Just delta <- Text.stripPrefix previous.pendingBody body ->
                                        feedInlineStream previousParser delta
                                _ -> feedInlineStream emptyInlineStreamState body
                    in Just (PendingInline key kind body parser)
        in MarkdownStreamState fences prose pending
  where
    -- Only the first completed line can extend the previous inline parser.
    -- Reclassification or a changed body prefix falls back to the batch oracle.
    consumeLine :: Maybe PendingInline -> Map (Int, Int) ProseState
        -> (Int, Int, Text) -> Map (Int, Int) ProseState
    consumeLine previous parsers (chunk, section, source) =
        let (kind, body) = lineParts source
            inlines = case previous of
                Just pending
                    | Just parser <- pending.pendingParser
                    , pending.pendingKey == (chunk, section)
                    , pending.pendingKind == kind
                    , Just delta <- Text.stripPrefix pending.pendingBody body ->
                        inlineStreamSnapshot (feedInlineStream parser delta)
                _ -> parseInline body
        in Map.alter
            (Just . appendLine source (MarkdownLine kind inlines) . fromMaybe emptyProseState)
            (chunk, section) parsers

markdownStreamSnapshot :: MarkdownStreamState -> [MarkdownSection]
markdownStreamSnapshot state = map section (fenceStreamLayout state.fenceParser)
  where
    pendingProse = fenceStreamPendingProse state.fenceParser
    section (FenceCodeLayout chunk block) = MarkdownCodeSection chunk block
    section (FenceProseLayout chunk index stable) =
        let key = (chunk, index)
            committed = Map.findWithDefault emptyProseState key state.proseParsers
            preview = case pendingProse of
                Just (pendingChunk, pendingIndex, source)
                    | key == (pendingChunk, pendingIndex) ->
                        let node = case state.pendingInline of
                                Just pending | pending.pendingKey == key ->
                                    MarkdownLine pending.pendingKind
                                        (maybe (parseInline pending.pendingBody) inlineStreamSnapshot pending.pendingParser)
                                _ -> parseLine source
                        in appendLine source node committed
                _ -> committed
        in MarkdownProseSection chunk index stable preview.allWhitespace (proseBlocks preview)

-- | Finishing accepts the current literal fallback for incomplete syntax. It
-- does not append a synthetic newline or change the interpretation of fences.
finishMarkdownStream :: MarkdownStreamState -> [MarkdownSection]
finishMarkdownStream = markdownStreamSnapshot

proseBlocks :: ProseState -> Seq MarkdownBlock
proseBlocks state = case state.activeTable of
    Just (alignments, rows) -> state.completedBlocks |> MarkdownTable alignments rows
    Nothing -> case state.candidateHeader of
        Just (_, node) -> state.completedBlocks |> node
        Nothing -> state.completedBlocks

appendLine :: Text -> MarkdownBlock -> ProseState -> ProseState
appendLine source node initial =
    append initial{allWhitespace = initial.allWhitespace && Text.all isSpace source}
  where
    append state = case state.activeTable of
        Just (alignments, rows)
            | Text.any (== '|') source
            , Just cells <- Block.splitTableRow source ->
                state{activeTable = Just (alignments, rows |> map parseCell cells)}
            | otherwise ->
                append state
                    { completedBlocks = state.completedBlocks |> MarkdownTable alignments rows
                    , activeTable = Nothing
                    }
        Nothing -> case state.candidateHeader of
            Just (header, headerNode)
                | Just (table, _) <- Block.takeTableRows [header, source] ->
                    state
                        { candidateHeader = Nothing
                        , activeTable = Just
                            (table.tableAlignments, Seq.fromList (map (map parseCell) table.tableRows))
                        }
                | otherwise ->
                    append state
                        { completedBlocks = state.completedBlocks |> headerNode
                        , candidateHeader = Nothing
                        }
            Nothing
                | Text.any (== '|') source
                , Just _ <- Block.splitTableRow source ->
                    state{candidateHeader = Just (source, node)}
                | otherwise ->
                    state{completedBlocks = state.completedBlocks |> node}

parseLine :: Text -> MarkdownBlock
parseLine source =
    let (kind, body) = lineParts source
    in MarkdownLine kind (parseInline body)

lineParts :: Text -> (MarkdownLineKind, Text)
lineParts source
    -- An ASCII letter at column zero cannot later become a block marker.
    -- Avoid running every block recognizer on the common prose path.
    | Just (first, _) <- Text.uncons source
    , (first >= 'a' && first <= 'z') || (first >= 'A' && first <= 'Z') = (ProseLine, source)
    | Just (_, body) <- Block.headingPartsWith (== ' ') source = (HeadingLine, body)
    | Just (indent, body) <- Block.bulletPartsWith (== ' ') source = (BulletLine indent, body)
    | Just (indent, number, body) <- Block.orderedParts source = (OrderedLine indent number, body)
    | Just quote <- Block.blockQuoteRemainder source = (QuoteLine, fromMaybe quote (Text.stripPrefix " " quote))
    | Text.null (Text.strip source) = (BlankLine, "")
    | Just (marker, _) <- Text.uncons (Text.stripStart source)
    , marker `elem` ['-', '*', '_']
    , Block.isThematicBreak source = (ThematicLine, "")
    | otherwise = (ProseLine, source)

parseCell :: Text -> MarkdownCell
parseCell source =
    let inlines = parseInline source
        widths = map graphemeCellWidth
            (graphemeClusters (displayTerminalText (inlinePlainText inlines)))
    in MarkdownCell inlines (sum widths) (maximum (1 : widths))
