module Agent.CLI.Render.MarkdownStream
    ( MarkdownStreamState
    , emptyMarkdownStreamState
    , feedMarkdownStream
    , flushMarkdownStream
    , streamMarkdownText
    ) where

import Agent.CLI.Markdown
    ( MarkdownFragmentSplit(..)
    , renderMarkdown
    , renderMarkdownFragment
    , splitMarkdownFragment
    )
import Agent.TUI.FencedCode
    ( FenceMarker
    , fenceOpener
    , isFenceCloser
    )
import Agent.TUI.TextWidth (splitTerminalGraphemeSuffix)
import Data.Char (isDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as Text

data MarkdownStreamState = MarkdownStreamState
    { pending :: !Text
    , context :: !(Maybe Char)
    , streamMode :: !MarkdownStreamMode
    , blockPending :: !Text
    }

data MarkdownStreamMode
    = StreamLineStart
    | StreamProse
    | StreamFence !FenceMarker
    | StreamTableCandidate
    | StreamTable

emptyMarkdownStreamState :: MarkdownStreamState
emptyMarkdownStreamState =
    MarkdownStreamState "" Nothing StreamLineStart ""

streamMarkdownText
    :: MarkdownStreamState
    -> Text
    -> (MarkdownStreamState, Text)
streamMarkdownText = feedMarkdownStream

feedMarkdownStream
    :: MarkdownStreamState
    -> Text
    -> (MarkdownStreamState, Text)
feedMarkdownStream state input = case state.streamMode of
    StreamLineStart -> feedLineStart state input
    StreamProse -> feedProse state input
    StreamFence marker -> feedFence marker state input
    StreamTableCandidate -> feedTableCandidate state input
    StreamTable -> feedTable state input

feedLineStart
    :: MarkdownStreamState
    -> Text
    -> (MarkdownStreamState, Text)
feedLineStart state input =
    let buffered = state.blockPending <> input
    in case takeCompleteLine buffered of
        Just (line, rest) ->
            classifyCompleteLine state{blockPending = ""} line rest
        Nothing
            | lineNeedsLookahead buffered ->
                (state{blockPending = buffered}, "")
            | otherwise ->
                feedProse
                    state
                        { streamMode = StreamProse
                        , blockPending = ""
                        }
                    buffered

classifyCompleteLine
    :: MarkdownStreamState
    -> Text
    -> Text
    -> (MarkdownStreamState, Text)
classifyCompleteLine state line rest
    | Just (marker, _) <- fenceOpener (dropLineEnding line) =
        feedMarkdownStream
            state
                { streamMode = StreamFence marker
                , blockPending = line
                }
            rest
    | isPossibleTableHeader line =
        feedMarkdownStream
            state
                { streamMode = StreamTableCandidate
                , blockPending = line
                }
            rest
    | lineIsBlock line =
        let (nextState, output) =
                feedMarkdownStream
                    state
                        { streamMode = StreamLineStart
                        , blockPending = ""
                        }
                    rest
        in (nextState, renderMarkdown True line <> output)
    | otherwise =
        feedProse
            state
                { streamMode = StreamProse
                , blockPending = ""
                }
            (line <> rest)

feedProse
    :: MarkdownStreamState
    -> Text
    -> (MarkdownStreamState, Text)
feedProse state input =
    case Text.breakOn "\n" input of
        (linePart, rest)
            | Text.null rest ->
                let source = state.pending <> linePart
                    MarkdownFragmentSplit
                        { markdownReady = parsedReady
                        , markdownPending = parsedPending
                        , markdownPrevChar = parsedContext
                        } =
                        splitMarkdownFragment state.context source
                    (stablePrefix, graphemePending) =
                        splitTerminalGraphemeSuffix parsedReady
                    MarkdownFragmentSplit
                        { markdownReady = ready
                        , markdownPending = reparsedPending
                        , markdownPrevChar = nextContext
                        }
                        | Text.null graphemePending =
                            MarkdownFragmentSplit
                                { markdownReady = parsedReady
                                , markdownPending = ""
                                , markdownPrevChar = parsedContext
                                }
                        | otherwise =
                            splitMarkdownFragment
                                state.context
                                stablePrefix
                    pending' =
                        reparsedPending
                            <> graphemePending
                            <> parsedPending
                in ( state
                        { pending = pending'
                        , context = nextContext
                        , streamMode = StreamProse
                        }
                   , renderMarkdownFragment True state.context ready
                   )
            | otherwise ->
                let source = state.pending <> linePart <> "\n"
                    MarkdownFragmentSplit
                        { markdownReady = ready
                        , markdownPending = pending'
                        } =
                        splitMarkdownFragment state.context source
                    rendered =
                        renderMarkdownFragment True state.context
                            (ready <> pending')
                    reset =
                        state
                            { pending = ""
                            , context = Nothing
                            , streamMode = StreamLineStart
                            , blockPending = ""
                            }
                    (nextState, following) =
                        feedMarkdownStream reset (Text.drop 1 rest)
                in (nextState, rendered <> following)

feedFence
    :: FenceMarker
    -> MarkdownStreamState
    -> Text
    -> (MarkdownStreamState, Text)
feedFence marker state input =
    let buffered = state.blockPending <> input
        (lines_, partial) = completeLines buffered
        (beforeCloser, closingAndAfter) =
            break (isFenceCloser marker . dropLineEnding) (drop 1 lines_)
    in case closingAndAfter of
        [] -> (state{blockPending = buffered}, "")
        closing : after ->
            let block = Text.concat (take 1 lines_ <> beforeCloser <> [closing])
                rest = Text.concat after <> partial
                reset =
                    state
                        { streamMode = StreamLineStart
                        , blockPending = ""
                        , pending = ""
                        , context = Nothing
                        }
                (nextState, following) = feedMarkdownStream reset rest
            in (nextState, renderMarkdown True block <> following)

feedTableCandidate
    :: MarkdownStreamState
    -> Text
    -> (MarkdownStreamState, Text)
feedTableCandidate state input =
    let buffered = state.blockPending <> input
        (lines_, partial) = completeLines buffered
    in case lines_ of
        header : separator : after
            | isTableSeparator separator ->
                feedMarkdownStream
                    state
                        { streamMode = StreamTable
                        , blockPending = header <> separator
                        }
                    (Text.concat after <> partial)
            | otherwise ->
                let reset =
                        state
                            { streamMode = StreamLineStart
                            , blockPending = ""
                            }
                    (nextState, following) =
                        feedMarkdownStream reset
                            (separator <> Text.concat after <> partial)
                in (nextState, renderMarkdown True header <> following)
        _ -> (state{blockPending = buffered}, "")

feedTable
    :: MarkdownStreamState
    -> Text
    -> (MarkdownStreamState, Text)
feedTable state input =
    let buffered = state.blockPending <> input
        (lines_, partial) = completeLines buffered
        (tableLines, after) =
            case lines_ of
                header : separator : rows ->
                    let (body, following) =
                            span isPossibleTableHeader rows
                    in (header : separator : body, following)
                _ -> (lines_, [])
    in case after of
        [] -> (state{blockPending = buffered}, "")
        line : rest ->
            let table = Text.concat tableLines
                reset =
                    state
                        { streamMode = StreamLineStart
                        , blockPending = ""
                        }
                (nextState, following) =
                    feedMarkdownStream reset
                        (line <> Text.concat rest <> partial)
            in (nextState, renderMarkdown True table <> following)

flushMarkdownStream :: MarkdownStreamState -> Text
flushMarkdownStream state = case state.streamMode of
    StreamProse ->
        renderMarkdownFragment True state.context state.pending
    StreamLineStart ->
        renderMarkdown True state.blockPending
    StreamFence _ ->
        renderMarkdown True state.blockPending
    StreamTableCandidate ->
        renderMarkdown True state.blockPending
    StreamTable ->
        renderMarkdown True state.blockPending

takeCompleteLine :: Text -> Maybe (Text, Text)
takeCompleteLine text =
    case Text.breakOn "\n" text of
        (_, rest) | Text.null rest -> Nothing
        (line, rest) -> Just (line <> "\n", Text.drop 1 rest)

completeLines :: Text -> ([Text], Text)
completeLines = go []
  where
    go reversed remaining =
        case takeCompleteLine remaining of
            Nothing -> (reverse reversed, remaining)
            Just (line, rest) -> go (line : reversed) rest

dropLineEnding :: Text -> Text
dropLineEnding = Text.dropWhileEnd (== '\n')

lineNeedsLookahead :: Text -> Bool
lineNeedsLookahead line =
    let stripped = Text.dropWhile isSpace line
        markerRun marker = Text.span (== marker) stripped
        allMarkerOrSpace marker =
            Text.all (\character -> character == marker || isSpace character)
                stripped
    in case Text.uncons stripped of
        Nothing -> True
        Just ('#', _) ->
            let (marks, after) = markerRun '#'
            in Text.length marks <= 6
                && (Text.null after || Text.isPrefixOf " " after)
        Just ('>', _) -> True
        Just ('|', _) -> True
        Just ('`', _) ->
            let (ticks, after) = markerRun '`'
            in Text.null after || Text.length ticks >= 3
        Just ('~', _) ->
            let (tildes, after) = markerRun '~'
            in Text.null after || Text.length tildes >= 3
        Just ('+', after) -> Text.null after || Text.isPrefixOf " " after
        Just ('*', after) ->
            Text.null after
                || Text.isPrefixOf " " after
                || allMarkerOrSpace '*'
        Just ('-', after) ->
            Text.null after
                || Text.isPrefixOf " " after
                || allMarkerOrSpace '-'
        Just ('_', _) -> allMarkerOrSpace '_'
        Just (character, _)
            | isDigit character ->
                let (digits, after) = Text.span isDigit stripped
                in not (Text.null digits)
                    && ( Text.null after
                        || after == "."
                        || Text.isPrefixOf ". " after
                       )
        _ -> False

lineIsBlock :: Text -> Bool
lineIsBlock line =
    let stripped = Text.dropWhile isSpace (dropLineEnding line)
        (marks, afterHeading) = Text.span (== '#') stripped
        heading =
            not (Text.null marks)
                && Text.length marks <= 6
                && Text.isPrefixOf " " afterHeading
        quote = Text.isPrefixOf ">" stripped
        bullet = any (\prefix -> prefix `Text.isPrefixOf` stripped)
            ["- ", "* ", "+ "]
        (digits, orderedRest) = Text.span isDigit stripped
        ordered =
            not (Text.null digits) && Text.isPrefixOf ". " orderedRest
        thematic marker =
            let compact = Text.filter (not . isSpace) stripped
            in Text.length compact >= 3 && Text.all (== marker) compact
    in heading
        || quote
        || bullet
        || ordered
        || thematic '-'
        || thematic '*'
        || thematic '_'

isPossibleTableHeader :: Text -> Bool
isPossibleTableHeader =
    Text.isPrefixOf "|" . Text.dropWhile isSpace . dropLineEnding

isTableSeparator :: Text -> Bool
isTableSeparator line =
    let stripped =
            Text.dropWhile (== '|')
                (Text.dropWhileEnd (== '|')
                    (Text.strip (dropLineEnding line)))
        cells = map Text.strip (Text.splitOn "|" stripped)
        valid cell =
            Text.any (== '-') cell
                && Text.null
                    (Text.filter (\character ->
                        character `notElem` ['-', ':', ' ']) cell)
    in not (null cells) && all valid cells
