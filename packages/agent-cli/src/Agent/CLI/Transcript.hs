{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Pure projection and Markdown rendering for persisted session history.
module Agent.CLI.Transcript
    ( assistantResponseBodies
    , projectTranscriptTurn
    , foldTranscriptTurns
    , renderTranscriptMarkdown
    , renderSessionTranscript
    , searchTranscriptBlocks
    , markdownFence
    ) where

import Agent.CLI.Session
    ( SessionMeta(..)
    , SessionTurn(..)
    , TranscriptEffect(..)
    )
import Agent.CLI.TUI.History
    ( HistoryTurn(historyTurnBlocks)
    )
import Agent.CLI.TUI.SessionHistory (sessionHistoryTurn)
import Agent.Provider (providerSlug)
import Agent.TUI.Model
    ( BlockKind(..)
    , BlockState(..)
    , UiBlock(..)
    )
import Data.Foldable (toList)
import Data.Int (Int64)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text

-- | Project one durable turn using exactly the same rules as the fullscreen
-- history.  The cursor is only used as the stable block-generation input.
projectTranscriptTurn :: Int64 -> SessionTurn -> [UiBlock]
projectTranscriptTurn cursor turn =
    toList (sessionHistoryTurn cursor turn).historyTurnBlocks

-- | Fold durable turns into the visible transcript.  Replacement checkpoints
-- remain scrollback (the fullscreen UI archives them), while reset markers
-- discard all preceding visible blocks.
foldTranscriptTurns :: [(Int64, SessionTurn)] -> [UiBlock]
foldTranscriptTurns =
    List.foldl' step []
  where
    step blocks (cursor, turn) =
        let projected = projectTranscriptTurn cursor turn
        in case turn.turnEffect of
            TranscriptReset -> projected
            TranscriptAppend -> blocks <> projected
            TranscriptReplace -> blocks <> projected

-- | Render a complete session transcript as portable Markdown.
renderSessionTranscript :: SessionMeta -> [SessionTurn] -> Text
renderSessionTranscript meta =
    renderTranscriptMarkdown meta
        . foldTranscriptTurns
        . zip [0 ..]

renderTranscriptMarkdown :: SessionMeta -> [UiBlock] -> Text
renderTranscriptMarkdown meta blocks =
    Text.unlines $
        [ "# " <> escapeHeading (nonEmptyOr "Session transcript" meta.metaTitle)
        , ""
        , "- Session: `" <> meta.metaId <> "`"
        , "- Provider: `" <> providerSlug meta.metaProvider <> "`"
        , "- Model: `" <> meta.metaModel <> "`"
        , "- Created: " <> shown meta.metaCreatedAt
        , "- Updated: " <> shown meta.metaUpdatedAt
        ]
            <> concatMap renderBlock blocks
  where
    renderBlock block =
        [ ""
        , "## " <> blockHeading block.blockKind
        ]
            <> timestamp block
            <> optionalTitle block
            <> renderContent block

    timestamp block
        | Text.null (Text.strip block.blockTimestamp) = []
        | otherwise = ["", "_ " <> block.blockTimestamp <> " _"]

    optionalTitle block
        | Text.null (Text.strip block.blockTitle)
            || block.blockTitle == blockHeading block.blockKind = []
        | otherwise = ["", "**" <> escapeInline block.blockTitle <> "**"]

    renderContent block =
        let body = block.blockBody
            detail = block.blockDetail
            stateLine =
                if block.blockState == BlockComplete
                    then []
                    else ["", "_State: " <> blockStateText block.blockState <> "_"]
            bodyLines
                | Text.null body = []
                | otherwise = ["", markdownFence body]
            detailLines
                | Text.null (Text.strip detail) = []
                | otherwise = ["", "**Details**", "", markdownFence detail]
        in bodyLines <> detailLines <> stateLine

-- | Match transcript blocks case-insensitively across their visible fields.
-- An empty query deliberately returns the complete transcript so @/find@
-- can hand search control to the pager.
searchTranscriptBlocks :: Text -> [UiBlock] -> [UiBlock]
searchTranscriptBlocks rawQuery blocks
    | Text.null query = blocks
    | otherwise = filter matches blocks
  where
    query = Text.toCaseFold (Text.strip rawQuery)
    matches block =
        any
            (Text.isInfixOf query . Text.toCaseFold)
            [ block.blockTitle
            , block.blockBody
            , block.blockDetail
            , block.blockTimestamp
            ]

-- | Non-empty assistant messages, newest first, for Grok-compatible @/copy N@.
assistantResponseBodies :: [UiBlock] -> [Text]
assistantResponseBodies =
    reverse
        . map (.blockBody)
        . filter
            (\block ->
                block.blockKind == BlockAssistant
                    && not (Text.null (Text.strip block.blockBody)))

shown :: Show a => a -> Text
shown = Text.pack . show

nonEmptyOr :: Text -> Text -> Text
nonEmptyOr fallback value
    | Text.null (Text.strip value) = fallback
    | otherwise = value

escapeInline :: Text -> Text
escapeInline = Text.replace "`" "\\`"

escapeHeading :: Text -> Text
escapeHeading =
    Text.replace "#" "\\#"
        . Text.replace "\n" " "

blockHeading :: BlockKind -> Text
blockHeading = \case
    BlockUser -> "User"
    BlockAssistant -> "Assistant"
    BlockThinking -> "Thinking"
    BlockTool -> "Tool"
    BlockInspect -> "Inspect"
    BlockTodo -> "Todo"
    BlockShell -> "Shell"
    BlockEdit -> "Edit"
    BlockSystem -> "System"
    BlockRecap -> "Recap"
    BlockError -> "Error"

blockStateText :: BlockState -> Text
blockStateText = \case
    BlockStreaming -> "streaming"
    BlockRunning -> "running"
    BlockComplete -> "complete"
    BlockFailed -> "failed"
    BlockCancelled -> "cancelled"
    BlockDenied -> "denied"

-- | Fence arbitrary text without allowing embedded backticks to terminate it.
-- A minimum of three backticks keeps ordinary output readable.
markdownFence :: Text -> Text
markdownFence body =
    let longest =
            maximum
                (0 :
                    [ Text.length run
                    | run <- Text.groupBy (==) body
                    , not (Text.null run)
                    , Text.head run == '`'
                    ])
        fence = Text.replicate (max 3 (longest + 1)) "`"
    in fence <> "\n" <> body <> "\n" <> fence
