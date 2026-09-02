-- | Stable chunking and cache policy for the retained conversation transcript.
module Agent.CLI.TUI.Transcript
    ( coalesceInspectionBlocks
    , transcriptChunkCacheKey
    , transcriptChunkSize
    , transcriptChunks
    ) where

import Agent.TUI.Model
    ( BlockId
    , BlockKind(BlockInspect)
    , BlockState(..)
    , UiBlock(..)
    )
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text

-- | Collapse adjacent, finished read-only tool calls into one visual event.
--
-- New live calls are grouped by the reducer. This projection gives restored
-- transcripts written by older versions the same compact treatment without
-- changing persisted history. The newest block ID represents the run, matching
-- normal selection after tools finish and keeping interaction stable.
coalesceInspectionBlocks :: Seq UiBlock -> Seq UiBlock
coalesceInspectionBlocks =
    Seq.fromList . go . toList
  where
    go [] = []
    go (block : rest)
        | coalescibleInspection block =
            let (following, remaining) =
                    span coalescibleInspection rest
                run = block : following
            in mergeRun run : go remaining
        | otherwise = block : go rest

    mergeRun [block] = block
    mergeRun run =
        let representative = last run
        in representative
            { blockTitle = inspectionRunTitle run
            , blockBody =
                inspectionRunBody representative.blockExpanded run
            , blockDetail = ""
            , blockState = inspectionRunState run
            , blockInspectionGroupable = False
            }

coalescibleInspection :: UiBlock -> Bool
coalescibleInspection block =
    block.blockKind == BlockInspect
        && block.blockState == BlockComplete
        && block.blockInspectionGroupable
        && inspectionCategory block.blockTitle /= InspectionView

inspectionRunTitle :: [UiBlock] -> Text
inspectionRunTitle blocks =
    Text.intercalate ", " summaries <> statusSuffix
  where
    classified = map (inspectionCategory . (.blockTitle)) blocks
    categoryCount category =
        length (filter (== category) classified)
    known =
        [ (InspectionRead, "Read", "file", "files")
        , (InspectionList, "Listed", "dir", "dirs")
        , (InspectionSearch, "Searched", "query", "queries")
        , (InspectionView, "Viewed", "item", "items")
        , (InspectionFetch, "Fetched", "source", "sources")
        , (InspectionOther, "Inspected", "item", "items")
        ]
    summaries =
        [ verb
            <> " "
            <> Text.pack (show count)
            <> " "
            <> if count == 1 then singular else plural
        | (category, verb, singular, plural) <- known
        , let count = categoryCount category
        , count > 0
        ]
    failed =
        length
            (filter
                ((== BlockFailed) . (.blockState))
                blocks)
    interrupted =
        length
            (filter
                ((`elem` [BlockCancelled, BlockDenied]) . (.blockState))
                blocks)
    suffixes =
        [ Text.pack (show failed) <> " failed" | failed > 0 ]
            <> [ Text.pack (show interrupted) <> " interrupted"
               | interrupted > 0
               ]
    statusSuffix =
        case suffixes of
            [] -> ""
            _ -> " · " <> Text.intercalate ", " suffixes

data InspectionCategory
    = InspectionRead
    | InspectionList
    | InspectionSearch
    | InspectionView
    | InspectionFetch
    | InspectionOther
    deriving (Eq)

inspectionCategory :: Text -> InspectionCategory
inspectionCategory title
    | startsWith "Read" = InspectionRead
    | startsWith "Listed" || startsWith "Globbed" = InspectionList
    | startsWith "Searched" = InspectionSearch
    | startsWith "Viewed" = InspectionView
    | startsWith "Fetched" = InspectionFetch
    | otherwise = InspectionOther
  where
    startsWith prefix =
        title == prefix || (prefix <> " ") `Text.isPrefixOf` title

inspectionRunBody :: Bool -> [UiBlock] -> Text
inspectionRunBody expanded =
    Text.intercalate "\n" . concatMap renderItem
  where
    renderItem block
        | not expanded = [inspectionItemTitle block]
        | Text.null (Text.strip block.blockBody) =
            [inspectionItemTitle block, ""]
        | otherwise =
            [ inspectionItemTitle block
            , Text.unlines
                (map ("    " <>) (Text.lines block.blockBody))
            ]

    inspectionItemTitle block =
        "  "
            <> inspectionStateGlyph block.blockState
            <> " "
            <> if Text.null (Text.strip block.blockDetail)
                then block.blockTitle
                else block.blockTitle <> " " <> block.blockDetail

inspectionStateGlyph :: BlockState -> Text
inspectionStateGlyph = \case
    BlockComplete -> "◇"
    BlockFailed -> "✗"
    BlockCancelled -> "⊘"
    BlockDenied -> "⊘"
    BlockStreaming -> "◆"
    BlockRunning -> "◆"

inspectionRunState :: [UiBlock] -> BlockState
inspectionRunState blocks
    | any ((== BlockFailed) . (.blockState)) blocks = BlockFailed
    | any ((== BlockDenied) . (.blockState)) blocks = BlockDenied
    | any ((== BlockCancelled) . (.blockState)) blocks = BlockCancelled
    | otherwise = BlockComplete

-- | Group retained blocks so completed history can be cached as larger images.
transcriptChunks :: Seq UiBlock -> [Seq UiBlock]
transcriptChunks =
    toList . Seq.chunksOf transcriptChunkSize

-- | Return a stable cache key when a transcript chunk is safe to retain.
--
-- Only full chunks are cached. Brick keeps cached results until explicit
-- invalidation, so caching the growing final chunk under a changing endpoint
-- would retain an obsolete large image after every appended block.
transcriptChunkCacheKey
    :: (UiBlock -> Bool)
    -- ^ Whether an individual block is stable and cacheable.
    -> [BlockId]
    -- ^ Blocks with dynamic interaction state, such as selection or hover.
    -> Seq UiBlock
    -> Maybe (BlockId, BlockId)
transcriptChunkCacheKey cacheable dynamicBlockIds blocks
    | Seq.length blocks /= transcriptChunkSize = Nothing
    | not (all cacheable blocks) = Nothing
    | any (.blockExpanded) blocks = Nothing
    | any ((`elem` dynamicBlockIds) . (.blockId)) blocks = Nothing
    | otherwise = case
        ( Seq.lookup 0 blocks
        , Seq.lookup (Seq.length blocks - 1) blocks
        ) of
        (Just firstBlock, Just lastBlock) ->
            Just (firstBlock.blockId, lastBlock.blockId)
        _ -> Nothing

transcriptChunkSize :: Int
transcriptChunkSize = 32
