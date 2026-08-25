-- | Stable chunking and cache policy for the retained conversation transcript.
module Agent.CLI.TUI.Transcript
    ( transcriptChunkCacheKey
    , transcriptChunkSize
    , transcriptChunks
    ) where

import Agent.TUI.Model (BlockId, UiBlock(..))
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

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
