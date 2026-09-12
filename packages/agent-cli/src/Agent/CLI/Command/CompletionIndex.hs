-- | A compressed radix index for case-insensitive command-name completion.
module Agent.CLI.Command.CompletionIndex
    ( CompletionIndex
    , buildCompletionIndex
    , buildGroupedCompletionIndex
    , completeCommandNames
    , matchingCommandGroups
    ) where

import Control.DeepSeq (NFData(..), force)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

-- | Edges contain the longest shared prefix, rather than allocating one node
-- per character. Each node retains its candidates in catalog order: traversal
-- order must not change completion ordering or discard duplicate names.
-- Candidate Text values are shared between ancestor lists.
data CompletionIndex = CompletionIndex
    !Text
    ![Text]
    !IntSet
    !(Map Char CompletionIndex)
    deriving (Eq, Show)

instance NFData CompletionIndex where
    rnf (CompletionIndex prefix candidates groups children) =
        rnf prefix `seq` rnf candidates `seq` rnf groups `seq` rnf children

-- | Names omit the leading slash. Matching uses Unicode lowercasing, while
-- results retain their original spelling and include the leading slash.
buildCompletionIndex :: [Text] -> CompletionIndex
buildCompletionIndex =
    buildGroupedCompletionIndex . map (: [])

-- | Each group contains a canonical name followed by its aliases. Group
-- ordinals are stable catalog positions; duplicate spellings in distinct
-- groups must retain both groups. Force construction here so lookup does not
-- retain or evaluate temporary construction tuples.
buildGroupedCompletionIndex :: [[Text]] -> CompletionIndex
buildGroupedCompletionIndex groups =
    force $ buildNode
        [ (Text.toLower name, (ordinal, "/" <> name))
        | (ordinal, names) <- zip [0 ..] groups
        , name <- names
        ]

buildNode :: [(Text, (Int, Text))] -> CompletionIndex
buildNode [] = CompletionIndex "" [] IntSet.empty Map.empty
buildNode entries@((firstName, _) : remaining) =
    let prefix = foldl' (\shared (name, _) -> commonPrefix shared name)
            firstName remaining
        prefixLength = Text.length prefix
        children = foldr (groupEntry prefixLength) Map.empty entries
    in CompletionIndex prefix
        (map (snd . snd) entries)
        (IntSet.fromList (map (fst . snd) entries))
        (Map.map buildNode children)
  where
    commonPrefix first second =
        case Text.commonPrefixes first second of
            Nothing -> ""
            Just (shared, _, _) -> shared
    groupEntry prefixLength (name, candidate) groups =
        let suffix = Text.drop prefixLength name
        in case Text.uncons suffix of
            Nothing -> groups
            Just (initial, _) ->
                Map.insertWith (<>) initial [(suffix, candidate)] groups

-- | The query must already be lowercased and have its leading slashes removed.
-- A query ending inside a compressed edge matches the entire subtree.
completeCommandNames :: CompletionIndex -> Text -> [Text]
completeCommandNames (CompletionIndex prefix candidates _ children) query
    | query `Text.isPrefixOf` prefix = candidates
    | otherwise =
        case Text.stripPrefix prefix query of
            Nothing -> []
            Just suffix ->
                case Text.uncons suffix of
                    Nothing -> candidates
                    Just (initial, _) ->
                        case Map.lookup initial children of
                            Nothing -> []
                            Just child -> completeCommandNames child suffix

-- | Find groups with at least one name containing the query as a subsequence.
-- This is a candidate prefilter only: existing fuzzy scoring still determines
-- ranking and highlight positions. The query must already be lowercased.
--
-- Greedy subsequence consumption is shared along compressed edges. Once the
-- query is exhausted, every descendant matches and its cached group set can
-- be returned without traversing the remaining suffixes.
matchingCommandGroups :: CompletionIndex -> Text -> IntSet
matchingCommandGroups (CompletionIndex prefix _ groups children) query =
    let remaining = consumeSubsequence query prefix
    in if Text.null remaining
        then groups
        else Map.foldl'
            (\matches child ->
                IntSet.union matches (matchingCommandGroups child remaining))
            IntSet.empty
            children

consumeSubsequence :: Text -> Text -> Text
consumeSubsequence query edge =
    case (Text.uncons query, Text.uncons edge) of
        (Nothing, _) -> query
        (_, Nothing) -> query
        (Just (wanted, remaining), Just (actual, suffix)) ->
            consumeSubsequence
                (if wanted == actual then remaining else query)
                suffix
