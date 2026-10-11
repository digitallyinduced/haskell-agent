-- | Replay and retention invariants over a host's native transcript items.
-- No serialization or conversion is required, so opaque provider payloads
-- remain intact. Hosts supply protocol classification and retention policy.
module Agent.History
    ( replayFromLatestCheckpoint
    , repairToolPairs
    , trimPreservingLatestActivation
    , trimPreservingLatestCheckpoint
    ) where

import Data.Maybe (mapMaybe)

-- | Include the newest checkpoint and its suffix, discarding earlier items.
replayFromLatestCheckpoint :: (item -> Bool) -> [item] -> [item]
replayFromLatestCheckpoint isCheckpoint items = snd (foldr step (False, []) items)
  where
    step item (foundCheckpoint, suffix)
        | foundCheckpoint = (True, suffix)
        | isCheckpoint item = (True, item : suffix)
        | otherwise = (False, item : suffix)

-- | Remove outputs with no retained call, and insert interrupted outputs
-- immediately after calls with no output. References should include the call
-- kind when different tool protocols can reuse identifiers. The host supplies
-- the interrupted-output representation and any user-facing wording.
repairToolPairs
    :: Eq reference
    => (item -> Maybe reference)
    -> (item -> Maybe reference)
    -> (reference -> item)
    -> [item]
    -> [item]
repairToolPairs callReference outputReference interruptedOutput items =
    concatMap completeCall (filter (not . staleOutput) items)
  where
    calledReferences = mapMaybe callReference items
    outputReferences = mapMaybe outputReference items
    staleOutput item = maybe False (`notElem` calledReferences) (outputReference item)
    completeCall item = case callReference item of
        Just reference | reference `notElem` outputReferences ->
            [item, interruptedOutput reference]
        _ -> [item]

-- | Wrap a host trimmer, reserving space for the most recent activation if
-- normal trimming would remove it. An activation larger than the budget is
-- retained in full; the budget is therefore deliberately a soft limit.
trimPreservingLatestActivation
    :: Eq item
    => (entry -> item)
    -> (entry -> Bool)
    -> (item -> Int)
    -> (Int -> [entry] -> [item])
    -> Int
    -> [entry]
    -> [item]
trimPreservingLatestActivation entryItem isActivation estimate trim budget entries =
    case break isActivation (reverse entries) of
        (_, []) -> trimmed
        (suffixReversed, activationEntry : _)
            | activation `elem` trimmed -> trimmed
            | otherwise ->
                activation : trim (max 0 (budget - estimate activation)) (reverse suffixReversed)
          where
            activation = entryItem activationEntry
  where
    trimmed = trim budget entries

-- | Cut at the latest checkpoint and reserve its space before invoking the
-- suffix trimmer. The checkpoint itself is never truncated, even when it
-- exceeds the budget.
trimPreservingLatestCheckpoint
    :: (entry -> item)
    -> (item -> Bool)
    -> (item -> Int)
    -> (Int -> [entry] -> [item])
    -> Int
    -> [entry]
    -> [item]
trimPreservingLatestCheckpoint entryItem isCheckpoint estimate trim budget entries =
    case replayFromLatestCheckpoint (isCheckpoint . entryItem) entries of
        [] -> []
        first : suffix
            | isCheckpoint (entryItem first) ->
                let checkpoint = entryItem first
                in checkpoint : trim (max 0 (budget - estimate checkpoint)) suffix
        retained -> trim budget retained
