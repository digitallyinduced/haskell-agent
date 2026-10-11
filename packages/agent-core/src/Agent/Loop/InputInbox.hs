{-# LANGUAGE BlockArguments, NamedFieldPuns, OverloadedRecordDot #-}

-- | Pull-based input tracking across provider retries and durable completion.
module Agent.Loop.InputInbox
    ( InputInbox
    , newInputInbox
    , readInputInbox
    , acknowledgeInputInbox
    , acceptedInputInboxItems
    , acceptedInputInboxIds
    , submittedInputInboxIds
    , outstandingInputInboxIds
    , acceptedSteeringIds
    , completeInputInbox
    ) where

import Prelude
import Control.Concurrent.MVar
import qualified Data.Set as Set

-- | One activation owns one inbox across provider retries. Reading returns
-- the unaccepted batch, extended by rows published since the previous read;
-- the batch's existing prefix never changes. Model acceptance advances only
-- the prefix that its request contained, never messages published while the
-- model was running. A request abandoned for newer input is therefore resent
-- with that input. Durable completion is deliberately separate: an accepted
-- request still needs a successfully delivered user-visible result.
--
-- The host must serialize request submission and acknowledgement for each
-- inbox. Reads may extend the outstanding prefix while a request is running,
-- but its acknowledgement must use the count captured for that request.
data InputInbox key item = InputInbox
    { initialIds :: [key]
    , loadPending :: IO [(key, item)]
    , inboxState :: MVar (InboxState key item)
    }

data InboxState key item = InboxState
    { accepted :: [(key, item)]
    , outstanding :: [(key, item)]
    }

-- | Initial identities are already present in the activation's first input.
-- The loader must return unique, stable identities in the desired input order.
-- It runs under the inbox lock and must not call back into this inbox.
newInputInbox :: [key] -> IO [(key, item)] -> IO (InputInbox key item)
newInputInbox initialIds loadPending = do
    inboxState <- newMVar (InboxState [] [])
    pure InputInbox { initialIds, loadPending, inboxState }

readInputInbox :: Ord key => InputInbox key item -> IO [item]
readInputInbox inbox =
    modifyMVar inbox.inboxState \state -> do
        rows <- inbox.loadPending
        let seen = Set.fromList
                (inbox.initialIds <> map fst state.accepted <> map fst state.outstanding)
            extended = state.outstanding
                <> filter (\(key, _) -> Set.notMember key seen) rows
        pure (state { outstanding = extended }, map snd extended)

acknowledgeInputInbox :: InputInbox key item -> Int -> IO ()
acknowledgeInputInbox inbox count =
    modifyMVar_ inbox.inboxState \state ->
        if count < 0 || count > length state.outstanding
            then fail "InputInbox: acknowledgement exceeds the frozen batch"
            else do
                let (committed, remaining) = splitAt count state.outstanding
                pure state
                    { accepted = state.accepted <> committed
                    , outstanding = remaining
                    }

-- | Re-present accepted steering when reconstructing a failed activation from
-- its original history boundary. Outstanding inputs are returned by read again.
acceptedInputInboxItems :: InputInbox key item -> IO [item]
acceptedInputInboxItems inbox = map snd . (.accepted) <$> readMVar inbox.inboxState

acceptedSteeringIds :: InputInbox key item -> IO [key]
acceptedSteeringIds inbox = map fst . (.accepted) <$> readMVar inbox.inboxState

acceptedInputInboxIds :: InputInbox key item -> IO [key]
acceptedInputInboxIds inbox = do
    state <- readMVar inbox.inboxState
    pure (inbox.initialIds <> map fst state.accepted)

-- | Freeze this before submitting the model request. Publishing another row
-- concurrently cannot make that row part of this successful request's coverage.
submittedInputInboxIds :: InputInbox key item -> IO [key]
submittedInputInboxIds inbox = do
    state <- readMVar inbox.inboxState
    pure (inbox.initialIds <> map fst state.accepted <> map fst state.outstanding)

-- | The unaccepted batch as of the last read: the steering of the request
-- that is about to be submitted.
outstandingInputInboxIds :: InputInbox key item -> IO [key]
outstandingInputInboxIds inbox = map fst . (.outstanding) <$> readMVar inbox.inboxState

-- | Call only after successful delivery. Failure leaves all durable inputs
-- pending, allowing a new activation to recover after process termination.
-- The callback owns persistence and must support repeated completion attempts.
completeInputInbox :: InputInbox key item -> ([key] -> IO ()) -> IO ()
completeInputInbox inbox persistCompletion =
    acceptedInputInboxIds inbox >>= persistCompletion
