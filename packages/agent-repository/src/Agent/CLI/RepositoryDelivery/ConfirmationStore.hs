module Agent.CLI.RepositoryDelivery.ConfirmationStore
    ( ConfirmationStore
    , closeConfirmationStore
    , confirmationCount
    , insertConfirmation
    , newConfirmationStore
    , newConfirmationStoreWithExpiryWait
    , takeConfirmation
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newEmptyMVar
    , newMVar
    , putMVar
    , readMVar
    , takeMVar
    )
import Control.Exception.Safe
    ( bracket
    , bracketOnError
    , finally
    )
import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Unique (Unique, newUnique)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

data ConfirmationStore value = ConfirmationStore
    { confirmationEntries :: !(MVar (Map.Map Text (ConfirmationEntry value)))
    , confirmationWaitUntil :: !(Word64 -> IO ())
    }

data ConfirmationEntry value = ConfirmationEntry
    { confirmationDeadline :: !Word64
    , confirmationValue :: !value
    , confirmationExpiryOwner :: !Unique
    , confirmationExpiryWorker :: !(Async ())
    }

newConfirmationStore :: IO (ConfirmationStore value)
newConfirmationStore =
    newConfirmationStoreWithExpiryWait waitUntil

-- | The injectable wait keeps expiry behavior deterministic in tests. Production
-- callers should use 'newConfirmationStore'.
newConfirmationStoreWithExpiryWait
    :: (Word64 -> IO ())
    -> IO (ConfirmationStore value)
newConfirmationStoreWithExpiryWait confirmationWaitUntil = do
    confirmationEntries <- newMVar Map.empty
    pure ConfirmationStore{..}

insertConfirmation
    :: ConfirmationStore value
    -> Int
    -> Text
    -> Word64
    -> value
    -> IO Bool
insertConfirmation store capacity token deadline value = do
    published <- newEmptyMVar
    owner <- newUnique
    bracketOnError
        (asyncWithUnmask \restore ->
            restore do
                takeMVar published
                store.confirmationWaitUntil deadline
                removeOwned store token owner)
        (\worker ->
            removeOwned store token owner
                `finally` cancel worker)
        (\worker -> do
            monotonicNow <- getMonotonicTimeNSec
            (inserted, expired) <-
                modifyMVar store.confirmationEntries \entries ->
                    let (active, expiredEntries) =
                            Map.partition
                                (\entry ->
                                    entry.confirmationDeadline > monotonicNow)
                                entries
                    in if Map.size active >= capacity
                        || Map.member token active
                        then
                            pure
                                ( active
                                , (False, Map.elems expiredEntries)
                                )
                        else
                            pure
                                ( Map.insert
                                    token
                                    ConfirmationEntry
                                        { confirmationDeadline = deadline
                                        , confirmationValue = value
                                        , confirmationExpiryOwner = owner
                                        , confirmationExpiryWorker = worker
                                        }
                                    active
                                , (True, Map.elems expiredEntries)
                                )
            mapM_
                (cancel . (.confirmationExpiryWorker))
                expired
            if inserted
                then putMVar published ()
                else cancel worker
            pure inserted)

takeConfirmation
    :: ConfirmationStore value
    -> Text
    -> Word64
    -> IO (Maybe value)
takeConfirmation store token monotonicNow =
    bracket
        (modifyMVar store.confirmationEntries \entries ->
            let (entry, remaining) = Map.updateLookupWithKey
                    (\_ _ -> Nothing)
                    token
                    entries
            in pure (remaining, entry))
        (mapM_ (\entry -> cancel entry.confirmationExpiryWorker))
        (\case
            Just entry
                | entry.confirmationDeadline > monotonicNow ->
                    pure (Just entry.confirmationValue)
            _ -> pure Nothing)

confirmationCount :: ConfirmationStore value -> IO Int
confirmationCount store =
    Map.size <$> readMVar store.confirmationEntries

closeConfirmationStore :: ConfirmationStore value -> IO ()
closeConfirmationStore store = do
    entries <-
        modifyMVar store.confirmationEntries \active ->
            pure (Map.empty, Map.elems active)
    mapM_ (cancel . (.confirmationExpiryWorker)) entries

removeOwned
    :: ConfirmationStore value
    -> Text
    -> Unique
    -> IO ()
removeOwned store token owner =
    modifyMVar_ store.confirmationEntries $
        pure . Map.update removeIfOwned token
  where
    removeIfOwned entry
        | entry.confirmationExpiryOwner == owner = Nothing
        | otherwise = Just entry

waitUntil :: Word64 -> IO ()
waitUntil deadline = do
    monotonicNow <- getMonotonicTimeNSec
    when (monotonicNow < deadline) do
        threadDelay (boundedMicros (deadline - monotonicNow))
        waitUntil deadline

boundedMicros :: Word64 -> Int
boundedMicros nanos =
    fromIntegral (min micros (fromIntegral (maxBound :: Int)))
  where
    micros =
        nanos `div` 1_000
            + if nanos `mod` 1_000 == 0 then 0 else 1
