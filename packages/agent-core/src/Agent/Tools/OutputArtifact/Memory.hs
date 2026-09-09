{-# LANGUAGE BangPatterns #-}
module Agent.Tools.OutputArtifact.Memory
    ( OutputArtifactMemoryStore
    , MemoryOutputArtifact(..)
    , newOutputArtifactMemoryStore
    , insertMemoryOutputArtifact
    , lookupMemoryOutputArtifact
    , clearMemoryOutputArtifacts
    ) where

import qualified Data.ByteString as ByteString
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric (showHex)
import System.Entropy (getEntropy)

-- | A session-owned store shared with its delegated agents. Its accounting
-- charges a minimum entry cost as well as retained byte arrays, so empty
-- results cannot create an unbounded registry.
newtype OutputArtifactMemoryStore = OutputArtifactMemoryStore
    (IORef (Int, Map.Map Text MemoryOutputArtifact))

data MemoryOutputArtifact = MemoryOutputArtifact
    { memoryArtifactBytes :: !ByteString.ByteString
    , memoryArtifactObservedBytes :: !Int
    }

newOutputArtifactMemoryStore :: IO OutputArtifactMemoryStore
newOutputArtifactMemoryStore =
    OutputArtifactMemoryStore <$> newIORef (0, Map.empty)

insertMemoryOutputArtifact
    :: OutputArtifactMemoryStore
    -> Int
    -> ByteString.ByteString
    -> Int
    -> IO (Maybe Text)
insertMemoryOutputArtifact (OutputArtifactMemoryStore reference) budget bytes observed = do
    -- Host and sandbox processes share the handle namespace, not a Unique
    -- counter. Use OS entropy so independent processes cannot reuse counters.
    identifier <- getEntropy 16
    let handle = "output-memory-" <> Text.pack (concatMap hexByte (ByteString.unpack identifier))
        cost = ByteString.length bytes + 256
    atomicModifyIORef' reference \(used, entries) ->
        if cost > max 0 budget - used
            then ((used, entries), Nothing)
            else
                let !retained = MemoryOutputArtifact (ByteString.copy bytes) observed
                    !updatedEntries = Map.insert handle retained entries
                    !updatedBytes = used + cost
                in ((updatedBytes, updatedEntries), Just handle)
  where
    hexByte byte = case showHex byte "" of
        [digit] -> ['0', digit]
        digits -> digits

lookupMemoryOutputArtifact
    :: OutputArtifactMemoryStore -> Text -> IO (Maybe MemoryOutputArtifact)
lookupMemoryOutputArtifact (OutputArtifactMemoryStore reference) handle =
    Map.lookup handle . snd <$> readIORef reference

clearMemoryOutputArtifacts :: OutputArtifactMemoryStore -> IO ()
clearMemoryOutputArtifacts (OutputArtifactMemoryStore reference) =
    atomicModifyIORef' reference (const ((0, Map.empty), ()))
