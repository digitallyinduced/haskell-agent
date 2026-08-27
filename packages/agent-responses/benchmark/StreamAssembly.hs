module Main (main) where

import Agent.Error (ApiError(..))
import Agent.Responses.StreamAssembly
    ( ResponseFailure
    , StreamAssemblyConfig(..)
    , buildStreamResponseWithModel
    )
import Agent.Responses.Types
import Agent.TextBuffer
    ( appendTextBuffer
    , emptyTextBuffer
    , textBufferToText
    )
import Control.Exception (evaluate)
import Control.Monad (forM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntSet as IntSet
import Data.List (find, sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
    ( RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

data Workload
    = OldText
    | NewText
    | OldIndex
    | NewIndex
    | OldMixed
    | NewMixed
    | ProductionMixed
    | ProductionExplicit
    deriving (Eq)

data Item = Item
    { itemIndex :: !Int
    , itemId :: !Text
    , itemCallId :: !Text
    , itemType :: !Text
    , itemDone :: !Bool
    }

data Sample = Sample
    { wallMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    , resultChecksum :: !Int
    }

main :: IO ()
main = do
    statsEnabled <- getRTSStatsEnabled
    if statsEnabled then pure () else die "run with +RTS -T"
    getArgs >>= \case
        [modeArg, deltaCountArg, itemCountArg, sampleCountArg] -> do
            workload <- parseWorkload modeArg
            deltaCount <- parsePositive "delta count" deltaCountArg
            itemCount <- parsePositive "item count" itemCountArg
            sampleCount <- parsePositive "sample count" sampleCountArg
            samples <- forM [1 .. sampleCount] \sampleIndex -> do
                let chunks =
                        [ Text.pack
                            [ toEnum (97 + (sampleIndex + index) `mod` 26)
                            , toEnum (65 + index `mod` 26)
                            ]
                        | index <- [0 .. deltaCount - 1]
                        ]
                    items = makeItems sampleIndex itemCount
                    queries =
                        [ "id-" <> Text.pack (show
                            ((index * 37 + sampleIndex) `mod` itemCount))
                        | index <- [0 .. deltaCount - 1]
                        ]
                    productionEvents =
                        makeProductionEvents
                            (workload == ProductionExplicit)
                            sampleIndex
                            itemCount
                            deltaCount
                    -- Event constructors contain parsed response items and
                    -- JSON objects. Encoding here traverses all nested payloads
                    -- so parsing/input construction cannot leak into timing.
                    inputChecksum =
                        checksumBytes (Aeson.encode productionEvents)
                _ <- evaluate
                    (length chunks + length items + length queries
                        + length productionEvents
                        + sum (map Text.length chunks)
                        + inputChecksum)
                measure
                    (runWorkload workload chunks items queries productionEvents)
            let med = median samples
            printf "%s,%d,%d,%.3f,%.3f,%d,%d\n"
                modeArg
                deltaCount
                itemCount
                med.wallMillis
                med.cpuMillis
                med.allocatedBytes
                med.resultChecksum
        _ -> die
            "usage: stream-assembly-bench MODE DELTAS ITEMS SAMPLES\n\
            \modes: old-text, new-text, old-index, new-index, old-mixed, \
            \new-mixed, production-mixed, production-explicit"

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "old-text" -> pure OldText
    "new-text" -> pure NewText
    "old-index" -> pure OldIndex
    "new-index" -> pure NewIndex
    "old-mixed" -> pure OldMixed
    "new-mixed" -> pure NewMixed
    "production-mixed" -> pure ProductionMixed
    "production-explicit" -> pure ProductionExplicit
    other -> die ("unknown mode: " <> other)

parsePositive :: String -> String -> IO Int
parsePositive label raw =
    case reads raw of
        [(value, "")] | value > 0 -> pure value
        _ -> die ("invalid " <> label <> ": " <> raw)

makeItems :: Int -> Int -> [Item]
makeItems sampleIndex itemCount =
    [ Item
        { itemIndex = index
        , itemId = identity "id-" index
        , itemCallId = identity "call-" (index * 17)
        , itemType = if even index then "function_call" else "reasoning"
        , itemDone = index `mod` 5 == sampleIndex `mod` 5
        }
    | index <- [0 .. itemCount - 1]
    ]
  where
    -- Repeated identities exercise the duplicate/minimum-index contract.
    identity prefix index =
        prefix <> Text.pack (show (index `mod` max 1 (itemCount * 3 `div` 4)))

runWorkload
    :: Workload
    -> [Text]
    -> [Item]
    -> [Text]
    -> [ResponseStreamEvent]
    -> IO Int
runWorkload workload chunks items queries productionEvents =
    evaluate case workload of
        OldText -> oldText chunks
        NewText -> newText chunks
        OldIndex -> oldIndex items queries
        NewIndex -> newIndex items queries
        OldMixed -> oldText chunks + oldIndex items queries
        NewMixed -> newText chunks + newIndex items queries
        ProductionMixed -> productionChecksum productionEvents
        ProductionExplicit -> productionChecksum productionEvents

productionChecksum :: [ResponseStreamEvent] -> Int
productionChecksum events =
    case buildStreamResponseWithModel
            productionConfig (Just "benchmark-model") events of
        Left err -> error ("production assembly failed: " <> show err)
        Right response ->
            checksumBytes (Aeson.encode response)

productionConfig :: StreamAssemblyConfig
productionConfig = StreamAssemblyConfig
    { missingCompletionMessage = "benchmark stream did not complete"
    , classifyStreamError = const (ConnectionError "benchmark stream error")
    , classifyFailedResponse =
        constResponseFailure (ConnectionError "benchmark response failed")
    , incompleteAsFailure = False
    }
  where
    constResponseFailure :: ApiError -> ResponseFailure -> ApiError
    constResponseFailure = const

makeProductionEvents :: Bool -> Int -> Int -> Int -> [ResponseStreamEvent]
makeProductionEvents explicitIndexes sampleIndex itemCount deltaCount =
    createdEvent
        : addedEvents
        <> deltaEvents
        <> [completedEvent]
  where
    outputCount = itemCount
    addedEvents = map makeAdded [0 .. outputCount - 1]
    deltaEvents =
        [ makeDelta deltaIndex (deltaIndex `mod` outputCount)
        | deltaIndex <- [0 .. deltaCount - 1]
        ]
    createdEvent = ResponseCreatedEvent
        (Aeson.object ["id" Aeson..= responseId])
        Nothing
        KeyMap.empty
    completedEvent = ResponseCompletedEvent
        (Aeson.object ["id" Aeson..= responseId])
        Nothing
        KeyMap.empty
    responseId = "bench-response-" <> Text.pack (show sampleIndex)

    makeAdded outputIndex =
        ResponseOutputItemAddedEvent
            (parseResponseItem case outputIndex `mod` 3 of
                0 -> Aeson.object
                    [ "type" Aeson..= ("function_call" :: Text)
                    , "id" Aeson..= itemIdentity outputIndex
                    , "call_id" Aeson..= callIdentity outputIndex
                    , "name" Aeson..= ("function" :: Text)
                    , "arguments" Aeson..= ("" :: Text)
                    ]
                1 -> Aeson.object
                    [ "type" Aeson..= ("custom_tool_call" :: Text)
                    , "id" Aeson..= itemIdentity outputIndex
                    , "call_id" Aeson..= callIdentity outputIndex
                    , "name" Aeson..= ("custom" :: Text)
                    , "input" Aeson..= ("" :: Text)
                    ]
                _ -> Aeson.object
                    [ "type" Aeson..= ("reasoning" :: Text)
                    , "id" Aeson..= itemIdentity outputIndex
                    , "summary" Aeson..= ([] :: [Aeson.Value])
                    ])
            (Just outputIndex)
            Nothing
            KeyMap.empty

    makeDelta deltaIndex outputIndex =
        case outputIndex `mod` 3 of
            0 -> ResponseFunctionCallArgumentsDeltaEvent
                (Just chunk)
                (Just (itemIdentity outputIndex))
                explicitIndex
                Nothing
                KeyMap.empty
            1 -> ResponseCustomToolInputDeltaEvent
                (Just chunk)
                (Just (itemIdentity outputIndex))
                (Just (callIdentity outputIndex))
                explicitIndex
                Nothing
                KeyMap.empty
            _ -> OtherResponseStreamEvent
                EventReasoningSummaryTextDelta
                Nothing
                (KeyMap.fromList
                    [ ("item_id", Aeson.String (itemIdentity outputIndex))
                    , ("summary_index", Aeson.toJSON (0 :: Int))
                    , ("delta", Aeson.String chunk)
                    ]
                    <> maybe KeyMap.empty
                        (KeyMap.singleton "output_index" . Aeson.toJSON)
                        explicitIndex)
      where
        explicitIndex =
            if explicitIndexes then Just outputIndex else Nothing
        chunk =
            Text.pack
                [ toEnum (97 + (sampleIndex + deltaIndex) `mod` 26)
                , toEnum (65 + deltaIndex `mod` 26)
                ]

    itemIdentity outputIndex =
        "item-" <> Text.pack (show outputIndex)
    callIdentity outputIndex =
        "call-" <> Text.pack (show outputIndex)

parseResponseItem :: Aeson.Value -> ResponseItem
parseResponseItem value =
    case Aeson.fromJSON value of
        Aeson.Success item -> item
        Aeson.Error err -> error ("invalid benchmark response item: " <> err)

oldText :: [Text] -> Int
oldText =
    Text.foldl' checksumStep 5381 . foldl' (<>) ""

newText :: [Text] -> Int
newText =
    Text.foldl' checksumStep 5381
        . textBufferToText
        . foldl' (flip appendTextBuffer) emptyTextBuffer

oldIndex :: [Item] -> [Text] -> Int
oldIndex items =
    foldl'
        (\checksum wanted ->
            checksum
                + maybe (-1) (.itemIndex)
                    (find (\item ->
                        item.itemId == wanted || item.itemCallId == wanted) items)
                + maybe (-1) (.itemIndex)
                    (find (\item ->
                        not item.itemDone
                            && item.itemType == queryType wanted) items))
        0

newIndex :: [Item] -> [Text] -> Int
newIndex items =
    let identityIndex = foldl' addIdentities Map.empty items
        pendingIndex = foldl' addPending Map.empty items
    in foldl'
        (\checksum wanted ->
            checksum
                + maybe (-1) minimumIndex (Map.lookup wanted identityIndex)
                + maybe (-1) minimumIndex
                    (Map.lookup (queryType wanted) pendingIndex))
        0
  where
    addIdentities indexes item =
        add item.itemCallId item.itemIndex
            (add item.itemId item.itemIndex indexes)
    addPending indexes item
        | item.itemDone = indexes
        | otherwise = add item.itemType item.itemIndex indexes
    add key index =
        Map.insertWith IntSet.union key (IntSet.singleton index)
    minimumIndex = fst . IntSet.deleteFindMin

queryType :: Text -> Text
queryType wanted
    | Text.length wanted `mod` 2 == 0 = "function_call"
    | otherwise = "reasoning"

checksumStep :: Int -> Char -> Int
checksumStep checksum character =
    checksum * 33 + fromEnum character

checksumBytes :: LBS.ByteString -> Int
checksumBytes =
    LBS.foldl'
        (\checksum byte -> checksum * 33 + fromIntegral byte)
        5381

measure :: IO Int -> IO Sample
measure action = do
    performGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeWall <- getMonotonicTimeNSec
    result <- action
    _ <- evaluate result
    afterWall <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    performGC
    afterStats <- getRTSStats
    pure Sample
        { wallMillis = fromIntegral (afterWall - beforeWall) / 1.0e6
        , cpuMillis = fromIntegral (afterCpu - beforeCpu) / 1.0e9
        , allocatedBytes =
            fromIntegral
                (afterStats.allocated_bytes - beforeStats.allocated_bytes)
        , resultChecksum = result
        }

median :: [Sample] -> Sample
median samples =
    Sample
        { wallMillis = middle (sort (map (.wallMillis) samples))
        , cpuMillis = middle (sort (map (.cpuMillis) samples))
        , allocatedBytes = middle (sort (map (.allocatedBytes) samples))
        , resultChecksum =
            foldl'
                (\checksum sample ->
                    checksum * 33 + sample.resultChecksum)
                5381
                samples
        }
  where
    middle values = values !! (length values `div` 2)
