module Agent.CLI.NativeAgents
    ( NativeAgentView(..)
    , NativeAgentStore
    , emptyNativeAgentStore
    , applyNativeAgentEvent
    , nativeAgentEntries
    , nativeAgentLookup
    , nativeAgentTargets
    , nativeAgentTranscript
    , nativeAgentConversation
    , nativeAgentStoreBytes
    , nativeAgentStoreSize
    , nativeAgentMaxEntries
    , nativeAgentPreviewBytes
    , nativeAgentAggregateBytes
    , restoreNativeAgents
    , setNativeAgentSelection
    ) where

import Agent.CLI.AgentViewport (AgentEntry(..), AgentTarget(..))
import Agent.Json (RawJson, rawJsonBytes)
import Agent.Json.Decode qualified as Hermes
import Agent.Loop (LoopEvent(..), NativeAgentStatus(..))
import Agent.Responses.Types
    ( FunctionCall(..)
    , FunctionCallOutput(..)
    , ItemStatus(..)
    , ResponseItem(..)
    )
import Agent.TUI.Model
    ( BlockState(..)
    , UiEvent(..)
    , UiState
    , initialUiState
    , reduceUi
    )
import Data.Foldable (toList)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq((:<|), (:|>)))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)

-- These are logical, conservative payload budgets. A Text character is
-- charged as four bytes so non-ASCII output cannot evade the cap without an
-- encode/copy on every streamed delta.
nativeAgentMaxEntries, nativeAgentPreviewBytes, nativeAgentAggregateBytes :: Int
nativeAgentMaxEntries = 128
nativeAgentPreviewBytes = 16 * 1024
nativeAgentAggregateBytes = 16 * 1024 * 1024

nativeAgentSelectedBytes :: Int
nativeAgentSelectedBytes = 8 * 1024 * 1024

data NativeOutput = NativeOutput
    { outputChunks :: !(Seq Text)
    , outputBytes :: !Int
    , outputOmitted :: !Bool
    }
    deriving (Eq, Show)

data NativeAgentOrigin = NativeAgentLive | NativeAgentCanonical
    deriving (Eq, Show)

-- | Canonical retained state for one provider-managed agent. The streamed
-- body is held once as append-only chunks; no UiState or duplicate transcript
-- is retained for every tree row.
data NativeAgentView = NativeAgentView
    { nativeAgentId :: !Text
    , nativeAgentParent :: !(Maybe Text)
    , nativeAgentLabel :: !Text
    , nativeAgentModel :: !(Maybe Text)
    , nativeAgentStatus :: !Text
    , nativeAgentOutput :: !NativeOutput
    , nativeAgentTerminal :: !(Maybe BlockState)
    , nativeAgentOrigin :: !NativeAgentOrigin
    }
    deriving (Eq, Show)

data NativeAgentStore = NativeAgentStore
    { storeAgents :: !(Map.Map Text NativeAgentView)
    -- Oldest first. Lifecycle and output events touch an entry, making
    -- eviction deterministic and keeping the most recently active rows.
    , storeOrder :: !(Seq Text)
    , storeSelected :: !(Maybe Text)
    , storeBytes :: !Int
    }
    deriving (Eq, Show)

emptyNativeAgentStore :: NativeAgentStore
emptyNativeAgentStore = NativeAgentStore Map.empty Seq.empty Nothing 0

nativeAgentStoreSize :: NativeAgentStore -> Int
nativeAgentStoreSize = Map.size . (.storeAgents)

nativeAgentStoreBytes :: NativeAgentStore -> Int
nativeAgentStoreBytes = (.storeBytes)

nativeAgentLookup :: Text -> NativeAgentStore -> Maybe NativeAgentView
nativeAgentLookup identifier = Map.lookup identifier . (.storeAgents)

nativeAgentTargets :: NativeAgentStore -> [AgentTarget]
nativeAgentTargets =
    map (AgentNative . (.nativeAgentId)) . Map.elems . (.storeAgents)

setNativeAgentSelection :: Maybe Text -> NativeAgentStore -> NativeAgentStore
setNativeAgentSelection selected store =
    normalizeStore store{storeSelected = selected}

applyNativeAgentEvent :: LoopEvent -> NativeAgentStore -> NativeAgentStore
applyNativeAgentEvent event current =
    normalizeStore $
        case event of
            TurnStarted ->
                settleRunningNativeAgents NativeAgentCancelled current
            ResponseRestarted _ ->
                settleRunningNativeAgents NativeAgentCancelled current
            ResponseAttemptDiscarded ->
                settleRunningNativeAgents NativeAgentCancelled current
            NativeAgentStarted identifier parent label model ->
                alterTouched identifier
                    (\case
                        Nothing ->
                            newNativeAgentView
                                NativeAgentLive identifier parent label model
                        Just view ->
                            view
                                { nativeAgentParent = parent
                                , nativeAgentLabel = label
                                , nativeAgentModel = model
                                , nativeAgentStatus = "running"
                                , nativeAgentTerminal = Nothing
                                , nativeAgentOrigin = NativeAgentLive
                                })
                    current
            NativeAgentOutput identifier output ->
                alterTouched identifier
                    (\maybeView ->
                        let view = fromMaybe
                                (newNativeAgentView NativeAgentLive
                                    identifier Nothing identifier Nothing)
                                maybeView
                        in view
                            { nativeAgentOutput =
                                appendOutput nativeAgentSelectedBytes output
                                    view.nativeAgentOutput
                            , nativeAgentOrigin = NativeAgentLive
                            })
                    current
            NativeAgentFinished identifier status ->
                alterTouched identifier
                    (\maybeView ->
                        (fromMaybe
                            (newNativeAgentView NativeAgentLive
                                identifier Nothing identifier Nothing)
                            maybeView)
                            { nativeAgentStatus = nativeAgentStatusText status
                            , nativeAgentTerminal =
                                Just (nativeAgentBlockState status)
                            , nativeAgentOrigin = NativeAgentLive
                            })
                    current
            _ -> current

-- | Materialize presentation state only for the selected row. Other rows get
-- a small tail preview and the shared empty UiState.
nativeAgentEntries :: AgentTarget -> NativeAgentStore -> [AgentEntry]
nativeAgentEntries selected store =
    map (nativeAgentEntry selected store.storeAgents)
        (Map.elems store.storeAgents)

-- | Reconstruct only the newest bounded set of completed top-level
-- Claude-native agents from paired canonical call/output records. Live rows
-- absent from the canonical transcript are retained, but canonical history is
-- rebuilt rather than unioned with every row ever observed.
restoreNativeAgents
    :: AgentTarget
    -> [ResponseItem]
    -> NativeAgentStore
    -> NativeAgentStore
restoreNativeAgents selected items current =
    normalizeStore NativeAgentStore
        { storeAgents = Map.union liveUnpersisted canonicalAgents
        , storeOrder =
            canonicalOrder
                <> Seq.filter (`Map.member` liveUnpersisted) current.storeOrder
        , storeSelected = selectedIdentifier
        , storeBytes =
            retainedBytes (Map.union liveUnpersisted canonicalAgents)
        }
  where
    selectedIdentifier = case selected of
        AgentNative identifier -> Just identifier
        _ -> Nothing
    (canonicalAgents, canonicalOrder) =
        canonicalNativeAgents selectedIdentifier items
    liveUnpersisted =
        Map.restrictKeys current.storeAgents liveUnpersistedIds
    liveUnpersistedIds = closeParents initialLiveIds
    initialLiveIds =
        Map.keysSet $
            Map.filterWithKey
                (\identifier view ->
                    view.nativeAgentOrigin == NativeAgentLive
                        && Map.notMember identifier canonicalAgents)
                current.storeAgents
    closeParents ids =
        let withParents =
                Set.foldl'
                    (\found identifier ->
                        case Map.lookup identifier current.storeAgents
                                >>= (.nativeAgentParent) of
                            Nothing -> found
                            Just parent -> Set.insert parent found)
                    ids
                    ids
        in if Set.size withParents == Set.size ids
            then ids
            else closeParents withParents

nativeAgentTranscript :: NativeAgentView -> [Text]
nativeAgentTranscript = outputTexts . (.nativeAgentOutput)

nativeAgentConversation :: NativeAgentView -> UiState
nativeAgentConversation view =
    let started = reduceUi (UiLoop TurnStarted) initialUiState
        body = outputText view.nativeAgentOutput
        withBody
            | Text.null body = started
            | otherwise = reduceUi (UiLoop (TextDelta body)) started
    in maybe withBody
        (\terminal -> reduceUi (UiTurnEnded terminal) withBody)
        view.nativeAgentTerminal

canonicalNativeAgents
    :: Maybe Text
    -> [ResponseItem]
    -> (Map.Map Text NativeAgentView, Seq Text)
canonicalNativeAgents selected =
    go Map.empty Map.empty Seq.empty . reverse
  where
    go outputs agents order remaining
        | Map.size agents >= nativeAgentMaxEntries
        , maybe True (`Map.member` agents) selected =
            (agents, order)
        | otherwise =
            case remaining of
                [] -> (agents, order)
                item : rest -> case item of
                    FunctionCallOutputItem output
                        | output.provider == Just "claude-code"
                        , Map.member output.callId outputs
                            || Map.size outputs < nativeAgentMaxEntries
                            || selected == Just output.callId ->
                            go
                                (Map.insert output.callId output outputs)
                                agents
                                order
                                rest
                    FunctionCallItem call
                        | isClaudeNativeCall call
                        , Just output <- Map.lookup call.callId outputs ->
                            let view = restoredView
                                    (selected == Just output.callId)
                                    call
                                    output
                                (nextAgents, nextOrder) =
                                    insertCanonical call.callId view agents order
                            in go
                                (Map.delete call.callId outputs)
                                nextAgents
                                nextOrder
                                rest
                    _ -> go outputs agents order rest

    insertCanonical identifier view agents order
        | Map.size agents < nativeAgentMaxEntries =
            (Map.insert identifier view agents, identifier :<| order)
        | selected == Just identifier =
            case oldestUnselected order of
                Nothing -> (agents, order)
                Just oldest ->
                    ( Map.insert identifier view (Map.delete oldest agents)
                    , identifier :<| Seq.filter (/= oldest) order
                    )
        | otherwise = (agents, order)

    oldestUnselected =
        foldr
            (\identifier found ->
                if Just identifier == selected
                    then found
                    else Just identifier)
            Nothing

restoredView :: Bool -> FunctionCall -> FunctionCallOutput -> NativeAgentView
restoredView selected call output =
    let outputBody
            | selected =
                renderOutputBounded nativeAgentSelectedBytes output.output
            | otherwise = renderOutputPreview output.output
        terminal = case output.status of
            Just ItemIncomplete -> BlockFailed
            Just (ItemStatusUnknown status)
                | Text.toLower status `elem`
                    ["failed", "error", "cancelled", "canceled"] ->
                        BlockFailed
            _ -> BlockComplete
        started = newNativeAgentView
            NativeAgentCanonical
            call.callId
            Nothing
            (fromMaybe call.name
                (argumentText "description" call.arguments))
            (argumentText "model" call.arguments)
    in started
        { nativeAgentStatus =
            if terminal == BlockFailed then "error" else "done"
        , nativeAgentOutput =
            appendOutput nativeAgentSelectedBytes outputBody
                started.nativeAgentOutput
        , nativeAgentTerminal = Just terminal
        }

isClaudeNativeCall :: FunctionCall -> Bool
isClaudeNativeCall call =
    Text.toLower call.name `elem` ["agent", "task"]
        && call.provider == Just "claude-code"

argumentText :: Text -> Text -> Maybe Text
argumentText key raw = do
    value <- either (const Nothing) Just $
        Hermes.decodeEither
            (Hermes.object (Hermes.atKey key Hermes.text))
            (TextEncoding.encodeUtf8 raw)
    let stripped = Text.strip value
    if Text.null stripped then Nothing else Just stripped

renderOutput :: RawJson -> Text
renderOutput value =
    case Hermes.decodeEither Hermes.text (rawJsonBytes value) of
        Right text -> text
        Left _ ->
            TextEncoding.decodeUtf8With lenientDecode
                (rawJsonBytes value)

renderOutputPreview :: RawJson -> Text
renderOutputPreview =
    renderOutputBounded nativeAgentPreviewBytes

renderOutputBounded :: Int -> RawJson -> Text
renderOutputBounded budget value
    | BS.length bytes <= retainedRawBytes =
        renderOutput value
    | otherwise =
        "[older native-agent output omitted]\n"
            <> Text.dropAround (== '"')
                (TextEncoding.decodeUtf8With lenientDecode
                    (BS.drop
                        (BS.length bytes - retainedRawBytes)
                        bytes))
  where
    bytes = rawJsonBytes value
    retainedRawBytes = max 0 (budget `div` 4)

settleRunningNativeAgents
    :: NativeAgentStatus
    -> NativeAgentStore
    -> NativeAgentStore
settleRunningNativeAgents status store =
    store
        { storeAgents =
            Map.map
                (\view ->
                    if view.nativeAgentStatus /= "running"
                        then view
                        else view
                            { nativeAgentStatus = nativeAgentStatusText status
                            , nativeAgentTerminal =
                                Just (nativeAgentBlockState status)
                            })
                store.storeAgents
        }

nativeAgentBlockState :: NativeAgentStatus -> BlockState
nativeAgentBlockState = \case
    NativeAgentRunning -> BlockRunning
    NativeAgentCompleted -> BlockComplete
    NativeAgentFailed -> BlockFailed
    NativeAgentCancelled -> BlockCancelled

newNativeAgentView
    :: NativeAgentOrigin
    -> Text
    -> Maybe Text
    -> Text
    -> Maybe Text
    -> NativeAgentView
newNativeAgentView origin identifier parent label model =
    NativeAgentView
        { nativeAgentId = identifier
        , nativeAgentParent = parent
        , nativeAgentLabel = label
        , nativeAgentModel = model
        , nativeAgentStatus = "running"
        , nativeAgentOutput = NativeOutput Seq.empty 0 False
        , nativeAgentTerminal = Nothing
        , nativeAgentOrigin = origin
        }

nativeAgentStatusText :: NativeAgentStatus -> Text
nativeAgentStatusText = \case
    NativeAgentRunning -> "running"
    NativeAgentCompleted -> "done"
    NativeAgentFailed -> "error"
    NativeAgentCancelled -> "cancelled"

nativeAgentEntry
    :: AgentTarget
    -> Map.Map Text NativeAgentView
    -> NativeAgentView
    -> AgentEntry
nativeAgentEntry selected agents view =
    let isSelected = selected == AgentNative view.nativeAgentId
    in AgentEntry
        { agentTarget = AgentNative view.nativeAgentId
        , agentPath = nativeAgentPath agents Set.empty view
        , agentStatus = view.nativeAgentStatus
        , agentModel = view.nativeAgentModel
        , agentSteps = []
        , agentTranscript =
            if isSelected
                then nativeAgentTranscript view
                else outputTexts
                    (trimOutputTo nativeAgentPreviewBytes
                        view.nativeAgentOutput)
        , agentConversation =
            if isSelected
                then nativeAgentConversation view
                else initialUiState
        }

nativeAgentPath
    :: Map.Map Text NativeAgentView
    -> Set.Set Text
    -> NativeAgentView
    -> Text
nativeAgentPath agents visited view
    | Set.member view.nativeAgentId visited =
        "/native/" <> nativeAgentPathSegment view
    | otherwise =
        case view.nativeAgentParent >>= (`Map.lookup` agents) of
            Nothing -> "/native/" <> nativeAgentPathSegment view
            Just parent ->
                nativeAgentPath
                    agents
                    (Set.insert view.nativeAgentId visited)
                    parent
                    <> "/"
                    <> nativeAgentPathSegment view

nativeAgentPathSegment :: NativeAgentView -> Text
nativeAgentPathSegment view =
    let cleaned =
            Text.unwords
                (Text.words
                    (Text.map
                        (\char -> if char == '/' then '-' else char)
                        view.nativeAgentLabel))
    in if Text.null cleaned then view.nativeAgentId else cleaned

alterTouched
    :: Text
    -> (Maybe NativeAgentView -> NativeAgentView)
    -> NativeAgentStore
    -> NativeAgentStore
alterTouched identifier update store =
    let oldBytes = maybe 0
            ((.outputBytes) . (.nativeAgentOutput))
            (Map.lookup identifier store.storeAgents)
        next = update (Map.lookup identifier store.storeAgents)
        nextBytes = next.nativeAgentOutput.outputBytes
    in store
        { storeAgents = Map.insert identifier next store.storeAgents
        , storeOrder = touchOrder identifier store.storeOrder
        , storeBytes =
            saturatingNativeAdd
                (max 0 (store.storeBytes - oldBytes))
                nextBytes
        }

touchOrder :: Text -> Seq Text -> Seq Text
touchOrder identifier order =
    Seq.filter (/= identifier) order :|> identifier

appendOutput :: Int -> Text -> NativeOutput -> NativeOutput
appendOutput budget chunk output
    | Text.null chunk = output
    | otherwise =
        let appended =
                trimOutputTo budget
                    (output
                        { outputChunks =
                            if Text.null retainedChunk
                                then output.outputChunks
                                else output.outputChunks :|> retainedChunk
                        , outputBytes =
                            saturatingNativeAdd
                                output.outputBytes
                                (textBytes retainedChunk)
                        })
        in if chunkTruncated
            then appended { outputOmitted = True }
            else appended
  where
    -- Stream decoders may hand us a slice of a much larger event buffer.
    -- Copy only the tail which can survive the budget; copying a giant chunk
    -- in full before immediately trimming it would recreate the memory spike.
    chunkTruncated = textBytes chunk > max 0 budget
    retainedChunk =
        Text.copy $
            if chunkTruncated
                then Text.takeEnd (max 0 budget `div` 4) chunk
                else chunk

trimOutputTo :: Int -> NativeOutput -> NativeOutput
trimOutputTo budget output
    | output.outputBytes <= budget = output
    | otherwise =
        let (chunks, bytes) =
                dropOldestTo budget
                    output.outputBytes
                    output.outputChunks
        in NativeOutput
            { outputChunks = chunks
            , outputBytes = bytes
            , outputOmitted = True
            }

dropOldestTo :: Int -> Int -> Seq Text -> (Seq Text, Int)
dropOldestTo budget = go
  where
    go bytes chunks
        | bytes <= budget = (chunks, bytes)
        | otherwise =
            case Seq.viewl chunks of
                Seq.EmptyL -> (Seq.empty, 0)
                chunk Seq.:< rest ->
                    let chunkBytes = textBytes chunk
                        excess = bytes - budget
                    in if chunkBytes <= excess
                        then go (bytes - chunkBytes) rest
                        else
                            let dropChars = (excess + 3) `div` 4
                                -- A strict Text slice keeps the complete
                                -- original backing array alive. Copy the
                                -- retained tail so the cap is a real heap cap.
                                suffix = Text.copy (Text.drop dropChars chunk)
                                suffixBytes = textBytes suffix
                            in ( if Text.null suffix
                                    then rest
                                    else suffix :<| rest
                               , bytes - chunkBytes + suffixBytes
                               )

outputTexts :: NativeOutput -> [Text]
outputTexts output =
    (if output.outputOmitted then ["[older native-agent output omitted]\n"] else [])
        <> toList output.outputChunks

outputText :: NativeOutput -> Text
outputText = Text.concat . outputTexts

textBytes :: Text -> Int
textBytes text =
    let chars = Text.length text
    in if chars > maxBound `div` 4 then maxBound else chars * 4

saturatingNativeAdd :: Int -> Int -> Int
saturatingNativeAdd left right
    | right > maxBound - left = maxBound
    | otherwise = left + right

normalizeStore :: NativeAgentStore -> NativeAgentStore
normalizeStore store =
    compactOutputs (pruneEntries store)

pruneEntries :: NativeAgentStore -> NativeAgentStore
pruneEntries store
    | Map.size store.storeAgents <= nativeAgentMaxEntries = store
    | otherwise =
        case removableOldest of
            Nothing -> store
            Just identifier ->
                pruneEntries store
                    { storeAgents = Map.delete identifier store.storeAgents
                    , storeOrder = Seq.filter (/= identifier) store.storeOrder
                    , storeBytes =
                        store.storeBytes
                            - maybe 0
                                ((.outputBytes) . (.nativeAgentOutput))
                                (Map.lookup identifier store.storeAgents)
                    }
  where
    protected = protectedAgentIds store
    removableOldest =
        case oldestWhere (`Map.notMember` protected) of
            Just identifier -> Just identifier
            Nothing ->
                -- A provider can start more than the row limit without
                -- sending terminal events. Preserve the selected row, but
                -- still enforce the hard entry cap for all other rows.
                oldestWhere
                    (\identifier ->
                        Just identifier /= store.storeSelected)
    oldestWhere predicate =
        foldr
            (\identifier found ->
                if predicate identifier
                    then Just identifier
                    else found)
            Nothing
            store.storeOrder

protectedAgentIds :: NativeAgentStore -> Map.Map Text ()
protectedAgentIds store =
    Map.fromSet (const ()) (close initial)
  where
    initial =
        Set.fromList
            [ view.nativeAgentId
            | view <- Map.elems store.storeAgents
            , view.nativeAgentStatus == "running"
                || Just view.nativeAgentId == store.storeSelected
            ]
    close ids =
        let withParents =
                Set.foldl'
                    (\current identifier ->
                        case Map.lookup identifier store.storeAgents
                                >>= (.nativeAgentParent) of
                            Nothing -> current
                            Just parent -> Set.insert parent current)
                    ids
                    ids
        in if Set.size withParents == Set.size ids
            then ids
            else close withParents

compactOutputs :: NativeAgentStore -> NativeAgentStore
compactOutputs store
    | nativeAgentStoreBytes store <= nativeAgentAggregateBytes = store
    | otherwise =
        foldl' compact store candidates
  where
    candidates =
        [ identifier
        | identifier <- toList store.storeOrder
        , Just identifier /= store.storeSelected
        ]
    compact current identifier
        | nativeAgentStoreBytes current <= nativeAgentAggregateBytes = current
        | otherwise =
            current
                { storeAgents =
                    nextAgents
                , storeBytes = retainedBytes nextAgents
                }
      where
        nextAgents =
            Map.adjust
                (\view -> view
                    { nativeAgentOutput =
                        trimOutputTo nativeAgentPreviewBytes
                            view.nativeAgentOutput
                    })
                identifier
                current.storeAgents

retainedBytes :: Map.Map Text NativeAgentView -> Int
retainedBytes =
    Map.foldl'
        (\total view ->
            saturatingNativeAdd total view.nativeAgentOutput.outputBytes)
        0
