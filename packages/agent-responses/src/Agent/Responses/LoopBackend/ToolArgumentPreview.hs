-- | Attempt-local tool identity tracking and bounded live argument previews.
-- Keep the state opaque so callers advance all identity maps, preview buffers
-- and activity counters together.
module Agent.Responses.LoopBackend.ToolArgumentPreview
    ( ToolArgumentStreamState
    , emptyToolArgumentStreamState
    , toolArgumentStreamStep
    , toolArgumentActivityChunkChars
    , runawayToolArgumentWarningChars
    ) where

import Agent.JsonText (jsonTextFieldPartial)
import Agent.Loop (LoopEvent(..))
import Agent.Responses.LoopBackend.Output
    ( namespacedToolName
    , responseItemToToolCall
    )
import Agent.Responses.Types
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , canonicalToolName
    , isComputerToolCallKind
    , setToolCallArguments
    )
import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- | Emit an updated argument-streaming activity after this many additional
-- streamed argument characters.
toolArgumentActivityChunkChars :: Int
toolArgumentActivityChunkChars = 8192

-- | Warn after every additional this many streamed argument characters in a
-- single response. The largest legitimate call observed in practice is well
-- under half of this; degenerate repetition loops run to the provider's
-- output-token cap (hundreds of thousands of characters).
runawayToolArgumentWarningChars :: Int
runawayToolArgumentWarningChars = 100000

data ToolArgumentStreamState = ToolArgumentStreamState
    { toolNamesById :: !(Map ToolStreamIdentity Text)
    , toolCallsById :: !(Map ToolStreamIdentity ToolCall)
    , toolNamesByOutputIndex :: !(IntMap Text)
    , toolCallsByOutputIndex :: !(IntMap ToolCall)
    , currentToolCall :: !(Maybe ToolCall)
    , shellPreviewsByCallId :: !(Map ToolPreviewKey Text)
    , rawPreviewsByCallId :: !(Map ToolPreviewKey RawArgumentPreview)
    , currentToolName :: !(Maybe Text)
    , streamedArgumentChars :: !Int
    , announcedArgumentChars :: !Int
    , warnedArgumentChars :: !Int
    }

data ToolStreamKind
    = FunctionToolStream
    | CustomToolStream
    deriving (Eq, Ord)

data ToolStreamIdentity
    = ToolStreamItemId !ToolStreamKind !Text
    | ToolStreamCallId !ToolStreamKind !Text
    deriving (Eq, Ord)

data ToolPreviewKey = ToolPreviewKey !ToolStreamKind !Text
    deriving (Eq, Ord)

data RawArgumentPreview = RawArgumentPreview
    { publishedRawArguments :: !Text
    , pendingRawArgumentChunks :: ![Text]
    , pendingRawArgumentChars :: !Int
    , retainedRawArgumentChars :: !Int
    }

emptyToolArgumentStreamState :: ToolArgumentStreamState
emptyToolArgumentStreamState = ToolArgumentStreamState
    { toolNamesById = Map.empty
    , toolCallsById = Map.empty
    , toolNamesByOutputIndex = IntMap.empty
    , toolCallsByOutputIndex = IntMap.empty
    , currentToolCall = Nothing
    , shellPreviewsByCallId = Map.empty
    , rawPreviewsByCallId = Map.empty
    , currentToolName = Nothing
    , streamedArgumentChars = 0
    , announcedArgumentChars = 0
    , warnedArgumentChars = 0
    }

toolArgumentStreamStep
    :: ResponseStreamEvent
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
toolArgumentStreamStep event state = case event of
    ResponseOutputItemAddedEvent
        { item = FunctionCallItem call, outputIndex } ->
        announceToolCall
            (responseItemToToolCall (FunctionCallItem call))
            (namespacedToolName call.namespace call.name)
            outputIndex
            ( maybeToList
                (ToolStreamItemId FunctionToolStream <$> call.itemId)
                <> [ToolStreamCallId FunctionToolStream call.callId]
            )
            state
    ResponseOutputItemAddedEvent
        { item = CustomToolCallItem call, outputIndex } ->
        announceToolCall
            (responseItemToToolCall (CustomToolCallItem call))
            (namespacedToolName call.namespace call.name)
            outputIndex
            ( maybeToList
                (ToolStreamItemId CustomToolStream <$> call.itemId)
                <> [ToolStreamCallId CustomToolStream call.callId]
            )
            state
    ResponseOutputItemDoneEvent
        { item = FunctionCallItem responseCall, outputIndex }
        | Just call <- responseItemToToolCall (FunctionCallItem responseCall) ->
            finishOutputItemToolCall
                outputIndex
                [ ToolStreamItemId FunctionToolStream <$> responseCall.itemId
                , Just
                    (ToolStreamCallId
                        FunctionToolStream
                        responseCall.callId)
                ]
                call
                state
    ResponseOutputItemDoneEvent
        { item = CustomToolCallItem responseCall, outputIndex }
        | Just call <- responseItemToToolCall
            (CustomToolCallItem responseCall) ->
            finishOutputItemToolCall
                outputIndex
                [ ToolStreamItemId CustomToolStream <$> responseCall.itemId
                , Just
                    (ToolStreamCallId
                        CustomToolStream
                        responseCall.callId)
                ]
                call
                state
    ResponseFunctionCallArgumentsDeltaEvent
        { delta = Just deltaText, streamItemId, streamOutputIndex } ->
        updateToolArguments
            streamOutputIndex
            [ToolStreamItemId FunctionToolStream <$> streamItemId]
            deltaText
            state
    ResponseFunctionCallArgumentsDoneEvent
        { arguments, streamItemId, streamOutputIndex } ->
            finishToolArguments
                streamOutputIndex
                [ToolStreamItemId FunctionToolStream <$> streamItemId]
                arguments
                state
    ResponseCustomToolInputDeltaEvent
        { delta = Just deltaText, streamItemId, streamCallId
        , streamOutputIndex } ->
            updateToolArguments
                streamOutputIndex
                [ ToolStreamItemId CustomToolStream <$> streamItemId
                , ToolStreamCallId CustomToolStream <$> streamCallId
                ]
                deltaText
                state
    ResponseCustomToolInputDoneEvent
        { inputText, streamItemId, streamCallId, streamOutputIndex } ->
            finishToolArguments
                streamOutputIndex
                [ ToolStreamItemId CustomToolStream <$> streamItemId
                , ToolStreamCallId CustomToolStream <$> streamCallId
                ]
                inputText
                state
    _ -> (state, [])

announceToolCall
    :: Maybe ToolCall
    -> Text
    -> Maybe Int
    -> [ToolStreamIdentity]
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
announceToolCall maybeCall name outputIndex identities state =
    ( state
        { toolNamesById =
            foldr (\identity -> Map.insert identity name)
                state.toolNamesById
                identities
        , toolCallsById =
            case maybeCall of
                Nothing -> state.toolCallsById
                Just call ->
                    foldr (\identity -> Map.insert identity call)
                        state.toolCallsById
                        identities
        , toolNamesByOutputIndex =
            maybe state.toolNamesByOutputIndex
                (\index -> IntMap.insert index name
                    state.toolNamesByOutputIndex)
                outputIndex
        , toolCallsByOutputIndex =
            case (outputIndex, maybeCall) of
                (Just index, Just call) ->
                    IntMap.insert index call state.toolCallsByOutputIndex
                _ -> state.toolCallsByOutputIndex
        , currentToolCall = maybeCall <|> state.currentToolCall
        , currentToolName = Just name
        }
    , [ ActivityUpdated (writingToolCallActivity name Nothing)
      | maybe True (not . supportsLiveArgumentPreview) maybeCall
      ]
    )

finishOutputItemToolCall
    :: Maybe Int
    -> [Maybe ToolStreamIdentity]
    -> ToolCall
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
finishOutputItemToolCall outputIndex identities call state =
    let previousCall = resolveToolCall outputIndex identities state
        canRecover =
            supportsLiveArgumentPreview call
                && maybe True supportsLiveArgumentPreview previousCall
        baseCall
            | canRecover
            , Text.null call.arguments
            , Just previous <- previousCall =
                withToolArguments call previous.arguments
            | otherwise = call
        (next, completedCall)
            | canRecover
            , Text.null call.arguments =
                let prefixChars
                        | isLiveShellTool call.name =
                            liveShellArgumentPrefixChars
                        | otherwise = liveRawArgumentPrefixChars
                    (finished, recovered, _) =
                        finishBufferedLiveCall
                            prefixChars
                            baseCall
                            Nothing
                            state
                in (finished, recovered)
            | otherwise = (trackUpdatedToolCall call state, call)
    in (next, [ToolUpdated completedCall])

resolveToolName
    :: Maybe Int
    -> [Maybe ToolStreamIdentity]
    -> ToolArgumentStreamState
    -> Text
resolveToolName outputIndex identities state =
    fromMaybe "tool" $
        resolveToolValue
            outputIndex
            identities
            state.toolNamesByOutputIndex
            state.toolNamesById
            state.currentToolName

resolveToolCall
    :: Maybe Int
    -> [Maybe ToolStreamIdentity]
    -> ToolArgumentStreamState
    -> Maybe ToolCall
resolveToolCall outputIndex identities state =
    resolveToolValue
        outputIndex
        identities
        state.toolCallsByOutputIndex
        state.toolCallsById
        state.currentToolCall

resolveToolValue
    :: Maybe Int
    -> [Maybe ToolStreamIdentity]
    -> IntMap value
    -> Map ToolStreamIdentity value
    -> Maybe value
    -> Maybe value
resolveToolValue outputIndex identities byOutputIndex byIdentity fallback =
    (outputIndex >>= (`IntMap.lookup` byOutputIndex))
        <|> firstJust
            [ Map.lookup identity byIdentity
            | Just identity <- identities
            ]
        <|> if hasLocator then Nothing else fallback
  where
    hasLocator = isJust outputIndex || any isJust identities
    firstJust = foldr (<|>) Nothing

-- Keep live previews useful without retaining unbounded repeated strict Text
-- values for runaway calls. Shell previews only need one command line, while
-- raw calls need enough source to show a representative diff or structured
-- argument preview.
liveShellArgumentPrefixChars :: Int
liveShellArgumentPrefixChars = 4096

liveRawArgumentPrefixChars :: Int
liveRawArgumentPrefixChars = 64 * 1024

-- Structured JSON calls normally reveal their useful path, pattern, or query
-- near the start. Publish that prefix eagerly, then batch tiny provider deltas.
-- apply_patch remains batched after its first fragment because patch bodies can
-- be large and are useful even when the first fragment already has a header.
liveStructuredArgumentEagerChars :: Int
liveStructuredArgumentEagerChars = 128

-- Shell commands remain eager while their useful prefix is forming, then use
-- smaller batches than source-shaped arguments so the command still feels
-- live without reparsing and repainting a long prefix for every tiny delta.
liveShellArgumentPublishChunkChars :: Int
liveShellArgumentPublishChunkChars = 64

liveRawArgumentPublishChunkChars :: Int
liveRawArgumentPublishChunkChars = 256

updateToolArguments
    :: Maybe Int
    -> [Maybe ToolStreamIdentity]
    -> Text
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
updateToolArguments outputIndex identities delta state =
    let name = resolveToolName outputIndex identities state
        maybeCall = resolveToolCall outputIndex identities state
        (withDraft, previewEvents) = case maybeCall of
            Just call
                | not (supportsLiveArgumentPreview call) ->
                    (state, [])
                | isLiveShellTool call.name ->
                    updateLiveShellCall call delta state
                | otherwise ->
                    updateLiveRawCall
                        (if isApplyPatchTool call.name
                            then 0
                            else liveStructuredArgumentEagerChars)
                        call
                        delta
                        state
            _ -> (state, [])
        (counted, activityEvents) =
            countToolArgumentChars
                name
                (maybe False supportsLiveArgumentPreview maybeCall)
                (Text.length delta)
                withDraft
    in (counted, previewEvents <> activityEvents)

finishToolArguments
    :: Maybe Int
    -> [Maybe ToolStreamIdentity]
    -> Maybe Text
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
finishToolArguments outputIndex identities completeArguments state =
    case resolveToolCall outputIndex identities state of
        Just call
            | not (supportsLiveArgumentPreview call) -> (state, [])
            | isLiveShellTool call.name ->
                finishLiveShellCall call completeArguments state
            | otherwise ->
                finishLiveRawCall call completeArguments state
        Nothing -> (state, [])

updateLiveShellCall
    :: ToolCall
    -> Text
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
updateLiveShellCall call delta state =
    case updateBufferedLiveCall
            liveStructuredArgumentEagerChars
            liveShellArgumentPublishChunkChars
            liveShellArgumentPrefixChars
            call
            delta
            state of
        (next, Nothing) -> (next, [])
        (next, Just updatedCall) ->
            publishLiveShellPreview updatedCall next

finishLiveShellCall
    :: ToolCall
    -> Maybe Text
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
finishLiveShellCall call completeArguments state =
    let (next, updatedCall, _) =
            finishBufferedLiveCall
                liveShellArgumentPrefixChars
                call
                completeArguments
                state
    in publishLiveShellPreview updatedCall next

publishLiveShellPreview
    :: ToolCall
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
publishLiveShellPreview call state =
    let key = toolPreviewKey call
        maybeCommand = jsonTextFieldPartial "command" call.arguments
        preview = Text.takeWhile (/= '\n') <$> maybeCommand
        previousPreview = Map.lookup key state.shellPreviewsByCallId
        changed = maybe False
            (\value -> not (Text.null value) && Just value /= previousPreview)
            preview
        displayCall command =
            withToolArguments call $
                Text.decodeUtf8
                    (LBS.toStrict
                        (Aeson.encode (Aeson.object ["command" Aeson..= command])))
        next = state
            { shellPreviewsByCallId =
                maybe state.shellPreviewsByCallId
                    (\value -> Map.insert key value
                        state.shellPreviewsByCallId)
                    preview
            }
    in
    ( next
    , [ToolArgumentsUpdated (displayCall command)
      | changed
      , command <- maybeToList preview
      ]
    )

updateLiveRawCall
    :: Int
    -> ToolCall
    -> Text
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
updateLiveRawCall eagerChars call delta state =
    case updateBufferedLiveCall
            eagerChars
            liveRawArgumentPublishChunkChars
            liveRawArgumentPrefixChars
            call
            delta
            state of
        (next, Nothing) -> (next, [])
        (next, Just updatedCall) ->
            (next, [ToolArgumentsUpdated updatedCall])

finishLiveRawCall
    :: ToolCall
    -> Maybe Text
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
finishLiveRawCall call completeArguments state =
    let (next, updatedCall, changed) =
            finishBufferedLiveCall
                liveRawArgumentPrefixChars
                call
                completeArguments
                state
    in
    ( next
    , [ToolArgumentsUpdated updatedCall | changed]
    )

updateBufferedLiveCall
    :: Int
    -> Int
    -> Int
    -> ToolCall
    -> Text
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, Maybe ToolCall)
updateBufferedLiveCall
    eagerChars
    publishChunkChars
    prefixChars
    call
    delta
    state =
    let key = toolPreviewKey call
        preview = Map.findWithDefault
            (initialRawArgumentPreview prefixChars call)
            key
            state.rawPreviewsByCallId
        room = prefixChars - preview.retainedRawArgumentChars
        retainedDelta
            | room <= 0 = ""
            | Text.length delta <= room = delta
            | otherwise = Text.copy (Text.take room delta)
        retainedDeltaChars = Text.length retainedDelta
        pendingChars =
            preview.pendingRawArgumentChars + retainedDeltaChars
        withDelta = preview
            { pendingRawArgumentChunks =
                if retainedDeltaChars > 0
                    then retainedDelta : preview.pendingRawArgumentChunks
                    else preview.pendingRawArgumentChunks
            , pendingRawArgumentChars = pendingChars
            , retainedRawArgumentChars =
                preview.retainedRawArgumentChars + retainedDeltaChars
            }
        shouldPublish =
            retainedDeltaChars > 0
                && ( Text.null preview.publishedRawArguments
                    || withDelta.retainedRawArgumentChars <= eagerChars
                    || ( preview.retainedRawArgumentChars < eagerChars
                        && withDelta.retainedRawArgumentChars > eagerChars
                       )
                    || pendingChars >= publishChunkChars
                    || withDelta.retainedRawArgumentChars == prefixChars
                   )
        rawArguments =
            preview.publishedRawArguments
                <> Text.concat (reverse withDelta.pendingRawArgumentChunks)
        published = withDelta
            { publishedRawArguments = rawArguments
            , pendingRawArgumentChunks = []
            , pendingRawArgumentChars = 0
            }
        nextPreview = if shouldPublish then published else withDelta
        withPreview = state
            { rawPreviewsByCallId =
                Map.insert key nextPreview state.rawPreviewsByCallId
            }
        updatedCall = withToolArguments call rawArguments
    in if shouldPublish
        then
            ( trackUpdatedToolCall updatedCall withPreview
            , Just updatedCall
            )
        else (withPreview, Nothing)

finishBufferedLiveCall
    :: Int
    -> ToolCall
    -> Maybe Text
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, ToolCall, Bool)
finishBufferedLiveCall prefixChars call completeArguments state =
    let key = toolPreviewKey call
        preview = Map.findWithDefault
            (initialRawArgumentPreview prefixChars call)
            key
            state.rawPreviewsByCallId
        accumulated =
            preview.publishedRawArguments
                <> Text.concat (reverse preview.pendingRawArgumentChunks)
        rawArguments =
            Text.copy
                (Text.take prefixChars
                    (fromMaybe accumulated completeArguments))
        finalPreview = RawArgumentPreview
            { publishedRawArguments = rawArguments
            , pendingRawArgumentChunks = []
            , pendingRawArgumentChars = 0
            , retainedRawArgumentChars = Text.length rawArguments
            }
        updatedCall = withToolArguments call rawArguments
        next =
            trackUpdatedToolCall updatedCall state
                { rawPreviewsByCallId =
                    Map.insert key finalPreview state.rawPreviewsByCallId
                }
    in (next, updatedCall, rawArguments /= call.arguments)

initialRawArgumentPreview :: Int -> ToolCall -> RawArgumentPreview
initialRawArgumentPreview prefixChars call =
    let initial =
            Text.copy
                (Text.take prefixChars call.arguments)
    in RawArgumentPreview
        { publishedRawArguments = initial
        , pendingRawArgumentChunks = []
        , pendingRawArgumentChars = 0
        , retainedRawArgumentChars = Text.length initial
        }

trackUpdatedToolCall
    :: ToolCall
    -> ToolArgumentStreamState
    -> ToolArgumentStreamState
trackUpdatedToolCall updatedCall state =
    state
        { toolCallsById =
            Map.map
                (\known ->
                    if sameStreamedToolCall known updatedCall
                        then updatedCall
                        else known)
                state.toolCallsById
        , toolCallsByOutputIndex =
            IntMap.map
                (\known ->
                    if sameStreamedToolCall known updatedCall
                        then updatedCall
                        else known)
                state.toolCallsByOutputIndex
        , currentToolCall =
            fmap
                (\known ->
                    if sameStreamedToolCall known updatedCall
                        then updatedCall
                        else known)
                state.currentToolCall
        }

sameStreamedToolCall :: ToolCall -> ToolCall -> Bool
sameStreamedToolCall left right =
    left.callId == right.callId
        && toolStreamKind left == toolStreamKind right

toolPreviewKey :: ToolCall -> ToolPreviewKey
toolPreviewKey call = ToolPreviewKey (toolStreamKind call) call.callId

toolStreamKind :: ToolCall -> ToolStreamKind
toolStreamKind call = case call.callKind of
    CustomCallKind -> CustomToolStream
    _ -> FunctionToolStream

isApplyPatchTool :: Text -> Bool
isApplyPatchTool name =
    canonicalToolName name == "apply_patch"

isLiveShellTool :: Text -> Bool
isLiveShellTool name =
    canonicalToolName name `elem` ["shell_command", "run_terminal_cmd"]

supportsLiveArgumentPreview :: ToolCall -> Bool
supportsLiveArgumentPreview call =
    not call.argumentsEncrypted
        && not (isComputerToolCallKind call.callKind)
        && canonicalToolName call.name /= "computer"

withToolArguments :: ToolCall -> Text -> ToolCall
withToolArguments call arguments = setToolCallArguments arguments call

countToolArgumentChars
    :: Text
    -> Bool
    -> Int
    -> ToolArgumentStreamState
    -> (ToolArgumentStreamState, [LoopEvent])
countToolArgumentChars name hasLivePreview deltaChars state =
    let total = state.streamedArgumentChars + deltaChars
        announce =
            total - state.announcedArgumentChars
                >= toolArgumentActivityChunkChars
        warn =
            total - state.warnedArgumentChars
                >= runawayToolArgumentWarningChars
    in
    ( state
        { streamedArgumentChars = total
        , announcedArgumentChars =
            if announce then total else state.announcedArgumentChars
        , warnedArgumentChars =
            if warn then total else state.warnedArgumentChars
        }
    , [ ActivityUpdated (writingToolCallActivity name (Just total))
      | announce
      , not hasLivePreview
      ]
        <> [ WarningRaised (runawayToolArgumentWarning name total)
           | warn
           ]
    )

writingToolCallActivity :: Text -> Maybe Int -> Text
writingToolCallActivity name total =
    "Writing " <> name <> " call…"
        <> foldMap
            (\chars -> " (" <> formatCharCount chars <> ")")
            total

runawayToolArgumentWarning :: Text -> Int -> Text
runawayToolArgumentWarning name total =
    "The model has streamed "
        <> formatCharCount total
        <> " of "
        <> name
        <> " arguments in one response; it may be stuck in a repetition loop."

formatCharCount :: Int -> Text
formatCharCount chars
    | chars >= 10000 =
        Text.pack (show (chars `div` 1000)) <> "k chars"
    | otherwise = Text.pack (show chars) <> " chars"
