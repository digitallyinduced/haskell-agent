-- | Pure conversation-state transitions for one model turn.
module Agent.CLI.TurnState
    ( ConversationState(..)
    , FieldUpdate(..)
    , StartupUpdate(..)
    , GrokContextUpdate(..)
    , ConversationPatch(..)
    , PreparedTurn(..)
    , ConversationOutcome(..)
    , TurnAbort(..)
    , applyConversationPatch
    , finishConversation
    , inputOnlyTurnItems
    , interruptedTurnItems
    , isTurnAbortedNote
    , rebasePreparedTurn
    , restoreStartupContext
    , turnAbortedNote
    , turnInputsWithContext
    , turnNewItems
    , turnReplacesTranscript
    , isDisplayAttemptBoundary
    , uncommittedDisplayItems
    ) where

import Agent.Json (rawJsonFromEncoding)
import Agent.Loop
    ( LoopError(..)
    , LoopEvent(..)
    , LoopExecution(..)
    , LoopProgress(..)
    , TokenUsage
    , TurnInput(..)
    , TurnOutput(..)
    , addTokenUsage
    , emptyTokenUsage
    )
import Agent.CLI.Compaction.Types
    ( AutomaticCompactionBoundary(..)
    )
import Agent.Responses.LoopBackend
    ( responseItemToToolCall
    , toolResultToItem
    , turnInputsToItems
    )
import Agent.Responses.Types
    ( ComputerCallOutput(..)
    , CustomToolCall(..)
    , CustomToolCallOutput(..)
    , FunctionCall(..)
    , FunctionCallOutput(..)
    , ItemStatus(..)
    , MessageContent(..)
    , ResponseContentPart(..)
    , ResponseItem(..)
    , ResponseMessage(..)
    , ResponseRole(..)
    , TaggedObject(..)
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallMode(..)
    , ToolCallResult(..)
    , toolCallMode
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as AesonEncoding
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

data DisplayProjection = DisplayProjection
    { displayItems :: ![ResponseItem]
    , displayCanAppendText :: !Bool
    }

-- | Normalize the transient provider event journal into stable response
-- items used only by session-history projection. These items must never enter
-- model context.
uncommittedDisplayItems :: LoopExecution -> [ResponseItem]
uncommittedDisplayItems execution =
    (.displayItems) $
        foldl'
            projectDisplayEvent
            DisplayProjection
                { displayItems = []
                , displayCanAppendText = False
                }
            execution.executionUncommittedDisplayEvents

projectDisplayEvent :: DisplayProjection -> LoopEvent -> DisplayProjection
projectDisplayEvent projection = \case
    TextDelta delta
        | Text.null delta -> projection
        | projection.displayCanAppendText ->
            projection
                { displayItems =
                    appendAssistantDelta delta projection.displayItems
                }
        | otherwise ->
            projection
                { displayItems =
                    projection.displayItems <> [displayAssistantItem delta]
                , displayCanAppendText = True
                }
    ResponseRestarted _ ->
        projection
            { displayItems =
                projection.displayItems <> [displayAttemptBoundary]
            , displayCanAppendText = False
            }
    ToolStarted call ->
        projectDisplayCall call projection
    ToolUpdated call ->
        projectDisplayCall call projection
    ToolArgumentsUpdated call ->
        projectDisplayCall call projection
    ToolOutputUpdated callId output ->
        projection
            { displayItems =
                replaceDisplayOutput
                    callId
                    (displayToolOutput callId output ItemIncomplete)
                    projection.displayItems
            , displayCanAppendText = False
            }
    ToolFinished result ->
        projection
            { displayItems =
                replaceDisplayOutput
                    result.callId
                    (displayToolOutput
                        result.callId
                        result.output
                        ItemCompleted)
                    projection.displayItems
            , displayCanAppendText = False
            }
    ToolRetracted callId ->
        projection
            { displayItems =
                updateCurrentDisplayAttempt
                    (filter (not . belongsToDisplayCall callId))
                    projection.displayItems
            , displayCanAppendText = False
            }
    _ -> projection

projectDisplayCall :: ToolCall -> DisplayProjection -> DisplayProjection
projectDisplayCall call projection =
    projection
        { displayItems =
            replaceDisplayCall call projection.displayItems
        , displayCanAppendText = False
        }

replaceDisplayCall :: ToolCall -> [ResponseItem] -> [ResponseItem]
replaceDisplayCall call =
    updateOrAppendCurrentDisplayAttempt
        (isDisplayCall call.callId)
        (isDisplayCall call.callId)
        (displayToolCall call)

replaceDisplayOutput
    :: Text
    -> ResponseItem
    -> [ResponseItem]
    -> [ResponseItem]
replaceDisplayOutput callId replacement =
    updateOrAppendCurrentDisplayAttempt
        (isDisplayOutput callId)
        (isDisplayOutput callId)
        replacement

updateOrAppendCurrentDisplayAttempt
    :: (ResponseItem -> Bool)
    -> (ResponseItem -> Bool)
    -> ResponseItem
    -> [ResponseItem]
    -> [ResponseItem]
updateOrAppendCurrentDisplayAttempt exists replace replacement items =
    let (prior, current) = splitCurrentDisplayAttempt items
    in if any exists current
        then
            prior
                <> map
                    (\item ->
                        if replace item then replacement else item)
                    current
        else items <> [replacement]

updateCurrentDisplayAttempt
    :: ([ResponseItem] -> [ResponseItem])
    -> [ResponseItem]
    -> [ResponseItem]
updateCurrentDisplayAttempt update items =
    let (prior, current) = splitCurrentDisplayAttempt items
    in prior <> update current

splitCurrentDisplayAttempt
    :: [ResponseItem]
    -> ([ResponseItem], [ResponseItem])
splitCurrentDisplayAttempt items =
    let (currentReversed, priorReversed) =
            break isDisplayAttemptBoundary (reverse items)
    in (reverse priorReversed, reverse currentReversed)

displayAttemptBoundary :: ResponseItem
displayAttemptBoundary =
    UnknownResponseItem
        TaggedObject
            { tag = displayAttemptBoundaryTag
            }

displayAttemptBoundaryTag :: Text
displayAttemptBoundaryTag =
    "haskell_agent_display_attempt_boundary"

isDisplayAttemptBoundary :: ResponseItem -> Bool
isDisplayAttemptBoundary = \case
    UnknownResponseItem tagged ->
        tagged.tag == displayAttemptBoundaryTag
    _ -> False

displayAssistantItem :: Text -> ResponseItem
displayAssistantItem text =
    MessageItem ResponseMessage
        { messageId = Nothing
        , content =
            MessageContentParts
                [ OutputTextPart
                    { text
                    , annotations = Nothing
                    , logprobs = Nothing
                    }
                ]
        , role = RoleAssistant
        , status = Just ItemIncomplete
        , phase = Nothing
        , passthrough = Nothing
        }

appendAssistantDelta :: Text -> [ResponseItem] -> [ResponseItem]
appendAssistantDelta delta items =
    case reverse items of
        MessageItem message : rest
            | message.role == RoleAssistant ->
                reverse rest
                    <> [ MessageItem message
                            { content =
                                appendMessageText delta message.content
                            }
                       ]
        _ -> items <> [displayAssistantItem delta]

appendMessageText :: Text -> MessageContent -> MessageContent
appendMessageText delta = \case
    MessageContentText text ->
        MessageContentText (text <> delta)
    MessageContentParts parts ->
        MessageContentParts $
            case reverse parts of
                OutputTextPart{text, annotations, logprobs} : rest ->
                    reverse rest
                        <> [ OutputTextPart
                                { text = text <> delta
                                , annotations
                                , logprobs
                                }
                           ]
                _ ->
                    parts
                        <> [ OutputTextPart
                                { text = delta
                                , annotations = Nothing
                                , logprobs = Nothing
                                }
                           ]

displayToolCall :: ToolCall -> ResponseItem
displayToolCall call =
    FunctionCallItem FunctionCall
        { itemId = Nothing
        , callId = call.callId
        , name = call.name
        , namespace = Nothing
        , provider = Nothing
        , arguments =
            if call.argumentsEncrypted
                then "{}"
                else call.arguments
        , encryptedFunctionArgs = Nothing
        , status = Just ItemInProgress
        , async =
            case toolCallMode call of
                AsyncToolCall -> Just True
                BlockingToolCall -> Nothing
        }

displayToolOutput :: Text -> Text -> ItemStatus -> ResponseItem
displayToolOutput callId output status =
    FunctionCallOutputItem FunctionCallOutput
        { localOutcome = Nothing
        , itemId = Nothing
        , callId
        , name = Nothing
        , namespace = Nothing
        , provider = Nothing
        , output = rawJsonFromEncoding (AesonEncoding.text output)
        , status = Just status
        , async = Nothing
        }

belongsToDisplayCall :: Text -> ResponseItem -> Bool
belongsToDisplayCall callId item =
    isDisplayCall callId item || isDisplayOutput callId item

isDisplayCall :: Text -> ResponseItem -> Bool
isDisplayCall callId = \case
    FunctionCallItem call -> call.callId == callId
    _ -> False

isDisplayOutput :: Text -> ResponseItem -> Bool
isDisplayOutput callId = \case
    FunctionCallOutputItem output -> output.callId == callId
    _ -> False

data ConversationState = ConversationState
    { conversationPreviousResponseId :: !(Maybe Text)
    , conversationTranscript :: ![ResponseItem]
    , conversationStartupContext :: !(Maybe Text)
    , conversationGrokFirstTurnContext :: !(Maybe Text)
    , conversationUsage :: !TokenUsage
    , conversationLastAssistant :: !(Maybe Text)
    } deriving (Eq, Show)

data FieldUpdate a
    = KeepField
    | SetField !a
    deriving (Eq, Show)

data StartupUpdate
    = KeepStartup
    | RestoreStartup !Text
    deriving (Eq, Show)

data GrokContextUpdate
    = KeepGrokContext
    | RestoreGrokContext !Text
    deriving (Eq, Show)

-- | A description of the conversation mutations owned by a turn.
-- Usage is a delta so automatic compaction can add usage concurrently without
-- a later whole-state write losing it. Startup restoration is a merge for the
-- same reason: skill refreshes may append newer context while the turn runs.
data ConversationPatch = ConversationPatch
    { patchPreviousResponseId :: !(FieldUpdate (Maybe Text))
    , patchTranscript :: !(FieldUpdate [ResponseItem])
    , patchStartupContext :: !StartupUpdate
    , patchGrokFirstTurnContext :: !GrokContextUpdate
    , patchUsageDelta :: !TokenUsage
    , patchLastAssistant :: !(FieldUpdate (Maybe Text))
    } deriving (Eq, Show)

data PreparedTurn = PreparedTurn
    { preparedBeforeItems :: ![ResponseItem]
    , preparedConsumedStartup :: !(Maybe Text)
    , preparedConsumedGrokContext :: !(Maybe Text)
    , preparedTurnInputs :: ![TurnInput]
    } deriving (Eq, Show)

data ConversationOutcome
    = ConversationRestarted
    -- | The turn stopped before its final response. Carries the items the
    -- turn keeps after 'preparedBeforeItems'; see 'interruptedTurnItems'.
    | ConversationCancelled ![ResponseItem]
    | ConversationFailed ![ResponseItem]
    | ConversationInterrupted
    | ConversationProviderUnavailable
    | ConversationCompleted !Text !TokenUsage !(Maybe Text)
    deriving (Eq, Show)

-- | Why a turn stopped before its final response. Tool calls the loop never
-- ran are closed with a synthetic output naming this reason, so the
-- transcript stays replayable and the model cannot assume they executed.
data TurnAbort
    = TurnAbortedByUser
    | TurnAbortedByFailure !Text
    deriving (Eq, Show)

turnInputsWithContext
    :: Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> [TurnInput]
    -> [TurnInput]
turnInputsWithContext planReminder taskPlanReminder startup inputs =
    contextInput planReminder
        <> contextInput taskPlanReminder
        <> contextInput startup
        <> inputs
  where
    contextInput = maybe [] (pure . UserMessage)

inputOnlyTurnItems :: PreparedTurn -> [ResponseItem]
inputOnlyTurnItems = turnInputsToItems . (.preparedTurnInputs)

-- | Items an interrupted turn keeps after 'preparedBeforeItems'.
--
-- Every model step the loop committed is retained exactly as a successful
-- turn would retain it: assistant text, tool calls, the tool results that
-- were fed back, and the results still queued for the next step. Only the
-- sample that never committed is dropped. This mirrors Codex and Grok Build,
-- which record history per completed step rather than per turn, so a cancel
-- or failure no longer erases work that was already visible on screen.
--
-- Tool calls left without a result are closed with a synthetic output that
-- states the abort reason. After a user cancel such a call may never have
-- started or may have been stopped mid-run, so the output says it was
-- interrupted rather than claiming it never ran; a failure before tool
-- execution reports that the call was not executed. A user-initiated cancel
-- also appends 'turnAbortedNote' so the next request explains the gap.
--
-- While nothing committed — or when the committed state no longer extends
-- the prepared history, as after a mid-turn automatic compaction — only the
-- prepared inputs are retained, so a retry resubmits exactly them.
interruptedTurnItems
    :: PreparedTurn
    -> LoopExecution
    -> TurnAbort
    -> [ResponseItem]
interruptedTurnItems prepared execution abort =
    case execution.executionProgress of
        ResponseCommitted
            | prepared.preparedBeforeItems
                `isPrefixOf` execution.executionState ->
                let committed =
                        dropTruncatedCalls
                            execution.executionResult
                            (drop
                                (length prepared.preparedBeforeItems)
                                execution.executionState)
                    pendingResults =
                        [ toolResultToItem result
                        | CompletedTool result <- execution.executionPendingInputs
                        ]
                    retained = committed <> pendingResults
                    closed =
                        retained
                            <> map (abortedToolOutput abort)
                                (danglingToolCalls retained)
                in closed <> abortNote abort retained
        _ -> inputOnlyTurnItems prepared

-- | An incomplete final response can end inside a tool call. Grok Build's
-- length policy keeps only calls whose arguments are complete; a call cut off
-- mid-arguments is dropped here because replaying truncated arguments cannot
-- help the model, and a runaway sample may carry hundreds of kilobytes of
-- them. Partial assistant text is kept.
dropTruncatedCalls
    :: Either LoopError a
    -> [ResponseItem]
    -> [ResponseItem]
dropTruncatedCalls result items = case result of
    Left (LoopIncomplete turn) ->
        let incompleteCalls =
                Set.fromList (map (.callId) turn.toolCalls)
        in filter (not . isTruncatedCall incompleteCalls) items
    _ -> items
  where
    isTruncatedCall incompleteCalls = \case
        FunctionCallItem call ->
            Set.member call.callId incompleteCalls
                && ( call.status == Just ItemIncomplete
                    || not (hasCompleteArguments call)
                   )
        CustomToolCallItem call ->
            Set.member call.callId incompleteCalls
                && call.status == Just ItemIncomplete
        _ -> False
    hasCompleteArguments call =
        isJust call.encryptedFunctionArgs
            || isJust
                (Aeson.decodeStrict (Text.encodeUtf8 call.arguments)
                    :: Maybe Aeson.Value)

-- | Tool calls among the retained items that have no matching output.
danglingToolCalls :: [ResponseItem] -> [ToolCall]
danglingToolCalls items =
    [ call
    | item <- items
    , Just call <- [responseItemToToolCall item]
    , Set.notMember call.callId answered
    ]
  where
    answered = Set.fromList (concatMap outputCallId items)
    outputCallId = \case
        FunctionCallOutputItem output -> [output.callId]
        CustomToolCallOutputItem output -> [output.callId]
        ComputerCallOutputItem output -> [output.computerOutputCallId]
        _ -> []

abortedToolOutput :: TurnAbort -> ToolCall -> ResponseItem
abortedToolOutput abort call =
    toolResultToItem ToolCallResult
        { callId = call.callId
        , output = case abort of
            TurnAbortedByUser ->
                "Tool `" <> call.name
                    <> "` was interrupted: the user cancelled the turn. "
                    <> "It was not run, or was stopped before finishing and "
                    <> "may have partially executed."
            TurnAbortedByFailure reason ->
                "Tool `" <> call.name <> "` was not executed: "
                    <> reason <> "."
        , callKind = call.callKind
        , toolResultMode = BlockingToolCall
        , toolResultImages = []
        , toolResultOutcome = Nothing
        }

abortNote :: TurnAbort -> [ResponseItem] -> [ResponseItem]
abortNote abort retained = case abort of
    TurnAbortedByUser
        | any isModelOutputItem retained ->
            turnInputsToItems [UserMessage turnAbortedNote]
    _ -> []

-- | Recorded after a user-initiated cancel, adapted from the @<turn_aborted>@
-- marker Codex adds to its history. It is a user-role transcript item so any
-- Responses-compatible provider replays it; history projection hides it.
turnAbortedNote :: Text
turnAbortedNote =
    "<turn_aborted>\n\
    \The user interrupted the previous turn on purpose. Tool calls marked as \
    \interrupted were not run, or were stopped before finishing and may have \
    \partially executed. Do not resume the interrupted work unless the user \
    \asks for it.\n\
    \</turn_aborted>"

isTurnAbortedNote :: Text -> Bool
isTurnAbortedNote = Text.isPrefixOf "<turn_aborted>" . Text.stripStart

isModelOutputItem :: ResponseItem -> Bool
isModelOutputItem = \case
    MessageItem message -> message.role == RoleAssistant
    FunctionCallItem{} -> True
    CustomToolCallItem{} -> True
    ComputerCallItem{} -> True
    ReasoningItemValue{} -> True
    _ -> False

-- | Once automatic compaction has committed, the enclosing turn no longer
-- owns the superseded prefix. Rebase both its history and the exact inputs
-- accepted by the compacted continuation so success and failure persist only
-- post-checkpoint state.
rebasePreparedTurn
    :: Maybe AutomaticCompactionBoundary
    -> PreparedTurn
    -> PreparedTurn
rebasePreparedTurn boundary prepared =
    case boundary of
        Nothing -> prepared
        Just committed ->
            prepared
                { preparedBeforeItems =
                    committed.automaticCompactionHistory
                , preparedConsumedStartup = Nothing
                , preparedConsumedGrokContext = Nothing
                , preparedTurnInputs =
                    committed.automaticCompactionPendingInputs
                }

finishConversation :: PreparedTurn -> ConversationOutcome -> ConversationPatch
finishConversation prepared = \case
    ConversationRestarted ->
        basePatch
            { patchTranscript = SetField prepared.preparedBeforeItems
            , patchStartupContext =
                maybe KeepStartup RestoreStartup
                    prepared.preparedConsumedStartup
            , patchGrokFirstTurnContext = grokContextUpdate
            }
    ConversationCancelled retained -> retainItems retained
    ConversationFailed retained -> retainItems retained
    ConversationInterrupted -> restorePromptContext
    ConversationProviderUnavailable ->
        restorePromptContext
    ConversationCompleted _responseId usage assistant ->
        basePatch
            { patchUsageDelta = usage
            , patchLastAssistant = SetField assistant
            }
  where
    basePatch = ConversationPatch
        { patchPreviousResponseId = KeepField
        , patchTranscript = KeepField
        , patchStartupContext = KeepStartup
        , patchGrokFirstTurnContext = KeepGrokContext
        , patchUsageDelta = emptyTokenUsage
        , patchLastAssistant = KeepField
        }
    -- The response chain is invalidated: the next request replays the
    -- retained transcript in full.
    retainItems retained =
        basePatch
            { patchPreviousResponseId = SetField Nothing
            , patchTranscript =
                SetField (prepared.preparedBeforeItems <> retained)
            }
    restorePromptContext =
        basePatch
            { patchStartupContext =
                maybe KeepStartup RestoreStartup
                    prepared.preparedConsumedStartup
            , patchGrokFirstTurnContext = grokContextUpdate
            }
    grokContextUpdate =
        maybe KeepGrokContext RestoreGrokContext
            prepared.preparedConsumedGrokContext

applyConversationPatch
    :: ConversationPatch
    -> ConversationState
    -> ConversationState
applyConversationPatch patch state =
    state
        { conversationPreviousResponseId =
            applyField patch.patchPreviousResponseId
                state.conversationPreviousResponseId
        , conversationTranscript =
            applyField patch.patchTranscript state.conversationTranscript
        , conversationStartupContext =
            applyStartup patch.patchStartupContext
                state.conversationStartupContext
        , conversationGrokFirstTurnContext =
            applyGrokContext patch.patchGrokFirstTurnContext
                state.conversationGrokFirstTurnContext
        , conversationUsage =
            addTokenUsage state.conversationUsage patch.patchUsageDelta
        , conversationLastAssistant =
            applyField patch.patchLastAssistant
                state.conversationLastAssistant
        }

restoreStartupContext :: Text -> Maybe Text -> Maybe Text
restoreStartupContext consumed = Just . \case
    Nothing -> consumed
    Just newer -> consumed <> "\n\n" <> newer

turnNewItems :: [ResponseItem] -> [ResponseItem] -> [ResponseItem]
turnNewItems beforeItems afterItems
    | beforeItems `isPrefixOf` afterItems =
        drop (length beforeItems) afterItems
    | otherwise = afterItems

turnReplacesTranscript :: [ResponseItem] -> [ResponseItem] -> Bool
turnReplacesTranscript beforeItems afterItems =
    not (beforeItems `isPrefixOf` afterItems)

applyField :: FieldUpdate a -> a -> a
applyField update current = case update of
    KeepField -> current
    SetField value -> value

applyStartup :: StartupUpdate -> Maybe Text -> Maybe Text
applyStartup update current = case update of
    KeepStartup -> current
    RestoreStartup consumed -> restoreStartupContext consumed current

applyGrokContext :: GrokContextUpdate -> Maybe Text -> Maybe Text
applyGrokContext update current = case update of
    KeepGrokContext -> current
    RestoreGrokContext consumed -> case current of
        Nothing -> Just consumed
        Just _ -> current
