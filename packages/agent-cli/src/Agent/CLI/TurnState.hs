-- | Pure conversation-state transitions for one model turn.
module Agent.CLI.TurnState
    ( ConversationState(..)
    , FieldUpdate(..)
    , StartupUpdate(..)
    , ConversationPatch(..)
    , PreparedTurn(..)
    , ConversationOutcome(..)
    , applyConversationPatch
    , finishConversation
    , inputOnlyTurnItems
    , rebasePreparedTurn
    , restoreStartupContext
    , turnInputsWithContext
    , turnNewItems
    , turnReplacesTranscript
    ) where

import Agent.Loop
    ( TokenUsage
    , TurnInput(..)
    , addTokenUsage
    , emptyTokenUsage
    )
import Agent.CLI.Compaction.Types
    ( AutomaticCompactionBoundary(..)
    )
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Responses.Types (ResponseItem)
import Data.List (isPrefixOf)
import Data.Text (Text)

data ConversationState = ConversationState
    { conversationPreviousResponseId :: !(Maybe Text)
    , conversationTranscript :: ![ResponseItem]
    , conversationStartupContext :: !(Maybe Text)
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

-- | A description of the conversation mutations owned by a turn.
-- Usage is a delta so automatic compaction can add usage concurrently without
-- a later whole-state write losing it. Startup restoration is a merge for the
-- same reason: skill refreshes may append newer context while the turn runs.
data ConversationPatch = ConversationPatch
    { patchPreviousResponseId :: !(FieldUpdate (Maybe Text))
    , patchTranscript :: !(FieldUpdate [ResponseItem])
    , patchStartupContext :: !StartupUpdate
    , patchUsageDelta :: !TokenUsage
    , patchLastAssistant :: !(FieldUpdate (Maybe Text))
    } deriving (Eq, Show)

data PreparedTurn = PreparedTurn
    { preparedBeforeItems :: ![ResponseItem]
    , preparedConsumedStartup :: !(Maybe Text)
    , preparedTurnInputs :: ![TurnInput]
    } deriving (Eq, Show)

data ConversationOutcome
    = ConversationRestarted
    | ConversationCancelled
    | ConversationFailed
    | ConversationInterrupted
    | ConversationProviderUnavailable
    | ConversationCompleted !Text !TokenUsage !(Maybe Text)
    deriving (Eq, Show)

turnInputsWithContext
    :: Maybe Text
    -> Maybe Text
    -> [TurnInput]
    -> [TurnInput]
turnInputsWithContext planReminder startup inputs =
    contextInput planReminder
        <> contextInput startup
        <> inputs
  where
    contextInput = maybe [] (pure . UserMessage)

inputOnlyTurnItems :: PreparedTurn -> [ResponseItem]
inputOnlyTurnItems = turnInputsToItems . (.preparedTurnInputs)

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
            }
    ConversationCancelled -> retainInputs
    ConversationFailed -> retainInputs
    ConversationInterrupted -> restoreStartup
    ConversationProviderUnavailable ->
        restoreStartup
    ConversationCompleted responseId usage assistant ->
        basePatch
            { patchPreviousResponseId = SetField (Just responseId)
            , patchUsageDelta = usage
            , patchLastAssistant = SetField assistant
            }
  where
    basePatch = ConversationPatch
        { patchPreviousResponseId = KeepField
        , patchTranscript = KeepField
        , patchStartupContext = KeepStartup
        , patchUsageDelta = emptyTokenUsage
        , patchLastAssistant = KeepField
        }
    retainInputs =
        basePatch
            { patchPreviousResponseId = SetField Nothing
            , patchTranscript =
                SetField
                    (prepared.preparedBeforeItems <> inputOnlyTurnItems prepared)
            }
    restoreStartup =
        basePatch
            { patchStartupContext =
                maybe KeepStartup RestoreStartup
                    prepared.preparedConsumedStartup
            }

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
