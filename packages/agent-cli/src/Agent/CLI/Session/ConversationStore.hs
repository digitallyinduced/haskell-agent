-- | Concurrency-safe residency control for the live root conversation.
--
-- A cold checkpoint must close over durable identity (for example a session
-- id and turn cursor), not over the transcript it is intended to release.
module Agent.CLI.Session.ConversationStore
    ( ConversationStore
    , ConversationResidency(..)
    , TranscriptCheckpoint(..)
    , TranscriptGeneration
    , commitConversationTranscript
    , conversationResidency
    , currentTranscriptGeneration
    , evictConversationTranscript
    , modifyConversationAttachments
    , newColdConversationStore
    , newConversationStore
    , readConversationAttachments
    , readConversationPreviousResponseId
    , replaceConversationTranscript
    , resetConversationStore
    , withConversationTranscript
    , writeConversationPreviousResponseId
    ) where

import Agent.Loop (ImageAttachment)
import Agent.Responses.Types (ResponseItem)
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newMVar
    , readMVar
    )
import Control.Exception.Safe (finally, mask)
import Data.Text (Text)
import Data.Word (Word64)

newtype TranscriptGeneration = TranscriptGeneration Word64
    deriving (Eq, Ord, Show)

-- | A durable location from which an exact transcript can be reconstructed.
--
-- The description is for diagnostics only. The loader is deliberately opaque
-- so the store does not depend on PostgreSQL or session persistence details.
data TranscriptCheckpoint = TranscriptCheckpoint
    { checkpointDescription :: !Text
    , checkpointLoad :: !(IO [ResponseItem])
    }

data ConversationResidency
    = ConversationResident
    | ConversationCold
    deriving (Eq, Show)

data TranscriptState
    = ResidentTranscript ![ResponseItem]
    | ColdTranscript !TranscriptCheckpoint

data ConversationState = ConversationState
    { stateGeneration :: !TranscriptGeneration
    , stateTranscript :: !TranscriptState
    , statePreviousResponseId :: !(Maybe Text)
    , stateAttachments :: ![ImageAttachment]
    }

newtype ConversationStore = ConversationStore (MVar ConversationState)

newConversationStore
    :: Maybe Text
    -> [ResponseItem]
    -> [ImageAttachment]
    -> IO ConversationStore
newConversationStore previousResponseId transcript attachments =
    ConversationStore <$> newMVar ConversationState
        { stateGeneration = TranscriptGeneration 0
        , stateTranscript = ResidentTranscript transcript
        , statePreviousResponseId = previousResponseId
        , stateAttachments = attachments
        }

newColdConversationStore
    :: Maybe Text
    -> TranscriptCheckpoint
    -> [ImageAttachment]
    -> IO ConversationStore
newColdConversationStore previousResponseId checkpoint attachments =
    ConversationStore <$> newMVar ConversationState
        { stateGeneration = TranscriptGeneration 0
        , stateTranscript = ColdTranscript checkpoint
        , statePreviousResponseId = previousResponseId
        , stateAttachments = attachments
        }

conversationResidency :: ConversationStore -> IO ConversationResidency
conversationResidency (ConversationStore stateVar) = do
    state <- readMVar stateVar
    pure case state.stateTranscript of
        ResidentTranscript _ -> ConversationResident
        ColdTranscript _ -> ConversationCold

currentTranscriptGeneration
    :: ConversationStore
    -> IO TranscriptGeneration
currentTranscriptGeneration (ConversationStore stateVar) =
    (.stateGeneration) <$> readMVar stateVar

-- | Provide the exact transcript for a scoped operation.
--
-- A cold transcript is hydrated once, then returned to the same checkpoint
-- when the scope ends, provided no writer committed a newer generation.
withConversationTranscript
    :: ConversationStore
    -> ([ResponseItem] -> IO a)
    -> IO a
withConversationTranscript
        store@(ConversationStore stateVar)
        action =
    mask \restore -> do
        (generation, checkpoint, transcript) <-
            modifyMVar stateVar \state ->
                case state.stateTranscript of
                    ResidentTranscript items ->
                        pure (state, (state.stateGeneration, Nothing, items))
                    ColdTranscript cold -> do
                        items <- cold.checkpointLoad
                        let resident =
                                state
                                    { stateTranscript =
                                        ResidentTranscript items
                                    }
                        pure
                            ( resident
                            , (state.stateGeneration, Just cold, items)
                            )
        restore (action transcript)
            `finally` case checkpoint of
                Nothing -> pure ()
                Just cold -> do
                    _ <- evictConversationTranscript store generation cold
                    pure ()

-- | Publish a newer exact transcript and return its generation token.
commitConversationTranscript
    :: ConversationStore
    -> [ResponseItem]
    -> IO TranscriptGeneration
commitConversationTranscript (ConversationStore stateVar) transcript =
    modifyMVar stateVar \state -> do
        let generation = nextGeneration state.stateGeneration
        pure
            ( state
                { stateGeneration = generation
                , stateTranscript = ResidentTranscript transcript
                }
            , generation
            )

-- | Install provider/session startup state without disturbing queued images.
replaceConversationTranscript
    :: ConversationStore
    -> Maybe Text
    -> [ResponseItem]
    -> IO TranscriptGeneration
replaceConversationTranscript
        (ConversationStore stateVar)
        previousResponseId
        transcript =
    modifyMVar stateVar \state -> do
        let generation = nextGeneration state.stateGeneration
        pure
            ( state
                { stateGeneration = generation
                , stateTranscript = ResidentTranscript transcript
                , statePreviousResponseId = previousResponseId
                }
            , generation
            )

-- | Release a resident transcript only when it is still the expected version.
--
-- This makes a delayed persistence callback harmless after a concurrent newer
-- commit. Returns 'True' exactly when the transcript became cold.
evictConversationTranscript
    :: ConversationStore
    -> TranscriptGeneration
    -> TranscriptCheckpoint
    -> IO Bool
evictConversationTranscript
        (ConversationStore stateVar)
        expectedGeneration
        checkpoint =
    modifyMVar stateVar \state ->
        case state.stateTranscript of
            ResidentTranscript _
                | state.stateGeneration == expectedGeneration ->
                    pure
                        ( state
                            { stateTranscript =
                                ColdTranscript checkpoint
                            }
                        , True
                        )
            _ -> pure (state, False)

readConversationPreviousResponseId
    :: ConversationStore
    -> IO (Maybe Text)
readConversationPreviousResponseId (ConversationStore stateVar) =
    (.statePreviousResponseId) <$> readMVar stateVar

writeConversationPreviousResponseId
    :: ConversationStore
    -> Maybe Text
    -> IO ()
writeConversationPreviousResponseId (ConversationStore stateVar) value =
    modifyMVar_ stateVar \state ->
        pure state { statePreviousResponseId = value }

readConversationAttachments
    :: ConversationStore
    -> IO [ImageAttachment]
readConversationAttachments (ConversationStore stateVar) =
    (.stateAttachments) <$> readMVar stateVar

modifyConversationAttachments
    :: ConversationStore
    -> ([ImageAttachment] -> ([ImageAttachment], a))
    -> IO a
modifyConversationAttachments (ConversationStore stateVar) update =
    modifyMVar stateVar \state ->
        let (attachments, result) = update state.stateAttachments
        in pure (state { stateAttachments = attachments }, result)

resetConversationStore :: ConversationStore -> IO ()
resetConversationStore (ConversationStore stateVar) =
    modifyMVar_ stateVar \state ->
        pure state
            { stateGeneration = nextGeneration state.stateGeneration
            , stateTranscript = ResidentTranscript []
            , statePreviousResponseId = Nothing
            , stateAttachments = []
            }

nextGeneration :: TranscriptGeneration -> TranscriptGeneration
nextGeneration (TranscriptGeneration generation) =
    TranscriptGeneration (generation + 1)
