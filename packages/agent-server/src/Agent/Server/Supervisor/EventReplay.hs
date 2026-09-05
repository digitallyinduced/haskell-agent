-- | Pure bounded SSE replay policy. The supervisor installs buffers and
-- subscribers together inside its existing STM transaction.
module Agent.Server.Supervisor.EventReplay
    ( EventBuffer
    , emptyEventBuffer
    , appendReplayEvent
    , ReplaySelection(..)
    , selectReplay
    ) where

import Agent.Server.Types
import Data.Aeson (Value)
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

data EventBuffer = EventBuffer
    { eventBufferReplay :: !(Seq ServerEvent)
    , eventBufferNextId :: !Integer
    }

emptyEventBuffer :: EventBuffer
emptyEventBuffer = EventBuffer Seq.empty 1

appendReplayEvent
    :: Int
    -> UTCTime
    -> AccessBoundary
    -> Text
    -> Maybe TurnId
    -> Maybe Text
    -> Value
    -> EventBuffer
    -> (ServerEvent, EventBuffer)
appendReplayEvent limit now boundary eventType turnId sessionId value buffer =
    (event, buffer')
  where
    event = ServerEvent
        { serverEventId = buffer.eventBufferNextId
        , serverEventBoundary = boundary
        , serverEventType = eventType
        , serverEventTurnId = turnId
        , serverEventSessionId = sessionId
        , serverEventData = value
        , serverEventAt = now
        }
    replay = trimReplay limit (buffer.eventBufferReplay |> event)
    buffer' = EventBuffer
        { eventBufferReplay = replay
        , eventBufferNextId = buffer.eventBufferNextId + 1
        }

data ReplaySelection = ReplaySelection
    { replayEvents :: [ServerEvent]
    , replayResetRequired :: Bool
    , replayLatestEventId :: Maybe Integer
    }

selectReplay :: Maybe Integer -> EventBuffer -> ReplaySelection
selectReplay lastEventId buffer =
    ReplaySelection replayEvents resetRequired latest
  where
    replay = buffer.eventBufferReplay
    oldest = (.serverEventId) <$> Seq.lookup 0 replay
    latest = (.serverEventId) <$> Seq.lookup (Seq.length replay - 1) replay
    resetRequired = case (lastEventId, oldest) of
        (Nothing, _) -> False
        (Just requested, Nothing) -> requested > 0
        (Just requested, Just firstId) ->
            requested < firstId - 1
                || maybe False (requested >) latest
    replayEvents = case lastEventId of
        Nothing -> []
        Just requested ->
            filter ((> requested) . (.serverEventId)) (toList replay)

trimReplay :: Int -> Seq a -> Seq a
trimReplay limit values =
    Seq.drop (max 0 (Seq.length values - limit)) values
