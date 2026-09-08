-- | Compatibility adapter for the existing session storage format.
module Agent.CLI.Session.TurnRecord (sessionTurnFromRecord) where

import Agent.CLI.Session.Types (SessionTurn(..))
import Agent.Runtime.TurnEngine (modelItems, displayItems)
import Agent.Runtime.TurnRecord qualified as Runtime

sessionTurnFromRecord :: Runtime.TurnRecord -> SessionTurn
sessionTurnFromRecord record = SessionTurn
    { turnAt = record.turnAt
    , turnUserText = record.turnUserText
    , turnAssistantText = record.turnAssistantText
    , turnError = record.turnError
    , turnResponseId = record.turnResponseId
    , turnEffect = record.turnEffect
    , turnItems = modelItems record.turnItems
    , turnDisplayItems = displayItems record.turnDisplayItems
    , turnUsage = record.turnUsage
    , turnProviderTelemetry = record.turnProviderTelemetry
    }
