module Agent.Runtime.TurnRecordSpec (spec) where

import Agent.Runtime.Session.TurnRecord (sessionTurnFromRecord)
import Agent.Runtime.SessionSpec.Fixtures (sampleTurnTelemetry)
import Agent.Runtime.Session.Types (SessionTurn(..), TranscriptEffect(..), sessionTurnDecoder)
import Agent.Error (ApiError(..))
import Agent.Loop hiding (TurnCompleted)
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Runtime.TurnEngine
import Agent.Runtime.TurnRecord qualified as Record
import Agent.Runtime.TurnState (PreparedTurn(..))
import Agent.Json.Decode qualified as Hermes
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Time.Clock (UTCTime(..))
import Test.Hspec

spec :: Spec
spec = describe "typed turn persistence adapter" do
    it "preserves successful canonical history and all legacy metadata" do
        let result = LoopResult "response" (Just "answer") 1 (TokenUsage 10 4 1)
            final = finish baseExecution
                { executionState = turnInputsToItems [UserMessage "canonical"]
                , executionProgress = ResponseCommitted
                , executionResult = Right result
                }
            record = (recordFor final)
                { Record.turnAssistantText = Just "answer"
                , Record.turnResponseId = Just "response"
                , Record.turnUsage = Just result.tokenUsage
                , Record.turnEffect = TranscriptReplace
                }
            stored = sessionTurnFromRecord record
        stored `shouldBe` SessionTurn
            { turnAt = record.turnAt
            , turnUserText = "prompt"
            , turnAssistantText = Just "answer"
            , turnError = Nothing
            , turnResponseId = Just "response"
            , turnEffect = TranscriptReplace
            , turnItems = turnInputsToItems [UserMessage "canonical"]
            , turnDisplayItems = []
            , turnUsage = Just result.tokenUsage
            , turnProviderTelemetry = [sampleTurnTelemetry]
            }
        Hermes.decodeEither sessionTurnDecoder (LBS.toStrict (encode stored))
            `shouldBe` Right stored

    it "keeps interrupted visible output out of persisted model history" do
        let final = finish baseExecution
                { executionUncommittedDisplayEvents =
                    [TextDelta "partial visible answer", ResponseAttemptFailed]
                , executionResult = Left (LoopCancelled [])
                }
            stored = sessionTurnFromRecord
                (recordFor final) { Record.turnError = Just "cancelled" }
        stored.turnItems `shouldBe` turnInputsToItems [UserMessage "prompt"]
        stored.turnDisplayItems `shouldBe` displayItems final.finalizedDisplayItems
        stored.turnDisplayItems `shouldSatisfy` (not . null)
        stored.turnError `shouldBe` Just "cancelled"
        Hermes.decodeEither sessionTurnDecoder (LBS.toStrict (encode stored))
            `shouldBe` Right stored

recordFor :: FinalizedTurn -> Record.TurnRecord
recordFor final = Record.TurnRecord
    { turnAt = UTCTime (toEnum 60000) 0
    , turnUserText = "prompt"
    , turnAssistantText = Nothing
    , turnError = Nothing
    , turnResponseId = Nothing
    , turnEffect = TranscriptAppend
    , turnItems = final.finalizedModelItems
    , turnDisplayItems = final.finalizedDisplayItems
    , turnUsage = Nothing
    , turnProviderTelemetry = [sampleTurnTelemetry]
    }

finish :: LoopExecution -> FinalizedTurn
finish = finalizeTurn (TurnPolicy (const False) id) Nothing Nothing
    (PreparedTurn [] Nothing Nothing [UserMessage "prompt"])

baseExecution :: LoopExecution
baseExecution = LoopExecution
    { executionState = []
    , executionPendingInputs = [UserMessage "prompt"]
    , executionProgress = NoResponseCommitted
    , executionUncommittedAssistantText = Nothing
    , executionUncommittedDisplayEvents = []
    , executionProviderTelemetry = []
    , executionResult = Left (LoopTransport (ConnectionError "offline"))
    }
