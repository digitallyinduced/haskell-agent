-- | Transfer and metadata JSON compatibility.
module Agent.CLI.SessionSpec.JsonCodec (spec) where

import Agent.CLI.Session
import Agent.CLI.SessionSpec.Fixtures
import Agent.CLI.Session.Codec
    (fromStoredMetadata, fromStoredPromptSnapshot, toStoredMetadata, toStoredPromptSnapshot)
import Agent.CLI.Session.Types (restoreLegacyLocalCompactionMarker)
import Agent.CLI.ModelConfig (organizationGatewayConnectionId)
import Agent.Dialect (DialectId(..))
import Agent.Json.Decode qualified as Hermes
import Agent.Loop (TokenUsage(..))
import Agent.Provider (Provider(..))
import Agent.Responses.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "json codec" do
        it "round-trips an optional current task plan and accepts older transfers" do
            let transfer = SessionTransfer
                    { transferMeta = testMeta "session-plan"
                    , transferTaskPlan = Just sampleTaskPlan
                    , transferTurns = []
                    }
                encoded = Aeson.encode transfer
            Hermes.decodeEither sessionTransferDecoder (LBS.toStrict encoded)
                `shouldBe` Right transfer
            let legacy = case Aeson.toJSON transfer of
                    Aeson.Object object ->
                        Aeson.Object (KeyMap.delete "currentTaskPlan" object)
                    value -> value
            Hermes.decodeEither sessionTransferDecoder
                (LBS.toStrict (Aeson.encode legacy))
                `shouldBe` Right transfer { transferTaskPlan = Nothing }

        it "encodes and decodes SessionTurn" do
            let turn = SessionTurn
                    { turnAt = fixedTime
                    , turnUserText = "q"
                    , turnAssistantText = Nothing
                    , turnError = Just "cancelled"
                    , turnResponseId = Nothing
                    , turnItems = []
                    , turnDisplayItems = []
                    , turnUsage = Nothing
                    , turnEffect = TranscriptAppend
                    , turnProviderTelemetry = [sampleTurnTelemetry]
                    }
            Hermes.decodeEither sessionTurnDecoder
                (LBS.toStrict (Aeson.encode turn))
                `shouldBe` Right turn

        it "round-trips recap metadata" do
            let meta =
                    (testMeta "session-1")
                        { metaLastRecap = Just "We fixed auth retries."
                        , metaLastTurnSummary = Just "Auth retries wired"
                        , metaLastRecapMainTurns = 3
                        }
            Hermes.decodeEither sessionMetaDecoder
                (LBS.toStrict (Aeson.encode meta))
                `shouldBe` Right meta

        it "round-trips gateway protocol identity through every metadata codec" do
            let prompt =
                    (testPromptSnapshot "gateway-session")
                        { promptSnapshotProvider = OpenAIProvider
                        , promptSnapshotConnection =
                            organizationGatewayConnectionId
                        , promptSnapshotModel = "company-coder"
                        , promptSnapshotDialect = GenericResponsesDialect
                        }
                legacy = LegacySubagentTarget
                    { legacyTargetProvider = OpenAIProvider
                    , legacyTargetConnection =
                        organizationGatewayConnectionId
                    , legacyTargetEffectiveModel = "company-coder"
                    , legacyTargetDialect = GenericResponsesDialect
                    }
                meta =
                    (testMeta "gateway-session")
                        { metaProvider = OpenAIProvider
                        , metaConnection = organizationGatewayConnectionId
                        , metaGatewayIdentity =
                            Just "gateway-sha256:test-tenant"
                        , metaModel = "company-coder"
                        , metaTransportModel = Just "company-coder"
                        , metaDialect = GenericResponsesDialect
                        , metaLegacySubagentTarget = Just legacy
                        , metaPromptSnapshot = Just prompt
                        }
            Hermes.decodeEither sessionMetaDecoder
                (LBS.toStrict (Aeson.encode meta))
                `shouldBe` Right meta
            fromStoredMetadata (toStoredMetadata meta)
                `shouldBe` Right (meta { metaPromptSnapshot = Nothing })
            fromStoredPromptSnapshot (toStoredPromptSnapshot prompt)
                `shouldBe` Right prompt
            let legacyJson =
                    case Aeson.toJSON meta of
                        Aeson.Object object ->
                            Aeson.Object
                                (KeyMap.delete "gatewayIdentity" object)
                        value -> value
            Hermes.decodeEither sessionMetaDecoder
                (LBS.toStrict (Aeson.encode legacyJson))
                `shouldBe` Right (meta { metaGatewayIdentity = Nothing })

        it "infers transcript effects when importing legacy JSON turns" do
            let legacy userText items = Aeson.object
                    [ "at" Aeson..= fixedTime
                    , "userText" Aeson..= (userText :: Text.Text)
                    , "assistantText" Aeson..= (Nothing :: Maybe Text.Text)
                    , "error" Aeson..= (Nothing :: Maybe Text.Text)
                    , "responseId" Aeson..= (Nothing :: Maybe Text.Text)
                    , "items" Aeson..= (items :: [ResponseItem])
                    , "usage" Aeson..= (Nothing :: Maybe TokenUsage)
                    ]
                decoded userText items =
                    Hermes.decodeEither sessionTurnDecoder
                        (LBS.toStrict
                            (Aeson.encode (legacy userText items)))
                oldLocalSummary = MessageItem ResponseMessage
                    { messageId = Nothing
                    , content = MessageContentParts
                        [ OutputTextPart
                            "Compacted conversation summary:\nlegacy state"
                            Nothing
                            Nothing
                        ]
                    , role = RoleAssistant
                    , status = Nothing
                    , phase = Nothing
                    , passthrough = Nothing
                    }
                hasLocalMarker = any \case
                    MessageItem message ->
                        responseMessageHasContentItemKind
                            localCompactionSummaryContentItemKind
                            message
                    _ -> False
            fmap (.turnEffect) (decoded "/compact focus" [])
                `shouldBe` Right TranscriptReplace
            fmap (.turnEffect) (decoded "/rewind" [])
                `shouldBe` Right TranscriptReset
            fmap
                (\turn ->
                    ( turn.turnEffect
                    , hasLocalMarker turn.turnItems
                    ))
                (decoded "continue" [oldLocalSummary])
                `shouldBe` Right (TranscriptReplace, True)
            hasLocalMarker
                (restoreLegacyLocalCompactionMarker
                    TranscriptAppend
                    [oldLocalSummary])
                `shouldBe` False
