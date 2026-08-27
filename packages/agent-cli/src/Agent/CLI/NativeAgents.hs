module Agent.CLI.NativeAgents
    ( NativeAgentView(..)
    , applyNativeAgentEvent
    , nativeAgentEntries
    , restoreNativeAgents
    ) where

import Agent.CLI.AgentViewport (AgentEntry(..), AgentTarget(..))
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
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)

data NativeAgentView = NativeAgentView
    { nativeAgentId :: !Text
    , nativeAgentParent :: !(Maybe Text)
    , nativeAgentLabel :: !Text
    , nativeAgentModel :: !(Maybe Text)
    , nativeAgentStatus :: !Text
    , nativeAgentTranscript :: ![Text]
    , nativeAgentConversation :: !UiState
    }
    deriving (Eq, Show)

applyNativeAgentEvent
    :: LoopEvent
    -> Map.Map Text NativeAgentView
    -> Map.Map Text NativeAgentView
applyNativeAgentEvent event current =
    case event of
        TurnStarted ->
            settleRunningNativeAgents NativeAgentCancelled current
        ResponseRestarted _ ->
            settleRunningNativeAgents NativeAgentCancelled current
        ResponseAttemptDiscarded ->
            settleRunningNativeAgents NativeAgentCancelled current
        NativeAgentStarted identifier parent label model ->
            Map.alter
                (\case
                    Nothing ->
                        Just
                            (newNativeAgentView
                                identifier parent label model)
                    Just view ->
                        Just view
                            { nativeAgentParent = parent
                            , nativeAgentLabel = label
                            , nativeAgentModel = model
                            , nativeAgentStatus = "running"
                            })
                identifier
                current
        NativeAgentOutput identifier output ->
            let view =
                    Map.findWithDefault
                        (newNativeAgentView
                            identifier Nothing identifier Nothing)
                        identifier
                        current
            in Map.insert identifier
                view
                    { nativeAgentTranscript =
                        view.nativeAgentTranscript <> [output]
                    , nativeAgentConversation =
                        reduceUi
                            (UiLoop (TextDelta output))
                            view.nativeAgentConversation
                    }
                current
        NativeAgentFinished identifier status ->
            let view =
                    Map.findWithDefault
                        (newNativeAgentView
                            identifier Nothing identifier Nothing)
                        identifier
                        current
            in Map.insert identifier
                view
                    { nativeAgentStatus = nativeAgentStatusText status
                    , nativeAgentConversation =
                        reduceUi
                            (UiTurnEnded (nativeAgentBlockState status))
                            view.nativeAgentConversation
                    }
                current
        _ -> current

nativeAgentEntries :: Map.Map Text NativeAgentView -> [AgentEntry]
nativeAgentEntries agents =
    map (nativeAgentEntry agents) (Map.elems agents)

-- | Reconstruct completed top-level Claude-native agents from the canonical
-- tool items stored in the root transcript. Live entries win when the same
-- call is already present.
restoreNativeAgents
    :: [ResponseItem]
    -> Map.Map Text NativeAgentView
    -> Map.Map Text NativeAgentView
restoreNativeAgents items current =
    Map.union current restored
  where
    outputs = Map.fromList
        [ (output.callId, output)
        | FunctionCallOutputItem output <- items
        , isClaudeNativeItem output.extraFields
        ]
    restored = Map.fromList
        [ (call.callId, restoredView call)
        | FunctionCallItem call <- items
        , isClaudeNativeCall call
        , Map.member call.callId outputs
        ]
    restoredView call =
        let maybeOutput = Map.lookup call.callId outputs
            outputText = maybe "" (renderOutput . (.output)) maybeOutput
            terminal = case maybeOutput >>= (.status) of
                Just ItemIncomplete -> BlockFailed
                Just (ItemStatusUnknown status)
                    | Text.toLower status `elem`
                        ["failed", "error", "cancelled", "canceled"] ->
                            BlockFailed
                _ -> BlockComplete
            started = newNativeAgentView
                call.callId
                Nothing
                (fromMaybe call.name
                    (argumentText "description" call.arguments))
                (argumentText "model" call.arguments)
            withOutput
                | Text.null (Text.strip outputText) = started
                | otherwise =
                    started
                        { nativeAgentTranscript = [outputText]
                        , nativeAgentConversation =
                            reduceUi
                                (UiLoop (TextDelta outputText))
                                started.nativeAgentConversation
                        }
        in withOutput
            { nativeAgentStatus =
                if terminal == BlockFailed then "error" else "done"
            , nativeAgentConversation =
                reduceUi
                    (UiTurnEnded terminal)
                    withOutput.nativeAgentConversation
            }

isClaudeNativeCall :: FunctionCall -> Bool
isClaudeNativeCall call =
    Text.toLower call.name `elem` ["agent", "task"]
        && isClaudeNativeItem call.extraFields

isClaudeNativeItem :: KeyMap.KeyMap Aeson.Value -> Bool
isClaudeNativeItem fields =
    KeyMap.lookup "provider" fields
        == Just (Aeson.String "claude-code")

argumentText :: Text -> Text -> Maybe Text
argumentText key raw = do
    value <- either (const Nothing) Just $
        Hermes.decodeEither
            (Hermes.object (Hermes.atKey key Hermes.text))
            (TextEncoding.encodeUtf8 raw)
    let stripped = Text.strip value
    if Text.null stripped then Nothing else Just stripped

renderOutput :: Aeson.Value -> Text
renderOutput = \case
    Aeson.String text -> text
    value ->
        TextEncoding.decodeUtf8With lenientDecode
            (LazyByteString.toStrict (Aeson.encode value))

settleRunningNativeAgents
    :: NativeAgentStatus
    -> Map.Map Text NativeAgentView
    -> Map.Map Text NativeAgentView
settleRunningNativeAgents status =
    Map.map \view ->
        if view.nativeAgentStatus /= "running"
            then view
            else view
                { nativeAgentStatus = nativeAgentStatusText status
                , nativeAgentConversation =
                    reduceUi
                        (UiTurnEnded (nativeAgentBlockState status))
                        view.nativeAgentConversation
                }

nativeAgentBlockState :: NativeAgentStatus -> BlockState
nativeAgentBlockState = \case
    NativeAgentRunning -> BlockRunning
    NativeAgentCompleted -> BlockComplete
    NativeAgentFailed -> BlockFailed
    NativeAgentCancelled -> BlockCancelled

newNativeAgentView
    :: Text
    -> Maybe Text
    -> Text
    -> Maybe Text
    -> NativeAgentView
newNativeAgentView identifier parent label model =
    NativeAgentView
        { nativeAgentId = identifier
        , nativeAgentParent = parent
        , nativeAgentLabel = label
        , nativeAgentModel = model
        , nativeAgentStatus = "running"
        , nativeAgentTranscript = []
        , nativeAgentConversation =
            reduceUi (UiLoop TurnStarted) initialUiState
        }

nativeAgentStatusText :: NativeAgentStatus -> Text
nativeAgentStatusText = \case
    NativeAgentRunning -> "running"
    NativeAgentCompleted -> "done"
    NativeAgentFailed -> "error"
    NativeAgentCancelled -> "cancelled"

nativeAgentEntry
    :: Map.Map Text NativeAgentView
    -> NativeAgentView
    -> AgentEntry
nativeAgentEntry agents view =
    AgentEntry
        { agentTarget = AgentNative view.nativeAgentId
        , agentPath = nativeAgentPath agents Set.empty view
        , agentStatus = view.nativeAgentStatus
        , agentModel = view.nativeAgentModel
        , agentSteps = []
        , agentTranscript = view.nativeAgentTranscript
        , agentConversation = view.nativeAgentConversation
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
