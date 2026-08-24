-- | Translate the reusable SDK's typed message stream into the
-- provider-neutral events expected by the harness.
module Agent.Claude.Internal.Messages
    ( CompletedClaudeTurn(..)
    , interpretClaudeTurn
    ) where

import Agent.Loop (LoopEvent(..))
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    )
import Claude.Agent.SDK.Types
    ( AssistantMessage(..)
    , ContentBlock(..)
    , Message(..)
    , ResultMessage(..)
    , SystemMessage(..)
    , Usage(..)
    , UserMessage(..)
    , addUsage
    , emptyUsage
    , messageHasParentToolUseId
    , modelUsageToUsage
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)

data CompletedClaudeTurn = CompletedClaudeTurn
    { sessionId :: !Text
    , assistantText :: !(Maybe Text)
    , events :: ![LoopEvent]
    , tokenUsage :: !Usage
    , cumulativeModelUsage :: !(Maybe Usage)
    } deriving (Eq, Show)

interpretClaudeTurn
    :: [Message]
    -> ResultMessage
    -> Either Text CompletedClaudeTurn
interpretClaudeTurn messages result = do
    let visibleMessages =
            filter (not . messageHasParentToolUseId) messages
    validateSubscriptionSource visibleMessages
    let
        bufferedAssistantText =
            Text.concat
                [ text
                | MessageAssistant assistant <- visibleMessages
                , assistant.error == Nothing
                , TextBlock{text} <- assistant.content
                ]
        assistantText =
            firstNonEmptyText
                [ maybe "" id result.result
                , bufferedAssistantText
                ]
        toolEvents =
            canonicalToolEvents
                (concatMap messageToolEvents visibleMessages)
        events =
            toolEvents
                <> maybe [] (pure . TextDelta) assistantText
        cumulative =
            case Map.elems result.modelUsage of
                [] -> Nothing
                modelUsages ->
                    Just (foldl' addUsage emptyUsage
                        (map modelUsageToUsage modelUsages))
    pure CompletedClaudeTurn
        { sessionId = result.sessionId
        , assistantText
        , events
        , tokenUsage = result.usage
        , cumulativeModelUsage = cumulative
        }

validateSubscriptionSource :: [Message] -> Either Text ()
validateSubscriptionSource messages =
    case
        [ system.apiKeySource
        | MessageSystem system <- messages
        , system.subtype == "init"
        ] of
        [] ->
            Left
                "Claude Code completed before confirming subscription authentication."
        sources
            | Just source <- firstUnexpected sources ->
                Left
                    ( "Claude Code selected non-subscription credential source "
                        <> source
                        <> "."
                    )
            | Nothing `elem` sources ->
                Left
                    "Claude Code did not identify its credential source."
            | otherwise ->
                Right ()
  where
    firstUnexpected =
        foldr
            (\source found ->
                case source of
                    Just "none" -> found
                    Just value -> Just value
                    Nothing -> found)
            Nothing

data ClaudeToolEvent
    = ClaudeToolStarted !ToolCall
    | ClaudeToolFinished !ToolCallResult

messageToolEvents :: Message -> [ClaudeToolEvent]
messageToolEvents = \case
    MessageAssistant assistant
        | assistant.error == Nothing ->
            concatMap assistantBlockEvents assistant.content
    MessageUser user ->
        concatMap userBlockEvents user.content
    _ ->
        []

assistantBlockEvents :: ContentBlock -> [ClaudeToolEvent]
assistantBlockEvents = \case
    ToolUseBlock{toolUseId, name, input} ->
        [ ClaudeToolStarted ToolCall
            { callId = toolUseId
            , name
            , arguments = encodeValue input
            , callKind = FunctionCallKind
            , argumentsEncrypted = False
            }
        ]
    _ ->
        []

userBlockEvents :: ContentBlock -> [ClaudeToolEvent]
userBlockEvents = \case
    ToolResultBlock{toolUseId, content, isError} ->
        let rawOutput = maybe "" renderResultContent content
            output
                | isError == Just True = "Error: " <> rawOutput
                | otherwise = rawOutput
        in
            [ ClaudeToolFinished ToolCallResult
                { callId = toolUseId
                , output
                , callKind = FunctionCallKind
                }
            ]
    _ ->
        []

canonicalToolEvents :: [ClaudeToolEvent] -> [LoopEvent]
canonicalToolEvents toolEvents =
    reverse eventsRev
  where
    (_, _, eventsRev) =
        foldl' step (Set.empty, Set.empty, []) toolEvents
    step
        :: (Set Text, Set Text, [LoopEvent])
        -> ClaudeToolEvent
        -> (Set Text, Set Text, [LoopEvent])
    step (started, finished, events) = \case
        ClaudeToolStarted call
            | Set.member call.callId started ->
                (started, finished, events)
            | otherwise ->
                ( Set.insert call.callId started
                , finished
                , ToolStarted call : events
                )
        ClaudeToolFinished result
            | not (Set.member result.callId started)
                || Set.member result.callId finished ->
                (started, finished, events)
            | otherwise ->
                ( started
                , Set.insert result.callId finished
                , ToolFinished result : events
                )

renderResultContent :: Aeson.Value -> Text
renderResultContent = \case
    Aeson.String text -> text
    Aeson.Array values ->
        Text.intercalate "\n"
            (map renderResultContent (toList values))
    Aeson.Object object ->
        case KeyMap.lookup "text" object of
            Just (Aeson.String text) -> text
            _ -> encodeValue (Aeson.Object object)
    Aeson.Null -> ""
    other -> encodeValue other

encodeValue :: Aeson.Value -> Text
encodeValue =
    TextEncoding.decodeUtf8With lenientDecode
        . LazyByteString.toStrict
        . Aeson.encode

firstNonEmptyText :: [Text] -> Maybe Text
firstNonEmptyText values =
    case filter (not . Text.null) values of
        value : _ -> Just value
        [] -> Nothing
