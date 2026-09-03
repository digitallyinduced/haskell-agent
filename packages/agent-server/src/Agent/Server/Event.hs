-- | Safe, bounded projection of runtime events onto the public SSE protocol.
module Agent.Server.Event
    ( projectLoopEvent
    , projectAgentEntries
    , projectPublicValue
    , boundedPublicText
    ) where

import Agent.CLI.AgentViewport
    ( AgentEntry(..)
    , AgentStep(..)
    , AgentStepState(..)
    )
import Agent.Loop
    ( LoopEvent(..)
    , NativeAgentStatus(..)
    , TokenUsage(..)
    , TurnCompletion(..)
    , TurnOutput(..)
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    , toolCallResultImages
    )
import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text qualified as Text

-- | One projected runtime event. Event names are namespaced separately from
-- supervisor lifecycle events such as @turn.started@ and @turn.completed@.
projectLoopEvent :: LoopEvent -> (Text, Value)
projectLoopEvent = \case
    TextDelta delta ->
        ("response.text.delta", textPayload delta)
    ReasoningDelta delta ->
        ("response.reasoning.delta", textPayload delta)
    ActivityUpdated message ->
        ("turn.activity", textPayload message)
    ProviderLimitUpdated text warning ->
        ( "provider.limit"
        , object
            [ "text" .= fst (boundedPublicText text)
            , "warning" .= warning
            , "truncated" .= snd (boundedPublicText text)
            ]
        )
    WarningRaised message ->
        ("warning", textPayload message)
    ResponseRestarted reason ->
        ( "response.restarted"
        , object
            [ "reason" .= fst (boundedPublicText reason)
            , "truncated" .= snd (boundedPublicText reason)
            -- Failed-attempt material is presentation state only. Consumers
            -- must never append it to canonical model history.
            , "displayOnly" .= True
            ]
        )
    TurnStarted ->
        ("agent.turn.started", object [])
    TurnFinished output ->
        ( "agent.turn.finished"
        , object
            [ "responseId" .= publicText output.responseId
            , "assistantText" .=
                fmap (fst . boundedPublicText) output.assistantText
            , "assistantTextTruncated" .=
                maybe False (snd . boundedPublicText) output.assistantText
            , "usage" .= usageValue output.tokenUsage
            , "completion" .= completionValue output.completion
            ]
        )
    ToolStarted call ->
        ("tool.started", toolCallValue call)
    ToolUpdated call ->
        ("tool.updated", toolCallValue call)
    ToolArgumentsUpdated call ->
        ("tool.arguments.updated", toolCallValue call)
    ToolOutputUpdated name output ->
        ( "tool.output.updated"
        , object
            [ "name" .= publicText name
            , "output" .= fst (boundedPublicText output)
            , "truncated" .= snd (boundedPublicText output)
            ]
        )
    ToolFinished result ->
        ("tool.finished", toolResultValue result)
    ToolRetracted callId ->
        ( "tool.retracted"
        , object
            [ "callId" .= publicText callId
            , "displayOnly" .= True
            ]
        )
    ResponseAttemptDiscarded ->
        ( "response.attempt.discarded"
        , object ["displayOnly" .= True]
        )
    ResponseAttemptFailed ->
        ( "response.attempt.failed"
        , object ["displayOnly" .= True]
        )
    NativeAgentStarted agentId parentId label model ->
        ( "agent.started"
        , object
            [ "agentId" .= publicText agentId
            , "parentId" .= fmap publicText parentId
            , "label" .= publicText label
            , "model" .= fmap publicText model
            ]
        )
    NativeAgentOutput agentId output ->
        ( "agent.output"
        , object
            [ "agentId" .= publicText agentId
            , "output" .= fst (boundedPublicText output)
            , "truncated" .= snd (boundedPublicText output)
            ]
        )
    NativeAgentFinished agentId status ->
        ( "agent.finished"
        , object
            [ "agentId" .= publicText agentId
            , "status" .= nativeAgentStatusText status
            ]
        )

-- | Public agent snapshots intentionally omit transcripts and retained UI
-- state. Those may contain arbitrarily large or sensitive model/tool output.
projectAgentEntries :: [AgentEntry] -> Value
projectAgentEntries entries =
    projectPublicValue $
    toJSONList
        [ object
            [ "path" .= publicText entry.agentPath
            , "status" .= publicText entry.agentStatus
            , "model" .= fmap publicText entry.agentModel
            , "steps" .=
                [ object
                    [ "state" .= agentStepStateText step.agentStepState
                    , "title" .= fst
                        (boundedPublicText step.agentStepTitle)
                    , "detail" .= fmap
                        (fst . boundedPublicText)
                        step.agentStepDetail
                    ]
                | step <- take 100 entry.agentSteps
                ]
            ]
        | entry <- take 100 entries
        ]

-- | Recursively redact opaque provider ciphertext and cap the total public
-- JSON projection. This is used for durable history, including unknown
-- provider response-item variants that cannot safely be projected by
-- constructor alone.
projectPublicValue :: Value -> Value
projectPublicValue value =
    let (projected, _, truncated) =
            projectValue initialProjectionBudget value
    in case projected of
        Aeson.Object fields
            | truncated ->
                Aeson.Object
                    (KeyMap.insert "projectionTruncated" (Aeson.Bool True) fields)
        _ -> projected

data ProjectionBudget = ProjectionBudget
    { projectionTextRemaining :: !Int
    , projectionNodesRemaining :: !Int
    }

initialProjectionBudget :: ProjectionBudget
initialProjectionBudget = ProjectionBudget
    { projectionTextRemaining = 64 * 1024
    , projectionNodesRemaining = 2048
    }

projectValue
    :: ProjectionBudget
    -> Value
    -> (Value, ProjectionBudget, Bool)
projectValue budget value
    | budget.projectionNodesRemaining <= 0 =
        (Aeson.Null, budget, True)
    | otherwise =
        case value of
            Aeson.String textValue ->
                let available =
                        min
                            maximumPublicTextChars
                            budgetAfterNode.projectionTextRemaining
                    projected = Text.take available textValue
                    truncated = Text.length textValue > available
                in ( Aeson.String projected
                   , budgetAfterNode
                        { projectionTextRemaining =
                            budgetAfterNode.projectionTextRemaining
                                - Text.length projected
                        }
                   , truncated
                   )
            Aeson.Array values ->
                let (items, remaining, truncated) =
                        projectValues budgetAfterNode (toList values)
                in (Aeson.toJSON items, remaining, truncated)
            Aeson.Object fields ->
                let (entries, remaining, truncated) =
                        projectFields
                            budgetAfterNode
                            (KeyMap.toList fields)
                in ( Aeson.Object (KeyMap.fromList entries)
                   , remaining
                   , truncated
                   )
            primitive -> (primitive, budgetAfterNode, False)
  where
    budgetAfterNode = budget
        { projectionNodesRemaining =
            budget.projectionNodesRemaining - 1
        }

projectValues
    :: ProjectionBudget
    -> [Value]
    -> ([Value], ProjectionBudget, Bool)
projectValues budget = \case
    [] -> ([], budget, False)
    remainingValues
        | budget.projectionNodesRemaining <= 0 ->
            ([], budget, not (null remainingValues))
    value : rest ->
        let (projected, afterValue, valueTruncated) =
                projectValue budget value
            (projectedRest, finalBudget, restTruncated) =
                projectValues afterValue rest
        in ( projected : projectedRest
           , finalBudget
           , valueTruncated || restTruncated
           )

projectFields
    :: ProjectionBudget
    -> [(Key.Key, Value)]
    -> ([(Key.Key, Value)], ProjectionBudget, Bool)
projectFields budget = \case
    [] -> ([], budget, False)
    remainingFields
        | budget.projectionNodesRemaining <= 0 ->
            ([], budget, not (null remainingFields))
    (key, value) : rest
        | keyLength > maximumPublicKeyChars
            || keyLength > budget.projectionTextRemaining ->
                ([], budget, True)
        | sensitivePublicKey key ->
            let afterKey = budget
                    { projectionTextRemaining =
                        budget.projectionTextRemaining - keyLength
                    }
                (redacted, afterValue, _) =
                    projectValue afterKey (Aeson.String "<redacted>")
                (projectedRest, finalBudget, restTruncated) =
                    projectFields afterValue rest
            in ( (key, redacted) : projectedRest
               , finalBudget
               , restTruncated
               )
        | otherwise ->
            let afterKey = budget
                    { projectionTextRemaining =
                        budget.projectionTextRemaining - keyLength
                    }
                (projected, afterValue, valueTruncated) =
                    projectValue afterKey value
                (projectedRest, finalBudget, restTruncated) =
                    projectFields afterValue rest
            in ( (key, projected) : projectedRest
               , finalBudget
               , valueTruncated || restTruncated
               )
      where
        keyLength = Text.length (Key.toText key)

maximumPublicKeyChars :: Int
maximumPublicKeyChars = 256

sensitivePublicKey :: Key.Key -> Bool
sensitivePublicKey key =
    Text.toLower (Key.toText key)
        `elem`
            [ "encrypted_content"
            , "encryptedcontent"
            , "encrypted_function_args"
            , "encryptedfunctionargs"
            ]

boundedPublicText :: Text -> (Text, Bool)
boundedPublicText value
    | Text.length value <= maximumPublicTextChars = (value, False)
    | otherwise =
        (Text.take maximumPublicTextChars value <> "…", True)

maximumPublicTextChars :: Int
-- A Unicode scalar takes at most four UTF-8 bytes, keeping this below 64 KiB.
maximumPublicTextChars = 16 * 1024 - 1

publicText :: Text -> Text
publicText = fst . boundedPublicText

textPayload :: Text -> Value
textPayload value = object
    [ "text" .= fst bounded
    , "truncated" .= snd bounded
    ]
  where
    bounded = boundedPublicText value

toolCallValue :: ToolCall -> Value
toolCallValue call = object
    [ "callId" .= publicText call.callId
    , "name" .= publicText call.name
    , "kind" .= toolCallKindText call.callKind
    , "argumentsEncrypted" .= call.argumentsEncrypted
    , "arguments" .=
        if call.argumentsEncrypted
            then Nothing
            else Just (fst (boundedPublicText call.arguments))
    , "argumentsTruncated" .=
        (not call.argumentsEncrypted
            && snd (boundedPublicText call.arguments))
    ]

toolResultValue :: ToolCallResult -> Value
toolResultValue result = object
    [ "callId" .= publicText result.callId
    , "kind" .= toolCallKindText result.callKind
    , "output" .= fst (boundedPublicText result.output)
    , "truncated" .= snd (boundedPublicText result.output)
    -- Deliberately do not serialize image URLs/data URLs into SSE.
    , "imageCount" .= length (toolCallResultImages result)
    ]

usageValue :: TokenUsage -> Value
usageValue usage = object
    [ "inputTokens" .= usage.inputTokens
    , "outputTokens" .= usage.outputTokens
    , "cachedTokens" .= usage.cachedTokens
    ]

completionValue :: TurnCompletion -> Value
completionValue = \case
    TurnCompleted -> object ["status" .= ("completed" :: Text)]
    TurnIncomplete reason reasoningTokens -> object
        [ "status" .= ("incomplete" :: Text)
        , "reason" .= fst (boundedPublicText reason)
        , "reasoningTokens" .= reasoningTokens
        ]

toolCallKindText :: ToolCallKind -> Text
toolCallKindText = \case
    FunctionCallKind -> "function"
    CustomCallKind -> "custom"
    ComputerCallKind -> "computer"
    ComputerFunctionCallKind -> "computer_function"

nativeAgentStatusText :: NativeAgentStatus -> Text
nativeAgentStatusText = \case
    NativeAgentRunning -> "running"
    NativeAgentCompleted -> "completed"
    NativeAgentFailed -> "failed"
    NativeAgentCancelled -> "cancelled"

agentStepStateText :: AgentStepState -> Text
agentStepStateText = \case
    AgentStepRunning -> "running"
    AgentStepCompleted -> "completed"
    AgentStepFailed -> "failed"
    AgentStepInfo -> "info"

toJSONList :: [Value] -> Value
toJSONList = Aeson.toJSON
