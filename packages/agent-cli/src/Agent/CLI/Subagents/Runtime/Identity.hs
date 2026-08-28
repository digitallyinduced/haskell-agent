-- | Provider and model identity decisions for spawned child agents.
module Agent.CLI.Subagents.Runtime.Identity
    ( grokSpawnedChildIdentity
    , inheritedGrokChildModel
    , usesOpenAiChildTransport
    ) where

import Agent.CLI.Subagents.Runtime.Types (SubagentRuntime(..))
import Agent.Dialect
    ( DialectId
    , codexDialect
    , dialectId
    , dialectIdForModel
    )
import Agent.GrokBuild.Dialect.Task
    ( canonicalizeGrokChildModel
    , isLunaSubagentModel
    , lunaSubagentModel
    )
import Agent.Provider (Provider(..), providerSlug)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)

-- | Identity for a Grok-root child, including OpenAI Luna when requested.
grokSpawnedChildIdentity
    :: Provider
    -> Text
    -> (Text -> Text)
    -> Text
    -> DialectId
    -> Maybe Text
    -> (Provider, Text, Text, DialectId)
grokSpawnedChildIdentity
        parentProvider parentConnection mapModel parentModel parentDialect childModel =
    case childModel >>= canonicalizeGrokChildModel of
        Just model
            | isLunaSubagentModel model ->
                ( OpenAIProvider
                , providerSlug OpenAIProvider
                , lunaSubagentModel
                , dialectId codexDialect
                )
            | otherwise ->
                (parentProvider, parentConnection, model, parentDialect)
        Nothing ->
            ( parentProvider
            , parentConnection
            , maybe parentModel mapModel childModel
            , maybe
                parentDialect
                (dialectIdForModel parentProvider . mapModel)
                childModel
            )

inheritedGrokChildModel :: SubagentRuntime -> Text -> Text
inheritedGrokChildModel runtime parentModel =
    case runtime.subagentAllowedChildModels of
        Nothing -> parentModel
        Just allowed ->
            case canonicalizeGrokChildModel parentModel of
                Just slug | slug `elem` allowed -> slug
                _ -> fromMaybe parentModel (listToMaybe allowed)

-- | Luna requests and already-OpenAI descendants stay on Codex/OpenAI.
usesOpenAiChildTransport
    :: Maybe Provider
    -> Maybe Provider
    -> Maybe Text
    -> Bool
usesOpenAiChildTransport childSessionProvider parentSessionProvider childModel =
    childSessionProvider == Just OpenAIProvider
        || parentSessionProvider == Just OpenAIProvider
        || maybe False isLunaSubagentModel
            (childModel >>= canonicalizeGrokChildModel)
