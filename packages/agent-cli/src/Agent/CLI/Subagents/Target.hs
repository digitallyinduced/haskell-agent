module Agent.CLI.Subagents.Target
    ( activeSubagentTargetError
    , unsupportedDialectMessage
    , validatePersistedSubagentTarget
    ) where

import Agent.CLI.Session (LegacySubagentTarget(..))
import Agent.CLI.SubagentStore
    ( LegacySubagentTargetFields(..)
    , SubagentDiskMeta(..)
    , SubagentTarget(..)
    )
import Agent.CLI.Subagents.Types (SubagentSession(..))
import Agent.Dialect (DialectId, dialectSlug)
import Agent.Provider (Provider, providerSlug)
import Agent.Subagents (SubagentId(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)

validatePersistedSubagentTarget
    :: Provider
    -> Text
    -> Text
    -> DialectId
    -> Maybe LegacySubagentTarget
    -> SubagentDiskMeta
    -> Either Text SubagentTarget
validatePersistedSubagentTarget
        provider connection expectedEffectiveModel expectedDialect legacyTarget meta = do
    let expectedTarget = SubagentTarget
            { targetProvider = provider
            , targetConnection = connection
            , targetEffectiveModel = expectedEffectiveModel
            , targetDialect = expectedDialect
            }
    storedTarget <- normalizePersistedSubagentTarget
        legacyTarget
        expectedTarget
        meta
    case subagentTargetError expectedTarget storedTarget of
        Just err -> Left err
        Nothing -> Right ()
    Right storedTarget

normalizePersistedSubagentTarget
    :: Maybe LegacySubagentTarget
    -> SubagentTarget
    -> SubagentDiskMeta
    -> Either Text SubagentTarget
normalizePersistedSubagentTarget legacyTarget expectedTarget = \case
    CurrentSubagentDiskMeta _ storedTarget ->
        Right storedTarget
    LegacySubagentDiskMeta _ legacyFields -> do
        legacyDialect <- case
                legacyDialectForTarget legacyTarget expectedTarget
                of
            Just dialect -> Right dialect
            Nothing ->
                Left
                    "cannot restore a legacy subagent with incomplete target \
                    \metadata after changing the session target; reopen the \
                    \parent session under its original target first"
        Right SubagentTarget
            { targetProvider =
                fromMaybe
                    expectedTarget.targetProvider
                    legacyFields.legacyDiskProvider
            , targetConnection =
                fromMaybe
                    expectedTarget.targetConnection
                    legacyFields.legacyDiskConnection
            , targetEffectiveModel =
                fromMaybe
                    expectedTarget.targetEffectiveModel
                    legacyFields.legacyDiskEffectiveModel
            , targetDialect =
                fromMaybe legacyDialect legacyFields.legacyDiskDialect
            }

legacyDialectForTarget
    :: Maybe LegacySubagentTarget
    -> SubagentTarget
    -> Maybe DialectId
legacyDialectForTarget target expectedTarget = do
    legacy <- target
    if legacy.legacyTargetProvider == expectedTarget.targetProvider
        && legacy.legacyTargetConnection == expectedTarget.targetConnection
        && legacy.legacyTargetEffectiveModel
            == expectedTarget.targetEffectiveModel
        && legacy.legacyTargetDialect == expectedTarget.targetDialect
        then Just expectedTarget.targetDialect
        else Nothing

activeSubagentTargetError
    :: Provider
    -> Text
    -> Text
    -> SubagentSession
    -> Maybe Text
activeSubagentTargetError provider connection effectiveModel session =
    subagentTargetError
        SubagentTarget
            { targetProvider = provider
            , targetConnection = connection
            , targetEffectiveModel = effectiveModel
            , targetDialect = session.subSessionDialect
            }
        SubagentTarget
            { targetProvider = session.subSessionProvider
            , targetConnection = session.subSessionConnection
            , targetEffectiveModel = session.subSessionEffectiveModel
            , targetDialect = session.subSessionDialect
            }

subagentTargetError
    :: SubagentTarget
    -> SubagentTarget
    -> Maybe Text
subagentTargetError expected stored
    | stored.targetProvider /= expected.targetProvider =
        Just
            ( "cannot continue subagent created for the "
                <> providerSlug stored.targetProvider
                <> " transport under "
                <> providerSlug expected.targetProvider
            )
    | stored.targetConnection /= expected.targetConnection =
        Just
            ( "cannot continue subagent created for connection "
                <> stored.targetConnection
                <> " under connection "
                <> expected.targetConnection
            )
    | stored.targetEffectiveModel /= expected.targetEffectiveModel =
        Just
            ( "cannot continue subagent after its effective model changed \
                \from "
                <> stored.targetEffectiveModel
                <> " to "
                <> expected.targetEffectiveModel
            )
    | otherwise = Nothing

unsupportedDialectMessage :: Provider -> SubagentId -> DialectId -> Text
unsupportedDialectMessage provider agentId storedDialect =
    "cannot restore subagent "
        <> agentId.unSubagentId
        <> ": dialect "
        <> dialectSlug storedDialect
        <> " is not supported by the current "
        <> providerSlug provider
        <> " transport"
