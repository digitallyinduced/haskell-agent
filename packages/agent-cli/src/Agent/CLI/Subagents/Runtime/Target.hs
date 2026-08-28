-- | Compatibility checks for persisted and resident child-agent targets.
module Agent.CLI.Subagents.Runtime.Target
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
import Agent.CLI.Subagents.Runtime.Types (SubagentSession(..))
import Agent.Dialect (DialectId, dialectSlug)
import Agent.Provider (Provider, providerSlug)
import Agent.Subagents (SubagentId(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)

validatePersistedSubagentTarget
    :: Provider -> Text -> Text -> DialectId -> Maybe LegacySubagentTarget
    -> SubagentDiskMeta -> Either Text SubagentTarget
validatePersistedSubagentTarget
        provider connection effectiveModel dialect legacyTarget meta = do
    let expected = SubagentTarget provider connection effectiveModel dialect
    stored <- normalizePersistedSubagentTarget legacyTarget expected meta
    maybe (Right ()) Left (subagentTargetError expected stored)
    Right stored

normalizePersistedSubagentTarget
    :: Maybe LegacySubagentTarget -> SubagentTarget -> SubagentDiskMeta
    -> Either Text SubagentTarget
normalizePersistedSubagentTarget legacyTarget expected = \case
    CurrentSubagentDiskMeta _ stored -> Right stored
    LegacySubagentDiskMeta _ fields -> do
        legacyDialect <- maybe
            (Left "cannot restore a legacy subagent with incomplete target metadata after changing the session target; reopen the parent session under its original target first")
            Right
            (legacyDialectForTarget legacyTarget expected)
        Right SubagentTarget
            { targetProvider = fromMaybe expected.targetProvider
                fields.legacyDiskProvider
            , targetConnection = fromMaybe expected.targetConnection
                fields.legacyDiskConnection
            , targetEffectiveModel = fromMaybe expected.targetEffectiveModel
                fields.legacyDiskEffectiveModel
            , targetDialect = fromMaybe legacyDialect fields.legacyDiskDialect
            }

legacyDialectForTarget
    :: Maybe LegacySubagentTarget -> SubagentTarget -> Maybe DialectId
legacyDialectForTarget target expected = do
    legacy <- target
    if legacy.legacyTargetProvider == expected.targetProvider
        && legacy.legacyTargetConnection == expected.targetConnection
        && legacy.legacyTargetEffectiveModel == expected.targetEffectiveModel
        && legacy.legacyTargetDialect == expected.targetDialect
        then Just expected.targetDialect else Nothing

activeSubagentTargetError
    :: Provider -> Text -> Text -> SubagentSession -> Maybe Text
activeSubagentTargetError provider connection effectiveModel session =
    subagentTargetError
        (SubagentTarget provider connection effectiveModel session.subSessionDialect)
        (SubagentTarget session.subSessionProvider session.subSessionConnection
            session.subSessionEffectiveModel session.subSessionDialect)

subagentTargetError :: SubagentTarget -> SubagentTarget -> Maybe Text
subagentTargetError expected stored
    | stored.targetProvider /= expected.targetProvider =
        Just ("cannot continue subagent created for the "
            <> providerSlug stored.targetProvider <> " transport under "
            <> providerSlug expected.targetProvider)
    | stored.targetConnection /= expected.targetConnection =
        Just ("cannot continue subagent created for connection "
            <> stored.targetConnection <> " under connection "
            <> expected.targetConnection)
    | stored.targetEffectiveModel /= expected.targetEffectiveModel =
        Just ("cannot continue subagent after its effective model changed from "
            <> stored.targetEffectiveModel <> " to "
            <> expected.targetEffectiveModel)
    | otherwise = Nothing

unsupportedDialectMessage :: Provider -> SubagentId -> DialectId -> Text
unsupportedDialectMessage provider agentId dialect =
    "cannot restore subagent " <> agentId.unSubagentId <> ": dialect "
        <> dialectSlug dialect <> " is not supported by the current "
        <> providerSlug provider <> " transport"
