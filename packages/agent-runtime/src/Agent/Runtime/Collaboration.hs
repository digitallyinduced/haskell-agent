-- | Frontend-neutral collaboration startup policy and registry ownership.
-- Hosts supply their live admitted model catalog and persistence callback;
-- neither terminal interaction nor worktree management belongs here.
module Agent.Runtime.Collaboration
    ( CollaborationModels(..)
    , resolveMaxConcurrentAgents
    , shouldLoadOpenAiChild
    , resolveCollaborationModels
    , acquireCollaborationRegistry
    ) where

import Agent.GrokBuild.Dialect.Task (grokRootChildModels)
import Agent.Loop (LoopError(..))
import Agent.Provider (Provider(..))
import Agent.ResourceScope (logSlowCleanup)
import Agent.Runtime.Models
    ( ModelOption(..), ModelTarget(..), resolveModelOptionById )
import Agent.Subagents
    ( SubagentConfig(..), SubagentRegistry, closeSubagentRegistry
    , defaultMaxConcurrent, defaultSubagentConfig, interruptActiveSubagents
    , newSubagentRegistry )
import Agent.Tools.MultiAgents (CollaborationModelTarget(..))
import Control.Applicative ((<|>))
import Control.Exception.Safe (finally)
import Data.Acquire (Acquire, mkAcquire)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (OsPath)

resolveMaxConcurrentAgents :: Maybe Int -> Maybe Int -> Maybe Int -> Int
resolveMaxConcurrentAgents explicit project harness =
    fromMaybe defaultMaxConcurrent (explicit <|> project <|> harness)

-- | Gateway sessions must never load local credentials to extend admission.
shouldLoadOpenAiChild :: Bool -> Maybe [Text] -> Provider -> Bool
shouldLoadOpenAiChild enabled gatewayModels provider =
    enabled && isNothing gatewayModels && provider == XAIProvider

data CollaborationModels = CollaborationModels
    { childAllowedModels :: Maybe [Text]
    , childModelAllowed :: Maybe (Text -> IO Bool)
    , childResolveModel :: Maybe (Text -> IO (Maybe CollaborationModelTarget))
    , childGatewayModelOption :: Maybe (Text -> IO (Maybe ModelOption))
    }

-- | Keep gateway resolution live: the advertised startup list is not a
-- substitute for the currently admitted catalog. An unavailable catalog
-- fails closed rather than falling back to local provider routing.
resolveCollaborationModels
    :: Provider
    -> Maybe [Text]
    -> Bool
    -> IO (Maybe [ModelOption])
    -> CollaborationModels
resolveCollaborationModels provider gatewayModels hasOpenAiChild loadCatalog =
    CollaborationModels{..}
  where
    childAllowedModels = case gatewayModels of
        Just modelIds -> Just modelIds
        Nothing -> case provider of
            XAIProvider -> Just (grokRootChildModels hasOpenAiChild)
            _ -> Nothing
    childGatewayModelOption
        | isNothing gatewayModels = Nothing
        | otherwise = Just \requested ->
            loadCatalog >>= \case
                Nothing -> pure Nothing
                Just models ->
                    pure (resolveModelOptionById models (Text.strip requested))
    childModelAllowed = fmap (\resolve modelId -> isJust <$> resolve modelId)
        childGatewayModelOption
    childResolveModel = fmap
        (\resolve modelId -> fmap toTarget <$> resolve modelId)
        childGatewayModelOption
    toTarget option =
        let target = option.modelTarget
        in CollaborationModelTarget
            { collaborationTargetProvider = target.targetProvider
            , collaborationTargetConnection = target.targetConnectionId
            , collaborationTargetEffectiveModel = target.targetWireModelId
            , collaborationTargetDialect = target.targetDialect
            }

-- | Register ownership before later initialization. Snapshot interrupted
-- agents while their registry remains available, then always close and join
-- its supervisors, even if persistence fails or the owner is cancelled.
acquireCollaborationRegistry
    :: Int
    -> OsPath
    -> (SubagentRegistry -> IO ())
    -> Acquire SubagentRegistry
acquireCollaborationRegistry concurrency cwd snapshot = mkAcquire
    (newSubagentRegistry
        defaultSubagentConfig { maxConcurrent = concurrency }
        cwd
        (\_ _ _ _ -> pure $ Left LoopNoResponseId)
        (\_ _ -> pure ()))
    (\registry -> logSlowCleanup "collaboration agents" $
        (interruptActiveSubagents registry >> snapshot registry)
            `finally` closeSubagentRegistry registry)
