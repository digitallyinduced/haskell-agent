-- | Frontend-neutral startup context policy and quiet discovery. Presentation
-- and installation happen later, after persisted context has been considered.
module Agent.Runtime.Startup.Context
    ( SessionInitialContext(..)
    , resolveSessionInitialContext
    , resumeNeedsGeneratedContext
    , ContextPreloadPolicy(..)
    , preloadInitialContext
    , preloadAgentsContext
    , agentsDiscoverOptions
    , assembleInitialSkills
    , mergeSkillCatalogs
    ) where

import Agent.Dialect (Dialect)
import Agent.OpenAI.Compaction (hasReloadedGeneratedContextItems)
import Agent.ProjectInstructions
    ( DiscoverOptions(..), LoadedAgentsMd, defaultDiscoverOptions
    , discoverProjectInstructions )
import Agent.Responses.Types (ResponseItem)
import Agent.Runtime.Session (SessionMeta(..), SessionTurn(..))
import Agent.Runtime.Session.History (foldSessionItems)
import Agent.Runtime.Session.Types (TranscriptEffect(..))
import Agent.Runtime.Tools.Dialects (globalAgentsHomeDir)
import Agent.Skills (SkillCatalog(..))
import Control.Concurrent.Async (concurrently)
import Data.Maybe (isJust)
import Data.Text (Text)
import System.OsPath (OsPath)

data SessionInitialContext = SessionInitialContext
    { initialContextItems :: [ResponseItem]
    , initialContextResumeNeedsFresh :: Bool
    , initialContextPrevious :: Maybe Text
    , initialContextNeeded :: Bool
    , initialContextMayRestoreSnapshot :: Bool
    }
    deriving (Eq, Show)

resolveSessionInitialContext
    :: Bool
    -- ^ A session transition is pending.
    -> Bool
    -- ^ The resumed provider/connection/wire target changed.
    -> Maybe (SessionMeta, [SessionTurn])
    -> SessionInitialContext
resolveSessionInitialContext hasTransition resumeTargetChanged resumed =
    SessionInitialContext{..}
  where
    initialTurns = maybe [] snd resumed
    initialContextItems = maybe [] (foldSessionItems . snd) resumed
    initialContextResumeNeedsFresh = resumeNeedsGeneratedContext initialTurns
    initialContextPrevious
        | hasTransition || resumeTargetChanged = Nothing
        | otherwise = resumed >>= \(meta, _) -> meta.metaLastResponseId
    initialContextNeeded =
        initialContextResumeNeedsFresh
            || (null initialTurns && initialContextPrevious == Nothing)
    initialContextMayRestoreSnapshot =
        case resumed of
            Just (meta, turns) ->
                null turns
                    && initialContextPrevious == Nothing
                    && isJust meta.metaPromptSnapshot
            Nothing -> False

-- | Reload after the newest durable transcript replacement until a later
-- persisted turn proves generated context was consumed.
resumeNeedsGeneratedContext :: [SessionTurn] -> Bool
resumeNeedsGeneratedContext turns =
    case break isContextBoundary (reverse turns) of
        (_, []) -> False
        (newerTurns, _boundary : _) ->
            null newerTurns
                || not
                    (any
                        (hasReloadedGeneratedContextItems . (.turnItems))
                        newerTurns)
  where
    isContextBoundary turn = turn.turnEffect /= TranscriptAppend

data ContextPreloadPolicy = ContextPreloadPolicy
    { contextLoadsHostWorkspace :: Bool
    , contextRefreshDialect :: Bool
    }
    deriving (Eq, Show)

-- | Overlap quiet reads, skipping filesystem discovery when a snapshot may
-- satisfy startup. Learned skills remain independent of workspace permission.
-- Structured concurrency joins either reader on failure or cancellation.
preloadInitialContext
    :: ContextPreloadPolicy
    -> SessionInitialContext
    -> IO (Maybe agents)
    -> IO (Maybe skills)
    -> IO (Maybe agents, Maybe skills)
preloadInitialContext policy requirements loadAgents loadLearnedSkills =
    concurrently preloadAgents preloadLearnedSkills
  where
    preloadAgents
        | policy.contextLoadsHostWorkspace
            && (requirements.initialContextNeeded || policy.contextRefreshDialect)
            && (policy.contextRefreshDialect
                || not requirements.initialContextMayRestoreSnapshot) =
            loadAgents
        | otherwise = pure Nothing
    preloadLearnedSkills
        | requirements.initialContextNeeded = loadLearnedSkills
        | otherwise = pure Nothing

preloadAgentsContext
    :: Bool -> Dialect -> OsPath -> OsPath -> IO (Maybe LoadedAgentsMd)
preloadAgentsContext enabled dialect home cwd
    | not enabled = pure Nothing
    | otherwise =
        Just <$> discoverProjectInstructions (agentsDiscoverOptions dialect home) cwd

agentsDiscoverOptions :: Dialect -> OsPath -> DiscoverOptions
agentsDiscoverOptions dialect home =
    DiscoverOptions
        { discoverMaxBytes = defaultDiscoverOptions.discoverMaxBytes
        , discoverGlobalDir = Just (globalAgentsHomeDir dialect home)
        , discoverRootMarkers = defaultDiscoverOptions.discoverRootMarkers
        }

assembleInitialSkills :: Bool -> SkillCatalog -> IO SkillCatalog -> IO SkillCatalog
assembleInitialSkills enabled local loadRemote =
    mergeSkillCatalogs local <$> if enabled then loadRemote else pure (SkillCatalog [] [])

mergeSkillCatalogs :: SkillCatalog -> SkillCatalog -> SkillCatalog
mergeSkillCatalogs local remote =
    SkillCatalog
        (local.catalogSkills <> remote.catalogSkills)
        (local.catalogWarnings <> remote.catalogWarnings)
