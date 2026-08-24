module Agent.Tools.Scheduling
    ( ToolAccess(..)
    , ToolResource(..)
    , ToolResourceClaim(..)
    , ToolSchedulingPlan(..)
    , SchedulingDecision(..)
    , nextSchedulingWave
    , schedulingPlansConflict
    ) where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import System.OsPath
    ( OsPath
    , equalFilePath
    , isAbsolute
    , makeRelative
    , splitDirectories
    , unsafeEncodeUtf
    )

data ToolAccess
    = ToolRead
    | ToolWrite
    deriving (Eq, Show)

data ToolResource
    = ToolAllResources
    | ToolPath !OsPath
    | ToolPathTree !OsPath
    | ToolNamedResource !Text
    deriving (Eq, Show)

data ToolResourceClaim = ToolResourceClaim
    { claimAccess :: !ToolAccess
    , claimResource :: !ToolResource
    } deriving (Eq, Show)

data ToolSchedulingPlan
    = ToolResourceClaims !(NonEmpty ToolResourceClaim)
    | ToolExclusive
    deriving (Eq, Show)

data SchedulingDecision key = SchedulingDecision
    { schedulingReady :: ![key]
    , schedulingBlocked :: ![(key, key)]
    } deriving (Eq, Show)

-- | Select the next deterministic scheduling wave. A 'Nothing' plan denotes
-- work that does not execute a handler (for example, an approval rejection);
-- it is always ready and never blocks another call.
nextSchedulingWave
    :: [(key, Maybe ToolSchedulingPlan)]
    -> SchedulingDecision key
nextSchedulingWave entries =
    SchedulingDecision
        { schedulingReady =
            [ key
            | (key, blocker) <- decisions
            , maybe True (const False) blocker
            ]
        , schedulingBlocked =
            [ (key, blocker)
            | (key, Just blocker) <- decisions
            ]
        }
  where
    indexed = zip [0 :: Int ..] entries
    decisions =
        [ (key, blockerFor position plan)
        | (position, (key, plan)) <- indexed
        ]
    blockerFor _ Nothing = Nothing
    blockerFor position (Just current) =
        firstConflicting
            [ (key, plan)
            | (_, (key, plan)) <- take position indexed
            ]
            current
    firstConflicting [] _ = Nothing
    firstConflicting ((_, Nothing) : rest) current =
        firstConflicting rest current
    firstConflicting ((key, Just earlier) : rest) current
        | schedulingPlansConflict earlier current = Just key
        | otherwise = firstConflicting rest current

schedulingPlansConflict
    :: ToolSchedulingPlan
    -> ToolSchedulingPlan
    -> Bool
schedulingPlansConflict ToolExclusive _ = True
schedulingPlansConflict _ ToolExclusive = True
schedulingPlansConflict (ToolResourceClaims left) (ToolResourceClaims right) =
    or
        [ claimsConflict leftClaim rightClaim
        | leftClaim <- toList left
        , rightClaim <- toList right
        ]

claimsConflict :: ToolResourceClaim -> ToolResourceClaim -> Bool
claimsConflict left right =
    resourcesOverlap left.claimResource right.claimResource
        && (left.claimAccess == ToolWrite || right.claimAccess == ToolWrite)

resourcesOverlap :: ToolResource -> ToolResource -> Bool
resourcesOverlap ToolAllResources _ = True
resourcesOverlap _ ToolAllResources = True
resourcesOverlap (ToolNamedResource left) (ToolNamedResource right) =
    left == right
resourcesOverlap (ToolPath left) (ToolPath right) =
    equalFilePath left right
resourcesOverlap (ToolPathTree left) (ToolPath right) =
    pathInside left right
resourcesOverlap (ToolPath left) (ToolPathTree right) =
    pathInside right left
resourcesOverlap (ToolPathTree left) (ToolPathTree right) =
    pathInside left right || pathInside right left
resourcesOverlap _ _ = False

pathInside :: OsPath -> OsPath -> Bool
pathInside root path
    | equalFilePath root path = True
    | otherwise =
        let relative = makeRelative root path
        in not (isAbsolute relative)
            && case splitDirectories relative of
                first : _ -> first /= unsafeEncodeUtf ".."
                [] -> True
