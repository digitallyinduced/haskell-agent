module Agent.Tools.Scheduling
    ( ToolAccess(..)
    , ToolResource(..)
    , ToolResourceClaim(..)
    , ToolSchedulingPlan(..)
    , schedulingPlansConflict
    ) where

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
    = ToolAllPaths
    | ToolPath !OsPath
    | ToolPathTree !OsPath
    | ToolNamedResource !Text
    deriving (Eq, Show)

data ToolResourceClaim = ToolResourceClaim
    { claimAccess :: !ToolAccess
    , claimResource :: !ToolResource
    } deriving (Eq, Show)

data ToolSchedulingPlan
    = ToolUnconstrained
    | ToolResourceClaims ![ToolResourceClaim]
    | ToolExclusive
    deriving (Eq, Show)

schedulingPlansConflict
    :: ToolSchedulingPlan
    -> ToolSchedulingPlan
    -> Bool
schedulingPlansConflict ToolExclusive _ = True
schedulingPlansConflict _ ToolExclusive = True
schedulingPlansConflict ToolUnconstrained _ = False
schedulingPlansConflict _ ToolUnconstrained = False
schedulingPlansConflict (ToolResourceClaims left) (ToolResourceClaims right) =
    or
        [ claimsConflict leftClaim rightClaim
        | leftClaim <- left
        , rightClaim <- right
        ]

claimsConflict :: ToolResourceClaim -> ToolResourceClaim -> Bool
claimsConflict left right =
    resourcesOverlap left.claimResource right.claimResource
        && (left.claimAccess == ToolWrite || right.claimAccess == ToolWrite)

resourcesOverlap :: ToolResource -> ToolResource -> Bool
resourcesOverlap ToolAllPaths ToolAllPaths = True
resourcesOverlap ToolAllPaths ToolPath{} = True
resourcesOverlap ToolAllPaths ToolPathTree{} = True
resourcesOverlap ToolPath{} ToolAllPaths = True
resourcesOverlap ToolPathTree{} ToolAllPaths = True
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
