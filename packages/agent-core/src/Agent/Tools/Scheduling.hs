module Agent.Tools.Scheduling
    ( ToolAccess(..)
    , ToolResource(..)
    , ToolResourceClaim(..)
    , ToolSchedulingPlan(..)
    , schedulingPlansConflict
    , pathUsesSchedulingTrie
    , toolSchedulingWaves
    , toolSchedulingWavesLegacy
    ) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.Info (os)
import System.OsPath
    ( OsPath
    , equalFilePath
    , isAbsolute
    , makeRelative
    , normalise
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

-- | Partition model-ordered calls into the same execution waves as repeatedly
-- selecting calls with no earlier conflicting call. The result preserves
-- model order within every wave.
toolSchedulingWaves :: [(a, ToolSchedulingPlan)] -> [[a]]
toolSchedulingWaves calls =
    map reverse . IntMap.elems . snd $
        foldl' scheduleCall (emptySchedulingIndex, IntMap.empty) calls
  where
    scheduleCall (index, waves) (value, plan) =
        let wave = conflictWave index plan
            index' = recordPlan (wave + 1) plan index
        in (index', IntMap.insertWith (++) wave [value] waves)

-- | Quadratic reference implementation used to check and benchmark the
-- indexed scheduler.
toolSchedulingWavesLegacy :: [(a, ToolSchedulingPlan)] -> [[a]]
toolSchedulingWavesLegacy calls =
    go (zip [0 :: Int ..] calls)
  where
    go [] = []
    go remaining =
        let ready =
                [ (index, value)
                | (index, value@(_, plan)) <- remaining
                , not (any (earlierConflicts index plan) remaining)
                ]
            readyIndexes = IntSet.fromList (map fst ready)
            pending =
                filter
                    (\(index, _) -> IntSet.notMember index readyIndexes)
                    remaining
        in map (fst . snd) ready : go pending

    earlierConflicts index plan (earlierIndex, (_, earlierPlan)) =
        earlierIndex < index
            && schedulingPlansConflict earlierPlan plan

-- Stored wave numbers are one-based, so zero is also the empty frontier.
data AccessFrontier = AccessFrontier
    { latestRead :: !Int
    , latestWrite :: !Int
    }

data PathTrie = PathTrie
    { pathSubtree :: !AccessFrontier
    , pathExact :: !AccessFrontier
    , pathTree :: !AccessFrontier
    , pathChildren :: !(Map OsPath PathTrie)
    }

data SchedulingIndex = SchedulingIndex
    { latestCall :: !Int
    , latestExclusive :: !Int
    , namedResources :: !(Map Text AccessFrontier)
    , allPathResources :: !AccessFrontier
    , allPathsClaims :: !AccessFrontier
    , pathResources :: !PathTrie
    , nonCanonicalPaths :: ![RecordedPathClaim]
    , recordedPaths :: ![RecordedPathClaim]
    }

data RecordedPathClaim = RecordedPathClaim
    { recordedWave :: !Int
    , recordedClaim :: !ToolResourceClaim
    }

emptyFrontier :: AccessFrontier
emptyFrontier = AccessFrontier 0 0

emptyPathTrie :: PathTrie
emptyPathTrie = PathTrie emptyFrontier emptyFrontier emptyFrontier Map.empty

emptySchedulingIndex :: SchedulingIndex
emptySchedulingIndex =
    SchedulingIndex
        { latestCall = 0
        , latestExclusive = 0
        , namedResources = Map.empty
        , allPathResources = emptyFrontier
        , allPathsClaims = emptyFrontier
        , pathResources = emptyPathTrie
        , nonCanonicalPaths = []
        , recordedPaths = []
        }

conflictWave :: SchedulingIndex -> ToolSchedulingPlan -> Int
conflictWave index = \case
    ToolExclusive -> index.latestCall
    ToolUnconstrained -> index.latestExclusive
    ToolResourceClaims claims ->
        foldl'
            (\wave claim -> max wave (claimConflict index claim))
            index.latestExclusive
            claims

claimConflict :: SchedulingIndex -> ToolResourceClaim -> Int
claimConflict index (ToolResourceClaim access resource) =
    case resource of
        ToolNamedResource name ->
            frontierConflict access $
                Map.findWithDefault emptyFrontier name index.namedResources
        ToolAllPaths ->
            frontierConflict access index.allPathResources
        ToolPath path ->
            max
                (frontierConflict access index.allPathsClaims)
                (pathResourceConflict index access False path)
        ToolPathTree path ->
            max
                (frontierConflict access index.allPathsClaims)
                (pathResourceConflict index access True path)

recordPlan :: Int -> ToolSchedulingPlan -> SchedulingIndex -> SchedulingIndex
recordPlan wave plan index =
    case plan of
        ToolUnconstrained ->
            index { latestCall = max wave index.latestCall }
        ToolExclusive ->
            index
                { latestCall = max wave index.latestCall
                , latestExclusive = max wave index.latestExclusive
                }
        ToolResourceClaims claims ->
            foldl'
                (flip (recordClaim wave))
                index { latestCall = max wave index.latestCall }
                claims

recordClaim :: Int -> ToolResourceClaim -> SchedulingIndex -> SchedulingIndex
recordClaim wave (ToolResourceClaim access resource) index =
    case resource of
        ToolNamedResource name ->
            index
                { namedResources =
                    Map.alter
                        (Just . recordAccess wave access
                            . maybe emptyFrontier id)
                        name
                        index.namedResources
                }
        ToolAllPaths ->
            index
                { allPathResources =
                    recordAccess wave access index.allPathResources
                , allPathsClaims =
                    recordAccess wave access index.allPathsClaims
                }
        ToolPath path ->
            recordPathClaim wave access (ToolPath path) index
                { allPathResources =
                    recordAccess wave access index.allPathResources
                }
        ToolPathTree path ->
            recordPathClaim wave access (ToolPathTree path) index
                { allPathResources =
                    recordAccess wave access index.allPathResources
                }

frontierConflict :: ToolAccess -> AccessFrontier -> Int
frontierConflict ToolRead frontier = frontier.latestWrite
frontierConflict ToolWrite frontier =
    max frontier.latestRead frontier.latestWrite

recordAccess :: Int -> ToolAccess -> AccessFrontier -> AccessFrontier
recordAccess wave ToolRead frontier =
    frontier { latestRead = max wave frontier.latestRead }
recordAccess wave ToolWrite frontier =
    frontier { latestWrite = max wave frontier.latestWrite }

pathResourceConflict
    :: SchedulingIndex
    -> ToolAccess
    -> Bool
    -> OsPath
    -> Int
pathResourceConflict index access queryTree path
    | canonicalPath path =
        max
            (pathConflict access queryTree
                (splitDirectories path)
                index.pathResources)
            (recordedConflict
                (ToolResourceClaim access
                    (if queryTree then ToolPathTree path else ToolPath path))
                index.nonCanonicalPaths)
    | otherwise =
        recordedConflict
            (ToolResourceClaim access
                (if queryTree then ToolPathTree path else ToolPath path))
            index.recordedPaths

recordedConflict :: ToolResourceClaim -> [RecordedPathClaim] -> Int
recordedConflict claim =
    foldl'
        (\latest recorded ->
            if schedulingPlansConflict
                    (ToolResourceClaims [recorded.recordedClaim])
                    (ToolResourceClaims [claim])
                then max latest recorded.recordedWave
                else latest)
        0

recordPathClaim
    :: Int
    -> ToolAccess
    -> ToolResource
    -> SchedulingIndex
    -> SchedulingIndex
recordPathClaim wave access resource index =
    let claim = ToolResourceClaim access resource
        recorded = RecordedPathClaim wave claim
        path = case resource of
            ToolPath value -> value
            ToolPathTree value -> value
            _ -> error "recordPathClaim: non-path resource"
        isTree = case resource of
            ToolPathTree{} -> True
            _ -> False
    in index
        { pathResources =
            if canonicalPath path
                then recordPath wave access isTree
                    (splitDirectories path)
                    index.pathResources
                else index.pathResources
        , nonCanonicalPaths =
            if canonicalPath path
                then index.nonCanonicalPaths
                else recorded : index.nonCanonicalPaths
        , recordedPaths = recorded : index.recordedPaths
        }

-- The trie uses 'Map OsPath', whose component ordering is case-sensitive.
-- Windows 'equalFilePath' is case-insensitive, so Windows paths must use the
-- exact conflict predicate retained in 'recordedPaths' instead.
pathUsesSchedulingTrie :: OsPath -> Bool
pathUsesSchedulingTrie path =
    os /= "mingw32"
        && isAbsolute path
        && normalise path == path

canonicalPath :: OsPath -> Bool
canonicalPath = pathUsesSchedulingTrie

pathConflict :: ToolAccess -> Bool -> [OsPath] -> PathTrie -> Int
pathConflict access queryTree = go 0
  where
    go :: Int -> [OsPath] -> PathTrie -> Int
    go ancestorTrees [] node =
        maximum
            [ ancestorTrees
            , frontierConflict access node.pathTree
            , if queryTree
                then frontierConflict access node.pathSubtree
                else frontierConflict access node.pathExact
            ]
    go ancestorTrees (component : rest) node =
        let ancestorTrees' =
                max ancestorTrees (frontierConflict access node.pathTree)
        in case Map.lookup component node.pathChildren of
            Nothing -> ancestorTrees'
            Just child -> go ancestorTrees' rest child

recordPath
    :: Int
    -> ToolAccess
    -> Bool
    -> [OsPath]
    -> PathTrie
    -> PathTrie
recordPath wave access isTree = go
  where
    go :: [OsPath] -> PathTrie -> PathTrie
    go [] node =
        node
            { pathSubtree = recordAccess wave access node.pathSubtree
            , pathExact =
                if isTree
                    then node.pathExact
                    else recordAccess wave access node.pathExact
            , pathTree =
                if isTree
                    then recordAccess wave access node.pathTree
                    else node.pathTree
            }
    go (component : rest) node =
        node
            { pathSubtree = recordAccess wave access node.pathSubtree
            , pathChildren =
                Map.alter
                    (Just . go rest . maybe emptyPathTrie id)
                    component
                    node.pathChildren
            }
