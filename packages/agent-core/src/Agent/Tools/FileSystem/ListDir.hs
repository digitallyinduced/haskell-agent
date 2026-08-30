module Agent.Tools.FileSystem.ListDir
    ( ListDirArgs(..)
    , ListDirSpeculation
    , listDirTool
    , listDirToolWithSpeculation
    , newListDirSpeculation
    , closeListDirSpeculation
    , waitForListDirSpeculation
    , runListDir
    , listDirResolved
    , DirNode(..)
    , ListDirOperations(..)
    , collectDirWith
    , capNodes
    , renderTree
    ) where

import Agent.Json.Decode (Decoder)
import qualified Agent.Json.Decode as Json
import Agent.OsPath (fromText, toText, unsafeToFilePath)
import Agent.ToolArgs (objectArgs, reqText)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch
    ( StreamedTool(..)
    , StreamedToolFactory
    , ToolInput(..)
    , ToolResult
    , typedTool
    )
import Agent.Tools.FileSystem.GitIgnore (ignoredPaths)
import Agent.Tools.FileSystem.PathPrefix
    ( FileFingerprint
    , PathProgress(..)
    , cancelAndJoin
    , directoryFingerprint
    , fingerprintsMatch
    , jsonStringFieldProgress
    , maximumConcurrentSpeculativeTasks
    , minimumPredictionPrefix
    , uniqueWorkspaceCandidate
    , workspaceDirectoryIndex
    )
import Agent.Tools.IO
    ( displayPathInWorkspace
    , listDirectoryEntries
    , resolveForRead
    , resolveForReadWithoutAccessRequest
    )
import Agent.Tools.Scheduling
    ( ToolAccess(..)
    , ToolResource(..)
    , ToolResourceClaim(..)
    )
import Agent.Tools.Types
    ( AppTool
    , ToolEnv(..)
    , ToolExecutionPolicy(..)
    , jsonTool
    , withToolArgumentInterpreter
    , withTypedResourceClaims
    )
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , waitCatch
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newMVar
    , readMVar
    )
import Control.Exception (evaluate)
import Control.Exception.Safe (mask)
import Control.Monad (forM_, void)
import Data.Acquire (mkAcquire)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory.OsPath (doesDirectoryExist, pathIsSymbolicLink)
import System.OsPath
    ( OsPath
    , equalFilePath
    , takeExtension
    , (</>)
    )

newtype ListDirArgs = ListDirArgs { targetDirectory :: Text }

listDirArgsDecoder :: Decoder ListDirArgs
listDirArgsDecoder = objectArgs \object -> ListDirArgs <$> reqText object "target_directory"

listDirTool :: ToolEnv -> AppTool
listDirTool env =
    listDirToolWithSpeculation env Nothing

listDirToolWithSpeculation
    :: ToolEnv
    -> Maybe ListDirSpeculation
    -> AppTool
listDirToolWithSpeculation env speculation =
    withToolArgumentInterpreter
        (maybe
            (listDirArgumentInterpreter env)
            listDirArgumentInterpreterWithCache
            speculation) $
    withTypedResourceClaims listDirArgsDecoder (listDirClaims env) $
    jsonTool "list_dir" listDirDescription
    [ PropertySchema "target_directory" PropertyString True $ Just
        "Path to a directory within an allowed filesystem root. Relative paths use the workspace root; absolute paths may resolve within the workspace or session temp directory."
    ]
    True
    ParallelSafe
    (typedTool "list_dir" listDirArgsDecoder (runListDir env))

listDirClaims
    :: ToolEnv
    -> ListDirArgs
    -> IO (Either Text [ToolResourceClaim])
listDirClaims env args =
    resolveForRead env (fromText args.targetDirectory)
        >>= pure . fmap
            (\path ->
                [ToolResourceClaim ToolRead (ToolPathTree path)])

listDirDescription :: Text
listDirDescription =
    "Lists files and directories in a given path.\n\
    \The 'target_directory' parameter can be relative to the workspace root or absolute within an allowed filesystem root.\n\
    \\n\
    \Other details:\n\
    \    - The result does not display dot-files and dot-directories.\n\
    \    - Respects .gitignore patterns (files/directories ignored by git are not shown).\n\
    \    - Large directories are summarized with file counts and extension breakdowns instead of listing all files."

maxListItems :: Int
maxListItems = 200

runListDir :: ToolEnv -> ListDirArgs -> IO (Either Text Text)
runListDir env args = resolveForRead env (fromText args.targetDirectory) >>= \case
    Left err -> pure (Left err)
    Right path -> do
        display <- displayPathInWorkspace env path
        listDirResolved env path display

listDirResolved :: ToolEnv -> OsPath -> Text -> IO (Either Text Text)
listDirResolved env path displayName = doesDirectoryExist path >>= \case
    False -> pure $ Left $
        "Error: " <> displayName <> " is not a valid directory"
    True ->
        collectDir env.toolCwd maxListItems path >>= \case
            Left err -> pure (Left err)
            Right listing -> pure (Right (formatListing displayName listing))

formatListing :: Text -> ([DirNode], Bool) -> Text
formatListing displayName (entries, truncated) =
    "Directory listing for " <> displayName <> ":\n" <> tree <> notice
  where
    tree = renderTree 0 entries
    notice
        | truncated =
            "\nLarge directory summarized; some nested entries were omitted."
        | otherwise = ""

data DirNode
    = FileNode OsPath
    | DirectoryNode OsPath [DirNode]
    | ErrorNode OsPath Text
    deriving (Eq, Show)

data ListDirOperations = ListDirOperations
    { readDirectoryEntries
        :: OsPath -> IO (Either Text [(OsPath, Bool)])
    , findIgnoredPaths
        :: OsPath -> [OsPath] -> IO (Set.Set FilePath)
    , isEntrySymbolicLink
        :: OsPath -> IO Bool
    }

filesystemOperations :: ListDirOperations
filesystemOperations = ListDirOperations
    { readDirectoryEntries = listDirectoryEntries
    , findIgnoredPaths = ignoredPaths
    , isEntrySymbolicLink = pathIsSymbolicLink
    }

-- | Traverse only nodes that can appear in the result.  The old implementation
-- built the complete tree and applied 'capNodes' afterwards, which meant a
-- request for a large directory could scan millions of descendants despite the
-- 200-node result limit.
collectDir :: OsPath -> Int -> OsPath -> IO (Either Text ([DirNode], Bool))
collectDir = collectDirWith filesystemOperations

collectDirWith
    :: ListDirOperations
    -> OsPath
    -> Int
    -> OsPath
    -> IO (Either Text ([DirNode], Bool))
collectDirWith operations cwd budget path = do
    visibleEntries operations cwd path >>= \case
        Left err -> pure (Left err)
        Right entries ->
            Right <$> collectEntries operations cwd budget path entries

visibleEntries
    :: ListDirOperations
    -> OsPath
    -> OsPath
    -> IO (Either Text [(OsPath, Bool)])
visibleEntries operations cwd path = do
    listed <- operations.readDirectoryEntries path
    case listed of
        Left err -> pure (Left err)
        Right raw -> do
            let candidates = sortOn fst
                    [ (name, isDir)
                    | (name, isDir) <- raw
                    , not ("." `Text.isPrefixOf` toText name)
                    ]
            ignored <-
                operations.findIgnoredPaths cwd
                    [path </> name | (name, _) <- candidates]
            Right . foldr addVisible [] <$> traverse (classify ignored) candidates
  where
    classify ignored (name, isDir)
        | Set.member (unsafeToFilePath (path </> name)) ignored = pure Nothing
        | not isDir = pure (Just (name, False))
        | otherwise = do
            isLink <- operations.isEntrySymbolicLink (path </> name)
            pure $ Just (name, not isLink)

    addVisible (Just entry) entries = entry : entries
    addVisible Nothing entries = entries

collectEntries
    :: ListDirOperations
    -> OsPath
    -> Int
    -> OsPath
    -> [(OsPath, Bool)]
    -> IO ([DirNode], Bool)
collectEntries operations cwd budget parent entries =
    fillBounded operations cwd (max 0 budget) (length entries) parent entries

-- | Preserve the existing sibling-reservation behavior while using the
-- remaining result budget to decide whether to descend into a directory.
fillBounded
    :: ListDirOperations
    -> OsPath
    -> Int
    -> Int
    -> OsPath
    -> [(OsPath, Bool)]
    -> IO ([DirNode], Bool)
fillBounded _ _ _ _ _ [] = pure ([], False)
fillBounded _ _ remaining _ _ _ | remaining <= 0 = pure ([], True)
fillBounded operations cwd remaining restCount parent (entry : rest) = do
    let reserved = min (remaining - 1) (restCount - 1)
        available = remaining - reserved
    (node, nodeTruncated) <-
        toNodeBounded operations cwd available parent entry
    (more, restTruncated) <-
        fillBounded operations cwd
            (remaining - countNodes [node])
            (restCount - 1)
            parent
            rest
    pure (node : more, nodeTruncated || restTruncated)

toNodeBounded
    :: ListDirOperations
    -> OsPath
    -> Int
    -> OsPath
    -> (OsPath, Bool)
    -> IO (DirNode, Bool)
toNodeBounded _ _ _ _ (name, False) = pure (FileNode name, False)
toNodeBounded operations cwd budget parent (name, True) = do
    let full = parent </> name
    visibleEntries operations cwd full >>= \case
        Left err -> pure (ErrorNode name err, False)
        Right entries
            | isLargeFlatDirectory entries ->
                pure (summarizedDirectory name entries, False)
            | otherwise -> do
                (children, truncated) <-
                    collectEntries operations cwd
                        (max 0 (budget - 1))
                        full
                        entries
                pure (DirectoryNode name children, truncated)

isLargeFlatDirectory :: [(OsPath, Bool)] -> Bool
isLargeFlatDirectory entries =
    length entries > 20 && all (not . snd) entries

summarizedDirectory :: OsPath -> [(OsPath, Bool)] -> DirNode
summarizedDirectory name entries =
    summarizeDir name [FileNode child | (child, _) <- entries]

-- | Keep as many nodes as @budget@ allows. A directory that does not fit in
-- full is included as a stub (and maybe a truncated child list) rather than
-- dropped. Later siblings keep a reserved slot so a large subdirectory cannot
-- hide the rest of the listing.
capNodes :: Int -> [DirNode] -> ([DirNode], Bool)
capNodes budget nodes =
    fill (max 0 budget) (length nodes) nodes

-- | @fill remaining restCount nodes@ spends at most @remaining@ slots. @restCount@
-- is the length of @nodes@, carried so later-sibling reservation stays linear.
fill :: Int -> Int -> [DirNode] -> ([DirNode], Bool)
fill _ _ [] = ([], False)
fill remaining _ _ | remaining <= 0 = ([], True)
fill remaining restCount (node : rest) =
    let reserved = min (remaining - 1) (restCount - 1)
        available = remaining - reserved
        (keptNode, used, nodeTruncated) = takeNode available node
        (more, restTruncated) =
            fill (remaining - used) (restCount - 1) rest
    in (keptNode : more, nodeTruncated || restTruncated)

takeNode :: Int -> DirNode -> (DirNode, Int, Bool)
takeNode budget node = case node of
    FileNode name -> (FileNode name, 1, False)
    ErrorNode name message -> (ErrorNode name message, 1, False)
    DirectoryNode name children
        | budget <= 1 ->
            ( DirectoryNode name []
            , 1
            , not (null children)
            )
        | otherwise ->
            let (keptChildren, truncated) =
                    fill (budget - 1) (length children) children
            in
                ( DirectoryNode name keptChildren
                , 1 + countNodes keptChildren
                , truncated
                )

countNodes :: [DirNode] -> Int
countNodes = sum . map \case
    FileNode _ -> 1
    DirectoryNode _ children -> 1 + countNodes children
    ErrorNode _ _ -> 1

summarizeDir :: OsPath -> [DirNode] -> DirNode
summarizeDir name children =
    let files = [file | FileNode file <- children]
        dirs = [dir | dir@DirectoryNode{} <- children]
        errors = [err | err@ErrorNode{} <- children]
    in if length files > 20 && null dirs && null errors
        then DirectoryNode
            (fromText (toText name <> " " <> extensionSummary files)) []
        else DirectoryNode name children

extensionSummary :: [OsPath] -> Text
extensionSummary files =
    let counts = Map.fromListWith (+)
            [ (ext, 1 :: Int)
            | file <- files
            , let ext = case takeExtension file of
                    e | e == fromText "" -> fromText "(no ext)"
                    e -> e
            ]
        rendered =
            [ Text.pack (show n) <> " *" <> toText ext
            | (ext, n) <- sortOn (negate . snd) (Map.toList counts)
            ]
    in "(" <> Text.pack (show (length files)) <> " files: "
        <> Text.intercalate ", " (take 4 rendered) <> ")"

renderTree :: Int -> [DirNode] -> Text
renderTree depth = Text.unlines . concatMap (renderNodeLines depth)

renderNodeLines :: Int -> DirNode -> [Text]
renderNodeLines depth = \case
    FileNode name -> [indent <> "- " <> toText name]
    DirectoryNode name children ->
        (indent <> "- " <> directoryLabel name)
            : concatMap (renderNodeLines (depth + 1)) children
    ErrorNode name message ->
        [ indent
            <> "- "
            <> toText name
            <> "/ (listing failed: "
            <> message
            <> ")"
        ]
  where
    indent = Text.replicate depth "  "

directoryLabel :: OsPath -> Text
directoryLabel name =
    let label = toText name
    in if "/" `Text.isSuffixOf` label || "(" `Text.isInfixOf` label
        then label
        else label <> "/"

--------------------------------------------------------------------------------
-- Streamed prefetch
--------------------------------------------------------------------------------

data ListDirSpeculation = ListDirSpeculation
    { listEnv :: !ToolEnv
    , listState :: !(MVar ListDirState)
    }

data ListDirState = ListDirState
    { listClosed :: !Bool
    , listDirectories :: !(Maybe (Set.Set Text))
    , listIndexTask :: !(Maybe (Async ()))
    , listNextKey :: !Int
    , listActive :: !(Map.Map Int (Async (Maybe PrefetchedListing)))
    }

data PrefetchedListing = PrefetchedListing
    { listingPath :: !OsPath
    , listingSnapshot :: !ListingSnapshot
    , listingOutput :: !ToolResult
    }

data ListingSnapshot = ListingSnapshot
    { snapshotDirectories :: !(Map.Map OsPath FileFingerprint)
    , snapshotIgnoreDecisions :: !(Map.Map OsPath Bool)
    }

emptyListingSnapshot :: ListingSnapshot
emptyListingSnapshot = ListingSnapshot
    { snapshotDirectories = Map.empty
    , snapshotIgnoreDecisions = Map.empty
    }

-- | Use the ordinary bounded traversal while recording everything needed to
-- prove that its output is still current. Re-validating after traversal closes
-- the gap between fingerprinting a parent and visiting its descendants.
collectDirSnapshot
    :: OsPath
    -> Int
    -> OsPath
    -> IO (Either Text (([DirNode], Bool), ListingSnapshot))
collectDirSnapshot cwd budget path = do
    snapshotVar <- newMVar emptyListingSnapshot
    collectDirWith (snapshotOperations snapshotVar) cwd budget path >>= \case
        Left err -> pure (Left err)
        Right listing@(nodes, _)
            | containsListingError nodes ->
                pure (Left "nested directory listing failed during speculation")
            | otherwise -> do
                snapshot <- readMVar snapshotVar
                listingSnapshotMatches cwd snapshot >>= \case
                    True -> pure (Right (listing, snapshot))
                    False -> pure (Left "directory changed during speculation")

snapshotOperations :: MVar ListingSnapshot -> ListDirOperations
snapshotOperations snapshotVar = filesystemOperations
    { readDirectoryEntries = \path -> do
        directoryFingerprint path >>= \case
            Nothing ->
                pure (Left "directory disappeared during speculation")
            Just before ->
                listDirectoryEntries path >>= \case
                    Left err -> pure (Left err)
                    Right entries ->
                        directoryFingerprint path >>= \after ->
                            if fingerprintsMatch after (Just before)
                                then do
                                    modifyMVar_ snapshotVar \snapshot ->
                                        pure snapshot
                                            { snapshotDirectories =
                                                Map.insert path before
                                                    snapshot.snapshotDirectories
                                            }
                                    pure (Right entries)
                                else
                                    pure (Left
                                        "directory changed during speculation")
    , findIgnoredPaths = \cwd paths -> do
        ignored <- ignoredPaths cwd paths
        let decisions = Map.fromList
                [ (path, Set.member (unsafeToFilePath path) ignored)
                | path <- paths
                ]
        modifyMVar_ snapshotVar \snapshot ->
            pure snapshot
                { snapshotIgnoreDecisions =
                    Map.union decisions snapshot.snapshotIgnoreDecisions
                }
        pure ignored
    }

containsListingError :: [DirNode] -> Bool
containsListingError = any \case
    FileNode _ -> False
    DirectoryNode _ children -> containsListingError children
    ErrorNode _ _ -> True

data ListCandidate = ListCandidate
    { listCandidateTarget :: !Text
    , listCandidateKey :: !Int
    , listCandidateTask :: !(Async (Maybe PrefetchedListing))
    }

data PartialListCall = PartialListCall
    { partialListArguments :: !Text
    , partialListCandidate :: !(Maybe ListCandidate)
    }

listDirArgumentInterpreter :: ToolEnv -> StreamedToolFactory
listDirArgumentInterpreter environment =
    streamedListDir
        <$> mkAcquire
            (newListDirSpeculation environment)
            closeListDirSpeculation

listDirArgumentInterpreterWithCache :: ListDirSpeculation -> StreamedToolFactory
listDirArgumentInterpreterWithCache speculation =
    streamedListDir
        <$> mkAcquire (pure speculation) (\_ -> pure ())

waitForListDirSpeculation :: ListDirSpeculation -> IO ()
waitForListDirSpeculation speculation = do
    initial <- readMVar speculation.listState
    mapM_ (void . waitCatch) initial.listIndexTask
    current <- readMVar speculation.listState
    mapM_ (void . waitCatch) (Map.elems current.listActive)

newListDirSpeculation :: ToolEnv -> IO ListDirSpeculation
newListDirSpeculation env = do
    state <- newMVar ListDirState
        { listClosed = False
        , listDirectories = Nothing
        , listIndexTask = Nothing
        , listNextKey = 0
        , listActive = Map.empty
        }
    pure ListDirSpeculation { listEnv = env, listState = state }

closeListDirSpeculation :: ListDirSpeculation -> IO ()
closeListDirSpeculation speculation = mask \_ -> do
    (indexTask, active) <-
        modifyMVar speculation.listState \current ->
            case current of
                ListDirState
                    { listIndexTask = indexTask
                    , listActive = active
                    } ->
                    pure
                        ( current
                            { listClosed = True
                            , listIndexTask = Nothing
                            , listActive = Map.empty
                            }
                        , (indexTask, Map.elems active)
                        )
    mapM_ cancelAndJoin indexTask
    mapM_ cancelAndJoin active

streamedListDir :: ListDirSpeculation -> StreamedTool
streamedListDir speculation =
    StreamedTool
        { streamedStart = pure emptyPartialList
        , streamedInterpret = interpretListDir speculation
        , streamedConsume = \_call _emit -> consumeListDir speculation
        , streamedClose = closePartialList speculation
        }

emptyPartialList :: PartialListCall
emptyPartialList =
    PartialListCall
        { partialListArguments = ""
        , partialListCandidate = Nothing
        }

interpretListDir
    :: ListDirSpeculation
    -> PartialListCall
    -> ToolInput
    -> IO (Either (ListDirArgs, PartialListCall) PartialListCall)
interpretListDir speculation state = \case
    ToolPrefix text ->
        Right <$> refreshListCall speculation state { partialListArguments = text }
    ToolDone text -> do
        next <- refreshListCall speculation state { partialListArguments = text }
        case decodeListDirArgs next.partialListArguments of
            Nothing -> pure (Right next)
            Just args -> pure (Left (args, next))

refreshListCall
    :: ListDirSpeculation
    -> PartialListCall
    -> IO PartialListCall
refreshListCall speculation partial = do
    startListIndex speculation
    refreshed <- refreshListCandidate speculation partial
    pending <- pendingListIndex speculation refreshed
    case pending of
        Nothing -> pure refreshed
        Just indexTask -> do
            void (waitCatch indexTask)
            refreshListCandidate speculation refreshed

closePartialList :: ListDirSpeculation -> PartialListCall -> IO ()
closePartialList speculation =
    mapM_ (cancelListCandidate speculation) . (.partialListCandidate)

refreshListCandidate
    :: ListDirSpeculation
    -> PartialListCall
    -> IO PartialListCall
refreshListCandidate speculation partial = mask \_ -> do
    current <- readMVar speculation.listState
    let progress = jsonStringFieldProgress "target_directory" partial.partialListArguments
        desired = desiredListTarget current.listDirectories progress
    case (partial.partialListCandidate, desired) of
        (Just existing, Just target)
            | existing.listCandidateTarget == target ->
                pure partial
        (Just existing, Nothing)
            | candidateStillMatches progress existing.listCandidateTarget ->
                pure partial
        (existing, next) -> do
            forM_ existing (cancelListCandidate speculation)
            nextCandidate <-
                case next of
                    Nothing -> pure Nothing
                    Just target -> startListCandidate speculation target
            pure partial { partialListCandidate = nextCandidate }

pendingListIndex
    :: ListDirSpeculation
    -> PartialListCall
    -> IO (Maybe (Async ()))
pendingListIndex speculation partial
    | not (isNothing partial.partialListCandidate) = pure Nothing
    | otherwise =
        case jsonStringFieldProgress "target_directory" partial.partialListArguments of
            Just (PathPrefix prefix)
                | Text.length prefix >= minimumPredictionPrefix ->
                    (.listIndexTask) <$> readMVar speculation.listState
            _ -> pure Nothing

desiredListTarget
    :: Maybe (Set.Set Text)
    -> Maybe PathProgress
    -> Maybe Text
desiredListTarget _ Nothing = Nothing
desiredListTarget _ (Just (PathComplete target))
    | Text.null target = Nothing
    | otherwise = Just target
desiredListTarget directories (Just (PathPrefix prefix))
    | Text.length prefix < minimumPredictionPrefix = Nothing
    | otherwise =
        uniqueWorkspaceCandidate prefix (fromMaybe Set.empty directories)

candidateStillMatches :: Maybe PathProgress -> Text -> Bool
candidateStillMatches progress candidate =
    case progress of
        Just (PathPrefix prefix) ->
            not (Text.null candidate) && prefix `Text.isPrefixOf` candidate
        Just (PathComplete target) -> target == candidate
        Nothing -> False

startListIndex :: ListDirSpeculation -> IO ()
startListIndex speculation =
    modifyMVar_ speculation.listState \current ->
        case (current.listClosed, current.listDirectories, current.listIndexTask) of
            (False, Nothing, Nothing) -> do
                worker <- asyncWithUnmask \restore ->
                    restore (workspaceDirectoryIndex speculation.listEnv)
                        >>= installListIndex speculation
                pure current { listIndexTask = Just worker }
            _ -> pure current

installListIndex :: ListDirSpeculation -> Set.Set Text -> IO ()
installListIndex speculation paths =
    modifyMVar_ speculation.listState \current ->
        if current.listClosed
            then pure current
            else pure current
                { listDirectories = Just paths
                , listIndexTask = Nothing
                }

startListCandidate
    :: ListDirSpeculation
    -> Text
    -> IO (Maybe ListCandidate)
startListCandidate speculation target = mask \_ -> do
    candidate <-
        modifyMVar speculation.listState \current ->
            if current.listClosed
                || Map.size current.listActive >= maximumConcurrentSpeculativeTasks
                then pure (current, Nothing)
                else do
                    let key = current.listNextKey
                    worker <-
                        asyncWithUnmask \restore ->
                            restore (prefetchListing speculation.listEnv target)
                    pure
                        ( current
                            { listNextKey = key + 1
                            , listActive = Map.insert key worker current.listActive
                            }
                        , Just ListCandidate
                            { listCandidateTarget = target
                            , listCandidateKey = key
                            , listCandidateTask = worker
                            }
                        )
    pure candidate

prefetchListing :: ToolEnv -> Text -> IO (Maybe PrefetchedListing)
prefetchListing env target =
    resolveForReadWithoutAccessRequest env (fromText target) >>= \case
        Left _ -> pure Nothing
        Right path -> do
            collectDirSnapshot env.toolCwd maxListItems path >>= \case
                Left _ -> pure Nothing
                Right (listing, snapshot) -> do
                    display <- displayPathInWorkspace env path
                    let output = formatListing display listing
                    void (evaluate (Text.length output))
                    pure $ Just PrefetchedListing
                        { listingPath = path
                        , listingSnapshot = snapshot
                        , listingOutput = Right output
                        }

cancelListCandidate :: ListDirSpeculation -> ListCandidate -> IO ()
cancelListCandidate speculation candidate = do
    releaseListCandidate speculation candidate
    cancelAndJoin candidate.listCandidateTask

releaseListCandidate :: ListDirSpeculation -> ListCandidate -> IO ()
releaseListCandidate speculation candidate =
    modifyMVar_ speculation.listState \current ->
        pure current
            { listActive = Map.delete candidate.listCandidateKey current.listActive
            }

consumeListDir
    :: ListDirSpeculation
    -> ListDirArgs
    -> PartialListCall
    -> IO ToolResult
consumeListDir speculation args partial =
    case partial.partialListCandidate of
        Nothing -> runListDir speculation.listEnv args
        Just selected ->
            waitCatch selected.listCandidateTask >>= \case
                Left _ -> miss selected
                Right Nothing -> miss selected
                Right (Just prefetched) ->
                    resolveForRead
                        speculation.listEnv
                        (fromText args.targetDirectory) >>= \case
                            Left _ -> miss selected
                            Right finalPath
                                | not (equalFilePath finalPath prefetched.listingPath) ->
                                    miss selected
                                | otherwise -> do
                                    valid <- listingSnapshotMatches
                                        speculation.listEnv.toolCwd
                                        prefetched.listingSnapshot
                                    if valid
                                        then do
                                            releaseListCandidate speculation selected
                                            pure prefetched.listingOutput
                                        else miss selected
  where
    miss selected = do
        cancelListCandidate speculation selected
        runListDir speculation.listEnv args

listingSnapshotMatches :: OsPath -> ListingSnapshot -> IO Bool
listingSnapshotMatches cwd snapshot = do
    directoriesBefore <- directoriesMatch
    if not directoriesBefore
        then pure False
        else do
            ignored <-
                ignoredPaths cwd (Map.keys snapshot.snapshotIgnoreDecisions)
            let decisionsMatch = Map.foldrWithKey
                    (\path expected matches ->
                        matches
                            && Set.member (unsafeToFilePath path) ignored
                                == expected)
                    True
                    snapshot.snapshotIgnoreDecisions
            directoriesAfter <- directoriesMatch
            pure (decisionsMatch && directoriesAfter)
  where
    directoriesMatch = and <$> mapM
        (\(path, expected) ->
            (`fingerprintsMatch` Just expected)
                <$> directoryFingerprint path)
        (Map.toList snapshot.snapshotDirectories)

decodeListDirArgs :: Text -> Maybe ListDirArgs
decodeListDirArgs text =
    case Json.decodeText listDirArgsDecoder text of
        Right args -> Just args
        Left _ -> Nothing
