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
    , capNodes
    , renderTree
    ) where

import Agent.Json.Decode (Decoder)
import qualified Agent.Json.Decode as Json
import Agent.OsPath (fromText, toText)
import Agent.ToolArgs (objectArgs, reqText)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch
    ( StreamedTool(..)
    , StreamedToolFactory
    , ToolInput(..)
    , ToolResult
    , typedTool
    )
import Agent.Tools.FileSystem.GitIgnore (isGitIgnored)
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
        collectDir env.toolCwd path >>= \case
            Left err -> pure (Left err)
            Right entries -> pure (Right (formatListing displayName entries))

formatListing :: Text -> [DirNode] -> Text
formatListing displayName entries =
    "Directory listing for " <> displayName <> ":\n" <> tree <> notice
  where
    (shown, truncated) = capNodes maxListItems entries
    tree = renderTree 0 shown
    notice
        | truncated =
            "\nLarge directory summarized; some nested entries were omitted."
        | otherwise = ""

data DirNode
    = FileNode OsPath
    | DirectoryNode OsPath [DirNode]
    | ErrorNode OsPath Text
    deriving (Eq, Show)

collectDir :: OsPath -> OsPath -> IO (Either Text [DirNode])
collectDir cwd path = do
    listed <- listDirectoryEntries path
    case listed of
        Left err -> pure (Left err)
        Right raw -> do
            let visible = visibleEntries raw
            Right <$> (fmap concat $ mapM (toNode cwd path) visible)

visibleEntries :: [(OsPath, Bool)] -> [(OsPath, Bool)]
visibleEntries =
    sortOn fst
        . filter (\(name, _) -> not ("." `Text.isPrefixOf` toText name))

-- | Collect a listing, fingerprints for every directory visited, and every
-- git-ignore decision that affected the output. A root fingerprint alone is
-- insufficient for recursive listings: changing an entry in a nested
-- directory does not necessarily change the root's metadata. Likewise,
-- fingerprinting nearby .gitignore files misses repository excludes and
-- global Git configuration. Replaying the decisions at consumption verifies
-- the effective policy rather than trying to enumerate all of its inputs.
collectDirSnapshot
    :: OsPath
    -> OsPath
    -> IO (Either Text ([DirNode], ListingSnapshot))
collectDirSnapshot cwd path = do
    before <- directoryFingerprint path
    case before of
        Nothing -> pure (Left "directory disappeared during speculation")
        Just fingerprint -> do
            listed <- listDirectoryEntries path
            case listed of
                Left err -> pure (Left err)
                Right raw -> do
                    let visible = visibleEntries raw
                    children <- mapM (toNodeSnapshot cwd path) visible
                    after <- directoryFingerprint path
                    case sequence children of
                        Left err -> pure (Left err)
                        Right collected
                            | fingerprintsMatch after (Just fingerprint) ->
                                let childrenSnapshot =
                                        mergeListingSnapshots (map snd collected)
                                    snapshot = childrenSnapshot
                                        { snapshotDirectories =
                                            Map.insert
                                                path
                                                fingerprint
                                                childrenSnapshot.snapshotDirectories
                                        }
                                in pure $ Right
                                    (concatMap fst collected, snapshot)
                            | otherwise ->
                                pure (Left "directory changed during speculation")

toNodeSnapshot
    :: OsPath
    -> OsPath
    -> (OsPath, Bool)
    -> IO (Either Text ([DirNode], ListingSnapshot))
toNodeSnapshot cwd parent (name, isDir) = do
    let full = parent </> name
    ignored <- isGitIgnored cwd full
    let decision = emptyListingSnapshot
            { snapshotIgnoreDecisions = Map.singleton full ignored
            }
    if ignored
        then pure (Right ([], decision))
        else if not isDir
            then pure (Right ([FileNode name], decision))
            else do
                isLink <- pathIsSymbolicLink full
                if isLink
                    then pure (Right ([FileNode name], decision))
                    else do
                        collectDirSnapshot cwd full >>= \case
                            Left err -> pure (Left err)
                            Right (children, snapshot) ->
                                pure $ Right
                                    ( [summarizeDir name children]
                                    , mergeListingSnapshots [decision, snapshot]
                                    )

toNode :: OsPath -> OsPath -> (OsPath, Bool) -> IO [DirNode]
toNode cwd parent (name, isDir) = do
    let full = parent </> name
    ignored <- isGitIgnored cwd full
    if ignored
        then pure []
        else if not isDir
            then pure [FileNode name]
            else do
                isLink <- pathIsSymbolicLink full
                if isLink
                    then pure [FileNode name]
                    else
                        collectDir cwd full >>= \case
                            Left err ->
                                pure [ErrorNode name err]
                            Right children ->
                                pure [summarizeDir name children]

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

mergeListingSnapshots :: [ListingSnapshot] -> ListingSnapshot
mergeListingSnapshots snapshots = ListingSnapshot
    { snapshotDirectories =
        Map.unions (map (.snapshotDirectories) snapshots)
    , snapshotIgnoreDecisions =
        Map.unions (map (.snapshotIgnoreDecisions) snapshots)
    }

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
            collectDirSnapshot env.toolCwd path >>= \case
                Left _ -> pure Nothing
                Right (entries, snapshot) -> do
                    display <- displayPathInWorkspace env path
                    let output = formatListing display entries
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
            decisionsMatch <- and <$> mapM
                (\(path, expected) ->
                    (== expected) <$> isGitIgnored cwd path)
                (Map.toList snapshot.snapshotIgnoreDecisions)
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
