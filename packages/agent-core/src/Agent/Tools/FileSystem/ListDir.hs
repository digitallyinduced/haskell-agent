module Agent.Tools.FileSystem.ListDir
    ( listDirTool
    , DirNode(..)
    , ListDirOperations(..)
    , collectDirWith
    , capNodes
    , renderTree
    ) where

import Agent.Json.Decode (Decoder)
import Agent.OsPath (fromText, toText, unsafeToFilePath)
import Agent.ToolArgs (objectArgs, reqText)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch
    ( ToolCall(..)
    , decodeToolArguments
    , toolArgumentsValue
    , typedTool
    )
import Agent.Tools.FileSystem.GitIgnore (ignoredPaths)
import Agent.Tools.IO
    ( displayPathInWorkspace
    , listDirectoryEntries
    , resolveForRead
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
    , withToolResourceClaims
    )
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory.OsPath (doesDirectoryExist, pathIsSymbolicLink)
import System.OsPath (OsPath, takeExtension, (</>))

newtype ListDirArgs = ListDirArgs { targetDirectory :: Text }

listDirArgsDecoder :: Decoder ListDirArgs
listDirArgsDecoder = objectArgs \object -> ListDirArgs <$> reqText object "target_directory"

listDirTool :: ToolEnv -> AppTool
listDirTool env = withToolResourceClaims (listDirClaims env) $
    jsonTool "list_dir" listDirDescription
    [ PropertySchema "target_directory" PropertyString True $ Just
        "Path to a directory within an allowed filesystem root. Relative paths use the workspace root; absolute paths may resolve within the workspace or session temp directory."
    ]
    True
    ParallelSafe
    (typedTool "list_dir" listDirArgsDecoder (runListDir env))

listDirClaims
    :: ToolEnv
    -> ToolCall
    -> IO (Either Text [ToolResourceClaim])
listDirClaims env call =
    case
        decodeToolArguments listDirArgsDecoder (toolArgumentsValue call.arguments)
            :: Either Text ListDirArgs
    of
        Left err -> pure (Left err)
        Right args ->
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
        doesDirectoryExist path >>= \case
            False -> pure $ Left $
                "Error: " <> display <> " is not a valid directory"
            True ->
                collectDir env.toolCwd maxListItems path >>= \case
                    Left err -> pure (Left err)
                    Right (entries, truncated) -> do
                        let tree = renderTree 0 entries
                            notice
                                | truncated =
                                    "\nLarge directory summarized; some nested entries were omitted."
                                | otherwise = ""
                        pure $ Right $
                            "Directory listing for "
                                <> display
                                <> ":\n"
                                <> tree
                                <> notice

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
