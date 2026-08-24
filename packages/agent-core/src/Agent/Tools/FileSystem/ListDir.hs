module Agent.Tools.FileSystem.ListDir (listDirTool) where

import Agent.OsPath (fromText, toText)
import Agent.ToolArgs (objectArgs, reqText)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch (typedTool)
import Agent.Tools.FileSystem.GitIgnore (isGitIgnored)
import Agent.Tools.IO (listDirectoryEntries, resolveForRead)
import Agent.Tools.Types
    ( AppTool
    , ToolEnv(..)
    , ToolExecutionPolicy(..)
    , jsonTool
    )
import Data.Aeson (FromJSON(..))
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory.OsPath (doesDirectoryExist, pathIsSymbolicLink)
import System.OsPath (OsPath, takeExtension, (</>))

newtype ListDirArgs = ListDirArgs { targetDirectory :: Text }

instance FromJSON ListDirArgs where
    parseJSON = objectArgs \object -> ListDirArgs <$> reqText object "target_directory"

listDirTool :: ToolEnv -> AppTool
listDirTool env = jsonTool "list_dir" listDirDescription
    [ PropertySchema "target_directory" PropertyString True $ Just
        "Path to a directory within an allowed filesystem root. Relative paths use the workspace root; absolute paths may resolve within the workspace or session temp directory."
    ]
    True
    ParallelSafe
    (typedTool "list_dir" (runListDir env))

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
    Right path -> doesDirectoryExist path >>= \case
        False -> pure $ Left $
            "Error: " <> args.targetDirectory <> " is not a valid directory"
        True -> do
            entries <- collectDir env.toolCwd path
            let (shown, truncated) = capNodes maxListItems entries
                tree = renderTree 0 shown
                notice
                    | truncated =
                        "\nLarge directory summarized; some nested entries were omitted."
                    | otherwise = ""
            pure $ Right $
                "Directory listing for " <> args.targetDirectory <> ":\n" <> tree <> notice

data DirNode
    = FileNode OsPath
    | DirectoryNode OsPath [DirNode]
    deriving (Eq, Show)

collectDir :: OsPath -> OsPath -> IO [DirNode]
collectDir cwd path = do
    listed <- listDirectoryEntries path
    case listed of
        Left _ -> pure []
        Right raw -> do
            let visible = sortOn fst
                    [ (name, isDir)
                    | (name, isDir) <- raw
                    , not ("." `Text.isPrefixOf` toText name)
                    ]
            fmap concat $ mapM (toNode cwd path) visible

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
                    else do
                        children <- collectDir cwd full
                        pure [summarizeDir name children]

capNodes :: Int -> [DirNode] -> ([DirNode], Bool)
capNodes budget nodes =
    let (kept, remaining) = go budget nodes
    in (kept, remaining <= 0 && countNodes nodes > budget)
  where
    go remaining [] = ([], remaining)
    go remaining _ | remaining <= 0 = ([], remaining)
    go remaining (node : rest) =
        let size = countNodes [node]
            (more, left) = go (remaining - min size remaining) rest
        in if size > remaining
            then ([], 0)
            else (node : more, left)

countNodes :: [DirNode] -> Int
countNodes = sum . map \case
    FileNode _ -> 1
    DirectoryNode _ children -> 1 + countNodes children

summarizeDir :: OsPath -> [DirNode] -> DirNode
summarizeDir name children =
    let files = [file | FileNode file <- children]
        dirs = [dir | dir@DirectoryNode{} <- children]
    in if length files > 20 && null dirs
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
renderTree depth = Text.unlines . map (renderNode depth)

renderNode :: Int -> DirNode -> Text
renderNode depth = \case
    FileNode name -> indent <> "- " <> toText name
    DirectoryNode name children ->
        let header = indent <> "- " <> toText name
                <> if "/" `Text.isSuffixOf` toText name || "(" `Text.isInfixOf` toText name
                    then ""
                    else "/"
        in if null children
            then header
            else header <> "\n" <> renderTree (depth + 1) children
  where
    indent = Text.replicate depth "  "
