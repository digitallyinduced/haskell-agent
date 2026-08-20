-- | Grok-build coding tools.
--
-- Wire names, JSON keys, and output phrasing are copied from
-- xai-org/grok-build @ crates/codegen/xai-grok-tools/src/implementations/grok_build.
-- Do not rename these to match Codex; Grok models are trained on this dialect.
module Agent.Tools.Grok
    ( grokTools
    , filterGrokToolsForType
    , newGrokSession
    , closeGrokSession
    , GrokSession
    ) where

import Agent.ToolArgs
    ( objectArgs
    , optBool
    , optInt
    , optText
    , reqText
    )
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    )
import Agent.ToolDispatch (ToolHandler, typedTool)
import Control.Applicative ((<|>))
import Agent.Tools.Ghci (GhciSession, runGhciTool)
import Agent.Tools.Grok.Shell
    ( GrokSession(..)
    , closeGrokSession
    , hasUnwaitedBackgroundOp
    , killTask
    , newGrokSession
    , readTaskOutput
    , runForeground
    , startBackground
    )
import Agent.Tools.IO
    ( CommandResult(..)
    , listDirectoryEntries
    , readTextFile
    , resolveUnderCwd
    , writeTextFile
    )
import Agent.Tools.PlanMode
    ( PlanModeEnv
    , askUserQuestionTool
    , enterPlanModeTool
    , exitPlanModeTool
    , isPlanFileEditTarget
    , isPlanModeActive
    , planFilePath
    , planModeBlockedEditMessage
    )
import Agent.Tools.Dangerous (commandLooksLikeRmRf, forbiddenRmRfReason)
import Agent.Tools.Grok.Task
    ( filterGrokToolsForType
    , isSubagentIdText
    , taskTool
    )
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Agent.Subagents
    ( SubagentId(..)
    , SubagentStatus(..)
    , closeSubagent
    , getStatus
    , waitSubagents
    )
import Agent.Tools.Types
    ( AppTool(..)
    , AppToolKind(..)
    , ToolEnv(..)
    )
import Data.Aeson (FromJSON(..), Object)
import Data.Aeson.Types (Parser)
import Data.IORef
import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (doesDirectoryExist, doesFileExist, findExecutable)
import System.Exit (ExitCode(..))
import System.FilePath (takeExtension, (</>))
import System.Process (readProcessWithExitCode)

-- Upstream: grok-build grok_build::{read_file, grep, list_dir, search_replace, bash,
-- get_task_output, kill_task, task, enter_plan_mode, exit_plan_mode, ask_user_question}.
-- Local extension: run_ghci (persistent GHCi with per-call purity approval).
grokTools
    :: GrokSession
    -> GhciSession
    -> PlanModeEnv
    -> Maybe MultiAgentContext
    -> IORef (Map SubagentId Text)
    -> [AppTool]
grokTools session ghci planMode multi typesRef =
    let env = session.grokEnv
        base =
            [ readFileTool env
            , grepTool env
            , listDirTool env
            , searchReplaceTool env planMode
            , runTerminalCmdTool session
            , runGhciTool ghci
            , getTaskOutputTool session multi
            , killTaskTool session multi
            , enterPlanModeTool planMode
            , exitPlanModeTool planMode
            , askUserQuestionTool planMode
            ]
    in case multi of
        Nothing -> base
        Just ctx -> base ++ [taskTool ctx typesRef]

jsonTool
    :: Text
    -> Text
    -> [PropertySchema]
    -> Bool
    -> ToolHandler
    -> AppTool
jsonTool name description parameters readOnly handler = AppTool
    { appToolName = name
    , appToolDescription = description
    , appToolParameters = parameters
    , appToolHandler = handler
    , appToolKind = JsonFunction
    , appToolReadOnly = readOnly
    , appToolIsReadOnlyCall = Nothing
    }

--------------------------------------------------------------------------------
-- read_file
--------------------------------------------------------------------------------

data ReadFileArgs = ReadFileArgs
    { targetFile :: Text
    , offset :: Maybe Int
    , limit :: Maybe Int
    , pages :: Maybe Text
    , format :: Maybe Text
    } deriving (Eq, Show)

instance FromJSON ReadFileArgs where
    parseJSON = objectArgs \object -> ReadFileArgs
        <$> reqText object "target_file"
        <*> optInt object "offset"
        <*> optInt object "limit"
        <*> optText object "pages"
        <*> optText object "format"

readFileTool :: ToolEnv -> AppTool
readFileTool env = jsonTool "read_file" readFileDescription
    [ PropertySchema "target_file" PropertyString True $ Just
        "The path of the file to read. You can use either a relative path in the workspace or an absolute path. If an absolute path is provided, it will be preserved as is."
    , PropertySchema "offset" PropertyInteger False $ Just
        "The line number to start reading from. Only provide if the file is too large to read at once."
    , PropertySchema "limit" PropertyInteger False $ Just
        "The number of lines to read. Only provide if the file is too large to read at once."
    , PropertySchema "pages" PropertyString False $ Just
        "Page range for PDF files (e.g. '1-5', '3', '10-'). Required for PDFs with more than 10 pages. Max 20 pages per call. Ignored for non-PDF files."
    , PropertySchema "format" PropertyString False $ Just
        "Output format for PDF files. 'image' (default) renders pages as images. 'text' extracts text content. Ignored for non-PDF files."
    ]
    True
    (typedTool "read_file" (runReadFile env))

readFileDescription :: Text
readFileDescription =
    "Read a file.\n\
    \\n\
    \- The target_file parameter can be a relative path in the workspace or an absolute path\n\
    \- By default, it reads up to 1000 lines starting from the beginning of the file\n\
    \- Line numbers (1-based) appear as anchors in the format LINE_NUMBER\8594LINE_CONTENT on the first returned line and on every 10th line of the file; the lines in between show content only. Count from the nearest anchor when referring to a specific line"

maxReadLines :: Int
maxReadLines = 1000

maxReadTokens :: Int
maxReadTokens = 25000

runReadFile :: ToolEnv -> ReadFileArgs -> IO (Either Text Text)
runReadFile env args = resolveUnderCwd env (Text.unpack args.targetFile) >>= \case
    Left err -> pure (Left err)
    Right path
        | ".pdf" `Text.isSuffixOf` Text.toLower args.targetFile ->
            pure $ Left
                "PDF rendering is not available. Use run_terminal_cmd with pdftotext, or convert the file to text first."
        | otherwise -> doesFileExist path >>= \case
            False -> pure $ Left $ "File not found: " <> args.targetFile
            True -> readTextFile path >>= \case
                Left err -> pure (Left err)
                Right content -> do
                    -- pages/format apply only to PDFs; ignore them for text.
                    _ <- pure (args.pages, args.format)
                    pure (formatReadFile content args)

formatReadFile :: Text -> ReadFileArgs -> Either Text Text
formatReadFile content args =
    let allLines = Text.splitOn "\n" content
        total = length allLines
        start = resolveReadStartLine content args.offset
        window = drop (start - 1) allLines
        takeCount = min maxReadLines (fromMaybe maxReadLines args.limit)
        taken = take takeCount window
        numbered = formatNumbered start taken
        tokens = estimateTokens numbered
        rangeSpecified = args.offset /= Nothing || args.limit /= Nothing
    in if start > total && total > 0
        then Right $ "Offset " <> Text.pack (show start)
            <> " is beyond the end of the file ("
            <> Text.pack (show total) <> " lines)."
        else if tokens > maxReadTokens
            then Left (tokenLimitMessage tokens rangeSpecified args)
            else Right numbered

resolveReadStartLine :: Text -> Maybe Int -> Int
resolveReadStartLine content offset = case fromMaybe 1 offset of
    0 -> 1
    n | n > 0 -> n
    n ->
        let totalFields = length (Text.splitOn "\n" content)
            extra
                | not (Text.null content) && not (Text.isSuffixOf "\n" content) = 1
                | otherwise = 0
        in max 1 (totalFields + extra + n + 1)

estimateTokens :: Text -> Int
estimateTokens text = max 1 (Text.length text `div` 4)

tokenLimitMessage :: Int -> Bool -> ReadFileArgs -> Text
tokenLimitMessage tokens rangeSpecified args
    | rangeSpecified =
        "The requested line range (offset=" <> off <> ", limit=" <> lim
            <> ") contains " <> Text.pack (show tokens)
            <> " tokens, which exceeds the maximum allowed tokens ("
            <> Text.pack (show maxReadTokens)
            <> " tokens).\nTry a smaller `limit`, a different starting `offset`, \
               \or use the 'grep' tool to search for specific content."
    | otherwise =
        "File content (" <> Text.pack (show tokens)
            <> " tokens) exceeds maximum allowed tokens ("
            <> Text.pack (show maxReadTokens)
            <> " tokens).\nTry a smaller `limit`, a different starting `offset`, \
               \or use the 'grep' tool to search for specific content."
  where
    off = maybe "1" (Text.pack . show) args.offset
    lim = maybe "to end" (Text.pack . show) args.limit

formatNumbered :: Int -> [Text] -> Text
formatNumbered start lines_ =
    Text.intercalate "\n" (zipWith fmt [start..] lines_)
  where
    fmt n line
        | n == start || n `mod` 10 == 0 =
            Text.pack (show n) <> "\8594" <> line
        | otherwise = line

--------------------------------------------------------------------------------
-- grep
--------------------------------------------------------------------------------

data GrepOutputMode = GrepContent | GrepFilesWithMatches | GrepCount
    deriving (Eq, Show)

data GrepArgs = GrepArgs
    { pattern :: Text
    , path :: Maybe Text
    , glob :: Maybe Text
    , before :: Maybe Int
    , after :: Maybe Int
    , context :: Maybe Int
    , caseInsensitive :: Bool
    , fileType :: Maybe Text
    , headLimit :: Maybe Int
    , multiline :: Bool
    , outputMode :: GrepOutputMode
    }

instance FromJSON GrepArgs where
    parseJSON = objectArgs \object -> do
        modeText <- optText object "output_mode"
        GrepArgs
            <$> reqText object "pattern"
            <*> optText object "path"
            <*> optText object "glob"
            <*> optInt object "-B"
            <*> optInt object "-A"
            <*> optInt object "-C"
            <*> (fromMaybe False <$> optBool object "-i")
            <*> optText object "type"
            <*> optInt object "head_limit"
            <*> (fromMaybe False <$> optBool object "multiline")
            <*> pure (parseOutputMode modeText)

parseOutputMode :: Maybe Text -> GrepOutputMode
parseOutputMode = \case
    Just "files_with_matches" -> GrepFilesWithMatches
    Just "count" -> GrepCount
    _ -> GrepContent

grepTool :: ToolEnv -> AppTool
grepTool env = jsonTool "grep" grepDescription
    [ PropertySchema "pattern" PropertyString True $ Just
        "The regular expression pattern to search for in file contents (rg --regexp)"
    , PropertySchema "path" PropertyString False $ Just
        "File or directory to search in (rg pattern -- PATH). Defaults to workspace path."
    , PropertySchema "glob" PropertyString False $ Just
        "Glob pattern (rg --glob GLOB -- PATH) to filter files (e.g. \"*.js\", \"*.{ts,tsx}\")."
    , PropertySchema "-B" PropertyInteger False $ Just
        "Number of lines to show before each match (rg -B)."
    , PropertySchema "-A" PropertyInteger False $ Just
        "Number of lines to show after each match (rg -A)."
    , PropertySchema "-C" PropertyInteger False $ Just
        "Number of lines to show before and after each match (rg -C)."
    , PropertySchema "-i" PropertyBoolean False $ Just
        "Case insensitive search (rg -i)."
    , PropertySchema "type" PropertyString False $ Just
        "File type to search (rg --type). Common types: js, py, rust, go, java, etc. More efficient than glob for standard file types."
    , PropertySchema "head_limit" PropertyInteger False $ Just
        "Limit output to first N lines/entries, equivalent to \"| head -N\". Defaults to 200 lines or 500 entries."
    , PropertySchema "multiline" PropertyBoolean False $ Just
        "Enable multiline mode where . matches newlines and patterns can span lines (rg -U --multiline-dotall)."
    , PropertySchema "output_mode" PropertyString False $ Just
        "content (default), files_with_matches, or count."
    ]
    True
    (typedTool "grep" (runGrep env))

grepDescription :: Text
grepDescription =
    "Search file contents with regular expressions (ripgrep).\n\
    \\n\
    \- Full regex syntax, so escape literal special characters: `functionCall\\(`, or `interface\\{\\}` to find interface{} in Go.\n\
    \- Pass pattern as a raw regex string — no surrounding quotes.\n\
    \- Respects .gitignore unless you pass a broad glob like '--glob *'.\n\
    \- Only filter by 'type' or 'glob' when you are sure of the file type; import paths may not match source file types (.js vs .ts).\n\
    \- Output is ripgrep-style: ':' marks match lines, '-' marks context lines, grouped by file. Large results are capped and report \"at least\" counts."

runGrep :: ToolEnv -> GrepArgs -> IO (Either Text Text)
runGrep env args = do
    let searchRoot = maybe env.toolCwd Text.unpack args.path
    resolveUnderCwd env searchRoot >>= \case
        Left err -> pure (Left err)
        Right path -> findExecutable "rg" >>= \case
            Nothing -> pure $ Left
                "rg is not installed. Install ripgrep to use the grep tool."
            Just rgPath -> do
                let limit = effectiveHeadLimit args
                runRipgrep rgPath path args limit >>= \case
                    Left err -> pure (Left err)
                    Right raw -> pure $ Right (formatGrepCard env.toolCwd raw limit)

effectiveHeadLimit :: GrepArgs -> Int
effectiveHeadLimit args = case args.outputMode of
    GrepContent -> min 2000 (fromMaybe 200 args.headLimit)
    _ -> min 10000 (fromMaybe 500 args.headLimit)

runRipgrep :: FilePath -> FilePath -> GrepArgs -> Int -> IO (Either Text Text)
runRipgrep rgPath path args _limit = do
    let modeFlags = case args.outputMode of
            GrepContent -> ["--heading", "--with-filename", "--line-number"]
            GrepFilesWithMatches -> ["--files-with-matches"]
            GrepCount -> ["--count"]
        -- All options must come before `--`; ripgrep treats everything after
        -- as paths, so a trailing `--glob` becomes "No such file or directory".
        rgArgs = concat
            [ modeFlags
            , ["--color=never", "--max-columns", "1000"]
            , maybe [] (\g -> ["--glob", Text.unpack g]) args.glob
            , maybe [] (\n -> ["-B", show n]) args.before
            , maybe [] (\n -> ["-A", show n]) args.after
            , maybe [] (\n -> ["-C", show n]) args.context
            , ["-i" | args.caseInsensitive]
            , maybe [] (\t -> ["--type", Text.unpack t]) args.fileType
            , if args.multiline then ["-U", "--multiline-dotall"] else []
            , ["--regexp", Text.unpack args.pattern, "--", path]
            ]
    (code, stdout, stderr) <- readProcessWithExitCode rgPath rgArgs ""
    let raw = Text.pack stdout
    case code of
        ExitSuccess -> pure $ Right raw
        ExitFailure 1 | null stdout ->
            if null stderr
                then pure (Right "")
                else pure (Left (Text.pack stderr))
        ExitFailure _ ->
            pure $ Left $ Text.pack (if null stderr then stdout else stderr)

formatGrepCard :: FilePath -> Text -> Int -> Text
formatGrepCard cwd raw limit
    | Text.null (Text.strip raw) = "No matches found."
    | otherwise =
        let ls = Text.lines raw
            truncated = length ls > limit
            kept = take limit ls
            body = Text.unlines kept
            footer
                | truncated =
                    "\n[at least " <> Text.pack (show limit)
                        <> " lines; output truncated]"
                | otherwise = ""
        in "<workspace_result workspace_path=\""
            <> Text.pack cwd
            <> "\">\n"
            <> body
            <> footer
            <> "</workspace_result>"

--------------------------------------------------------------------------------
-- list_dir
--------------------------------------------------------------------------------

newtype ListDirArgs = ListDirArgs { targetDirectory :: Text }

instance FromJSON ListDirArgs where
    parseJSON = objectArgs \object -> ListDirArgs <$> reqText object "target_directory"

listDirTool :: ToolEnv -> AppTool
listDirTool env = jsonTool "list_dir" listDirDescription
    [ PropertySchema "target_directory" PropertyString True $ Just
        "Path to directory to list contents of, relative to the workspace root or absolute."
    ]
    True
    (typedTool "list_dir" (runListDir env))

listDirDescription :: Text
listDirDescription =
    "Lists files and directories in a given path.\n\
    \The 'target_directory' parameter can be relative to the workspace root or absolute.\n\
    \\n\
    \Other details:\n\
    \    - The result does not display dot-files and dot-directories.\n\
    \    - Respects .gitignore patterns (files/directories ignored by git are not shown).\n\
    \    - Large directories are summarized with file counts and extension breakdowns instead of listing all files."

maxListItems :: Int
maxListItems = 200

runListDir :: ToolEnv -> ListDirArgs -> IO (Either Text Text)
runListDir env args = resolveUnderCwd env (Text.unpack args.targetDirectory) >>= \case
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
    = FileNode FilePath
    | DirectoryNode FilePath [DirNode]
    deriving (Eq, Show)

collectDir :: FilePath -> FilePath -> IO [DirNode]
collectDir cwd path = do
    listed <- listDirectoryEntries path
    case listed of
        Left _ -> pure []
        Right raw -> do
            let visible = sortOn fst
                    [ (name, isDir)
                    | (name, isDir) <- raw
                    , not ("." `Text.isPrefixOf` Text.pack name)
                    ]
            fmap concat $ mapM (toNode cwd path) visible

toNode :: FilePath -> FilePath -> (FilePath, Bool) -> IO [DirNode]
toNode cwd parent (name, isDir) = do
    let full = parent </> name
    ignored <- isGitIgnored cwd full
    if ignored
        then pure []
        else if not isDir
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

summarizeDir :: FilePath -> [DirNode] -> DirNode
summarizeDir name children =
    let files = [file | FileNode file <- children]
        dirs = [dir | dir@DirectoryNode{} <- children]
    in if length files > 20 && null dirs
        then DirectoryNode (name <> " " <> extensionSummary files) []
        else DirectoryNode name children

extensionSummary :: [FilePath] -> String
extensionSummary files =
    let counts = Map.fromListWith (+)
            [ (ext, 1 :: Int)
            | file <- files
            , let ext = case takeExtension file of
                    "" -> "(no ext)"
                    e -> e
            ]
        rendered =
            [ Text.pack (show n) <> " *" <> Text.pack ext
            | (ext, n) <- sortOn (negate . snd) (Map.toList counts)
            ]
    in "(" <> show (length files) <> " files: "
        <> Text.unpack (Text.intercalate ", " (take 4 rendered)) <> ")"

renderTree :: Int -> [DirNode] -> Text
renderTree depth = Text.unlines . map (renderNode depth)

renderNode :: Int -> DirNode -> Text
renderNode depth = \case
    FileNode name -> indent <> "- " <> Text.pack name
    DirectoryNode name children ->
        let header = indent <> "- " <> Text.pack name
                <> if "/" `Text.isSuffixOf` Text.pack name || "(" `Text.isInfixOf` Text.pack name
                    then ""
                    else "/"
        in if null children
            then header
            else header <> "\n" <> renderTree (depth + 1) children
  where
    indent = Text.replicate depth "  "

--------------------------------------------------------------------------------
-- search_replace
--------------------------------------------------------------------------------

data SearchReplaceArgs = SearchReplaceArgs
    { filePath :: Text
    , oldString :: Text
    , newString :: Text
    , replaceAll :: Bool
    }

instance FromJSON SearchReplaceArgs where
    parseJSON = objectArgs \object -> SearchReplaceArgs
        <$> reqText object "file_path"
        <*> reqText object "old_string"
        <*> reqText object "new_string"
        <*> (fromMaybe False <$> optBool object "replace_all")

searchReplaceTool :: ToolEnv -> PlanModeEnv -> AppTool
searchReplaceTool env planMode = jsonTool "search_replace" searchReplaceDescription
    [ PropertySchema "file_path" PropertyString True $ Just
        "The path to the file to modify. You can use either a relative path in the workspace or an absolute path."
    , PropertySchema "old_string" PropertyString True $ Just
        "The text to replace"
    , PropertySchema "new_string" PropertyString True $ Just
        "The text to replace it with (must be different from old_string)"
    , PropertySchema "replace_all" PropertyBoolean False $ Just
        "Replace all occurrences of old_string (default false)"
    ]
    False
    (typedTool "search_replace" (runSearchReplace env planMode))

searchReplaceDescription :: Text
searchReplaceDescription =
    "Replace an exact string in a file.\n\
    \\n\
    \- read_file prefixes each line with \"LINE_NUMBER\8594\". That prefix is not part of the file: match only what comes after the \8594, with its exact indentation.\n\
    \- old_string must match exactly one place in the file. If it appears more than once, add surrounding lines to make it unique, or set replace_all to change every occurrence (handy for renaming an identifier).\n\
    \- To create a new file, set old_string to an empty string. An empty old_string cannot overwrite an existing non-empty file."

runSearchReplace :: ToolEnv -> PlanModeEnv -> SearchReplaceArgs -> IO (Either Text Text)
runSearchReplace env planMode args = do
    active <- isPlanModeActive planMode
    if not active
        then runSearchReplaceBody env args
        else do
            planPath <- planFilePath planMode
            resolved <- resolveUnderCwd env (Text.unpack args.filePath)
            case resolved of
                Left err -> pure (Left err)
                Right path
                    | isPlanFileEditTarget planPath path ->
                        runSearchReplaceBody env args
                    | otherwise ->
                        pure (Left (planModeBlockedEditMessage planPath))

runSearchReplaceBody :: ToolEnv -> SearchReplaceArgs -> IO (Either Text Text)
runSearchReplaceBody env args
    | args.oldString == args.newString =
        pure (Left "Old string and new string are the same")
    | Text.null args.oldString = createNewFile env args
    | otherwise = replaceInFile env args

createNewFile :: ToolEnv -> SearchReplaceArgs -> IO (Either Text Text)
createNewFile env args = resolveUnderCwd env (Text.unpack args.filePath) >>= \case
    Left err -> pure (Left err)
    Right path -> gitignoreGuard env path args.filePath >>= \case
        Just err -> pure (Left err)
        Nothing -> doesFileExist path >>= \case
            True -> readTextFile path >>= \case
                Left err -> pure (Left err)
                Right existing
                    | Text.null existing -> writeCreated path
                    | otherwise ->
                        pure $ Left "An empty old_string cannot overwrite an existing non-empty file."
            False -> writeCreated path
  where
    writeCreated path = writeTextFile path args.newString >>= \case
        Left err -> pure (Left err)
        Right () -> pure $ Right $
            "The file " <> args.filePath <> " has been created successfully."

replaceInFile :: ToolEnv -> SearchReplaceArgs -> IO (Either Text Text)
replaceInFile env args = resolveUnderCwd env (Text.unpack args.filePath) >>= \case
    Left err -> pure (Left err)
    Right path -> gitignoreGuard env path args.filePath >>= \case
        Just err -> pure (Left err)
        Nothing -> doesFileExist path >>= \case
            False -> pure $ Left $ "File not found: " <> args.filePath
            True -> readTextFile path >>= \case
                Left err -> pure (Left err)
                Right content ->
                    let count = countOccurrences args.oldString content
                    in case count of
                        0 -> pure $ Left $
                            "The string to replace was not found in the file, use the read_file tool to see the correct string. The user may have changed the file since you last read it."
                                <> nearestMatchHint content args.oldString
                        n | n > 1 && not args.replaceAll ->
                            pure $ Left
                                "The string to replace was found multiple times in the file. Use replace_all to replace all occurrences, or include more context to only edit one occurrence."
                        _ ->
                            let updated = replaceOccurrences args.oldString args.newString args.replaceAll content
                            in writeTextFile path updated >>= \case
                                Left err -> pure (Left err)
                                Right () -> pure $ Right $
                                    if args.replaceAll && count > 1
                                        then "The file " <> args.filePath
                                            <> " has been updated. All occurrences were successfully replaced."
                                        else "The file " <> args.filePath
                                            <> " has been updated successfully."

gitignoreGuard :: ToolEnv -> FilePath -> Text -> IO (Maybe Text)
gitignoreGuard env path display = do
    ignored <- isGitIgnored env.toolCwd path
    pure $ if ignored
        then Just $ "Error: " <> display <> " is ignored by .gitignore and cannot be edited."
        else Nothing

nearestMatchHint :: Text -> Text -> Text
nearestMatchHint file oldString =
    let firstLine = fromMaybe oldString (listToMaybe (Text.lines oldString))
        keyword = case sortOn (negate . Text.length) (Text.words firstLine) of
            (w : _) -> w
            [] -> oldString
        hit = listToMaybe
            [ (n, line)
            | (n, line) <- zip [1 :: Int ..] (Text.lines file)
            , not (Text.null keyword)
            , keyword `Text.isInfixOf` line
            ]
    in case hit of
        Nothing -> ""
        Just (n, line) ->
            let full = "\n\nNearest match: line " <> Text.pack (show n) <> ": " <> Text.stripEnd line
            in if Text.length full > 200 then Text.take 197 full <> "..." else full

countOccurrences :: Text -> Text -> Int
countOccurrences needle hay
    | Text.null needle = 0
    | otherwise = go hay 0
  where
    go rest n = case Text.breakOn needle rest of
        (_, after)
            | Text.null after -> n
            | otherwise -> go (Text.drop (Text.length needle) after) (n + 1)

replaceOccurrences :: Text -> Text -> Bool -> Text -> Text
replaceOccurrences old new replaceAll hay
    | replaceAll = Text.replace old new hay
    | otherwise = case Text.breakOn old hay of
        (before, after)
            | Text.null after -> hay
            | otherwise -> before <> new <> Text.drop (Text.length old) after

--------------------------------------------------------------------------------
-- run_terminal_cmd
--------------------------------------------------------------------------------

data TerminalArgs = TerminalArgs
    { command :: Text
    , timeout :: Maybe Int
    , description :: Text
    , background :: Bool
    }

instance FromJSON TerminalArgs where
    parseJSON = objectArgs \object -> TerminalArgs
        <$> reqText object "command"
        <*> optionalTimeout object
        <*> reqText object "description"
        <*> (fromMaybe False <$> optBool object "background")

optionalTimeout :: Object -> Parser (Maybe Int)
optionalTimeout object = do
    fromInt <- optInt object "timeout"
    fromText <- optText object "timeout"
    pure $ fromInt <|> (fromText >>= readTimeout)

readTimeout :: Text -> Maybe Int
readTimeout text =
    case reads (Text.unpack text) of
        [(n, "")] -> Just n
        _ -> Nothing

runTerminalCmdTool :: GrokSession -> AppTool
runTerminalCmdTool session = jsonTool "run_terminal_cmd" terminalDescription
    [ PropertySchema "command" PropertyString True $ Just
        "The bash command to run."
    , PropertySchema "timeout" PropertyInteger False $ Just
        "Optional timeout in milliseconds (max 300000). Default: 120000 (2 minutes), enforced for foreground commands only."
    , PropertySchema "description" PropertyString True $ Just
        "One sentence explanation as to why this command needs to be run and how it contributes to the goal."
    , PropertySchema "background" PropertyBoolean False $ Just
        "Set to true for long-running commands that should run in the background (e.g., dev servers, long builds). Returns a task id immediately while the command keeps running in the background; you are notified on completion, so do not poll or sleep-wait for it."
    ]
    False
    (typedTool "run_terminal_cmd" (runTerminal session))

terminalDescription :: Text
terminalDescription =
    "Run a bash command and return its output.\n\
    \- Always set a timeout for commands that may hang.\n\
    \- Prefer dedicated tools (read_file, grep, list_dir, search_replace) over shell equivalents when they exist."

runTerminal :: GrokSession -> TerminalArgs -> IO (Either Text Text)
runTerminal session args
    | Text.null args.description =
        pure (Left "Missing parameter: description")
    | commandLooksLikeRmRf args.command =
        pure (Left (forbiddenRmRfReason args.command))
    | not args.background && hasUnwaitedBackgroundOp args.command =
        pure $ Left
            "The command contains a background '&'. Set background=true to run it as a background task, or append `wait` if you meant to wait for the children."
    | args.background = startBackground session args.command
    | otherwise = do
        let timeoutMs = min 300000 (max 1 (fromMaybe 120000 args.timeout))
        result <- runForeground session (Text.unpack args.command) timeoutMs
        let body = stripAnsi (combinedOutput result)
        if result.commandCancelled
            then pure $ Right $ "exit: cancelled\n" <> body
            else if result.commandTimedOut
            then pure $ Right $ "exit: killed (timeout)\n" <> body
            else
                let code = fromMaybe 1 result.commandExitCode
                in pure $ Right $ "exit: " <> Text.pack (show code) <> "\n" <> body

data TaskOutputArgs = TaskOutputArgs
    { taskId :: Text
    , timeout :: Maybe Int
    }

instance FromJSON TaskOutputArgs where
    parseJSON = objectArgs \object ->
        TaskOutputArgs
            <$> (reqText object "task_id" <|> reqText object "task_ids")
            <*> optionalTimeout object

getTaskOutputTool :: GrokSession -> Maybe MultiAgentContext -> AppTool
getTaskOutputTool session multi = jsonTool "get_task_output" getTaskOutputDescription
    [ PropertySchema "task_id" PropertyString True $ Just
        "The task id from a background run_terminal_cmd or the subagent_id from task."
    , PropertySchema "timeout" PropertyInteger False $ Just
        "Optional wait in milliseconds. If omitted, return a snapshot immediately."
    ]
    True
    (typedTool "get_task_output" (runGetTaskOutput session multi))

getTaskOutputDescription :: Text
getTaskOutputDescription =
    "Read output from a background command or subagent.\n\
    \Use this for a snapshot of current output, or one bounded wait — not a polling loop."

runGetTaskOutput
    :: GrokSession
    -> Maybe MultiAgentContext
    -> TaskOutputArgs
    -> IO (Either Text Text)
runGetTaskOutput session multi args = case multi of
    Just ctx | isSubagentIdText args.taskId -> do
        let agentId = SubagentId args.taskId
            timeoutMs = fromMaybe 0 args.timeout
        if timeoutMs <= 0
            then do
                status <- getStatus ctx.multiRegistry agentId
                pure $ Right $ formatAgentWait args.taskId False (Just status)
            else do
                (statuses, timedOut) <-
                    waitSubagents ctx.multiRegistry [agentId] timeoutMs
                pure $ Right $ formatAgentWait args.taskId timedOut (Map.lookup agentId statuses)
    _ ->
        Right . stripAnsi <$> readTaskOutput session args.taskId args.timeout

formatAgentWait :: Text -> Bool -> Maybe SubagentStatus -> Text
formatAgentWait taskId timedOut mstatus = case mstatus of
    Just (Completed (Just text)) ->
        "subagent_id: " <> taskId <> "\nstatus: completed\nfinal:\n" <> text
    Just (Completed Nothing) ->
        "subagent_id: " <> taskId <> "\nstatus: completed"
    Just (Errored err) ->
        "subagent_id: " <> taskId <> "\nstatus: errored\nerror: " <> err
    Just Interrupted ->
        "subagent_id: " <> taskId <> "\nstatus: interrupted"
    Just Closed ->
        "subagent_id: " <> taskId <> "\nstatus: shutdown"
    Just Running
        | timedOut -> "subagent_id: " <> taskId <> "\nstatus: running (wait timed out)"
        | otherwise -> "subagent_id: " <> taskId <> "\nstatus: running"
    Just Pending ->
        "subagent_id: " <> taskId <> "\nstatus: pending"
    Just NotFound ->
        "Unknown task_id: " <> taskId
    Nothing ->
        "Unknown task_id: " <> taskId

newtype KillTaskArgs = KillTaskArgs { taskId :: Text }

instance FromJSON KillTaskArgs where
    parseJSON = objectArgs \object -> KillTaskArgs <$> reqText object "task_id"

killTaskTool :: GrokSession -> Maybe MultiAgentContext -> AppTool
killTaskTool session multi = jsonTool "kill_task" killTaskDescription
    [ PropertySchema "task_id" PropertyString True $ Just
        "The task id from a background run_terminal_cmd or the subagent_id from task."
    ]
    False
    (typedTool "kill_task" (runKillTask session multi))

killTaskDescription :: Text
killTaskDescription =
    "Kill a background command or close a subagent."

runKillTask
    :: GrokSession
    -> Maybe MultiAgentContext
    -> KillTaskArgs
    -> IO (Either Text Text)
runKillTask session multi args = case multi of
    Just ctx | isSubagentIdText args.taskId -> do
        result <- closeSubagent ctx.multiRegistry (SubagentId args.taskId)
        pure $ case result of
            Left err -> Left err
            Right _ -> Right ("closed subagent " <> args.taskId)
    _ ->
        Right . stripAnsi <$> killTask session args.taskId

combinedOutput :: CommandResult -> Text
combinedOutput result
    | Text.null result.commandStderr = result.commandStdout
    | Text.null result.commandStdout = result.commandStderr
    | otherwise = result.commandStdout <> "\n" <> result.commandStderr

stripAnsi :: Text -> Text
stripAnsi = Text.concat . go
  where
    go text = case Text.break (== '\ESC') text of
        (before, rest)
            | Text.null rest -> [before]
            | otherwise ->
                let afterEsc = Text.drop 1 rest
                in case Text.uncons afterEsc of
                    Just ('[', csi) ->
                        let dropped = Text.dropWhile (not . isCsiFinal) csi
                            rest' = if Text.null dropped then "" else Text.drop 1 dropped
                        in before : go rest'
                    _ -> before : "\ESC" : go afterEsc

    isCsiFinal c = c >= '@' && c <= '~'

--------------------------------------------------------------------------------
-- gitignore
--------------------------------------------------------------------------------

isGitIgnored :: FilePath -> FilePath -> IO Bool
isGitIgnored cwd path = findExecutable "git" >>= \case
    Nothing -> pure False
    Just git -> do
        (code, _, _) <- readProcessWithExitCode git
            ["-C", cwd, "check-ignore", "-q", "--", path] ""
        pure (code == ExitSuccess)
