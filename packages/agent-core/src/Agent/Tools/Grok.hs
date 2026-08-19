-- | Grok-build coding tools.
--
-- Wire names, JSON keys, and output phrasing are copied from
-- xai-org/grok-build @ crates/codegen/xai-grok-tools/src/implementations/grok_build.
-- Do not rename these to match Codex; Grok models are trained on this dialect.
module Agent.Tools.Grok
    ( grokTools
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
import Agent.Tools.IO
    ( CommandResult(..)
    , listDirectoryEntries
    , readTextFile
    , resolveUnderCwd
    , runShellCommand
    , truncateText
    , writeTextFile
    )
import Agent.Tools.Types
    ( AppTool(..)
    , AppToolKind(..)
    , ToolEnv(..)
    )
import Data.Aeson (FromJSON(..), Object)
import Data.Aeson.Types (Parser)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (doesDirectoryExist, doesFileExist, findExecutable)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

-- Upstream: grok-build grok_build::{read_file, grep, list_dir, search_replace, bash}.
grokTools :: ToolEnv -> [AppTool]
grokTools env =
    [ readFileTool env
    , grepTool env
    , listDirTool env
    , searchReplaceTool env
    , runTerminalCmdTool env
    ]

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
    }

--------------------------------------------------------------------------------
-- read_file
--------------------------------------------------------------------------------

data ReadFileArgs = ReadFileArgs
    { targetFile :: Text
    , offset :: Maybe Int
    , limit :: Maybe Int
    }

instance FromJSON ReadFileArgs where
    parseJSON = objectArgs \object -> ReadFileArgs
        <$> reqText object "target_file"
        <*> optInt object "offset"
        <*> optInt object "limit"

readFileTool :: ToolEnv -> AppTool
readFileTool env = jsonTool "read_file" readFileDescription
    [ PropertySchema "target_file" PropertyString True $ Just
        "The path of the file to read. You can use either a relative path in the workspace or an absolute path. If an absolute path is provided, it will be preserved as is."
    , PropertySchema "offset" PropertyInteger False $ Just
        "The line number to start reading from. Only provide if the file is too large to read at once."
    , PropertySchema "limit" PropertyInteger False $ Just
        "The number of lines to read. Only provide if the file is too large to read at once."
    ]
    True
    (typedTool "read_file" (runReadFile env))

readFileDescription :: Text
readFileDescription =
    "Read a file.\n\
    \\n\
    \- The target_file parameter can be a relative path in the workspace or an absolute path\n\
    \- By default, it reads up to 2000 lines starting from the beginning of the file\n\
    \- Line numbers (1-based) appear as anchors in the format LINE_NUMBER\8594LINE_CONTENT on the first returned line and on every 10th line of the file; the lines in between show content only. Count from the nearest anchor when referring to a specific line"

runReadFile :: ToolEnv -> ReadFileArgs -> IO (Either Text Text)
runReadFile env args = resolveUnderCwd env (Text.unpack args.targetFile) >>= \case
    Left err -> pure (Left err)
    Right path -> doesFileExist path >>= \case
        False -> pure $ Left $ "File not found: " <> args.targetFile
        True -> readTextFile path >>= \case
            Left err -> pure (Left err)
            Right content -> pure $ Right $ formatReadFile content args.offset args.limit

formatReadFile :: Text -> Maybe Int -> Maybe Int -> Text
formatReadFile content offsetArg limitArg =
    let allLines = Text.splitOn "\n" content
        total = length allLines
        start = max 1 (fromMaybe 1 offsetArg)
        window = drop (start - 1) allLines
        taken = maybe (take maxReadLines window) (`take` window) limitArg
        numbered = formatNumbered start taken
        truncated = truncateText 100000 numbered
    in if start > total && total > 0
        then "Offset " <> Text.pack (show start)
            <> " is beyond the end of the file ("
            <> Text.pack (show total) <> " lines)."
        else truncated

maxReadLines :: Int
maxReadLines = 2000

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
    }

instance FromJSON GrepArgs where
    parseJSON = objectArgs \object -> GrepArgs
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
        Right path -> do
            rg <- findExecutable "rg"
            let limit = min 2000 (fromMaybe 200 args.headLimit)
            output <- case rg of
                Just rgPath -> runRipgrep rgPath path args
                Nothing -> runNaiveGrep path args
            pure $ fmap (limitLines limit) output

runRipgrep :: FilePath -> FilePath -> GrepArgs -> IO (Either Text Text)
runRipgrep rgPath path args = do
    let rgArgs = concat
            [ ["--regexp", Text.unpack args.pattern, "--", path]
            , maybe [] (\g -> ["--glob", Text.unpack g]) args.glob
            , maybe [] (\n -> ["-B", show n]) args.before
            , maybe [] (\n -> ["-A", show n]) args.after
            , maybe [] (\n -> ["-C", show n]) args.context
            , ["-i" | args.caseInsensitive]
            , maybe [] (\t -> ["--type", Text.unpack t]) args.fileType
            , if args.multiline then ["-U", "--multiline-dotall"] else []
            ]
    (code, stdout, stderr) <- readProcessWithExitCode rgPath rgArgs ""
    case code of
        ExitSuccess -> pure $ Right (Text.pack stdout)
        ExitFailure 1 | null stdout ->
            if null stderr
                then pure (Right "No matches found.")
                else pure (Left (Text.pack stderr))
        ExitFailure _ ->
            pure $ Left $ Text.pack (if null stderr then stdout else stderr)

runNaiveGrep :: FilePath -> GrepArgs -> IO (Either Text Text)
runNaiveGrep path args = do
    isDir <- doesDirectoryExist path
    files <- if isDir
        then naiveCollectFiles path
        else pure [path]
    matches <- concat <$> mapM (grepFile args) files
    pure $ Right $ if null matches
        then "No matches found."
        else Text.unlines matches

naiveCollectFiles :: FilePath -> IO [FilePath]
naiveCollectFiles root = do
    entries <- listDirectoryEntries root
    case entries of
        Left _ -> pure []
        Right items -> fmap concat $ mapM follow items
  where
    follow (name, True)
        | name == ".git" = pure []
        | otherwise = naiveCollectFiles (root </> name)
    follow (name, False) = pure [root </> name]

grepFile :: GrepArgs -> FilePath -> IO [Text]
grepFile args path = readTextFile path >>= \case
    Left _ -> pure []
    Right content ->
        let needle = if args.caseInsensitive
                then Text.toLower args.pattern
                else args.pattern
            hay = if args.caseInsensitive then Text.toLower else id
            numbered = zip [1 :: Int ..] (Text.splitOn "\n" content)
            hits =
                [ Text.pack path <> ":" <> Text.pack (show n) <> ":" <> line
                | (n, line) <- numbered
                , needle `Text.isInfixOf` hay line
                ]
        in pure hits

limitLines :: Int -> Text -> Text
limitLines n text =
    let ls = Text.lines text
    in if length ls <= n
        then text
        else Text.unlines (take n ls) <> "...[truncated]"

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
    \The 'target_directory' parameter can be relative to the workspace root or absolute."

runListDir :: ToolEnv -> ListDirArgs -> IO (Either Text Text)
runListDir env args = resolveUnderCwd env (Text.unpack args.targetDirectory) >>= \case
    Left err -> pure (Left err)
    Right path -> doesDirectoryExist path >>= \case
        False -> pure $ Left $
            "Error: " <> args.targetDirectory <> " is not a valid directory"
        True -> listDirectoryEntries path >>= \case
            Left err -> pure (Left err)
            Right entries ->
                let sorted = sort entries
                    formatted =
                        [ if isDir then Text.pack name <> "/" else Text.pack name
                        | (name, isDir) <- sorted
                        ]
                    capped = take 500 formatted
                    body = Text.unlines capped
                    notice = if length formatted > 500
                        then "\n...[truncated]"
                        else ""
                in pure $ Right $
                    "Directory listing for " <> args.targetDirectory <> ":\n" <> body <> notice

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

searchReplaceTool :: ToolEnv -> AppTool
searchReplaceTool env = jsonTool "search_replace" searchReplaceDescription
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
    (typedTool "search_replace" (runSearchReplace env))

searchReplaceDescription :: Text
searchReplaceDescription =
    "Replace an exact string in a file.\n\
    \\n\
    \- read_file prefixes each line with \"LINE_NUMBER\8594\". That prefix is not part of the file: match only what comes after the \8594, with its exact indentation.\n\
    \- old_string must match exactly one place in the file. If it appears more than once, add surrounding lines to make it unique, or set replace_all to change every occurrence (handy for renaming an identifier).\n\
    \- To create a new file, set old_string to an empty string."

runSearchReplace :: ToolEnv -> SearchReplaceArgs -> IO (Either Text Text)
runSearchReplace env args
    | args.oldString == args.newString =
        pure (Left "Old string and new string are the same")
    | Text.null args.oldString = createNewFile env args
    | otherwise = replaceInFile env args

createNewFile :: ToolEnv -> SearchReplaceArgs -> IO (Either Text Text)
createNewFile env args = resolveUnderCwd env (Text.unpack args.filePath) >>= \case
    Left err -> pure (Left err)
    Right path -> doesFileExist path >>= \case
        True -> readTextFile path >>= \case
            Left err -> pure (Left err)
            Right existing
                | Text.null existing ->
                    writeCreated path
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
    Right path -> doesFileExist path >>= \case
        False -> pure $ Left $ "File not found: " <> args.filePath
        True -> readTextFile path >>= \case
            Left err -> pure (Left err)
            Right content ->
                let count = countOccurrences args.oldString content
                in case count of
                    0 -> pure $ Left $
                        "The string to replace was not found in the file, use the read_file tool to see the correct string. The user may have changed the file since you last read it."
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

runTerminalCmdTool :: ToolEnv -> AppTool
runTerminalCmdTool env = jsonTool "run_terminal_cmd" terminalDescription
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
    (typedTool "run_terminal_cmd" (runTerminal env))

terminalDescription :: Text
terminalDescription =
    "Run a bash command and return its output.\n\
    \- Always set a timeout for commands that may hang.\n\
    \- Prefer dedicated tools (read_file, grep, list_dir, search_replace) over shell equivalents when they exist."

runTerminal :: ToolEnv -> TerminalArgs -> IO (Either Text Text)
runTerminal env args
    | Text.null args.description =
        pure (Left "Missing parameter: description")
    | args.background =
        pure $ Left
            "Background execution is not available yet. Run the command in the foreground without background=true."
    | otherwise = do
        let timeoutMs = min 300000 (max 1 (fromMaybe 120000 args.timeout))
        result <- runShellCommand env env.toolCwd (Text.unpack args.command) timeoutMs
        if result.commandTimedOut
            then pure $ Left $
                "Error: Command timed out after " <> Text.pack (show timeoutMs) <> "ms"
            else
                let code = fromMaybe 1 result.commandExitCode
                    stdout = result.commandStdout
                    stderr = result.commandStderr
                    body
                        | Text.null stderr = stdout
                        | Text.null stdout = stderr
                        | otherwise = stdout <> "\nstderr:\n" <> stderr
                in pure $ Right $
                    "Exit code: " <> Text.pack (show code) <> "\n" <> body

