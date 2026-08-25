module Agent.Tools.FileSystem.Grep (grepTool) where

import Agent.OsPath (fromText, unsafeToFilePath)
import Agent.ToolArgs (objectArgs, optBool, optInt, optText, reqText)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch
    ( ToolCall(..)
    , decodeToolArguments
    , toolArgumentsValue
    , typedTool
    )
import Agent.Tools.IO (resolveForRead)
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
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (canonicalizePath, findExecutable)
import System.Exit (ExitCode(..))
import System.FilePath
    ( addTrailingPathSeparator
    , isRelative
    , makeRelative
    , splitDirectories
    )
import System.Process
    ( CreateProcess(cwd)
    , proc
    , readCreateProcessWithExitCode
    )
import System.OsPath (OsPath)

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
grepTool env = withToolResourceClaims (grepClaims env) $
    jsonTool "grep" grepDescription
    [ PropertySchema "pattern" PropertyString True $ Just
        "The regular expression pattern to search for in file contents (rg --regexp)"
    , PropertySchema "path" PropertyString False $ Just
        "File or directory to search in. Defaults to the workspace; absolute paths may point into the workspace or session temp directory."
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
    ParallelSafe
    (typedTool "grep" (runGrep env))

grepClaims
    :: ToolEnv
    -> ToolCall
    -> IO (Either Text [ToolResourceClaim])
grepClaims env call =
    case
        decodeToolArguments (toolArgumentsValue call.arguments)
            :: Either Text GrepArgs
    of
        Left err -> pure (Left err)
        Right args -> do
            let searchRoot = maybe env.toolCwd fromText args.path
            resolveForRead env searchRoot
                >>= pure . fmap
                    (\path ->
                        [ToolResourceClaim ToolRead (ToolPathTree path)])

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
    let searchRoot = maybe env.toolCwd fromText args.path
    resolveForRead env searchRoot >>= \case
        Left err -> pure (Left err)
        Right path -> findExecutable "rg" >>= \case
            Nothing -> pure $ Left
                "rg is not installed. Install ripgrep to use the grep tool."
            Just rgPath -> do
                let limit = effectiveHeadLimit args
                canonicalCwd <- canonicalizePath (unsafeToFilePath env.toolCwd)
                runRipgrep rgPath canonicalCwd path args >>= \case
                    Left err -> pure (Left err)
                    Right raw -> pure $ Right
                        (formatGrepCard env.toolCwd raw limit)

effectiveHeadLimit :: GrepArgs -> Int
effectiveHeadLimit args = case args.outputMode of
    GrepContent -> min 2000 (fromMaybe 200 args.headLimit)
    _ -> min 10000 (fromMaybe 500 args.headLimit)

runRipgrep
    :: FilePath
    -> FilePath
    -> OsPath
    -> GrepArgs
    -> IO (Either Text Text)
runRipgrep rgPath workspace path args = do
    let absoluteSearchPath = unsafeToFilePath path
        workspaceRelativePath = makeRelative workspace absoluteSearchPath
        searchWithinWorkspace =
            isRelative workspaceRelativePath
                && case splitDirectories workspaceRelativePath of
                    ".." : _ -> False
                    _ -> True
        (processCwd, searchPath)
            | searchWithinWorkspace =
                (Just workspace, workspaceRelativePath)
            | otherwise =
                (Nothing, absoluteSearchPath)
    let modeFlags = case args.outputMode of
            GrepContent -> ["--heading", "--with-filename", "--line-number"]
            GrepFilesWithMatches -> ["--files-with-matches"]
            GrepCount -> ["--count"]
        rgArgs = concat
            [ modeFlags
            , ["--no-config", "--color=never", "--max-columns", "1000"]
            , maybe [] (\g -> ["--glob", Text.unpack g]) args.glob
            , maybe [] (\n -> ["-B", show n]) args.before
            , maybe [] (\n -> ["-A", show n]) args.after
            , maybe [] (\n -> ["-C", show n]) args.context
            , ["-i" | args.caseInsensitive]
            , maybe [] (\t -> ["--type", Text.unpack t]) args.fileType
            , if args.multiline then ["-U", "--multiline-dotall"] else []
            , ["--regexp", Text.unpack args.pattern, "--", searchPath]
            ]
        process = (proc rgPath rgArgs) { cwd = processCwd }
    (code, stdout, stderr) <- readCreateProcessWithExitCode process ""
    let raw = Text.pack stdout
    case code of
        ExitSuccess -> pure $ Right raw
        ExitFailure 1 | null stdout ->
            if null stderr
                then pure (Right "")
                else pure (Left (Text.pack stderr))
        ExitFailure _ ->
            pure $ Left $ Text.pack (if null stderr then stdout else stderr)

formatGrepCard :: OsPath -> Text -> Int -> Text
formatGrepCard cwd raw limit
    | Text.null (Text.strip raw) = "No matches found."
    | otherwise =
        let ls = map (makeWorkspaceRelative cwd) (Text.lines raw)
            truncated = length ls > limit
            kept = take limit ls
            body = Text.unlines kept
            footer
                | truncated =
                    "\n[at least " <> Text.pack (show limit)
                        <> " lines; output truncated]"
                | otherwise = ""
        in "<workspace_result>\n"
            <> body
            <> footer
            <> "</workspace_result>"

makeWorkspaceRelative :: OsPath -> Text -> Text
makeWorkspaceRelative cwd line =
    fromMaybe line
        ( Text.stripPrefix workspacePrefix line
            <|> Text.stripPrefix currentDirectoryPrefix line
        )
  where
    workspacePrefix =
        Text.pack
            (addTrailingPathSeparator (unsafeToFilePath cwd))
    currentDirectoryPrefix =
        Text.pack (addTrailingPathSeparator ".")
