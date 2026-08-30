module Agent.Tools.FileSystem.Grep (grepTool) where

import Agent.Json.Decode (Decoder)
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
import Control.Concurrent.Async (withAsync, wait)
import Control.Exception.Safe (finally, tryIO)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
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
    ( CreateProcess(cwd, std_err, std_in, std_out)
    , ProcessHandle
    , StdStream(CreatePipe)
    , proc
    , createProcess
    , terminateProcess
    , waitForProcess
    )
import System.OsPath (OsPath)
import System.IO
    ( BufferMode(NoBuffering)
    , Handle
    , hClose
    , hGetLine
    , hSetBuffering
    , hSetEncoding
    , utf8
    )

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

grepArgsDecoder :: Decoder GrepArgs
grepArgsDecoder = objectArgs \object -> do
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
    (typedTool "grep" grepArgsDecoder (runGrep env))

grepClaims
    :: ToolEnv
    -> ToolCall
    -> IO (Either Text [ToolResourceClaim])
grepClaims env call =
    case
        decodeToolArguments grepArgsDecoder (toolArgumentsValue call.arguments)
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
    (mIn, mOut, mErr, ph) <- createProcess process
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
    forM_ mIn hClose
    stdoutHandle <- maybe (ioError (userError "rg stdout pipe unavailable")) pure mOut
    stderrHandle <- maybe (ioError (userError "rg stderr pipe unavailable")) pure mErr
    hSetEncoding stdoutHandle utf8
    hSetEncoding stderrHandle utf8
    hSetBuffering stdoutHandle NoBuffering
    hSetBuffering stderrHandle NoBuffering
    -- Keep only one line beyond the requested limit.  The producer is
    -- terminated as soon as that line arrives, rather than allowing a slow
    -- renderer to retain the complete ripgrep output in a String.
    ((stdout, truncated), stderr) <-
        withAsync (readLimitedLines ph stdoutHandle (effectiveHeadLimit args + 1)) \out ->
            withAsync (readLimitedBytes stderrHandle diagnosticLimit) \err -> do
                output <- wait out
                diagnostics <- wait err
                pure (output, diagnostics)
    code <- waitForProcess ph
    hClose stdoutHandle `finally` hClose stderrHandle
    let raw = Text.pack stdout
    case code of
        ExitSuccess -> pure $ Right raw
        -- terminateProcess is deliberate once the output limit is known.
        -- ripgrep may report a signal exit on that path; preserve the
        -- bounded, truncated result as a successful search.
        ExitFailure _ | truncated -> pure $ Right raw
        ExitFailure 1 | null stdout ->
            if null stderr
                then pure (Right "")
                else pure (Left (Text.pack stderr))
        ExitFailure _ ->
            pure $ Left $ Text.pack (if null stderr then stdout else stderr)

diagnosticLimit :: Int
diagnosticLimit = 1024 * 1024

readLimitedLines :: ProcessHandle -> Handle -> Int -> IO (String, Bool)
readLimitedLines ph handle maxLines = go [] 0
  where
    go acc count
        | count >= maxLines = do
            terminateProcess ph
            pure (concat (reverse acc), True)
        | otherwise = do
            result <- tryIO (hGetLine handle)
            case result of
                Left _ -> pure (concat (reverse acc), False)
                Right line -> go ((line <> "\n") : acc) (count + 1)

readLimitedBytes :: Handle -> Int -> IO String
readLimitedBytes handle limit = go [] 0
  where
    go acc total
        | total >= limit = drain
        | otherwise = do
            chunk <- BS.hGetSome handle (min 8192 (limit - total))
            if BS.null chunk
                then pure (concat (reverse acc))
                else go (BSC.unpack chunk : acc) (total + BS.length chunk)
    -- Keep draining after the diagnostic budget is reached so the child
    -- cannot block on a full stderr pipe while stdout is still being read.
    drain = do
        chunk <- BS.hGetSome handle 8192
        if BS.null chunk then pure (concat (reverse acc)) else drain

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
