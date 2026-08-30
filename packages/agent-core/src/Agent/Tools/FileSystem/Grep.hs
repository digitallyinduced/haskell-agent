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
import Control.Concurrent.Async (concurrently)
import Control.Exception.Safe
    ( mask
    , onException
    , tryAny
    )
import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
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
    ( Handle
    , hClose
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
                    Right (raw, truncation) -> pure $ Right
                        (formatGrepCard
                            env.toolCwd
                            raw
                            limit
                            truncation)

effectiveHeadLimit :: GrepArgs -> Int
effectiveHeadLimit args = case args.outputMode of
    GrepContent -> max 1 (min 2000 (fromMaybe 200 args.headLimit))
    _ -> max 1 (min 10000 (fromMaybe 500 args.headLimit))

data GrepTruncation
    = GrepComplete
    | GrepLineTruncated
    | GrepByteTruncated
    deriving (Eq)

data GrepCapture = GrepCapture
    { captureBytes :: !BS.ByteString
    , captureTruncation :: !GrepTruncation
    }

runRipgrep
    :: FilePath
    -> FilePath
    -> OsPath
    -> GrepArgs
    -> IO (Either Text (Text, GrepTruncation))
runRipgrep rgPath workspace path args = mask \restore -> do
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
        process = (proc rgPath rgArgs)
            { cwd = processCwd
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
    (mIn, mOut, mErr, ph) <- createProcess process
    let closeHandle handle = void (tryAny (hClose handle))
        cleanup handles = do
            void (tryAny (terminateProcess ph))
            mapM_ closeHandle handles
            void (tryAny (waitForProcess ph))
        requiredPipe label = maybe
            (ioError (userError ("rg " <> label <> " pipe unavailable")))
            pure
    restore (do
        stdinHandle <- requiredPipe "stdin" mIn
        stdoutHandle <- requiredPipe "stdout" mOut
        stderrHandle <- requiredPipe "stderr" mErr
        hClose stdinHandle
        (stdoutCapture, stderrCapture) <- concurrently
            (readBoundedStdout
                ph
                stdoutHandle
                (effectiveHeadLimit args + 1)
                grepOutputByteLimit)
            (readBoundedDrain stderrHandle grepDiagnosticByteLimit)
        code <- waitForProcess ph
        closeHandle stdoutHandle
        closeHandle stderrHandle
        let raw = decodeCapture stdoutCapture
            stderr = decodeDiagnostic stderrCapture
        case code of
            ExitSuccess ->
                pure $ Right (raw, stdoutCapture.captureTruncation)
            -- terminateProcess is deliberate once an output cap is known.
            -- A signal exit on that path still represents a successful,
            -- bounded search.
            ExitFailure _
                | stdoutCapture.captureTruncation /= GrepComplete ->
                    pure $ Right (raw, stdoutCapture.captureTruncation)
            ExitFailure 1
                | Text.null raw ->
                    if Text.null stderr
                        then pure (Right ("", GrepComplete))
                        else pure (Left stderr)
            ExitFailure _ ->
                pure $ Left $
                    if Text.null stderr then raw else stderr)
        `onException`
            cleanup [handle | Just handle <- [mIn, mOut, mErr]]

grepOutputByteLimit :: Int
grepOutputByteLimit = 16 * 1024 * 1024

grepDiagnosticByteLimit :: Int
grepDiagnosticByteLimit = 1024 * 1024

readBoundedStdout
    :: ProcessHandle
    -> Handle
    -> Int
    -> Int
    -> IO GrepCapture
readBoundedStdout processHandle handle lineLimit byteLimit =
    go [] 0 0
  where
    go reversed totalBytes totalLines = do
        chunk <- BS.hGetSome handle 32768
        if BS.null chunk
            then pure (finish reversed GrepComplete)
            else do
                let remainingBytes = max 0 (byteLimit - totalBytes)
                    withinBytes = BS.take remainingBytes chunk
                    neededLines = max 0 (lineLimit - totalLines)
                    (kept, hitLineLimit) =
                        takeThroughNewlines neededLines withinBytes
                    hitByteLimit =
                        BS.length chunk > remainingBytes
                            || (remainingBytes == 0
                                && not (BS.null chunk))
                    nextReversed =
                        if BS.null kept then reversed else kept : reversed
                    nextBytes = totalBytes + BS.length kept
                    nextLines = totalLines + BS.count 10 kept
                    truncation
                        | hitLineLimit = GrepLineTruncated
                        | hitByteLimit = GrepByteTruncated
                        | otherwise = GrepComplete
                if truncation == GrepComplete
                    then go nextReversed nextBytes nextLines
                    else do
                        void (tryAny (terminateProcess processHandle))
                        drainHandle handle
                        pure (finish nextReversed truncation)

    finish reversed truncation =
        GrepCapture
            { captureBytes = BS.concat (reverse reversed)
            , captureTruncation = truncation
            }

takeThroughNewlines
    :: Int
    -> BS.ByteString
    -> (BS.ByteString, Bool)
takeThroughNewlines needed bytes
    | needed <= 0 = (BS.empty, not (BS.null bytes))
    | otherwise =
        case drop (needed - 1) (BS.elemIndices 10 bytes) of
            index : _ -> (BS.take (index + 1) bytes, True)
            [] -> (bytes, False)

data DiagnosticCapture = DiagnosticCapture
    { diagnosticBytes :: !BS.ByteString
    , diagnosticTruncated :: !Bool
    }

readBoundedDrain :: Handle -> Int -> IO DiagnosticCapture
readBoundedDrain handle limit = go [] 0 False
  where
    go reversed total truncated = do
        chunk <- BS.hGetSome handle 32768
        if BS.null chunk
            then pure DiagnosticCapture
                { diagnosticBytes = BS.concat (reverse reversed)
                , diagnosticTruncated = truncated
                }
            else do
                let remaining = max 0 (limit - total)
                    kept = BS.take remaining chunk
                go
                    (if BS.null kept then reversed else kept : reversed)
                    (total + BS.length kept)
                    (truncated || BS.length chunk > remaining)

drainHandle :: Handle -> IO ()
drainHandle handle = do
    chunk <- BS.hGetSome handle 32768
    if BS.null chunk then pure () else drainHandle handle

decodeCapture :: GrepCapture -> Text
decodeCapture =
    TextEncoding.decodeUtf8With lenientDecode . (.captureBytes)

decodeDiagnostic :: DiagnosticCapture -> Text
decodeDiagnostic capture =
    let decoded =
            TextEncoding.decodeUtf8With lenientDecode
                capture.diagnosticBytes
    in if capture.diagnosticTruncated
        then decoded
            <> "\n[rg diagnostic truncated after "
            <> Text.pack (show grepDiagnosticByteLimit)
            <> " bytes]"
        else decoded

formatGrepCard
    :: OsPath
    -> Text
    -> Int
    -> GrepTruncation
    -> Text
formatGrepCard cwd raw limit truncation
    | Text.null (Text.strip raw) = "No matches found."
    | otherwise =
        let ls = map (makeWorkspaceRelative cwd) (Text.lines raw)
            lineTruncated =
                truncation == GrepLineTruncated || length ls > limit
            kept = take limit ls
            body = Text.unlines kept
            footer
                | lineTruncated =
                    "\n[at least " <> Text.pack (show limit)
                        <> " lines; output truncated]"
                | truncation == GrepByteTruncated =
                    "\n[output truncated after "
                        <> Text.pack (show grepOutputByteLimit)
                        <> " bytes]"
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
