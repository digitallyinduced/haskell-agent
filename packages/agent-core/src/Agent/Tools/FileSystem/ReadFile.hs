module Agent.Tools.FileSystem.ReadFile
    ( readFileTool
    , readFileToolWithSpeculation
    , ReadFileArgs(..)
    , formatReadFile
    , streamReadFile
    ) where

import Agent.OsPath (fromText)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch (typedTool)
import Agent.Tools.FileSystem.ReadFile.Internal
    ( ReadFileArgs(..)
    , formatReadFileContent
    , readFileArgsDecoder
    , runReadFile
    , streamReadFile
    )
import Agent.Tools.FileSystem.ReadFileSpeculation
    ( ReadFileSpeculation
    , readFileArgumentInterpreter
    , readFileArgumentInterpreterWithCache
    )
import Agent.Tools.IO (resolveForRead)
import Agent.Tools.Scheduling
    ( ToolAccess(..)
    , ToolResource(..)
    , ToolResourceClaim(..)
    )
import Agent.Tools.Types
    ( AppTool
    , ToolEnv
    , ToolExecutionPolicy(..)
    , jsonTool
    , withToolArgumentInterpreter
    , withTypedResourceClaims
    )
import Data.Text (Text)

readFileTool :: ToolEnv -> AppTool
readFileTool env =
    withToolArgumentInterpreter
        (readFileArgumentInterpreter env)
        (baseReadFileTool env)

readFileToolWithSpeculation
    :: ToolEnv
    -> Maybe ReadFileSpeculation
    -> AppTool
readFileToolWithSpeculation env speculation =
    maybe
        (baseReadFileTool env)
        (\cache ->
            withToolArgumentInterpreter
                (readFileArgumentInterpreterWithCache cache)
                (baseReadFileTool env))
        speculation

baseReadFileTool :: ToolEnv -> AppTool
baseReadFileTool env =
    withTypedResourceClaims readFileArgsDecoder (readFileClaims env) $
    jsonTool "read_file" readFileDescription
    [ PropertySchema "target_file" PropertyString True $ Just
        "The path of the file to read. Relative paths use the workspace; absolute paths may resolve within the workspace or session temp directory."
    , PropertySchema "offset" PropertyInteger False $ Just
        "The 1-based line number to start reading from. Negative values count from the end of the file (-1 is the last line). Only provide if the file is too large to read at once."
    , PropertySchema "limit" PropertyInteger False $ Just
        "The number of lines to read, starting at offset. Must be a positive integer. Only provide if the file is too large to read at once."
    ]
    True
    ParallelSafe
    (typedTool "read_file" readFileArgsDecoder (runReadFile env))

readFileClaims
    :: ToolEnv
    -> ReadFileArgs
    -> IO (Either Text [ToolResourceClaim])
readFileClaims env args =
    resolveForRead env (fromText args.targetFile)
        >>= pure . fmap
            (\path ->
                [ToolResourceClaim ToolRead (ToolPath path)])

readFileDescription :: Text
readFileDescription =
    "Read a file.\n\
    \\n\
    \- The target_file parameter can be relative to the workspace or an absolute path in an allowed filesystem root\n\
    \- By default, it reads up to 1000 lines starting from the beginning of the file\n\
    \- offset is 1-based. Negative offsets count from the end of the file (-1 is the last line).\n\
    \- Line numbers (1-based) appear as anchors in the format LINE_NUMBER\8594LINE_CONTENT on the first returned line and on every 10th line of the file; the lines in between show content only. Count from the nearest anchor when referring to a specific line"

formatReadFile :: Text -> ReadFileArgs -> Either Text Text
formatReadFile = formatReadFileContent
