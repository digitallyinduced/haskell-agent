module Agent.Tools.FileSystem.ReadFile
    ( readFileTool
    , readFileToolWithSpeculation
    ) where

import Agent.OsPath (fromText)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch
    ( ToolCall(..)
    , decodeToolArguments
    , toolArgumentsValue
    , typedTool
    , typedToolWithCall
    )
import Agent.Tools.FileSystem.ReadFile.Internal
    ( ReadFileArgs(..)
    , runReadFile
    )
import Agent.Tools.FileSystem.ReadFileSpeculation
    ( ReadFileSpeculation
    , takeSpeculatedRead
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
    , withToolResourceClaims
    )
import Data.Text (Text)

readFileTool :: ToolEnv -> AppTool
readFileTool env =
    readFileToolWithSpeculation env Nothing

readFileToolWithSpeculation
    :: ToolEnv
    -> Maybe ReadFileSpeculation
    -> AppTool
readFileToolWithSpeculation env speculation =
    withToolResourceClaims (readFileClaims env) $
    jsonTool "read_file" readFileDescription
    [ PropertySchema "target_file" PropertyString True $ Just
        "The path of the file to read. Relative paths use the workspace; absolute paths may resolve within the workspace or session temp directory."
    , PropertySchema "offset" PropertyInteger False $ Just
        "The line number to start reading from. Only provide if the file is too large to read at once."
    , PropertySchema "limit" PropertyInteger False $ Just
        "The number of lines to read. Only provide if the file is too large to read at once."
    ]
    True
    ParallelSafe
    handler
  where
    handler = case speculation of
        Nothing -> typedTool "read_file" (runReadFile env)
        Just cache ->
            typedToolWithCall "read_file" (runReadFileSpeculative env cache)

runReadFileSpeculative
    :: ToolEnv
    -> ReadFileSpeculation
    -> ToolCall
    -> ReadFileArgs
    -> IO (Either Text Text)
runReadFileSpeculative env speculation call args =
    takeSpeculatedRead speculation call >>= \case
        Just result -> pure result
        Nothing -> runReadFile env args

readFileClaims
    :: ToolEnv
    -> ToolCall
    -> IO (Either Text [ToolResourceClaim])
readFileClaims env call =
    case
        decodeToolArguments (toolArgumentsValue call.arguments)
            :: Either Text ReadFileArgs
    of
        Left err -> pure (Left err)
        Right args ->
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
    \- Line numbers (1-based) appear as anchors in the format LINE_NUMBER\8594LINE_CONTENT on the first returned line and on every 10th line of the file; the lines in between show content only. Count from the nearest anchor when referring to a specific line"
