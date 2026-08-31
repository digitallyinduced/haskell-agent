module Agent.GrokBuild.Dialect.SearchReplace
    ( searchReplaceTool
    , searchReplaceToolWithPrefetch
    ) where

import Agent.OsPath (fromText)
import System.OsPath
    ( OsPath
    , equalFilePath
    , takeDirectory
    , takeFileName
    )
import qualified Agent.Json.Decode as Json
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch
    ( StreamedTool(..)
    , StreamedToolFactory
    , ToolCall(..)
    , ToolInput(..)
    , decodeToolArguments
    , typedTool
    )
import Agent.Tools.FileSystem.FilePrefetch
    ( FileCallState
    , FilePrefetch
    , closeFileCall
    , closeFilePrefetch
    , consumePrefetchedFile
    , emptyFileCallState
    , jsonStringFieldProgress
    , newFilePrefetch
    , refreshFileCall
    )
import Agent.Tools.FileSystem.GitIgnore (isGitIgnored)
import Agent.GrokBuild.Dialect.Common (jsonTool)
import Agent.GrokBuild.Dialect.Json (optionalBool)
import Agent.Tools.IO
    ( displayPathInWorkspace
    , readTextFile
    , resolveUnderCwd
    , writeTextFile
    )
import Agent.Tools.Scheduling
    ( ToolAccess(..)
    , ToolResource(..)
    , ToolResourceClaim(..)
    )
import Agent.Tools.PlanMode
    ( PlanModeEnv
    , isPlanFileEditTarget
    , isPlanModeActive
    , planFileName
    , planFilePath
    , planModeBlockedEditMessage
    )
import Agent.Tools.Types
    ( AppTool
    , ToolEnv(..)
    , ToolExecutionPolicy(..)
    , withToolArgumentInterpreter
    , withToolResourceClaims
    )
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
    ( ExceptT(..)
    , runExceptT
    , throwE
    )
import Data.Acquire (mkAcquire)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory.OsPath (doesFileExist)

data SearchReplaceArgs = SearchReplaceArgs
    { filePath :: Text
    , oldString :: Text
    , newString :: Text
    , replaceAll :: Bool
    }

searchReplaceArgsDecoder :: Json.Decoder SearchReplaceArgs
searchReplaceArgsDecoder = Json.object $
    SearchReplaceArgs
        <$> Json.atKey "file_path" Json.text
        <*> Json.atKey "old_string" Json.text
        <*> Json.atKey "new_string" Json.text
        <*> (fromMaybe False <$> optionalBool "replace_all")

searchReplaceTool :: ToolEnv -> PlanModeEnv -> AppTool
searchReplaceTool env planMode =
    searchReplaceToolWithPrefetch env planMode Nothing

searchReplaceToolWithPrefetch
    :: ToolEnv
    -> PlanModeEnv
    -> Maybe FilePrefetch
    -> AppTool
searchReplaceToolWithPrefetch env planMode prefetch =
    withToolArgumentInterpreter
        (maybe
            (searchReplaceInterpreter env planMode)
            (searchReplaceInterpreterWithCache env planMode)
            prefetch) $
    withToolResourceClaims (searchReplaceResourceClaims env) $
    jsonTool "search_replace" searchReplaceDescription
    [ PropertySchema "file_path" PropertyString True $ Just
        "The path to the file to modify. Relative paths are resolved within the workspace. Absolute paths are accepted only when they resolve within the workspace."
    , PropertySchema "old_string" PropertyString True $ Just
        "The text to replace"
    , PropertySchema "new_string" PropertyString True $ Just
        "The text to replace it with (must be different from old_string)"
    , PropertySchema "replace_all" PropertyBoolean False $ Just
        "Replace all occurrences of old_string (default false)"
    ]
    False
    TurnSequential
    (typedTool "search_replace" searchReplaceArgsDecoder (runSearchReplace env planMode))

searchReplaceResourceClaims
    :: ToolEnv
    -> ToolCall
    -> IO (Either Text [ToolResourceClaim])
searchReplaceResourceClaims env call =
    case decodeToolArguments searchReplaceArgsDecoder call.arguments of
        Left err -> pure (Left err)
        Right args ->
            resolveUnderCwd env (fromText args.filePath)
                >>= pure . fmap claimsForResolved

claimsForResolved :: OsPath -> [ToolResourceClaim]
claimsForResolved resolved
    | isGitIgnorePolicyPath resolved =
        [ToolResourceClaim ToolWrite ToolAllPaths]
    | otherwise =
        [ToolResourceClaim ToolWrite (ToolPath resolved)]

-- | Ignore-policy edits change what later replacements may write, so they
-- conflict with every filesystem claim rather than only the ignore file path.
isGitIgnorePolicyPath :: OsPath -> Bool
isGitIgnorePolicyPath path =
    takeFileName path == fromText ".gitignore"
        || (takeFileName path == fromText "exclude"
            && takeFileName (takeDirectory path) == fromText "info"
            && takeFileName (takeDirectory (takeDirectory path))
                == fromText ".git")

searchReplaceDescription :: Text
searchReplaceDescription =
    "Replace an exact string in a file.\n\
    \\n\
    \- read_file prefixes each line with \"LINE_NUMBER\8594\". That prefix is not part of the file: match only what comes after the \8594, with its exact indentation.\n\
    \- old_string must match exactly one place in the file. If it appears more than once, add surrounding lines to make it unique, or set replace_all to change every occurrence (handy for renaming an identifier).\n\
    \- To create a new file or fill an existing empty file, set old_string to an empty string. An empty old_string cannot overwrite an existing non-empty file."

runSearchReplace :: ToolEnv -> PlanModeEnv -> SearchReplaceArgs -> IO (Either Text Text)
runSearchReplace env planMode args = runExceptT do
    guardPlanMode env planMode args.filePath
    runSearchReplaceBody env args

guardPlanMode :: ToolEnv -> PlanModeEnv -> Text -> ExceptT Text IO ()
guardPlanMode env planMode filePath = do
    active <- lift (isPlanModeActive planMode)
    when active (checkPlanPath env planMode filePath)

checkPlanPath :: ToolEnv -> PlanModeEnv -> Text -> ExceptT Text IO ()
checkPlanPath env planMode filePath = do
    planPath <- lift (planFilePath planMode)
    if equalFilePath planFileName (fromText filePath)
        then pure ()
        else do
            path <- resolvePath env filePath
            unless (isPlanFileEditTarget planPath path)
                (throwE (planModeBlockedEditMessage planPath))

runSearchReplaceBody :: ToolEnv -> SearchReplaceArgs -> ExceptT Text IO Text
runSearchReplaceBody env args
    | args.oldString == args.newString =
        throwE "Old string and new string are the same"
    | Text.null args.oldString = createNewFile env args
    | otherwise = replaceInFile env args

createNewFile :: ToolEnv -> SearchReplaceArgs -> ExceptT Text IO Text
createNewFile env args = do
    (path, display) <- resolveDisplayPath env args.filePath
    gitignoreGuard env path display
    exists <- lift (doesFileExist path)
    when exists do
        existing <- ExceptT (readTextFile path)
        unless (Text.null existing) $
            throwE "An empty old_string cannot overwrite an existing non-empty file."
    ExceptT (writeTextFile path args.newString)
    pure ("The file " <> display <> " has been created successfully.")

replaceInFile :: ToolEnv -> SearchReplaceArgs -> ExceptT Text IO Text
replaceInFile env args = replaceInFileWithContent env args Nothing

replaceInFileWithContent
    :: ToolEnv
    -> SearchReplaceArgs
    -> Maybe Text
    -> ExceptT Text IO Text
replaceInFileWithContent env args cachedContent = do
    (path, display) <- resolveDisplayPath env args.filePath
    gitignoreGuard env path display
    exists <- lift (doesFileExist path)
    unless exists $
        throwE ("File not found: " <> display)
    content <- case cachedContent of
        Just text -> pure text
        Nothing -> ExceptT (readTextFile path)
    let count = countOccurrences args.oldString content
    when (count == 0) $
        throwE $
            "The string to replace was not found in the file, use the read_file tool to see the correct string. The user may have changed the file since you last read it."
                <> nearestMatchHint content args.oldString
    when (count > 1 && not args.replaceAll) $
        throwE
            "The string to replace was found multiple times in the file. Use replace_all to replace all occurrences, or include more context to only edit one occurrence."
    let updated = replaceOccurrences args.oldString args.newString args.replaceAll content
    ExceptT (writeTextFile path updated)
    pure $
        if args.replaceAll && count > 1
            then "The file " <> display
                <> " has been updated. All occurrences were successfully replaced."
            else "The file " <> display
                <> " has been updated successfully."

resolvePath :: ToolEnv -> Text -> ExceptT Text IO OsPath
resolvePath env path =
    ExceptT (resolveUnderCwd env (fromText path))

resolveDisplayPath :: ToolEnv -> Text -> ExceptT Text IO (OsPath, Text)
resolveDisplayPath env requested = do
    path <- resolvePath env requested
    display <- lift (displayPathInWorkspace env path)
    pure (path, display)

gitignoreGuard :: ToolEnv -> OsPath -> Text -> ExceptT Text IO ()
gitignoreGuard env path display = do
    ignored <- lift (isGitIgnored env.toolCwd path)
    when ignored $
        throwE ("Error: " <> display <> " is ignored by .gitignore and cannot be edited.")

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

searchReplaceInterpreter :: ToolEnv -> PlanModeEnv -> StreamedToolFactory
searchReplaceInterpreter env planMode =
    streamedSearchReplace env planMode
        <$> mkAcquire (newFilePrefetch env) closeFilePrefetch

searchReplaceInterpreterWithCache
    :: ToolEnv
    -> PlanModeEnv
    -> FilePrefetch
    -> StreamedToolFactory
searchReplaceInterpreterWithCache env planMode prefetch =
    streamedSearchReplace env planMode
        <$> mkAcquire (pure prefetch) (\_ -> pure ())

streamedSearchReplace
    :: ToolEnv
    -> PlanModeEnv
    -> FilePrefetch
    -> StreamedTool
streamedSearchReplace env planMode prefetch =
    StreamedTool
        { streamedStart = pure emptyFileCallState
        , streamedInterpret = interpretSearchReplace prefetch
        , streamedConsume = \_call _emit args state ->
            consumeSearchReplace env planMode prefetch args state
        , streamedClose = closeFileCall prefetch
        }

interpretSearchReplace
    :: FilePrefetch
    -> FileCallState
    -> ToolInput
    -> IO (Either (SearchReplaceArgs, FileCallState) FileCallState)
interpretSearchReplace prefetch state = \case
    ToolPrefix text ->
        Right <$> refreshFileCall prefetch state text (jsonStringFieldProgress "file_path" text)
    ToolDone text -> do
        next <- refreshFileCall prefetch state text (jsonStringFieldProgress "file_path" text)
        case decodeSearchReplaceArgs text of
            Nothing -> pure (Right next)
            Just args -> pure (Left (args, next))

decodeSearchReplaceArgs :: Text -> Maybe SearchReplaceArgs
decodeSearchReplaceArgs text =
    case decodeToolArguments searchReplaceArgsDecoder text of
        Right args -> Just args
        Left _ -> Nothing

consumeSearchReplace
    :: ToolEnv
    -> PlanModeEnv
    -> FilePrefetch
    -> SearchReplaceArgs
    -> FileCallState
    -> IO (Either Text Text)
consumeSearchReplace env planMode prefetch args state = runExceptT do
    guardPlanMode env planMode args.filePath
    cached <- lift (consumePrefetchedFile prefetch args.filePath state)
    runSearchReplaceBodyWithCache env args (snd <$> cached)

runSearchReplaceBodyWithCache
    :: ToolEnv
    -> SearchReplaceArgs
    -> Maybe Text
    -> ExceptT Text IO Text
runSearchReplaceBodyWithCache env args cached
    | args.oldString == args.newString =
        throwE "Old string and new string are the same"
    | Text.null args.oldString = createNewFile env args
    | otherwise = replaceInFileWithContent env args cached
