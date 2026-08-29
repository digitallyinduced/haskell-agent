module Agent.GrokBuild.Dialect.SearchReplace (searchReplaceTool) where

import Agent.OsPath (fromText, toText)
import System.OsPath
    ( OsPath
    , takeDirectory
    , takeFileName
    )
import qualified Agent.Json.Decode as Json
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch
    ( ToolCall(..)
    , decodeToolArguments
    , typedTool
    )
import Agent.Tools.FileSystem.GitIgnore (isGitIgnored)
import Agent.Tools.PlanMode.File (renderPlanFileError)
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
    , planFilePath
    , planModeBlockedEditMessage
    , writePlanSnapshot
    )
import Agent.Tools.Types
    ( AppTool
    , PlanModeCapability(..)
    , ToolEnv(..)
    , ToolExecutionPolicy(..)
    , withPlanModeCapability
    , withToolCallNormalizer
    , withToolResourceClaims
    )
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
    ( ExceptT(..)
    , runExceptT
    , throwE
    )
import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.Aeson (object, (.=))
import qualified Data.Aeson.Text as Aeson
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
    withPlanModeCapability
        (PlanModePlanFileWrite (searchReplacePlanTarget planMode)) $
    withToolCallNormalizer (normalizePlanAlias planMode) $
    withToolResourceClaims (searchReplaceResourceClaims env planMode) $
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
    -> PlanModeEnv
    -> ToolCall
    -> IO (Either Text [ToolResourceClaim])
searchReplaceResourceClaims env planMode call =
    case decodeToolArguments searchReplaceArgsDecoder call.arguments of
        Left err -> pure (Left err)
        Right args -> do
            planPath <- planFilePath planMode
            active <- isPlanModeActive planMode
            if active && isPlanFileEditTarget planPath (fromText args.filePath)
                then pure (Right (claimsForResolved planPath))
                else
                    resolveUnderCwd env (fromText args.filePath)
                        >>= pure . fmap claimsForResolved

searchReplacePlanTarget
    :: PlanModeEnv
    -> ToolCall
    -> IO (Either Text OsPath)
searchReplacePlanTarget _ call =
    pure $
        fromText . (.filePath)
            <$> decodeToolArguments searchReplaceArgsDecoder call.arguments

-- | Grok Build models conventionally emit @plan.md@. Rewrite only that exact
-- provider alias while plan mode is active; every later stage sees the same
-- canonical call. Other relative paths are deliberately not aliases.
normalizePlanAlias
    :: PlanModeEnv
    -> ToolCall
    -> IO (Either Text ToolCall)
normalizePlanAlias planMode call = do
    active <- isPlanModeActive planMode
    if not active
        then pure (Right call)
        else case decodeToolArguments searchReplaceArgsDecoder call.arguments of
            Left err -> pure (Left err)
            Right args
                | args.filePath `elem` ["plan.md", "./plan.md"] -> do
                    path <- planFilePath planMode
                    pure . Right $
                        call
                            { arguments =
                                LazyText.toStrict
                                    . Aeson.encodeToLazyText
                                    $ object
                                        [ "file_path" .= toText path
                                        , "old_string" .= args.oldString
                                        , "new_string" .= args.newString
                                        , "replace_all" .= args.replaceAll
                                        ]
                            }
                | otherwise -> pure (Right call)

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
    runSearchReplaceBody env planMode args

guardPlanMode :: ToolEnv -> PlanModeEnv -> Text -> ExceptT Text IO ()
guardPlanMode env planMode filePath = do
    active <- lift (isPlanModeActive planMode)
    when active (checkPlanPath env planMode filePath)

checkPlanPath :: ToolEnv -> PlanModeEnv -> Text -> ExceptT Text IO ()
checkPlanPath env planMode filePath = do
    planPath <- lift (planFilePath planMode)
    path <- resolvePath env planMode filePath
    unless (isPlanFileEditTarget planPath path)
        (throwE (planModeBlockedEditMessage planPath))

runSearchReplaceBody
    :: ToolEnv
    -> PlanModeEnv
    -> SearchReplaceArgs
    -> ExceptT Text IO Text
runSearchReplaceBody env planMode args
    | args.oldString == args.newString =
        throwE "Old string and new string are the same"
    | Text.null args.oldString = createNewFile env planMode args
    | otherwise = replaceInFile env planMode args

createNewFile
    :: ToolEnv
    -> PlanModeEnv
    -> SearchReplaceArgs
    -> ExceptT Text IO Text
createNewFile env planMode args = do
    (path, display) <- resolveDisplayPath env planMode args.filePath
    gitignoreGuard env planMode path display
    exists <- lift (doesFileExist path)
    when exists do
        existing <- ExceptT (readTextFile path)
        unless (Text.null existing) $
            throwE "An empty old_string cannot overwrite an existing non-empty file."
    writeResolved planMode path args.newString
    pure ("The file " <> display <> " has been created successfully.")

replaceInFile
    :: ToolEnv
    -> PlanModeEnv
    -> SearchReplaceArgs
    -> ExceptT Text IO Text
replaceInFile env planMode args = do
    (path, display) <- resolveDisplayPath env planMode args.filePath
    gitignoreGuard env planMode path display
    exists <- lift (doesFileExist path)
    unless exists $
        throwE ("File not found: " <> display)
    content <- ExceptT (readTextFile path)
    let count = countOccurrences args.oldString content
    when (count == 0) $
        throwE $
            "The string to replace was not found in the file, use the read_file tool to see the correct string. The user may have changed the file since you last read it."
                <> nearestMatchHint content args.oldString
    when (count > 1 && not args.replaceAll) $
        throwE
            "The string to replace was found multiple times in the file. Use replace_all to replace all occurrences, or include more context to only edit one occurrence."
    let updated = replaceOccurrences args.oldString args.newString args.replaceAll content
    writeResolved planMode path updated
    pure $
        if args.replaceAll && count > 1
            then "The file " <> display
                <> " has been updated. All occurrences were successfully replaced."
            else "The file " <> display
                <> " has been updated successfully."

resolvePath :: ToolEnv -> PlanModeEnv -> Text -> ExceptT Text IO OsPath
resolvePath env planMode path = do
    active <- lift (isPlanModeActive planMode)
    expected <- lift (planFilePath planMode)
    if active && isPlanFileEditTarget expected (fromText path)
        then pure expected
        else ExceptT (resolveUnderCwd env (fromText path))

resolveDisplayPath
    :: ToolEnv
    -> PlanModeEnv
    -> Text
    -> ExceptT Text IO (OsPath, Text)
resolveDisplayPath env planMode requested = do
    path <- resolvePath env planMode requested
    expected <- lift (planFilePath planMode)
    display <-
        if isPlanFileEditTarget expected path
            then pure (toText path)
            else lift (displayPathInWorkspace env path)
    pure (path, display)

gitignoreGuard
    :: ToolEnv
    -> PlanModeEnv
    -> OsPath
    -> Text
    -> ExceptT Text IO ()
gitignoreGuard env planMode path display = do
    expected <- lift (planFilePath planMode)
    unless (isPlanFileEditTarget expected path) do
        ignored <- lift (isGitIgnored env.toolCwd path)
        when ignored $
            throwE
                ("Error: " <> display
                    <> " is ignored by .gitignore and cannot be edited.")

writeResolved
    :: PlanModeEnv
    -> OsPath
    -> Text
    -> ExceptT Text IO ()
writeResolved planMode path content = do
    expected <- lift (planFilePath planMode)
    if isPlanFileEditTarget expected path
        then
            ExceptT $
                writePlanSnapshot planMode content
                    >>= pure
                        . either
                            (Left . renderPlanFileError)
                            (const (Right ()))
        else ExceptT (writeTextFile path content)

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
