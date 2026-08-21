module Agent.Tools.Grok.SearchReplace (searchReplaceTool) where

import Agent.OsPath (OsPath, fromText)
import Agent.ToolArgs (objectArgs, optBool, reqText)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch (typedTool)
import Agent.Tools.Grok.Common (isGitIgnored, jsonTool)
import Agent.Tools.IO (readTextFile, resolveUnderCwd, writeTextFile)
import Agent.Tools.PlanMode
    ( PlanModeEnv
    , isPlanFileEditTarget
    , isPlanModeActive
    , planFilePath
    , planModeBlockedEditMessage
    )
import Agent.Tools.Types (AppTool, ToolEnv(..))
import Data.Aeson (FromJSON(..))
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
            resolved <- resolveUnderCwd env (fromText args.filePath)
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
createNewFile env args = resolveUnderCwd env (fromText args.filePath) >>= \case
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
replaceInFile env args = resolveUnderCwd env (fromText args.filePath) >>= \case
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

gitignoreGuard :: ToolEnv -> OsPath -> Text -> IO (Maybe Text)
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
