-- | Codex @apply_patch@ language.
--
-- Format copied from openai/codex @ codex-rs/apply-patch (Lark grammar in
-- parser.rs). This is a freeform/custom tool: the body is patch text, not JSON.
module Agent.Tools.ApplyPatch
    ( Hunk(..)
    , UpdateChunk(..)
    , parsePatch
    , applyPatch
    , applyPatchGrammar
    ) where

import Agent.OsPath (OsPath, fromFilePath)
import Agent.Tools.IO
    ( deleteTextFile
    , readTextFile
    , resolveUnderCwd
    , writeTextFile
    )
import Agent.Tools.Types (ToolEnv)
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
    ( ExceptT(..)
    , except
    , runExceptT
    , throwE
    )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory.OsPath (doesFileExist)


-- | Lark grammar Codex registers for the freeform apply_patch tool.
applyPatchGrammar :: Text
applyPatchGrammar =
    "start: begin_patch hunk+ end_patch\n\
    \begin_patch: \"*** Begin Patch\" LF\n\
    \end_patch: \"*** End Patch\" LF?\n\
    \\n\
    \hunk: add_hunk | delete_hunk | update_hunk\n\
    \add_hunk: \"*** Add File: \" filename LF add_line+\n\
    \delete_hunk: \"*** Delete File: \" filename LF\n\
    \update_hunk: \"*** Update File: \" filename LF change_move? change?\n\
    \\n\
    \filename: /(.+)/\n\
    \add_line: \"+\" /(.*)/ LF -> line\n\
    \\n\
    \change_move: \"*** Move to: \" filename LF\n\
    \change: (change_context | change_line)+ eof_line?\n\
    \change_context: (\"@@\" | \"@@ \" /(.+)/) LF\n\
    \change_line: (\"+\" | \"-\" | \" \") /(.*)/ LF\n\
    \eof_line: \"*** End of File\" LF\n\
    \\n\
    \%import common.LF\n"

data Hunk
    = AddFile FilePath Text
    | DeleteFile FilePath
    | UpdateFile FilePath (Maybe FilePath) [UpdateChunk]
    deriving (Eq, Show)

data UpdateChunk = UpdateChunk
    { chunkContext :: !(Maybe Text)
    , chunkOld :: ![Text]
    , chunkNew :: ![Text]
    , chunkEof :: !Bool
    } deriving (Eq, Show)

parsePatch :: Text -> Either Text [Hunk]
parsePatch raw = do
    let trimmed = Text.strip raw
        ls = map Text.stripEnd (Text.lines trimmed)
    case ls of
        [] -> Left "Invalid patch: empty"
        first : rest
            | Text.strip first /= "*** Begin Patch" ->
                Left "The first line of the patch must be '*** Begin Patch'"
            | otherwise -> do
                withoutEnd <- case reverse rest of
                    lastLine : bodyRev
                        | Text.strip lastLine == "*** End Patch" ->
                            Right (dropEnvironment (reverse bodyRev))
                    _ -> Left "The last line of the patch must be '*** End Patch'"
                parseHunks withoutEnd

dropEnvironment :: [Text] -> [Text]
dropEnvironment (line : rest)
    | "*** Environment ID:" `Text.isPrefixOf` Text.strip line = rest
dropEnvironment lines_ = lines_

parseHunks :: [Text] -> Either Text [Hunk]
parseHunks [] = Right []
parseHunks (line : rest)
    | Just path <- stripPrefix "*** Add File: " line = do
        let (addLines, remaining) = span isPlus rest
        contents <- traverse plusLine addLines
        whenEmpty addLines "Add file hunk is empty" path
        restHunks <- parseHunks remaining
        Right (AddFile (Text.unpack (Text.strip path)) (joinLines contents) : restHunks)
    | Just path <- stripPrefix "*** Delete File: " line = do
        restHunks <- parseHunks rest
        Right (DeleteFile (Text.unpack (Text.strip path)) : restHunks)
    | Just path <- stripPrefix "*** Update File: " line = do
        let (movePath, afterMove) = case rest of
                (moveLine : more)
                    | Just dest <- stripPrefix "*** Move to: " moveLine ->
                        (Just (Text.unpack (Text.strip dest)), more)
                _ -> (Nothing, rest)
        (chunks, remaining) <- parseChunks afterMove
        if null chunks
            then Left $ "Update file hunk for path '" <> path <> "' is empty"
            else do
                restHunks <- parseHunks remaining
                Right (UpdateFile (Text.unpack (Text.strip path)) movePath chunks : restHunks)
    | Text.null (Text.strip line) = parseHunks rest
    | otherwise =
        Left $ "Invalid patch hunk: unexpected line: " <> line
  where
    whenEmpty [] message path = Left $ message <> " for path '" <> path <> "'"
    whenEmpty _ _ _ = Right ()

parseChunks :: [Text] -> Either Text ([UpdateChunk], [Text])
parseChunks lines_
    | nextHunk lines_ = Right ([], lines_)
    | null lines_ = Right ([], [])
    | otherwise = do
        let (header, afterHeader) = case lines_ of
                (line : rest)
                    | line == "@@" -> (Nothing, rest)
                    | Just ctx <- stripPrefix "@@ " line -> (Just (Text.strip ctx), rest)
                _ -> (Nothing, lines_)
        let (body, afterBody) = span isChangeLine afterHeader
            (eof, remaining) = case afterBody of
                (line : rest) | Text.strip line == "*** End of File" -> (True, rest)
                _ -> (False, afterBody)
        chunk <- buildChunk header body eof
        (more, leftover) <- parseChunks remaining
        Right (chunk : more, leftover)

nextHunk :: [Text] -> Bool
nextHunk (line : _) =
    "*** Add File: " `Text.isPrefixOf` line
        || "*** Delete File: " `Text.isPrefixOf` line
        || "*** Update File: " `Text.isPrefixOf` line
        || Text.strip line == "*** End Patch"
nextHunk [] = True

isPlus :: Text -> Bool
isPlus line = "+" `Text.isPrefixOf` line

isChangeLine :: Text -> Bool
isChangeLine line =
    "+" `Text.isPrefixOf` line
        || "-" `Text.isPrefixOf` line
        || " " `Text.isPrefixOf` line

plusLine :: Text -> Either Text Text
plusLine line = case Text.uncons line of
    Just ('+', rest) -> Right rest
    _ -> Left "Expected a '+' line in an add-file hunk"

buildChunk :: Maybe Text -> [Text] -> Bool -> Either Text UpdateChunk
buildChunk header body eof = Right UpdateChunk
    { chunkContext = header
    , chunkOld = [Text.drop 1 line | line <- body, startsWithOneOf line ['-', ' ']]
    , chunkNew = [Text.drop 1 line | line <- body, startsWithOneOf line ['+', ' ']]
    , chunkEof = eof
    }

startsWithOneOf :: Text -> [Char] -> Bool
startsWithOneOf line chars = case Text.uncons line of
    Just (c, _) -> c `elem` chars
    Nothing -> False

joinLines :: [Text] -> Text
joinLines ls = Text.unlines ls

stripPrefix :: Text -> Text -> Maybe Text
stripPrefix prefix line = Text.stripPrefix prefix (Text.stripStart line)

applyPatch :: ToolEnv -> Text -> IO (Either Text Text)
applyPatch env raw =
    runExceptT do
        hunks <- except (parsePatch raw)
        if null hunks
            then throwE "No files were modified."
            else applyHunks env hunks

applyHunks :: ToolEnv -> [Hunk] -> ExceptT Text IO Text
applyHunks env hunks = go hunks [] [] []
  where
    go [] added modified deleted =
        pure (summary added modified deleted)
    go (hunk : rest) added modified deleted = case hunk of
        AddFile path contents -> do
            resolved <- resolvePath env path
            ExceptT (writeTextFile resolved contents)
            go rest (path : added) modified deleted
        DeleteFile path -> do
            resolved <- resolvePath env path
            exists <- lift (doesFileExist resolved)
            unless exists $
                throwE ("Failed to delete file " <> Text.pack path)
            ExceptT (deleteTextFile resolved)
            go rest added modified (path : deleted)
        UpdateFile path moveTo chunks -> do
            resolved <- resolvePath env path
            original <- ExceptT (readTextFile resolved)
            newLines <- except (applyChunks chunks (Text.lines original))
            let newContents = joinFileLines newLines original
            case moveTo of
                Nothing -> do
                    ExceptT (writeTextFile resolved newContents)
                    go rest added (path : modified) deleted
                Just dest -> do
                    destResolved <- resolvePath env dest
                    ExceptT (writeTextFile destResolved newContents)
                    ExceptT (deleteTextFile resolved)
                    go rest added (dest : modified) deleted

resolvePath :: ToolEnv -> FilePath -> ExceptT Text IO OsPath
resolvePath env path =
    ExceptT (resolveUnderCwd env (fromFilePath path))

applyChunks :: [UpdateChunk] -> [Text] -> Either Text [Text]
applyChunks chunks start = foldl applyOne (Right start) chunks
  where
    applyOne (Left err) _ = Left err
    applyOne (Right lines_) chunk = applyChunk chunk lines_

applyChunk :: UpdateChunk -> [Text] -> Either Text [Text]
applyChunk chunk fileLines =
    let afterContext = case chunk.chunkContext of
            Nothing -> 0
            Just ctx ->
                fromMaybe (length fileLines)
                    (findIndexEqual ctx fileLines)
        searchFrom = case chunk.chunkContext of
            Nothing -> 0
            Just _ -> afterContext + 1
    in if null chunk.chunkOld
        then
            let insertAt
                    | chunk.chunkEof || chunk.chunkContext == Nothing =
                        length fileLines
                    | otherwise = searchFrom
            in Right (insertAtPos insertAt chunk.chunkNew fileLines)
        else case findSequence chunk.chunkOld fileLines searchFrom of
            Nothing ->
                -- Retry from the start when the @@ context did not pin a unique site.
                case findSequence chunk.chunkOld fileLines 0 of
                    Nothing -> Left "Failed to find expected lines in the file to update"
                    Just idx -> Right (replaceAt idx (length chunk.chunkOld) chunk.chunkNew fileLines)
            Just idx ->
                Right (replaceAt idx (length chunk.chunkOld) chunk.chunkNew fileLines)

findIndexEqual :: Text -> [Text] -> Maybe Int
findIndexEqual needle = go 0
  where
    go _ [] = Nothing
    go i (x : xs)
        | x == needle = Just i
        | otherwise = go (i + 1) xs

findSequence :: [Text] -> [Text] -> Int -> Maybe Int
findSequence needle haystack from
    | null needle = Just from
    | otherwise = go from
  where
    go i
        | i > length haystack - length needle = Nothing
        | take (length needle) (drop i haystack) == needle = Just i
        | otherwise = go (i + 1)

replaceAt :: Int -> Int -> [Text] -> [Text] -> [Text]
replaceAt idx count newLines fileLines =
    take idx fileLines ++ newLines ++ drop (idx + count) fileLines

insertAtPos :: Int -> [Text] -> [Text] -> [Text]
insertAtPos idx newLines fileLines =
    take idx fileLines ++ newLines ++ drop idx fileLines

joinFileLines :: [Text] -> Text -> Text
joinFileLines newLines original
    | Text.isSuffixOf "\n" original || Text.null original =
        Text.unlines newLines
    | otherwise =
        Text.intercalate "\n" newLines

summary :: [FilePath] -> [FilePath] -> [FilePath] -> Text
summary added modified deleted =
    Text.unlines $
        "Success. Updated the following files:"
            : [ "A " <> Text.pack p | p <- reverse added ]
            ++ [ "M " <> Text.pack p | p <- reverse modified ]
            ++ [ "D " <> Text.pack p | p <- reverse deleted ]
