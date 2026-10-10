-- | Codex @apply_patch@ language.
--
-- Format copied from openai/codex @ codex-rs/apply-patch (Lark grammar in
-- parser.rs). This is a freeform/custom tool: the body is patch text, not JSON.
module Agent.Codex.Dialect.ApplyPatch
    ( Hunk(..)
    , UpdateChunk(..)
    , parsePatch
    , applyPatch
    , applyPatchGrammar
    ) where

import Agent.OsPath (fromText, relativeDisplayPath)
import Agent.Tools.IO
    ( deleteTextFile
    , readTextFile
    , resolveUnderCwd
    , writeTextFile
    )
import Agent.Tools.Types (ToolEnv(..))
import Control.Monad (foldM, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
    ( ExceptT(..)
    , catchE
    , except
    , runExceptT
    , throwE
    )
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory.OsPath (doesFileExist)
import System.OsPath (OsPath, unsafeEncodeUtf)


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
    , chunkContextIndices :: ![(Int, Int)]
    , chunkEof :: !Bool
    } deriving (Eq, Show)

parsePatch :: Text -> Either Text [Hunk]
parsePatch raw = do
    let trimmed = Text.strip raw
        ls = map (\line -> fromMaybe line (Text.stripSuffix "\r" line)) (Text.lines trimmed)
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
        case (header, body, eof, lines_) of
            (Nothing, [], False, line : _) ->
                Left $
                    "Invalid update line; expected '@@', '+', '-', or space prefix: "
                        <> Text.take 200 line
            _ -> do
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
    , chunkContextIndices = contextIndices 0 0 body
    , chunkEof = eof
    }
  where
    contextIndices _ _ [] = []
    contextIndices oldIndex newIndex (line : rest) = case Text.uncons line of
        Just (' ', _) -> (newIndex, oldIndex) : contextIndices (oldIndex + 1) (newIndex + 1) rest
        Just ('-', _) -> contextIndices (oldIndex + 1) newIndex rest
        Just ('+', _) -> contextIndices oldIndex (newIndex + 1) rest
        _ -> contextIndices oldIndex newIndex rest

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
applyHunks env hunks = do
    (actions, resultSummary) <-
        prepareHunks env hunks `catchE` \err ->
            throwE $
                "Patch validation failed; no files were changed.\n" <> err
    mapM_ applyPreparedAction actions `catchE` \err ->
        throwE $
            "Patch commit failed; files may have been partially changed.\n" <> err
    pure resultSummary

data VirtualFile
    = VirtualContents !Text
    | VirtualMissing

data PreparedAction
    = PreparedWrite !OsPath !Text
    | PreparedDelete !OsPath

-- | Validate every hunk against an in-memory view of earlier hunks before
-- returning any filesystem actions. A stale later hunk therefore cannot leave
-- earlier files modified even though the overall tool call reports failure.
prepareHunks
    :: ToolEnv
    -> [Hunk]
    -> ExceptT Text IO ([PreparedAction], Text)
prepareHunks env hunks = go hunks Map.empty [] [] [] []
  where
    go [] _ actions added modified deleted =
        pure
            ( reverse actions
            , summary env.toolCwd added modified deleted
            )
    go (hunk : rest) files actions added modified deleted = case hunk of
        AddFile path contents -> do
            resolved <- resolvePath env path
            go
                rest
                (Map.insert resolved (VirtualContents contents) files)
                (PreparedWrite resolved contents : actions)
                (path : added)
                modified
                deleted
        DeleteFile path -> do
            resolved <- resolvePath env path
            exists <- virtualFileExists resolved files
            unless exists $
                throwE ("Failed to delete file " <> Text.pack path)
            go
                rest
                (Map.insert resolved VirtualMissing files)
                (PreparedDelete resolved : actions)
                added
                modified
                (path : deleted)
        UpdateFile path moveTo chunks -> do
            resolved <- resolvePath env path
            original <- virtualFileContents path resolved files
            let sourceLines = splitFileLines original
                preferredEnding = case sourceLines of
                    SourceLine _ ending : _ | not (Text.null ending) -> ending
                    _ -> "\n"
            newLines <- case applyChunks preferredEnding chunks sourceLines of
                Left err ->
                    throwE $
                        "Failed to update file '"
                            <> Text.pack path
                            <> "': "
                            <> err
                Right lines_ -> pure (toList lines_)
            let newContents = joinFileLines preferredEnding newLines original
            case moveTo of
                Nothing ->
                    go
                        rest
                        (Map.insert resolved (VirtualContents newContents) files)
                        (PreparedWrite resolved newContents : actions)
                        added
                        (path : modified)
                        deleted
                Just dest -> do
                    destResolved <- resolvePath env dest
                    let movedFiles =
                            Map.insert resolved VirtualMissing $
                                Map.insert
                                    destResolved
                                    (VirtualContents newContents)
                                    files
                    go
                        rest
                        movedFiles
                        ( PreparedDelete resolved
                            : PreparedWrite destResolved newContents
                            : actions
                        )
                        added
                        (dest : modified)
                        deleted

    virtualFileExists resolved files =
        case Map.lookup resolved files of
            Just (VirtualContents _) -> pure True
            Just VirtualMissing -> pure False
            Nothing -> lift (doesFileExist resolved)

    virtualFileContents path resolved files =
        case Map.lookup resolved files of
            Just (VirtualContents contents) -> pure contents
            Just VirtualMissing ->
                throwE $
                    "Failed to update file '"
                        <> Text.pack path
                        <> "': file does not exist"
            Nothing -> ExceptT (readTextFile resolved)

applyPreparedAction :: PreparedAction -> ExceptT Text IO ()
applyPreparedAction = \case
    PreparedWrite path contents ->
        ExceptT (writeTextFile path contents)
    PreparedDelete path ->
        ExceptT (deleteTextFile path)

resolvePath :: ToolEnv -> FilePath -> ExceptT Text IO OsPath
resolvePath env path =
    ExceptT (resolveUnderCwd env (unsafeEncodeUtf path))

-- Keep terminators separate from text so context matching is independent of
-- the file's line endings and unchanged lines retain even mixed styles.
data SourceLine = SourceLine
    { sourceText :: !Text
    , sourceEnding :: !Text
    }

splitFileLines :: Text -> [SourceLine]
splitFileLines contents
    | Text.null contents = []
    | otherwise =
        let (body, suffix) = Text.break (\c -> c == '\r' || c == '\n') contents
        in case Text.uncons suffix of
            Nothing -> [SourceLine body ""]
            Just ('\r', rest) | Just remaining <- Text.stripPrefix "\n" rest ->
                SourceLine body "\r\n" : splitFileLines remaining
            Just (ending, rest) -> SourceLine body (Text.singleton ending) : splitFileLines rest

applyChunks :: Text -> [UpdateChunk] -> [SourceLine] -> Either Text (Seq SourceLine)
applyChunks preferredEnding chunks start =
    foldM (flip (applyChunk preferredEnding)) (Seq.fromList start) chunks

applyChunk :: Text -> UpdateChunk -> Seq SourceLine -> Either Text (Seq SourceLine)
applyChunk preferredEnding chunk fileLines =
    let lineTexts = fmap (.sourceText) fileLines
        afterContext = case chunk.chunkContext of
            Nothing -> 0
            Just ctx ->
                fromMaybe (Seq.length fileLines)
                    (Seq.elemIndexL ctx lineTexts)
        searchFrom = case chunk.chunkContext of
            Nothing -> 0
            Just _ -> afterContext + 1
        oldLines = Seq.fromList chunk.chunkOld
        newLines = Seq.fromList (map (\body -> SourceLine body preferredEnding) chunk.chunkNew)
        replace idx =
            let contextIndices = Map.fromList chunk.chunkContextIndices
                retainedLines = Seq.mapWithIndex
                    (\newIndex line -> case Map.lookup newIndex contextIndices of
                        Just oldIndex -> fromMaybe line (Seq.lookup (idx + oldIndex) fileLines)
                        Nothing -> line)
                    newLines
            in replaceAt idx (Seq.length oldLines) retainedLines fileLines
    in if Seq.null oldLines
        then
            let insertAt
                    | chunk.chunkEof || chunk.chunkContext == Nothing =
                        Seq.length fileLines
                    | otherwise = searchFrom
            in Right (insertAtPos insertAt newLines fileLines)
        else case findSequence oldLines lineTexts searchFrom of
            Nothing ->
                -- Retry from the start when the @@ context did not pin a unique site.
                case findSequence oldLines lineTexts 0 of
                    Nothing -> Left "Failed to find expected lines in the file to update"
                    Just idx ->
                        Right (replace idx)
            Just idx ->
                Right (replace idx)

findSequence :: Seq Text -> Seq Text -> Int -> Maybe Int
findSequence needle haystack from
    | Seq.null needle = Just from
    | otherwise = go from
  where
    needleLength = Seq.length needle
    haystackLength = Seq.length haystack
    go i
        | i > haystackLength - needleLength = Nothing
        | Seq.take needleLength (Seq.drop i haystack) == needle = Just i
        | otherwise = go (i + 1)

replaceAt :: Int -> Int -> Seq a -> Seq a -> Seq a
replaceAt idx count newLines fileLines =
    Seq.take idx fileLines <> newLines <> Seq.drop (idx + count) fileLines

insertAtPos :: Int -> Seq a -> Seq a -> Seq a
insertAtPos idx newLines fileLines =
    Seq.take idx fileLines <> newLines <> Seq.drop idx fileLines

joinFileLines :: Text -> [SourceLine] -> Text -> Text
joinFileLines preferredEnding newLines original = Text.concat (render newLines)
  where
    terminated = Text.null original || Text.isSuffixOf "\n" original || Text.isSuffixOf "\r" original
    ending :: SourceLine -> Text
    ending line
        | Text.null line.sourceEnding = preferredEnding
        | otherwise = line.sourceEnding
    render :: [SourceLine] -> [Text]
    render [] = []
    render [line] = [line.sourceText, if terminated then ending line else ""]
    render (line : rest) = line.sourceText : ending line : render rest

summary :: OsPath -> [FilePath] -> [FilePath] -> [FilePath] -> Text
summary workspace added modified deleted =
    Text.unlines $
        "Success. Updated the following files:"
            : [ "A " <> display p | p <- reverse added ]
            ++ [ "M " <> display p | p <- reverse modified ]
            ++ [ "D " <> display p | p <- reverse deleted ]
  where
    display path =
        relativeDisplayPath workspace (fromText (Text.pack path))
