-- | Codex multi-agent v2 task paths (@/root/...@).
--
-- Subset of openai/codex 'AgentPath': absolute paths under @/root@ with
-- lowercase alphanumeric / underscore segments.
module Agent.Subagents.TaskPath
    ( TaskPath
    , taskPathRoot
    , taskPathText
    , taskPathName
    , isTaskPathRoot
    , parseTaskPath
    , joinTaskPath
    , resolveTaskPath
    , validateTaskName
    ) where

import Data.Char (isAsciiLower, isDigit)
import Data.Text (Text)
import qualified Data.Text as Text

newtype TaskPath = TaskPath { unTaskPath :: Text }
    deriving (Eq, Ord, Show)

taskPathRoot :: TaskPath
taskPathRoot = TaskPath "/root"

taskPathText :: TaskPath -> Text
taskPathText (TaskPath text) = text

isTaskPathRoot :: TaskPath -> Bool
isTaskPathRoot (TaskPath text) = text == "/root"

taskPathName :: TaskPath -> Text
taskPathName path@(TaskPath text)
    | isTaskPathRoot path = "root"
    | otherwise =
        case reverse (Text.splitOn "/" text) of
            (name : _) | not (Text.null name) -> name
            _ -> "root"

parseTaskPath :: Text -> Either Text TaskPath
parseTaskPath raw = do
    let path = Text.strip raw
    if Text.null path
        then Left "task path must not be empty"
        else do
            validateAbsolute path
            pure (TaskPath path)

validateTaskName :: Text -> Either Text Text
validateTaskName name
    | Text.null name = Left "task_name must not be empty"
    | name == "root" = Left "task_name must not be \"root\""
    | Text.any (not . isNameChar) name =
        Left "task_name must use lowercase letters, digits, and underscores only"
    | otherwise = Right name
  where
    isNameChar c = isAsciiLower c || isDigit c || c == '_'

joinTaskPath :: TaskPath -> Text -> Either Text TaskPath
joinTaskPath (TaskPath parent) name = do
    validated <- validateTaskName name
    parseTaskPath (parent <> "/" <> validated)

-- | Resolve a target reference relative to the caller's path.
-- Absolute @/root/...@ paths parse directly; relative names join the caller.
resolveTaskPath :: TaskPath -> Text -> Either Text TaskPath
resolveTaskPath current@(TaskPath currentText) reference
    | Text.null reference = Left "target must not be empty"
    | reference == "/root" = Right taskPathRoot
    | "/" `Text.isPrefixOf` reference = parseTaskPath reference
    | otherwise = do
        validateRelative reference
        parseTaskPath (currentText <> "/" <> reference)

validateAbsolute :: Text -> Either Text ()
validateAbsolute path
    | path == "/root" = Right ()
    | "/root/" `Text.isPrefixOf` path = do
        let rest = Text.drop (Text.length "/root/") path
        validateSegments (Text.splitOn "/" rest)
    | otherwise = Left "task path must start with /root"

validateRelative :: Text -> Either Text ()
validateRelative reference
    | "/" `Text.isPrefixOf` reference =
        Left "relative target must not start with /"
    | otherwise =
        validateSegments (Text.splitOn "/" reference)

validateSegments :: [Text] -> Either Text ()
validateSegments segments
    | any Text.null segments = Left "task path must not contain empty segments"
    | otherwise = mapM_ validateTaskName segments
