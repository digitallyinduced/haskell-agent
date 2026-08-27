-- | Pure composer cursor and deletion operations.
module Agent.TUI.Model.Edit
    ( deleteToLineStart
    , deleteToLineEnd
    , deleteWordAfter
    , deleteWordBefore
    , lineEndCursor
    , lineStartCursor
    , moveWordLeft
    , moveWordRight
    ) where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Delete whitespace and the previous non-whitespace word before the cursor.
deleteWordBefore :: Text -> Int -> (Text, Int)
deleteWordBefore text cursor =
    let cursor' = max 0 (min (Text.length text) cursor)
        before = Text.take cursor' text
        after = Text.drop cursor' text
        reversed = Text.reverse before
        withoutSpace = Text.dropWhile isSpace reversed
        remaining = Text.dropWhile (not . isSpace) withoutSpace
        kept = Text.reverse remaining
        after'
            | Text.isSuffixOf " " kept
            , Text.isPrefixOf " " after =
                Text.drop 1 after
            | otherwise = after
    in (kept <> after', Text.length kept)

-- | Delete whitespace and the next non-whitespace word after the cursor.
deleteWordAfter :: Text -> Int -> (Text, Int)
deleteWordAfter text cursor =
    let cursor' = max 0 (min (Text.length text) cursor)
        before = Text.take cursor' text
        after = Text.drop cursor' text
        withoutSpace = Text.dropWhile isSpace after
        remaining = Text.dropWhile (not . isSpace) withoutSpace
    in (before <> remaining, cursor')

-- | Delete from the cursor to the beginning of its logical line.
deleteToLineStart :: Text -> Int -> (Text, Int)
deleteToLineStart text cursor =
    let cursor' = max 0 (min (Text.length text) cursor)
        before = Text.take cursor' text
        after = Text.drop cursor' text
        kept = case Text.breakOnEnd "\n" before of
            ("", _) -> ""
            (prefix, _) -> prefix
    in (kept <> after, Text.length kept)

-- | Delete from the cursor to the end of its logical line.
deleteToLineEnd :: Text -> Int -> (Text, Int)
deleteToLineEnd text cursor =
    let cursor' = max 0 (min (Text.length text) cursor)
        before = Text.take cursor' text
        after = Text.drop cursor' text
        keptAfter = case Text.break (== '\n') after of
            (_, rest) -> rest
    in (before <> keptAfter, cursor')

lineStartCursor :: Text -> Int -> Int
lineStartCursor text cursor =
    let cursor' = max 0 (min (Text.length text) cursor)
        before = Text.take cursor' text
    in Text.length (fst (Text.breakOnEnd "\n" before))

lineEndCursor :: Text -> Int -> Int
lineEndCursor text cursor =
    let cursor' = max 0 (min (Text.length text) cursor)
        after = Text.drop cursor' text
        (line, _) = Text.break (== '\n') after
    in cursor' + Text.length line

moveWordLeft :: Text -> Int -> Int
moveWordLeft text cursor =
    let cursor' = max 0 (min (Text.length text) cursor)
        reversed = Text.reverse (Text.take cursor' text)
        withoutSpace = Text.dropWhile isSpace reversed
        remaining = Text.dropWhile (not . isSpace) withoutSpace
    in Text.length remaining

moveWordRight :: Text -> Int -> Int
moveWordRight text cursor =
    let cursor' = max 0 (min (Text.length text) cursor)
        after = Text.drop cursor' text
        withoutWord = Text.dropWhile (not . isSpace) after
        remaining = Text.dropWhile isSpace withoutWord
    in Text.length text - Text.length remaining
