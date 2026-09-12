{-# LANGUAGE BangPatterns #-}

-- | A self-contained undo chain. The head owns one complete snapshot; older
-- entries own only the text changed between adjacent snapshots. Keeping the
-- head independent of the composer also preserves undo across draft changes
-- which intentionally do not create an undo entry (e.g. dictation).
module Agent.CLI.TUI.Composer.Undo
    ( UndoEntry
    , pushUndoSnapshot
    , popUndoSnapshot
    ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Array as Array
import qualified Data.Text.Internal as Internal
import Data.Word (Word8)

-- Constructors stay private: a non-empty chain always starts with a snapshot.
data UndoEntry
    = Snapshot !Text !Int
    | Splice !Int !Int !Text !Int
    deriving (Eq, Show)

pushUndoSnapshot :: Int -> Text -> Int -> [UndoEntry] -> [UndoEntry]
pushUndoSnapshot limit text cursor entries
    | limit <= 0 = []
    | limit == 1 = let !entry = Snapshot text cursor in [entry]
    | otherwise =
        let !headEntry = Snapshot text cursor
        in case entries of
            [] -> [headEntry]
            Snapshot previous previousCursor : rest ->
                -- Force the splice now, not when it is eventually undone:
                -- otherwise its thunk would retain the previous full draft.
                let !delta = inverseSplice text previous previousCursor
                    !tailEntries = trimEntries (limit - 1) (delta : rest)
                in headEntry : tailEntries
            Splice {} : _ -> error "undo chain must begin with a snapshot"

-- Evaluate the bounded spine so discarded entries cannot remain reachable
-- through a suspended 'take'.
trimEntries :: Int -> [UndoEntry] -> [UndoEntry]
trimEntries count _ | count <= 0 = []
trimEntries _ [] = []
trimEntries count (entry : rest) =
    let !kept = trimEntries (count - 1) rest
    in entry : kept

popUndoSnapshot :: [UndoEntry] -> Maybe ((Text, Int), [UndoEntry])
popUndoSnapshot [] = Nothing
popUndoSnapshot (Snapshot text cursor : rest) =
    let !next = case rest of
            Splice prefix replaced removed previousCursor : remaining ->
                let !previous = sliceBytes 0 prefix text <> removed
                        <> sliceBytes (prefix + replaced)
                            (byteLength text - prefix - replaced) text
                    !entry = Snapshot previous previousCursor
                in entry : remaining
            _ -> rest
    in Just ((text, cursor), next)
popUndoSnapshot (Splice {} : _) =
    error "undo chain must begin with a snapshot"

inverseSplice :: Text -> Text -> Int -> UndoEntry
inverseSplice (Internal.Text newArray newOffset newLength)
    older@(Internal.Text oldArray oldOffset oldLength) cursor =
    let !sharedLimit = min newLength oldLength
        -- Compare long unchanged regions in bulk. A scalar byte loop alone
        -- makes every keystroke scan a large pasted draft unnecessarily slowly.
        -- The first unequal chunk (or final short region) is scanned bytewise.
        prefixLoop !index
            | sharedLimit - index >= 1024
            , Array.equal newArray (newOffset + index)
                oldArray (oldOffset + index) 1024 =
                    prefixLoop (index + 1024)
            | otherwise = prefixBytes index
        prefixBytes !index
            | index < sharedLimit
            , Array.unsafeIndex newArray (newOffset + index)
                == Array.unsafeIndex oldArray (oldOffset + index) =
                    prefixBytes (index + 1)
            | otherwise = index
        -- A byte mismatch can be inside a multibyte code point. Move back to
        -- its start before constructing any Text slices.
        prefixBoundary !index
            | index > 0 && index < newLength
            , continuation (Array.unsafeIndex newArray (newOffset + index)) =
                prefixBoundary (index - 1)
            | otherwise = index
        !prefix = prefixBoundary (prefixLoop 0)
        !suffixLimit = sharedLimit - prefix
        suffixLoop !count
            | suffixLimit - count >= 1024
            , Array.equal newArray (newOffset + newLength - count - 1024)
                oldArray (oldOffset + oldLength - count - 1024) 1024 =
                    suffixLoop (count + 1024)
            | otherwise = suffixBytes count
        suffixBytes !count
            | count < suffixLimit
            , Array.unsafeIndex newArray (newOffset + newLength - count - 1)
                == Array.unsafeIndex oldArray (oldOffset + oldLength - count - 1) =
                    suffixBytes (count + 1)
            | otherwise = count
        -- A matching suffix may start at a continuation byte even though its
        -- preceding lead bytes differ. Exclude that partial code point.
        suffixBoundary !count
            | count > 0
            , continuation (Array.unsafeIndex newArray (newOffset + newLength - count)) =
                suffixBoundary (count - 1)
            | otherwise = count
        !suffix = suffixBoundary (suffixLoop 0)
        !replaced = newLength - prefix - suffix
        -- Text.take/drop share their backing array. Copy only the removed
        -- fragment, so this entry cannot keep the old full draft alive.
        !removed = Text.copy (sliceBytes prefix (oldLength - prefix - suffix) older)
    in Splice prefix replaced removed cursor

-- Internal offsets are UTF-8 bytes; cursors remain Unicode character offsets.
-- Only boundaries proven above (or inherited from such a splice) reach here.
sliceBytes :: Int -> Int -> Text -> Text
sliceBytes offset count (Internal.Text array start _)
    | count <= 0 = Text.empty
    | otherwise = Internal.Text array (start + offset) count

byteLength :: Text -> Int
byteLength (Internal.Text _ _ count) = count

continuation :: Word8 -> Bool
continuation byte = byte >= 0x80 && byte < 0xc0
