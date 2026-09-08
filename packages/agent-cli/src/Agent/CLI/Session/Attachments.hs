-- | Queue and preview image attachments for an interactive session.
module Agent.CLI.Session.Attachments
    ( putImagePreview
    , putChartPreview
    , queueAttachedImages
    , queueClipboardImages
    ) where

import Agent.CLI.Clipboard
    ( appendBoundedImageAttachments
    , formatImageSize
    , pendingImageAttachmentByteLimit
    , pendingImageAttachmentCountLimit
    , readClipboardImagesImageFirst
    )
import Agent.CLI.ImagePreview
    ( detectImagePreviewProtocol
    , previewColumnsFor
    , previewRowsFor
    , renderImagePreview
    )
import Agent.CLI.Session.History
    ( LiveConversation
    , modifyLiveAttachments
    )
import Agent.CLI.Style (roleMuted)
import Agent.Loop (ImageAttachment(..))
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.IORef (IORef, atomicModifyIORef')
import Data.Maybe
    ( fromMaybe
    , isJust
    )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Console.ANSI (getTerminalSize)
import System.Environment (lookupEnv)
import System.IO
    ( hFlush
    , stdout
    )

-- | Queue clipboard / Finder-paste images and optionally draw an in-terminal
-- thumbnail. The caller reports the returned message through the active UI.
queueAttachedImages
    :: IORef LiveConversation
    -> IORef Int
    -> Bool
    -> Bool
    -> [ImageAttachment]
    -> IO Text
queueAttachedImages attachmentsRef previewIdRef color showPreview images = do
    (added, duplicateCount, rejectedCount, pendingCount) <-
        modifyLiveAttachments attachmentsRef \existing ->
            let (pending, unique, duplicates, rejected) =
                    appendBoundedImageAttachments existing images
            in ( pending
               , (unique, duplicates, rejected, length pending)
               )
    let sizes =
            Text.intercalate ", "
                [ img.imageMime
                    <> " ("
                    <> formatImageSize (BS.length img.imageBytes)
                    <> ")"
                | img <- added
                ]
        queued = Text.pack (show pendingCount)
        duplicateSuffix
            | duplicateCount == 0 = ""
            | otherwise =
                "; skipped "
                    <> Text.pack (show duplicateCount)
                    <> " duplicate image"
                    <> if duplicateCount == 1 then "" else "s"
        rejectedSuffix
            | rejectedCount == 0 = ""
            | otherwise =
                "; skipped "
                    <> Text.pack (show rejectedCount)
                    <> " image"
                    <> (if rejectedCount == 1 then "" else "s")
                    <> " over the "
                    <> Text.pack
                        (show pendingImageAttachmentCountLimit)
                    <> "-image / "
                    <> formatImageSize pendingImageAttachmentByteLimit
                    <> " limit"
    when (showPreview && not (null added)) $
        putImagePreview previewIdRef color added
    pure $ case (added, duplicateCount, rejectedCount) of
        ([], _, rejected) | rejected > 0 ->
            "image attachment limit reached"
                <> duplicateSuffix
                <> rejectedSuffix
                <> " ("
                <> queued
                <> " queued)"
        ([], _, _) ->
            "image already attached — not added again ("
                <> queued
                <> " queued)"
        (_, _, _) ->
            "attached "
                <> sizes
                <> duplicateSuffix
                <> rejectedSuffix
                <> " — send with next message ("
                <> queued
                <> " queued)"

queueClipboardImages
    :: IORef LiveConversation
    -> IORef Int
    -> Bool
    -> Bool
    -> IO (Either Text Text)
queueClipboardImages
    attachmentsRef
    previewIdRef
    color
    showPreview = do
    -- Terminal image pastes normally expose bitmap bytes directly. Avoid the
    -- comparatively slow macOS Finder file-list coercion unless that fast
    -- path fails.
    imagesResult <- readClipboardImagesImageFirst
    case imagesResult of
        Right images@(_:_) ->
            Right <$> queueAttachedImages
                attachmentsRef previewIdRef color showPreview images
        Right [] ->
            pure (Left "no image found on the clipboard")
        Left err -> pure (Left err)

putImagePreview :: IORef Int -> Bool -> [ImageAttachment] -> IO ()
putImagePreview = putSizedImagePreview False

putChartPreview :: IORef Int -> Bool -> [ImageAttachment] -> IO ()
putChartPreview = putSizedImagePreview True

putSizedImagePreview :: Bool -> IORef Int -> Bool -> [ImageAttachment] -> IO ()
putSizedImagePreview chart previewIdRef color images = do
    protocol <- detectImagePreviewProtocol stdout
    inTmux <- isJust <$> lookupEnv "TMUX"
    size <- getTerminalSize
    let (termRows, termCols) = fromMaybe (24, 80) size
        columns = if chart then max 1 (min 72 (termCols - 2)) else previewColumnsFor termCols
        -- The chart is 8:5; Kitty derives width from rows. Allow roughly
        -- two pixels of cell height per pixel of cell width on narrow panes.
        rows = if chart then max 1 (minimum [24, termRows - 4, columns * 5 `div` 16]) else previewRowsFor termRows
    startId <- atomicModifyIORef' previewIdRef \n ->
        (n + max 1 (length images), n)
    case renderImagePreview
        protocol inTmux (roleMuted color) columns rows startId images of
        Nothing -> pure ()
        Just block -> do
            -- Graphics sequences must not go through the Solarized wash.
            Text.putStrLn block
            hFlush stdout
