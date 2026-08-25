-- | Queue and preview image attachments for an interactive session.
module Agent.CLI.Session.Attachments
    ( putImagePreview
    , queueAttachedImages
    , queueClipboardImages
    ) where

import Agent.CLI.Clipboard
    ( appendUniqueImageAttachments
    , formatImageSize
    , readClipboardImagesForPaste
    )
import Agent.CLI.ImagePreview
    ( detectImagePreviewProtocol
    , previewColumnsFor
    , previewRowsFor
    , renderImagePreview
    )
import Agent.CLI.Style (roleMuted)
import Agent.Loop (ImageAttachment(..))
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    )
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
    :: IORef [ImageAttachment]
    -> IORef Int
    -> Bool
    -> Bool
    -> [ImageAttachment]
    -> IO Text
queueAttachedImages attachmentsRef previewIdRef color showPreview images = do
    (added, pendingCount) <- atomicModifyIORef' attachmentsRef \existing ->
        let (pending, unique) =
                appendUniqueImageAttachments existing images
        in (pending, (unique, length pending))
    let sizes =
            Text.intercalate ", "
                [ img.imageMime
                    <> " ("
                    <> formatImageSize (BS.length img.imageBytes)
                    <> ")"
                | img <- added
                ]
        duplicateCount = length images - length added
        queued = Text.pack (show pendingCount)
    when (showPreview && not (null added)) $
        putImagePreview previewIdRef color added
    pure $ case (added, duplicateCount) of
        ([], _) ->
            "image already attached — not added again ("
                <> queued
                <> " queued)"
        (_, 0) ->
            "attached "
                <> sizes
                <> " — send with next message ("
                <> queued
                <> " queued)"
        _ ->
            "attached "
                <> sizes
                <> "; skipped "
                <> Text.pack (show duplicateCount)
                <> " duplicate image"
                <> (if duplicateCount == 1 then "" else "s")
                <> " ("
                <> queued
                <> " queued)"

queueClipboardImages
    :: IORef [ImageAttachment]
    -> IORef Int
    -> Bool
    -> Bool
    -> IO (Either Text Text)
queueClipboardImages
    attachmentsRef
    previewIdRef
    color
    showPreview = do
    imagesResult <- readClipboardImagesForPaste
    case imagesResult of
        Right images@(_:_) ->
            Right <$> queueAttachedImages
                attachmentsRef previewIdRef color showPreview images
        Right [] ->
            pure (Left "no image found on the clipboard")
        Left err -> pure (Left err)

putImagePreview :: IORef Int -> Bool -> [ImageAttachment] -> IO ()
putImagePreview previewIdRef color images = do
    protocol <- detectImagePreviewProtocol stdout
    inTmux <- isJust <$> lookupEnv "TMUX"
    size <- getTerminalSize
    let (termRows, termCols) = fromMaybe (24, 80) size
        columns = previewColumnsFor termCols
        rows = previewRowsFor termRows
    startId <- atomicModifyIORef' previewIdRef \n ->
        (n + max 1 (length images), n)
    case renderImagePreview
        protocol inTmux (roleMuted color) columns rows startId images of
        Nothing -> pure ()
        Just block -> do
            -- Graphics sequences must not go through the Solarized wash.
            Text.putStrLn block
            hFlush stdout
