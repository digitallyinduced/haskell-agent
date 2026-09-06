-- | Clipboard paste and pending-attachment commands.
module Agent.CLI.Runtime.Repl.Attachments
    ( handleAttachmentAction
    , ClipboardInput(..)
    , handleClipboardInput
    ) where

import Agent.CLI.Clipboard
    ( formatImageSize, loadImagesFromPastedText, nonEmptyClipboardImages,
      readClipboardImagesForPaste, readClipboardImagesImageFirst )
import Agent.CLI.Command
    ( AttachmentAction(ReplPaste, ReplShowAttachments, ReplClearAttachments,
                 ReplRemoveAttachment) )
import Agent.CLI.ProviderTransition ( TurnResult )
import Agent.CLI.Render ( resetRenderPrintedText )
import Agent.CLI.Runtime.Types ( RunResult )
import Agent.CLI.Session.Attachments
    ( putImagePreview, queueAttachedImages, queueClipboardImages )
import Agent.CLI.SessionState (removeImageAttachmentAt)
import Agent.CLI.Session.History
    ( modifyLiveAttachments, readLiveAttachments )
import Agent.CLI.SessionEnv ( SessionEnv(..) )
import Agent.CLI.Style ( glyphOk, glyphSession, roleError, roleMuted )
import Agent.CLI.TUI.App
    ( emitUiEvent, setFullscreenImagePreviews )
import Agent.CLI.Terminal ( resolveColor )
import Agent.CLI.Turn ( runOneTurn )
import Agent.Loop
    ( ImageAttachment(imageBytes, imageMime)
    , TurnAttachment(ImageAttachmentItem)
    , userMessageWithAttachments
    )
import Agent.TUI.Model
    ( UiEvent(UiUserSubmitted, UiSetNotice, UiErrorMessage, UiSystemMessage) )
import Control.Monad ( forM_, when )
import Data.Maybe ( isNothing )
import Data.Text ( Text )
import System.IO ( stderr, stdout )
import qualified Data.ByteString as BS ( length )
import qualified Data.Text as Text ( intercalate, null )
import qualified Data.Text.IO as Text ( hPutStrLn, putStrLn )

-- | Clipboard operations after the REPL input has been classified.
data ClipboardInput
    = ClipboardPaste !Text !(Maybe [ImageAttachment])
    | ClipboardPasteOrText !Text !Text !Text

handleClipboardInput
    :: SessionEnv
    -> (Text -> IO RunResult)
    -> Bool
    -> ClipboardInput
    -> IO RunResult
handleClipboardInput
        SessionEnv
            { sessionConversation = conversationRef
            , sessionPreviewId = previewIdRef
            , sessionFullscreen = fullscreen
            }
        continueWith
        stdoutColor = \case
    ClipboardPaste keptDraft clipboardPasteImages -> do
        case clipboardPasteImages of
            Just images@(_:_) -> do
                message <- queueAttachedImages
                    conversationRef
                    previewIdRef
                    stdoutColor
                    (isNothing fullscreen)
                    images
                syncFullscreenImagePreviews
                fullscreenEvent (UiSetNotice Nothing)
                displayInfo message $
                    Text.putStrLn
                        (roleMuted stdoutColor
                            (glyphOk <> message))
            _ ->
                queueClipboardImages
                    conversationRef
                    previewIdRef
                    stdoutColor
                    (isNothing fullscreen)
                    >>= \case
                        Left err ->
                            displayError err do
                                errColor <- resolveColor stderr
                                Text.hPutStrLn stderr (roleError errColor err)
                        Right message -> do
                            syncFullscreenImagePreviews
                            displayInfo message $
                                Text.putStrLn
                                    (roleMuted stdoutColor
                                        (glyphOk <> message))
        continueWith keptDraft
    ClipboardPasteOrText keptDraft pasted pastedDraft -> do
        pastedImages <- loadImagesFromPastedText pasted
        imagesResult <- case pastedImages of
            Just images@(_:_) -> pure (Just images)
            _ ->
                nonEmptyClipboardImages
                    <$> readClipboardImagesImageFirst
        case imagesResult of
            Just images -> do
                message <- queueAttachedImages
                    conversationRef
                    previewIdRef
                    stdoutColor
                    (isNothing fullscreen)
                    images
                syncFullscreenImagePreviews
                fullscreenEvent (UiSetNotice Nothing)
                displayInfo message $
                    Text.putStrLn
                        (roleMuted stdoutColor
                            (glyphOk <> message))
                continueWith keptDraft
            _ -> do
                fullscreenEvent (UiSetNotice Nothing)
                continueWith pastedDraft
  where
    fullscreenEvent event = case fullscreen of
        Nothing -> pure ()
        Just runtime -> emitUiEvent runtime event
    syncFullscreenImagePreviews =
        forM_ fullscreen \runtime ->
            readLiveAttachments conversationRef
                >>= setFullscreenImagePreviews runtime
    displayInfo message minimalAction = case fullscreen of
        Nothing -> minimalAction
        Just runtime -> emitUiEvent runtime (UiSystemMessage message)
    displayError message minimalAction = case fullscreen of
        Nothing -> minimalAction
        Just runtime -> emitUiEvent runtime (UiErrorMessage message)

handleAttachmentAction
    :: SessionEnv
    -> (Bool -> TurnResult -> IO RunResult)
    -> IO RunResult
    -> AttachmentAction
    -> IO RunResult
handleAttachmentAction
        env@SessionEnv
            { sessionRender = render
            , sessionConversation = conversationRef
            , sessionPreviewId = previewIdRef
            , sessionFullscreen = fullscreen
            }
        finishTurn
        continue = \case
    ReplPaste pasteImmediate pasteCaption -> do
        color <- resolveColor stdout
        errColor <- resolveColor stderr
        imagesResult <- readClipboardImagesForPaste
        case imagesResult of
            Left err -> do
                displayError err $
                    Text.hPutStrLn stderr
                        (roleError errColor err)
                continue
            Right [] -> do
                displayError "no image found on the clipboard" $
                    Text.hPutStrLn stderr
                        (roleError errColor
                            "no image found on the clipboard")
                continue
            Right images -> do
                let sizes =
                        Text.intercalate ", "
                            [ img.imageMime <> " (" <> formatImageSize (BS.length img.imageBytes) <> ")"
                            | img <- images
                            ]
                if pasteImmediate
                    then do
                        let promptText =
                                if Text.null pasteCaption
                                    then "See attached image."
                                    else pasteCaption
                        when (isNothing fullscreen) $
                            putImagePreview previewIdRef color images
                        displayInfo ("pasted " <> sizes) $
                            Text.putStrLn
                                (roleMuted color
                                    (glyphOk <> "pasted " <> sizes))
                        resetRenderPrintedText render
                        fullscreenEvent
                            (UiUserSubmitted promptText)
                        let turnInputs =
                                [ userMessageWithAttachments
                                    promptText
                                    (map ImageAttachmentItem images)
                                ]
                        result <- runOneTurn env promptText turnInputs
                        finishTurn False result
                    else do
                        message <- queueAttachedImages
                            conversationRef
                            previewIdRef
                            color
                            (isNothing fullscreen)
                            images
                        syncFullscreenImagePreviews
                        displayInfo message $
                            Text.putStrLn
                                (roleMuted color
                                    (glyphOk <> message))
                        continue
    ReplShowAttachments -> do
        pending <- readLiveAttachments conversationRef
        color <- resolveColor stdout
        let message =
                if null pending
                    then "attachments: (none)"
                    else "attachments: "
                        <> Text.intercalate ", "
                            [ img.imageMime
                                <> " ("
                                <> formatImageSize
                                    (BS.length img.imageBytes)
                                <> ")"
                            | img <- pending
                            ]
        displayInfo message $
            Text.putStrLn
                (roleMuted color (glyphSession <> message))
        continue
    ReplClearAttachments -> do
        modifyLiveAttachments conversationRef (\_ -> ([], ()))
        forM_ fullscreen \runtime ->
            setFullscreenImagePreviews runtime []
        color <- resolveColor stdout
        displayInfo "attachments cleared" $
            Text.putStrLn
                (roleMuted color
                    (glyphOk <> "attachments cleared"))
        continue
    ReplRemoveAttachment index -> do
        removed <- modifyLiveAttachments conversationRef
            (removeImageAttachmentAt index)
        syncFullscreenImagePreviews
        color <- resolveColor stdout
        let message =
                if removed
                    then "attachment removed"
                    else "attachment is no longer available"
        displayInfo message $
            Text.putStrLn
                (roleMuted color (glyphOk <> message))
        continue
  where
    fullscreenEvent event = case fullscreen of
        Nothing -> pure ()
        Just runtime -> emitUiEvent runtime event
    syncFullscreenImagePreviews =
        forM_ fullscreen \runtime ->
            readLiveAttachments conversationRef
                >>= setFullscreenImagePreviews runtime
    displayInfo message minimalAction = case fullscreen of
        Nothing -> minimalAction
        Just runtime -> emitUiEvent runtime (UiSystemMessage message)
    displayError message minimalAction = case fullscreen of
        Nothing -> minimalAction
        Just runtime -> emitUiEvent runtime (UiErrorMessage message)
