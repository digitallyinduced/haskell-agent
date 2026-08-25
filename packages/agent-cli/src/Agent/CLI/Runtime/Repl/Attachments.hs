-- | Clipboard image and pending-attachment actions used by the REPL.
module Agent.CLI.Runtime.Repl.Attachments
    ( AttachmentContext(..)
    , attachImagesFromPrompt
    , clearAttachments
    , handleClipboardPaste
    , handleClipboardPasteOrText
    , pasteAttachments
    , showAttachments
    ) where

import Agent.CLI.Clipboard
    ( formatImageSize
    , loadImagesFromPastedText
    , nonEmptyClipboardImages
    , readClipboardImagesForPaste
    , readClipboardImagesImageFirst
    )
import Agent.CLI.ProviderTransition ( TurnResult )
import Agent.CLI.Render ( resetRenderPrintedText )
import Agent.CLI.Runtime.Types ( RunResult )
import Agent.CLI.Session.Attachments
    ( putImagePreview
    , queueAttachedImages
    , queueClipboardImages
    )
import Agent.CLI.Session.History
    ( modifyLiveAttachments
    , readLiveAttachments
    )
import Agent.CLI.SessionEnv ( SessionEnv(..) )
import Agent.CLI.Style
    ( glyphOk
    , glyphSession
    , roleError
    , roleMuted
    )
import Agent.CLI.TUI.App
    ( emitUiEvent
    , setFullscreenImagePreviews
    )
import Agent.CLI.Terminal ( resolveColor )
import Agent.CLI.Turn ( runOneTurn )
import Agent.Loop
    ( ImageAttachment(imageBytes, imageMime)
    , TurnInput(UserMultimodal, userImages, userText)
    )
import Agent.TUI.Model
    ( UiEvent(UiSetNotice, UiUserSubmitted, UiErrorMessage, UiSystemMessage)
    )
import Control.Monad ( forM_, when )
import Data.Text ( Text )
import System.IO ( stderr, stdout )
import qualified Data.ByteString as BS ( length )
import qualified Data.Text as Text ( intercalate, null )
import qualified Data.Text.IO as Text ( hPutStrLn, putStrLn )

data AttachmentContext = AttachmentContext
    { attachmentSession :: !SessionEnv
    , attachmentContinueWith :: !(Text -> IO RunResult)
    , attachmentFinishTurn :: !(Bool -> TurnResult -> IO RunResult)
    }

handleClipboardPaste
    :: AttachmentContext
    -> Bool
    -> Text
    -> Maybe [ImageAttachment]
    -> IO RunResult
handleClipboardPaste context color keptDraft clipboardPasteImages = do
    case clipboardPasteImages of
        Just images@(_:_) ->
            queueAndReport context color True images
        _ ->
            queueClipboardImages
                context.attachmentSession.sessionConversation
                context.attachmentSession.sessionPreviewId
                color
                (isMinimal context)
                >>= \case
                    Left err ->
                        displayError context err do
                            errColor <- resolveColor stderr
                            Text.hPutStrLn stderr (roleError errColor err)
                    Right message -> do
                        syncFullscreenImagePreviews context
                        displayInfo context message $
                            Text.putStrLn
                                (roleMuted color (glyphOk <> message))
    context.attachmentContinueWith keptDraft

handleClipboardPasteOrText
    :: AttachmentContext
    -> Bool
    -> Text
    -> Text
    -> Text
    -> IO RunResult
handleClipboardPasteOrText context color keptDraft pasted pastedDraft = do
    pastedImages <- loadImagesFromPastedText pasted
    imagesResult <- case pastedImages of
        Just images@(_:_) -> pure (Just images)
        _ ->
            nonEmptyClipboardImages
                <$> readClipboardImagesImageFirst
    case imagesResult of
        Just images -> do
            queueAndReport context color True images
            context.attachmentContinueWith keptDraft
        _ -> do
            fullscreenEvent context (UiSetNotice Nothing)
            context.attachmentContinueWith pastedDraft

attachImagesFromPrompt
    :: AttachmentContext
    -> Bool
    -> Text
    -> IO Bool
attachImagesFromPrompt context color text =
    loadImagesFromPastedText text >>= \case
        Just images@(_:_) -> do
            queueAndReport context color False images
            pure True
        _ ->
            pure False

pasteAttachments
    :: AttachmentContext
    -> Bool
    -> Text
    -> IO RunResult
pasteAttachments context pasteImmediate pasteCaption = do
    color <- resolveColor stdout
    errColor <- resolveColor stderr
    readClipboardImagesForPaste >>= \case
        Left err -> do
            displayError context err $
                Text.hPutStrLn stderr (roleError errColor err)
            continue context
        Right [] -> do
            let err = "no image found on the clipboard"
            displayError context err $
                Text.hPutStrLn stderr (roleError errColor err)
            continue context
        Right images -> do
            let sizes =
                    Text.intercalate ", "
                        [ image.imageMime
                            <> " ("
                            <> formatImageSize (BS.length image.imageBytes)
                            <> ")"
                        | image <- images
                        ]
            if pasteImmediate
                then do
                    let promptText =
                            if Text.null pasteCaption
                                then "See attached image."
                                else pasteCaption
                    when (isMinimal context) $
                        putImagePreview
                            context.attachmentSession.sessionPreviewId
                            color
                            images
                    displayInfo context ("pasted " <> sizes) $
                        Text.putStrLn
                            (roleMuted color (glyphOk <> "pasted " <> sizes))
                    resetRenderPrintedText
                        context.attachmentSession.sessionRender
                    fullscreenEvent context (UiUserSubmitted promptText)
                    result <-
                        runOneTurn
                            context.attachmentSession
                            promptText
                            [ UserMultimodal
                                { userText = promptText
                                , userImages = images
                                }
                            ]
                    context.attachmentFinishTurn False result
                else do
                    queueAndReport context color False images
                    continue context

showAttachments :: AttachmentContext -> IO RunResult
showAttachments context = do
    pending <-
        readLiveAttachments
            context.attachmentSession.sessionConversation
    color <- resolveColor stdout
    let message =
            if null pending
                then "attachments: (none)"
                else "attachments: "
                    <> Text.intercalate ", "
                        [ image.imageMime
                            <> " ("
                            <> formatImageSize (BS.length image.imageBytes)
                            <> ")"
                        | image <- pending
                        ]
    displayInfo context message $
        Text.putStrLn (roleMuted color (glyphSession <> message))
    continue context

clearAttachments :: AttachmentContext -> IO RunResult
clearAttachments context = do
    modifyLiveAttachments
        context.attachmentSession.sessionConversation
        (\_ -> ([], ()))
    forM_ context.attachmentSession.sessionFullscreen \runtime ->
        setFullscreenImagePreviews runtime []
    color <- resolveColor stdout
    displayInfo context "attachments cleared" $
        Text.putStrLn
            (roleMuted color (glyphOk <> "attachments cleared"))
    continue context

queueAndReport
    :: AttachmentContext
    -> Bool
    -> Bool
    -> [ImageAttachment]
    -> IO ()
queueAndReport context color clearNotice images = do
    message <-
        queueAttachedImages
            context.attachmentSession.sessionConversation
            context.attachmentSession.sessionPreviewId
            color
            (isMinimal context)
            images
    syncFullscreenImagePreviews context
    when clearNotice $
        fullscreenEvent context (UiSetNotice Nothing)
    displayInfo context message $
        Text.putStrLn (roleMuted color (glyphOk <> message))

continue :: AttachmentContext -> IO RunResult
continue context =
    context.attachmentContinueWith ""

syncFullscreenImagePreviews :: AttachmentContext -> IO ()
syncFullscreenImagePreviews context =
    forM_ context.attachmentSession.sessionFullscreen \runtime ->
        readLiveAttachments
            context.attachmentSession.sessionConversation
            >>= setFullscreenImagePreviews runtime

fullscreenEvent :: AttachmentContext -> UiEvent -> IO ()
fullscreenEvent context event =
    forM_
        context.attachmentSession.sessionFullscreen
        (`emitUiEvent` event)

displayInfo :: AttachmentContext -> Text -> IO () -> IO ()
displayInfo context message minimalAction =
    case context.attachmentSession.sessionFullscreen of
        Nothing -> minimalAction
        Just runtime -> emitUiEvent runtime (UiSystemMessage message)

displayError :: AttachmentContext -> Text -> IO () -> IO ()
displayError context message minimalAction =
    case context.attachmentSession.sessionFullscreen of
        Nothing -> minimalAction
        Just runtime -> emitUiEvent runtime (UiErrorMessage message)

isMinimal :: AttachmentContext -> Bool
isMinimal context =
    case context.attachmentSession.sessionFullscreen of
        Nothing -> True
        Just _ -> False
