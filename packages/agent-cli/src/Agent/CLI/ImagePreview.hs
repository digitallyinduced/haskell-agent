-- | In-terminal preview of clipboard images (Kitty graphics + iTerm2 OSC 1337).
--
-- Grok Build draws a thumbnail above the prompt when you paste an image.
-- We follow the same two protocols: Kitty/Ghostty/WezTerm via APC @_G@, and
-- iTerm2 via OSC 1337. tmux needs DCS passthrough. Unsupported terminals
-- get a muted text fallback so the attachment still has a visible chip.
module Agent.CLI.ImagePreview
    ( ImagePreviewProtocol(..)
    , detectImagePreviewProtocol
    , parseImagePreviewProtocol
    , kittyImageSequence
    , itermImageSequence
    , wrapTmuxPassthrough
    , imagePreviewPayload
    , previewRowsFor
    , previewColumnsFor
    , renderImagePreview
    ) where

import Agent.Loop (ImageAttachment(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Environment (lookupEnv)
import System.IO (Handle, hIsTerminalDevice)

-- | How (if at all) this terminal can draw inline images.
data ImagePreviewProtocol
    = PreviewKitty
    | PreviewITerm
    | PreviewUnsupported
    deriving (Eq, Show)

-- | Kitty/Ghostty/WezTerm via @TERM@ / @TERM_PROGRAM@ / @KITTY_WINDOW_ID@.
-- iTerm2 via @ITERM_SESSION_ID@ / @TERM_PROGRAM=iTerm.app@.
parseImagePreviewProtocol
    :: Maybe String
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> ImagePreviewProtocol
parseImagePreviewProtocol mTerm mTermProgram mKittyWindow mItermSession
    | looksLikeKitty = PreviewKitty
    | looksLikeITerm = PreviewITerm
    | otherwise = PreviewUnsupported
  where
    term = map toLower (fromMaybeEmpty mTerm)
    program = map toLower (fromMaybeEmpty mTermProgram)
    looksLikeKitty =
        not (null (fromMaybeEmpty mKittyWindow))
            || "kitty" `elem` termWords
            || program `elem` ["kitty", "ghostty", "wezterm", "wezterm.app"]
            || "ghostty" `elem` termWords
            || "wezterm" `elem` termWords
    looksLikeITerm =
        not (null (fromMaybeEmpty mItermSession))
            || program `elem` ["iterm.app", "iterm2"]
            || "iterm" `elem` termWords
    termWords = words (map (\c -> if c == '-' then ' ' else c) (term <> " " <> program))

fromMaybeEmpty :: Maybe String -> String
fromMaybeEmpty = fromMaybe ""

-- | Inspect the process environment. Non-TTY handles report Unsupported.
detectImagePreviewProtocol :: Handle -> IO ImagePreviewProtocol
detectImagePreviewProtocol handle = do
    tty <- hIsTerminalDevice handle
    if not tty
        then pure PreviewUnsupported
        else do
            mTerm <- lookupEnv "TERM"
            mProgram <- lookupEnv "TERM_PROGRAM"
            mKitty <- lookupEnv "KITTY_WINDOW_ID"
            mIterm <- lookupEnv "ITERM_SESSION_ID"
            pure (parseImagePreviewProtocol mTerm mProgram mKitty mIterm)

-- | Kitty graphics transmit-and-display. @f=100@ is PNG; Kitty has no JPEG
-- format code (24/32 are raw RGB/RGBA). We always send 100 and let the
-- terminal sniff, matching Grok Build's overlay path for PNG screenshots.
--
-- Use the combined @a=T@ action rather than uploading with @a=t@ and creating
-- a separate @a=p@ placement. Besides saving a round trip, this keeps PNG
-- alpha on the terminal's normal transmit-and-display path (notably Ghostty).
--
-- Only @r@ is specified for the placement. Per the Kitty graphics protocol,
-- omitting @c@ makes the terminal derive the width from the source image and
-- cell dimensions, preserving the original aspect ratio. Supplying both
-- dimensions would stretch every image into the same cell rectangle.
--
-- Chunked at 4096 encoded bytes (Kitty's documented payload limit).
kittyImageSequence :: Int -> Int -> Int -> Text -> ByteString -> Text
kittyImageSequence imageId _columns rows mime bytes =
    let fmt = kittyFormat mime
        chunks = chunkBytes 4096 (Base64.encode bytes)
        total = length chunks
        transmit n chunk =
            let more = if n + 1 < total then 1 else 0 :: Int
                action = if n == 0 then "T" else "t"
                extras
                    | n == 0 =
                        ",f="
                            <> Text.pack (show fmt)
                            <> ",t=d,r="
                            <> Text.pack (show rows)
                            <> ",C=1"
                    | otherwise = ""
            in "\ESC_Ga="
                <> action
                <> ",q=2,i="
                <> Text.pack (show imageId)
                <> extras
                <> ",m="
                <> Text.pack (show more)
                <> ";"
                <> TextEncoding.decodeLatin1 chunk
                <> "\ESC\\"
    in Text.concat (zipWith transmit [0 ..] chunks)

kittyFormat :: Text -> Int
kittyFormat _ = 100

-- | iTerm2 / WezTerm OSC 1337 inline file. @preserveAspectRatio@ keeps the
-- thumbnail from stretching when the cell grid is not square.
itermImageSequence :: Int -> Int -> ByteString -> Text
itermImageSequence columns rows bytes =
    "\ESC]1337;File=inline=1;width="
        <> Text.pack (show columns)
        <> ";height="
        <> Text.pack (show rows)
        <> ";preserveAspectRatio=1:"
        <> TextEncoding.decodeLatin1 (Base64.encode bytes)
        <> "\BEL"

-- | tmux DCS passthrough so Kitty/iTerm sequences reach the outer emulator.
-- @ST@ must be @ESC \\@ (not BEL) so tmux does not swallow the payload.
wrapTmuxPassthrough :: Text -> Text
wrapTmuxPassthrough payload
    | Text.null payload = payload
    | otherwise =
        "\ESCPtmux;"
            <> Text.replace "\ESC" "\ESC\ESC" payload
            <> "\ESC\\"

-- | Protocol sequence for one image, already wrapped for tmux when needed.
imagePreviewPayload
    :: ImagePreviewProtocol
    -> Bool
    -> Int
    -> Int
    -> Int
    -> ImageAttachment
    -> Text
imagePreviewPayload protocol inTmux imageId columns rows ImageAttachment{imageMime, imageBytes} =
    let raw = case protocol of
            PreviewKitty ->
                kittyImageSequence imageId columns rows imageMime imageBytes
            PreviewITerm ->
                itermImageSequence columns rows imageBytes
            PreviewUnsupported ->
                mempty
        wrapped
            | inTmux && not (Text.null raw) = wrapTmuxPassthrough raw
            | otherwise = raw
    in wrapped

-- | Cap the thumbnail so a pasted screenshot does not eat the whole pane.
previewRowsFor :: Int -> Int
previewRowsFor terminalRows =
    max 4 (min 12 (max 1 (terminalRows `div` 4)))

previewColumnsFor :: Int -> Int
previewColumnsFor terminalColumns =
    max 12 (min 40 (max 1 (terminalColumns `div` 3)))

-- | Full preview block: graphics (when supported) plus a muted caption.
-- Returns @Nothing@ when there is nothing to draw (empty list).
renderImagePreview
    :: ImagePreviewProtocol
    -> Bool
    -- ^ @True@ when running inside tmux.
    -> (Text -> Text)
    -- ^ Muted styler, e.g. @roleMuted color@.
    -> Int
    -> Int
    -> Int
    -- ^ Starting Kitty image id (unique per paste so overlays do not clash).
    -> [ImageAttachment]
    -> Maybe Text
renderImagePreview protocol inTmux muted columns rows startId images =
    case images of
        [] -> Nothing
        _ ->
            Just $ Text.intercalate "\n" $
                zipWith (previewOne protocol inTmux muted columns rows) [startId ..] images

previewOne
    :: ImagePreviewProtocol
    -> Bool
    -> (Text -> Text)
    -> Int
    -> Int
    -> Int
    -> ImageAttachment
    -> Text
previewOne protocol inTmux muted columns rows imageId img =
    let graphics = imagePreviewPayload protocol inTmux imageId columns rows img
        caption = muted $
            "🖼  "
                <> img.imageMime
                <> " ("
                <> formatBytes (BS.length img.imageBytes)
                <> ")"
                <> case protocol of
                    PreviewUnsupported ->
                        " — inline preview needs Kitty, Ghostty, WezTerm, or iTerm2"
                    _ -> ""
        spacer =
            -- Kitty with @C=1@ does not advance the cursor, so leave @rows@
            -- blank lines for the bitmap. iTerm OSC 1337 already moves the
            -- cursor below the image.
            case protocol of
                PreviewKitty -> Text.replicate rows "\n"
                _ -> ""
    in if Text.null graphics
        then caption
        else graphics <> spacer <> caption

formatBytes :: Int -> Text
formatBytes n
    | n < 1024 = Text.pack (show n) <> " B"
    | n < 1024 * 1024 =
        Text.pack (show (n `div` 1024)) <> " KB"
    | otherwise =
        Text.pack (show (n `div` (1024 * 1024))) <> " MB"

chunkBytes :: Int -> ByteString -> [ByteString]
chunkBytes n bs
    | BS.null bs = [BS.empty]
    | otherwise = go bs
  where
    go rest
        | BS.null rest = []
        | otherwise =
            let (chunk, more) = BS.splitAt n rest
            in chunk : go more
