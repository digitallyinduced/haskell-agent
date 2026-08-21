-- | Terminal detection and escape-sequence integrations.
module Agent.CLI.Terminal
    ( TerminalKind(..)
    , TerminalCapabilities(..)
    , detectTerminalCapabilities
    , formatTerminalCapabilities
    , resolveColor
    , osc7WorkingDirectory
    , fileUri
    , osc9Notification
    , osc52Clipboard
    , osc133PromptStart
    , osc133PromptEnd
    , osc133CommandStart
    , osc133CommandFinished
    , synchronizedOutputBegin
    , synchronizedOutputEnd
    , shiftEnterCsiBodies
    , kittyKeyboardDisambiguatePush
    , kittyKeyboardPush
    , kittyKeyboardPop
    , wrapTerminalPassthrough
    , emitTerminalSequence
    , notifyTerminal
    , copyTerminalClipboard
    , reportTerminalCwd
    , withSynchronizedOutput
    ) where

import Control.Exception.Safe (bracket_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.Char (isAlphaNum, isAscii, toLower)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as Text
import Numeric (showHex)
import System.Environment (lookupEnv)
import System.IO (Handle, hFlush, hIsTerminalDevice)

data TerminalKind
    = TerminalGhostty
    | TerminalKitty
    | TerminalWezTerm
    | TerminalITerm
    | TerminalWindows
    | TerminalOther
    deriving (Eq, Show)

data TerminalCapabilities = TerminalCapabilities
    { terminalKind :: !TerminalKind
    , terminalIsTty :: !Bool
    , terminalInsideTmux :: !Bool
    , terminalInlineImages :: !Bool
    , terminalNativeProgress :: !Bool
    , terminalNotifications :: !Bool
    , terminalSemanticPrompts :: !Bool
    , terminalOsc52Clipboard :: !Bool
    , terminalSynchronizedOutput :: !Bool
    , terminalKittyKeyboard :: !Bool
    }
    deriving (Eq, Show)

detectTerminalCapabilities :: Handle -> IO TerminalCapabilities
detectTerminalCapabilities handle = do
    tty <- hIsTerminalDevice handle
    program <- fmap (map toLower . fromMaybe "") (lookupEnv "TERM_PROGRAM")
    term <- fmap (map toLower . fromMaybe "") (lookupEnv "TERM")
    kittyWindow <- lookupEnv "KITTY_WINDOW_ID"
    itermSession <- lookupEnv "ITERM_SESSION_ID"
    wtSession <- lookupEnv "WT_SESSION"
    tmux <- isJust <$> lookupEnv "TMUX"
    let has needle haystack = Text.pack needle `Text.isInfixOf` Text.pack haystack
        kind
            | program == "ghostty" || has "ghostty" term = TerminalGhostty
            | isJust kittyWindow || program == "kitty" || has "kitty" term =
                TerminalKitty
            | program `elem` ["wezterm", "wezterm.app"] || has "wezterm" term =
                TerminalWezTerm
            | isJust itermSession || program `elem` ["iterm.app", "iterm2"] =
                TerminalITerm
            | isJust wtSession || program == "windows_terminal" =
                TerminalWindows
            | otherwise = TerminalOther
        oneOf kinds = tty && kind `elem` kinds
    pure TerminalCapabilities
        { terminalKind = kind
        , terminalIsTty = tty
        , terminalInsideTmux = tmux
        , terminalInlineImages =
            oneOf [TerminalGhostty, TerminalKitty, TerminalWezTerm, TerminalITerm]
        , terminalNativeProgress = oneOf [TerminalGhostty, TerminalWindows]
        , terminalNotifications = oneOf [TerminalGhostty, TerminalITerm]
        , terminalSemanticPrompts = oneOf [TerminalGhostty, TerminalITerm]
        , terminalOsc52Clipboard = tty
        , terminalSynchronizedOutput =
            oneOf [TerminalGhostty, TerminalKitty, TerminalWezTerm]
        , terminalKittyKeyboard =
            not tmux && oneOf [TerminalGhostty, TerminalKitty, TerminalWezTerm]
        }

formatTerminalCapabilities :: TerminalCapabilities -> Text
formatTerminalCapabilities capabilities =
    Text.intercalate "\n"
        [ "terminal: " <> terminalKindName capabilities.terminalKind
        , "inline-images: " <> yesNo capabilities.terminalInlineImages
        , "native-progress: " <> yesNo capabilities.terminalNativeProgress
        , "notifications: " <> yesNo capabilities.terminalNotifications
        , "semantic-prompts: " <> yesNo capabilities.terminalSemanticPrompts
        , "osc52-clipboard: " <> yesNo capabilities.terminalOsc52Clipboard
        , "synchronized-output: "
            <> yesNo capabilities.terminalSynchronizedOutput
        , "kitty-keyboard: " <> yesNo capabilities.terminalKittyKeyboard
        , "tmux-passthrough: " <> yesNo capabilities.terminalInsideTmux
        ]
  where
    yesNo True = "yes"
    yesNo False = "no"
    terminalKindName = \case
        TerminalGhostty -> "ghostty"
        TerminalKitty -> "kitty"
        TerminalWezTerm -> "wezterm"
        TerminalITerm -> "iterm"
        TerminalWindows -> "windows-terminal"
        TerminalOther -> "other"

-- | Color when the handle is a TTY and @NO_COLOR@ is unset.
resolveColor :: Handle -> IO Bool
resolveColor handle = do
    isTty <- hIsTerminalDevice handle
    noColor <- lookupEnv "NO_COLOR"
    pure (isTty && maybe True (const False) noColor)

osc7WorkingDirectory :: FilePath -> Text
osc7WorkingDirectory path =
    "\ESC]7;" <> fileUri path <> "\ESC\\"

fileUri :: FilePath -> Text
fileUri path = "file://" <> percentEncodePath (Text.pack path)

osc9Notification :: Text -> Text
osc9Notification title =
    "\ESC]9;" <> sanitizeOsc title <> "\ESC\\"

osc52Clipboard :: Text -> Text
osc52Clipboard payload =
    "\ESC]52;c;"
        <> TextEncoding.decodeLatin1
            (Base64.encode (TextEncoding.encodeUtf8 payload))
        <> "\ESC\\"

osc133PromptStart, osc133PromptEnd, osc133CommandStart :: Text
osc133PromptStart = "\ESC]133;A\ESC\\"
osc133PromptEnd = "\ESC]133;B\ESC\\"
osc133CommandStart = "\ESC]133;C\ESC\\"

osc133CommandFinished :: Maybe Int -> Text
osc133CommandFinished exitCode =
    "\ESC]133;D"
        <> maybe "" (\code -> ";" <> Text.pack (show code)) exitCode
        <> "\ESC\\"

synchronizedOutputBegin, synchronizedOutputEnd :: Text
synchronizedOutputBegin = "\ESC[?2026h"
synchronizedOutputEnd = "\ESC[?2026l"

-- | CSI bodies used by enhanced-keyboard protocols for Shift+Enter.
--
-- Xterm's modifyOtherKeys protocol uses @CSI 27;2;13~@, while Kitty's
-- keyboard protocol (also implemented by Ghostty and WezTerm) uses
-- @CSI 13;2u@.
shiftEnterCsiBodies :: [String]
shiftEnterCsiBodies =
    [ "27;2;13~"
    , "13;2u"
    ]

-- | Push only Kitty's unambiguous-key flag. This is enough for an inline
-- editor to receive modified keys such as Cmd+V without also receiving key
-- release events.
kittyKeyboardDisambiguatePush :: Text
kittyKeyboardDisambiguatePush = "\ESC[>1u"

-- | Push Kitty keyboard flags 1 and 2. Pop before returning to legacy input.
kittyKeyboardPush, kittyKeyboardPop :: Text
kittyKeyboardPush = "\ESC[>3u"
kittyKeyboardPop = "\ESC[<u"

wrapTerminalPassthrough :: Bool -> Text -> Text
wrapTerminalPassthrough inTmux payload
    | not inTmux || Text.null payload = payload
    | otherwise =
        "\ESCPtmux;"
            <> Text.replace "\ESC" "\ESC\ESC" payload
            <> "\ESC\\"

emitTerminalSequence :: TerminalCapabilities -> Handle -> Text -> IO ()
emitTerminalSequence capabilities handle payload
    | not capabilities.terminalIsTty || Text.null payload = pure ()
    | otherwise = do
        Text.hPutStr handle
            (wrapTerminalPassthrough capabilities.terminalInsideTmux payload)
        hFlush handle

notifyTerminal :: TerminalCapabilities -> Handle -> Text -> IO ()
notifyTerminal capabilities handle title
    | capabilities.terminalNotifications =
        emitTerminalSequence capabilities handle (osc9Notification title)
    | otherwise = pure ()

copyTerminalClipboard :: TerminalCapabilities -> Handle -> Text -> IO Bool
copyTerminalClipboard capabilities handle payload
    | capabilities.terminalOsc52Clipboard = do
        emitTerminalSequence capabilities handle (osc52Clipboard payload)
        pure True
    | otherwise = pure False

reportTerminalCwd :: TerminalCapabilities -> Handle -> FilePath -> IO ()
reportTerminalCwd capabilities handle cwd =
    emitTerminalSequence capabilities handle (osc7WorkingDirectory cwd)

withSynchronizedOutput :: TerminalCapabilities -> Handle -> IO a -> IO a
withSynchronizedOutput capabilities handle action
    | not capabilities.terminalSynchronizedOutput = action
    | otherwise =
        bracket_
            (emitTerminalSequence capabilities handle synchronizedOutputBegin)
            (emitTerminalSequence capabilities handle synchronizedOutputEnd)
            action

sanitizeOsc :: Text -> Text
sanitizeOsc =
    Text.filter (\char -> char /= '\ESC' && char /= '\BEL')
        . Text.replace "\n" " "
        . Text.replace "\r" " "

percentEncodePath :: Text -> Text
percentEncodePath = Text.concatMap encodeChar
  where
    encodeChar char
        | (isAscii char && isAlphaNum char)
            || char `elem` ("/-._~:" :: String) = Text.singleton char
        | otherwise =
            Text.concat
                [ "%" <> hexByte byte
                | byte <- BS.unpack (TextEncoding.encodeUtf8 (Text.singleton char))
                ]
    hexByte byte =
        let raw = map upperHex (showHex byte "")
        in Text.pack (if length raw == 1 then '0' : raw else raw)
    upperHex c
        | c >= 'a' && c <= 'f' = toEnum (fromEnum c - 32)
        | otherwise = c
