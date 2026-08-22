-- | Shared ANSI roles for TTY chrome and markdown. Callers pass a color
-- flag; when 'False' (pipes, 'NO_COLOR', tests) text is unchanged.
--
-- Palette is Solarized Dark (Ethan Schoonover): truecolor RGB, not the
-- 256-color approximation, so washes match a Solarized terminal theme.
module Agent.CLI.Style
    ( style
    , styleBase
    , paintBackgroundLines
    , userBackground
    , agentBackground
    , beginBackground
    , endBackground
    , rolePrompt
    , roleToolArrow
    , roleToolName
    , roleToolDetail
    , roleToolPath
    , roleToolCommand
    , roleToolOutput
    , roleThinking
    , roleError
    , roleWarn
    , roleMuted
    , roleSuccess
    , glyphTool
    , glyphToolOut
    , glyphToolAccent
    , glyphOk
    , glyphErr
    , glyphWarn
    , glyphCancel
    , glyphSession
    , glyphThink
    , spinnerFrames
    , ColorLevel(..)
    , detectColorLevel
    , adaptSgr
    , osc8Link
    , solarizedCyan
    , solarizedMagenta
    , solarizedYellow
    , solarizedRed
    , solarizedGreen
    , solarizedBlue
    , solarizedViolet
    , solarizedOrange
    , solarizedBase01
    , cliWindowTitle
    , setCliWindowTitle
    ) where

import Agent.OsPath (toText)
import Data.Colour (Colour)
import Data.Colour.SRGB (RGB(..), sRGB24, toSRGB24)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import System.Console.ANSI
    ( ConsoleIntensity(..)
    , ConsoleLayer(..)
    , SGR(..)
    , hSetTitle
    )
import System.Console.ANSI.Codes
    ( clearFromCursorToLineEndCode
    , setSGRCode
    )
import System.Environment (lookupEnv)
import System.OsPath (OsPath, takeFileName)
import System.IO (Handle)
import System.IO.Unsafe (unsafePerformIO)

-- Solarized Dark (https://ethanschoonover.com/solarized/)
solarizedBase03, solarizedBase02, solarizedBase01 :: Colour Float
solarizedBase03 = sRGB24 0x00 0x2b 0x36
solarizedBase02 = sRGB24 0x07 0x36 0x42
solarizedBase01 = sRGB24 0x58 0x6e 0x75

solarizedYellow, solarizedOrange, solarizedRed :: Colour Float
solarizedYellow = sRGB24 0xb5 0x89 0x00
solarizedOrange = sRGB24 0xcb 0x4b 0x16
solarizedRed = sRGB24 0xdc 0x32 0x2f

solarizedMagenta, solarizedViolet, solarizedBlue :: Colour Float
solarizedMagenta = sRGB24 0xd3 0x36 0x82
solarizedViolet = sRGB24 0x6c 0x71 0xc4
solarizedBlue = sRGB24 0x26 0x8b 0xd2

solarizedCyan, solarizedGreen :: Colour Float
solarizedCyan = sRGB24 0x2a 0xa1 0x98
solarizedGreen = sRGB24 0x85 0x99 0x00

fg :: Colour Float -> SGR
fg = SetRGBColor Foreground

bg :: Colour Float -> SGR
bg = SetRGBColor Background

-- | Solarized base02 strip behind the REPL prompt and typed input.
userBackground :: [SGR]
userBackground = [bg solarizedBase02]

-- | Solarized base03 strip behind assistant stdout lines.
agentBackground :: [SGR]
agentBackground = [bg solarizedBase03]

-- | Apply SGR attributes when @color@ is 'True'; otherwise return @text@.
-- Ends with a full 'Reset'.
style :: Bool -> [SGR] -> Text -> Text
style color attrs = styleBase color [] attrs

-- | Like 'style', but after the span re-applies @base@ (e.g. a line
-- background) so nested chrome does not wipe the wash.
styleBase :: Bool -> [SGR] -> [SGR] -> Text -> Text
styleBase color base attrs text
    | not color || Text.null text = text
    | otherwise =
        Text.pack (setSGRCode (base <> attrs))
            <> text
            <> Text.pack (setSGRCode (Reset : base))

-- | Open a background wash and leave it active (for live typed input).
beginBackground :: Bool -> [SGR] -> Text
beginBackground color bgAttrs
    | not color || null bgAttrs = ""
    | otherwise = Text.pack (setSGRCode bgAttrs)

-- | Clear all SGR after a background wash.
endBackground :: Bool -> Text
endBackground color
    | not color = ""
    | otherwise = Text.pack (setSGRCode [Reset])

-- | Prefix each line with @bg@, extend the wash to the terminal edge via
-- clear-to-EOL, then reset. Preserves a trailing newline on @text@.
paintBackgroundLines :: Bool -> [SGR] -> Text -> Text
paintBackgroundLines color bgAttrs text
    | not color || null bgAttrs || Text.null text = text
    | otherwise =
        let endsWithNewline = Text.isSuffixOf "\n" text
            parts = Text.splitOn "\n" text
            lines_
                | endsWithNewline && not (null parts) = init parts
                | otherwise = parts
            painted = map (paintLine bgAttrs) lines_
        in Text.intercalate "\n" painted
            <> if endsWithNewline then "\n" else ""

paintLine :: [SGR] -> Text -> Text
paintLine bgAttrs line =
    Text.pack (setSGRCode bgAttrs)
        <> line
        <> Text.pack clearFromCursorToLineEndCode
        <> Text.pack (setSGRCode [Reset])

rolePrompt :: Bool -> Text -> Text
rolePrompt color =
    styleBase color userBackground
        [ SetConsoleIntensity BoldIntensity
        , fg solarizedCyan
        ]

roleToolArrow :: Bool -> Text -> Text
roleToolArrow color = style color [fg solarizedBase01]

roleToolName :: Bool -> Text -> Text
roleToolName color =
    style color
        [ SetConsoleIntensity BoldIntensity
        , fg solarizedMagenta
        ]

roleToolDetail :: Bool -> Text -> Text
roleToolDetail color = style color [fg solarizedBase01]

-- | File paths on tool rows (Solarized orange).
roleToolPath :: Bool -> Text -> Text
roleToolPath color = style color [fg solarizedOrange]

-- | Shell / GHCi command text on tool rows (Solarized yellow).
roleToolCommand :: Bool -> Text -> Text
roleToolCommand color = style color [fg solarizedYellow]

roleToolOutput :: Bool -> Text -> Text
roleToolOutput color = style color [fg solarizedBase01]

roleThinking :: Bool -> Text -> Text
roleThinking color =
    style color
        [ fg solarizedYellow
        ]

roleError :: Bool -> Text -> Text
roleError color =
    style color
        [ SetConsoleIntensity BoldIntensity
        , fg solarizedRed
        ]

roleWarn :: Bool -> Text -> Text
roleWarn color =
    style color
        [ SetConsoleIntensity BoldIntensity
        , fg solarizedYellow
        ]

roleMuted :: Bool -> Text -> Text
roleMuted color = style color [fg solarizedBase01]

roleSuccess :: Bool -> Text -> Text
roleSuccess color =
    style color
        [ fg solarizedGreen
        ]

supportsUnicodeChrome :: Bool
supportsUnicodeChrome = unsafePerformIO do
    lang <- lookupEnv "LANG"
    lc <- lookupEnv "LC_ALL"
    lcctype <- lookupEnv "LC_CTYPE"
    let hay = mconcat [lang, lc, lcctype]
    pure $ case hay of
        Nothing -> True
        Just s ->
            any (`Text.isInfixOf` Text.toLower (Text.pack s))
                ["utf-8", "utf8"]
{-# NOINLINE supportsUnicodeChrome #-}

pickGlyph :: Text -> Text -> Text
pickGlyph fancy ascii
    | supportsUnicodeChrome = fancy
    | otherwise = ascii

glyphTool, glyphToolOut, glyphToolAccent, glyphOk, glyphErr :: Text
glyphWarn, glyphCancel, glyphSession, glyphThink :: Text
glyphTool = pickGlyph "◆ " "* "
glyphToolOut = pickGlyph "┊ " "| "
glyphToolAccent = pickGlyph "❙ " "| "
glyphOk = pickGlyph "✓ " "+ "
glyphErr = pickGlyph "✗ " "x "
glyphWarn = pickGlyph "⚠ " "! "
glyphCancel = pickGlyph "⊘ " "o "
glyphSession = pickGlyph "⧉ " "# "
glyphThink = pickGlyph "◆ " "* "

spinnerFrames :: [Text]
spinnerFrames
    | supportsUnicodeChrome =
        ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]
    | otherwise =
        ["|", "/", "-", "\\"]

data ColorLevel = ColorNone | ColorBasic | Color256 | ColorTrue
    deriving (Eq, Ord, Show)

detectColorLevel :: IO ColorLevel
detectColorLevel = do
    noColor <- lookupEnv "NO_COLOR"
    case noColor of
        Just _ -> pure ColorNone
        Nothing -> do
            colorterm <- lookupEnv "COLORTERM"
            term <- lookupEnv "TERM"
            pure $ case fmap Text.toLower (Text.pack <$> colorterm) of
                Just ct | ct `elem` ["truecolor", "24bit"] -> ColorTrue
                _ -> case Text.pack <$> term of
                    Just t
                        | "256color" `Text.isInfixOf` t -> Color256
                        | "color" `Text.isInfixOf` t -> ColorBasic
                        | otherwise -> ColorBasic
                    Nothing -> ColorTrue

adaptSgr :: ColorLevel -> [SGR] -> [SGR]
adaptSgr ColorTrue attrs = attrs
adaptSgr ColorNone _ = []
adaptSgr _ attrs = map adaptOne attrs

adaptOne :: SGR -> SGR
adaptOne = \case
    SetRGBColor layer colour -> SetPaletteColor layer (rgbToXterm256 colour)
    other -> other

rgbToXterm256 :: Colour Float -> Word8
rgbToXterm256 colour =
    let RGB r g b = toSRGB24 colour
        mx = maximum [r, g, b]
        mn = minimum [r, g, b]
    in if fromIntegral (mx - mn) < (24 :: Double)
        then grayIndex (fromIntegral r)
        else cubeIndex r g b
  where
    cubeIndex r g b =
        let q x = min (5 :: Word8) (x `div` 51)
        in 16 + 36 * q r + 6 * q g + q b
    grayIndex :: Double -> Word8
    grayIndex avg =
        let idx = round ((avg - 8) / 10) :: Int
        in fromIntegral (max 0 (min 23 idx) + 232)

osc8Link :: Bool -> Text -> Text -> Text
osc8Link color url label
    | not color || Text.null url = label
    | otherwise =
        "\ESC]8;;" <> url <> "\ESC\\" <> label <> "\ESC]8;;\ESC\\"

-- | Window title: session name when known, otherwise the working directory.
cliWindowTitle :: OsPath -> Maybe Text -> Text
cliWindowTitle cwd sessionTitle =
    case sessionTitle of
        Just title
            | not (Text.null title)
            , title /= "untitled" -> title
        _ -> toText (takeFileName cwd)

-- | Set the terminal window title when @tty@ is 'True'; no-op otherwise.
setCliWindowTitle :: Bool -> Handle -> Text -> IO ()
setCliWindowTitle tty handle title
    | not tty = pure ()
    | otherwise = hSetTitle handle (Text.unpack title)
