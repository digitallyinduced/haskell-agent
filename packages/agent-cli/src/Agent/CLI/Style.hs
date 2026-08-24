-- | Shared ANSI roles for TTY chrome and markdown. Callers pass a color
-- flag; when 'False' (pipes, 'NO_COLOR', tests) text is unchanged.
--
-- Semantic colors use the terminal's configurable ANSI palette, while
-- foreground and background remain at their terminal defaults. This lets
-- Ghostty and other terminal themes control both light/dark appearance and
-- the actual accent colors.
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
    , motionGlyphSet
    , spinnerFrames
    , ColorLevel(..)
    , detectColorLevel
    , adaptSgr
    , osc8Link
    , terminalCyan
    , terminalMagenta
    , terminalYellow
    , terminalRed
    , terminalGreen
    , terminalBlue
    , terminalViolet
    , terminalOrange
    , terminalMuted
    , cliWindowTitle
    , setCliWindowTitle
    ) where

import Agent.TUI.Motion
    ( MotionGlyphSet(..)
    , foregroundSpinnerFrames
    )
import Data.Colour (Colour)
import Data.Colour.SRGB (RGB(..), toSRGB24)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import System.Console.ANSI
    ( ConsoleIntensity(..)
    , ConsoleLayer(..)
    , SGR(..)
    , Color(..)
    , ColorIntensity(..)
    , hSetTitle
    )
import System.Console.ANSI.Codes (setSGRCode)
import System.Environment (lookupEnv)
import System.OsPath (OsPath)
import System.IO (Handle)
import System.IO.Unsafe (unsafePerformIO)

terminalCyan, terminalMagenta, terminalYellow, terminalRed :: SGR
terminalGreen, terminalBlue, terminalViolet, terminalOrange, terminalMuted :: SGR
terminalCyan = SetColor Foreground Dull Cyan
terminalMagenta = SetColor Foreground Dull Magenta
terminalYellow = SetColor Foreground Dull Yellow
terminalRed = SetColor Foreground Dull Red
terminalGreen = SetColor Foreground Dull Green
terminalBlue = SetColor Foreground Dull Blue
terminalViolet = SetColor Foreground Vivid Magenta
terminalOrange = SetColor Foreground Vivid Yellow
terminalMuted = SetColor Foreground Vivid Black

-- | Prompt background. Empty means the terminal theme's default background.
userBackground :: [SGR]
userBackground = []

-- | Assistant background. Empty means the terminal theme's default background.
agentBackground :: [SGR]
agentBackground = []

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

-- | Prefix each line with @bg@, then reset. Preserves a trailing newline on
-- @text@.
--
-- Do not use erase-to-end-of-line to extend the wash. Some terminals erase
-- with their configured default background rather than the active SGR
-- background, which leaves visible patches when that default differs from the
-- agent palette.
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
        <> Text.pack (setSGRCode [Reset])

rolePrompt :: Bool -> Text -> Text
rolePrompt color =
    styleBase color userBackground
        [ SetConsoleIntensity BoldIntensity
        , terminalCyan
        ]

roleToolArrow :: Bool -> Text -> Text
roleToolArrow color = style color [terminalMuted]

roleToolName :: Bool -> Text -> Text
roleToolName color =
    style color
        [ SetConsoleIntensity BoldIntensity
        , terminalMagenta
        ]

roleToolDetail :: Bool -> Text -> Text
roleToolDetail color = style color [terminalMuted]

-- | File paths on tool rows.
roleToolPath :: Bool -> Text -> Text
roleToolPath color = style color [terminalOrange]

-- | Shell / GHCi command text on tool rows.
roleToolCommand :: Bool -> Text -> Text
roleToolCommand color = style color [terminalYellow]

roleToolOutput :: Bool -> Text -> Text
roleToolOutput color = style color [terminalMuted]

roleThinking :: Bool -> Text -> Text
roleThinking color =
    style color
        [ terminalYellow
        ]

roleError :: Bool -> Text -> Text
roleError color =
    style color
        [ SetConsoleIntensity BoldIntensity
        , terminalRed
        ]

roleWarn :: Bool -> Text -> Text
roleWarn color =
    style color
        [ SetConsoleIntensity BoldIntensity
        , terminalYellow
        ]

roleMuted :: Bool -> Text -> Text
roleMuted color = style color [terminalMuted]

roleSuccess :: Bool -> Text -> Text
roleSuccess color =
    style color
        [ terminalGreen
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
spinnerFrames = foregroundSpinnerFrames motionGlyphSet

motionGlyphSet :: MotionGlyphSet
motionGlyphSet
    | supportsUnicodeChrome = MotionUnicode
    | otherwise = MotionAscii

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

-- | Window title: session name when known, otherwise a stable placeholder.
cliWindowTitle :: OsPath -> Maybe Text -> Text
cliWindowTitle _cwd sessionTitle =
    case sessionTitle of
        Just title
            | not (Text.null title)
            , title /= "untitled" -> title
        _ -> "New session"

-- | Set the terminal window title when @tty@ is 'True'; no-op otherwise.
setCliWindowTitle :: Bool -> Handle -> Text -> IO ()
setCliWindowTitle tty handle title
    | not tty = pure ()
    | otherwise = hSetTitle handle (Text.unpack title)
