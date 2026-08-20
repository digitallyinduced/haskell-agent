-- | Shared ANSI roles for TTY chrome and markdown. Callers pass a color
-- flag; when 'False' (pipes, 'NO_COLOR', tests) text is unchanged.
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
    , roleToolOutput
    , roleThinking
    , roleError
    , roleWarn
    , roleMuted
    , roleSuccess
    , cliWindowTitle
    , setCliWindowTitle
    ) where

import Data.Text (Text)
import qualified Data.Text as Text
import System.Console.ANSI
    ( Color(..)
    , ColorIntensity(..)
    , ConsoleIntensity(..)
    , ConsoleLayer(..)
    , SGR(..)
    , hSetTitle
    )
import System.Console.ANSI.Codes
    ( clearFromCursorToLineEndCode
    , setSGRCode
    )
import System.FilePath (takeFileName)
import System.IO (Handle)

-- | Deep navy strip behind the REPL prompt and typed input.
userBackground :: [SGR]
userBackground = [SetPaletteColor Background 17]

-- | Charcoal strip behind assistant stdout lines.
agentBackground :: [SGR]
agentBackground = [SetPaletteColor Background 236]

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
beginBackground color bg
    | not color || null bg = ""
    | otherwise = Text.pack (setSGRCode bg)

-- | Clear all SGR after a background wash.
endBackground :: Bool -> Text
endBackground color
    | not color = ""
    | otherwise = Text.pack (setSGRCode [Reset])

-- | Prefix each line with @bg@, extend the wash to the terminal edge via
-- clear-to-EOL, then reset. Preserves a trailing newline on @text@.
paintBackgroundLines :: Bool -> [SGR] -> Text -> Text
paintBackgroundLines color bg text
    | not color || null bg || Text.null text = text
    | otherwise =
        let endsWithNewline = Text.isSuffixOf "\n" text
            parts = Text.splitOn "\n" text
            lines_
                | endsWithNewline && not (null parts) = init parts
                | otherwise = parts
            painted = map (paintLine bg) lines_
        in Text.intercalate "\n" painted
            <> if endsWithNewline then "\n" else ""

paintLine :: [SGR] -> Text -> Text
paintLine bg line =
    Text.pack (setSGRCode bg)
        <> line
        <> Text.pack clearFromCursorToLineEndCode
        <> Text.pack (setSGRCode [Reset])

rolePrompt :: Bool -> Text -> Text
rolePrompt color =
    styleBase color userBackground
        [ SetConsoleIntensity BoldIntensity
        , SetColor Foreground Dull Cyan
        ]

roleToolArrow :: Bool -> Text -> Text
roleToolArrow color = style color [SetConsoleIntensity FaintIntensity]

roleToolName :: Bool -> Text -> Text
roleToolName color =
    style color
        [ SetConsoleIntensity BoldIntensity
        , SetColor Foreground Dull Magenta
        ]

roleToolDetail :: Bool -> Text -> Text
roleToolDetail color = style color [SetConsoleIntensity FaintIntensity]

roleToolOutput :: Bool -> Text -> Text
roleToolOutput color = style color [SetConsoleIntensity FaintIntensity]

roleThinking :: Bool -> Text -> Text
roleThinking color =
    style color
        [ SetConsoleIntensity FaintIntensity
        , SetColor Foreground Dull Yellow
        ]

roleError :: Bool -> Text -> Text
roleError color =
    style color
        [ SetConsoleIntensity BoldIntensity
        , SetColor Foreground Vivid Red
        ]

roleWarn :: Bool -> Text -> Text
roleWarn color =
    style color
        [ SetConsoleIntensity BoldIntensity
        , SetColor Foreground Dull Yellow
        ]

roleMuted :: Bool -> Text -> Text
roleMuted color = style color [SetConsoleIntensity FaintIntensity]

roleSuccess :: Bool -> Text -> Text
roleSuccess color =
    style color
        [ SetColor Foreground Dull Green
        ]

-- | Window title: session name when known, otherwise the working directory.
cliWindowTitle :: FilePath -> Maybe Text -> Text
cliWindowTitle cwd sessionTitle =
    case sessionTitle of
        Just title
            | not (Text.null title)
            , title /= "untitled" -> title
        _ -> Text.pack (takeFileName cwd)

-- | Set the terminal window title when @tty@ is 'True'; no-op otherwise.
setCliWindowTitle :: Bool -> Handle -> Text -> IO ()
setCliWindowTitle tty handle title
    | not tty = pure ()
    | otherwise = hSetTitle handle (Text.unpack title)
