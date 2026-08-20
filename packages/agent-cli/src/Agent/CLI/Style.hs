-- | Shared ANSI roles for TTY chrome and markdown. Callers pass a color
-- flag; when 'False' (pipes, 'NO_COLOR', tests) text is unchanged.
module Agent.CLI.Style
    ( style
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
import System.Console.ANSI.Codes (setSGRCode)
import System.FilePath (takeFileName)
import System.IO (Handle)

-- | Apply SGR attributes when @color@ is 'True'; otherwise return @text@.
style :: Bool -> [SGR] -> Text -> Text
style color attrs text
    | not color || Text.null text = text
    | otherwise =
        Text.pack (setSGRCode attrs) <> text <> Text.pack (setSGRCode [Reset])

rolePrompt :: Bool -> Text -> Text
rolePrompt color =
    style color
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
