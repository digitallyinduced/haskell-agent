module Agent.GrokBuild.Dialect.Common
    ( jsonTool
    , optionalTimeout
    , stripAnsi
    ) where

import Agent.ToolArgs (optIntOrString)
import Agent.Tools.Types
    ( jsonTool
    )
import Data.Aeson (Object)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as Text

optionalTimeout :: Object -> Parser (Maybe Int)
optionalTimeout object =
    optIntOrString object "timeout"

stripAnsi :: Text -> Text
stripAnsi = Text.concat . go
  where
    go text = case Text.break (== '\ESC') text of
        (before, rest)
            | Text.null rest -> [before]
            | otherwise ->
                let afterEsc = Text.drop 1 rest
                in case Text.uncons afterEsc of
                    Just ('[', csi) ->
                        let dropped = Text.dropWhile (not . isCsiFinal) csi
                            rest' = if Text.null dropped then "" else Text.drop 1 dropped
                        in before : go rest'
                    _ -> before : "\ESC" : go afterEsc

    isCsiFinal c = c >= '@' && c <= '~'
