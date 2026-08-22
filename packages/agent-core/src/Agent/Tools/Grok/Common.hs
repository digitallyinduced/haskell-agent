module Agent.Tools.Grok.Common
    ( isGitIgnored
    , jsonTool
    , optionalTimeout
    , stripAnsi
    ) where

import Agent.ToolArgs (optIntOrString)
import Agent.OsPath (unsafeToFilePath)
import Agent.Tools.Types
    ( jsonTool
    )
import Data.Aeson (Object)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import System.OsPath (OsPath)

isGitIgnored :: OsPath -> OsPath -> IO Bool
isGitIgnored cwd path = findExecutable "git" >>= \case
    Nothing -> pure False
    Just git -> do
        (code, _, _) <- readProcessWithExitCode git
            ["-C", unsafeToFilePath cwd, "check-ignore", "-q", "--", unsafeToFilePath path] ""
        pure (code == ExitSuccess)

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
