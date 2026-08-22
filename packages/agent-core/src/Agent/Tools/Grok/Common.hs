module Agent.Tools.Grok.Common
    ( isGitIgnored
    , jsonTool
    , optionalTimeout
    , stripAnsi
    ) where

import Agent.ToolArgs (optInt, optText)
import Agent.ToolDSL (PropertySchema)
import Agent.ToolDispatch (ToolHandler)
import Agent.Tools.Types
    ( AppTool
    , ApprovalRule(..)
    , jsonAppTool
    )
import Control.Applicative ((<|>))
import Data.Aeson (Object)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Exception.Safe (impureThrow)
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import System.OsPath (OsPath, decodeUtf)

jsonTool
    :: Text
    -> Text
    -> [PropertySchema]
    -> Bool
    -> ToolHandler
    -> AppTool
jsonTool name description parameters readOnly =
    jsonAppTool name description parameters
        (if readOnly then AlwaysReadOnly else AlwaysPrompt)

isGitIgnored :: OsPath -> OsPath -> IO Bool
isGitIgnored cwd path = findExecutable "git" >>= \case
    Nothing -> pure False
    Just git -> do
        (code, _, _) <- readProcessWithExitCode git
            ["-C", decodeUtfPath cwd, "check-ignore", "-q", "--", decodeUtfPath path] ""
        pure (code == ExitSuccess)

optionalTimeout :: Object -> Parser (Maybe Int)
optionalTimeout object = do
    fromInt <- optInt object "timeout"
    fromText <- optText object "timeout"
    pure $ fromInt <|> (fromText >>= readTimeout)

readTimeout :: Text -> Maybe Int
readTimeout text =
    case reads (Text.unpack text) of
        [(n, "")] -> Just n
        _ -> Nothing

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

decodeUtfPath :: OsPath -> FilePath
decodeUtfPath = either impureThrow id . decodeUtf
