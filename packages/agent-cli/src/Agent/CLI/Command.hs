-- | Interactive REPL slash commands.
module Agent.CLI.Command
    ( ReplAction(..)
    , currentEffort
    , parseReplLine
    , setReasoningEffort
    ) where

import Agent.CLI.Options (parseEffort)
import Agent.OpenAI.Responses.Types
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

data ReplAction
    = ReplQuit
    | ReplPrompt Text
    | ReplShowEffort
    | ReplSetEffort Text
    | ReplCommandError Text
    deriving (Eq, Show)

parseReplLine :: Text -> ReplAction
parseReplLine raw =
    let line = Text.strip raw
    in if line == ":q" || line == ":quit"
        then ReplQuit
        else case Text.uncons line of
            Just ('/', _) -> parseSlash line
            _ -> ReplPrompt line

parseSlash :: Text -> ReplAction
parseSlash line = case Text.words line of
    [] -> ReplCommandError "unknown command: /"
    command : args
        | Text.toLower command == "/effort" -> parseEffortCommand args
        | otherwise -> ReplCommandError ("unknown command: " <> command)

parseEffortCommand :: [Text] -> ReplAction
parseEffortCommand = \case
    [] -> ReplShowEffort
    [level] -> case parseEffort level of
        Right effort -> ReplSetEffort effort
        Left err -> ReplCommandError (Text.pack err)
    _ -> ReplCommandError "usage: /effort [low|medium|high|xhigh]"

-- | Rebuild from the constructor: 'input' is also a field on 'CustomToolCall'.
setReasoningEffort :: Text -> ResponseCreateParams -> ResponseCreateParams
setReasoningEffort level ResponseCreateParams{..} =
    ResponseCreateParams
        { reasoning = Just updated
        , ..
        }
  where
    updated = case reasoning of
        Just ReasoningConfig{..} -> ReasoningConfig { effort = Just level, .. }
        Nothing -> ReasoningConfig
            { context = Nothing
            , effort = Just level
            , generateSummary = Nothing
            , reasoningMode = Nothing
            , summary = Nothing
            , extraFields = KeyMap.empty
            }

currentEffort :: ResponseCreateParams -> Text
currentEffort params =
    fromMaybe "low" (params.reasoning >>= (.effort))
