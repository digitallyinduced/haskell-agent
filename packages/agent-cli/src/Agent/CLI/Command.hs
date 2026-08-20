-- | Interactive REPL slash commands.
module Agent.CLI.Command
    ( ReplAction(..)
    , currentEffort
    , currentModel
    , parseReplLine
    , setModel
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
    | ReplShowModel
    | ReplSetModel Text
    | ReplToggleAlwaysApprove
    | ReplShowSession
    | ReplReloadAuth
    | ReplCommandError Text
    deriving (Eq, Show)

parseReplLine :: Text -> ReplAction
parseReplLine raw =
    let line = Text.strip raw
    in if line == ":q" || line == ":quit"
        then ReplQuit
        else case Text.uncons line of
            Just ('/', _) -> parseSlash line
            Just (':', _) -> parseColon line
            _ -> ReplPrompt line

parseColon :: Text -> ReplAction
parseColon line
    | isAlwaysApproveAlias (Text.drop 1 line) = ReplToggleAlwaysApprove
    | otherwise = ReplPrompt line

parseSlash :: Text -> ReplAction
parseSlash line = case Text.words line of
    [] -> ReplCommandError "unknown command: /"
    command : args
        | Text.toLower command == "/effort" -> parseEffortCommand args
        | Text.toLower command == "/model" -> parseModelCommand args
        | Text.toLower command == "/session" ->
            if null args
                then ReplShowSession
                else ReplCommandError "usage: /session"
        | Text.toLower command == "/reload-auth" ->
            if null args
                then ReplReloadAuth
                else ReplCommandError "usage: /reload-auth"
        | isAlwaysApproveAlias (Text.drop 1 command) ->
            if null args
                then ReplToggleAlwaysApprove
                else ReplCommandError "usage: /always-approve"
        | otherwise -> ReplCommandError ("unknown command: " <> command)

isAlwaysApproveAlias :: Text -> Bool
isAlwaysApproveAlias name =
    Text.toLower name `elem` ["always-approve", "yolo"]

parseEffortCommand :: [Text] -> ReplAction
parseEffortCommand = \case
    [] -> ReplShowEffort
    [level] -> case parseEffort level of
        Right effort -> ReplSetEffort effort
        Left err -> ReplCommandError (Text.pack err)
    _ -> ReplCommandError "usage: /effort [low|medium|high|xhigh]"

parseModelCommand :: [Text] -> ReplAction
parseModelCommand = \case
    [] -> ReplShowModel
    [name]
        | Text.null (Text.strip name) ->
            ReplCommandError "usage: /model [NAME]"
        | otherwise -> ReplSetModel name
    _ -> ReplCommandError "usage: /model [NAME]"

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

-- | Rebuild from the constructor: 'input' is also a field on 'CustomToolCall'.
setModel :: Text -> ResponseCreateParams -> ResponseCreateParams
setModel name ResponseCreateParams{..} =
    ResponseCreateParams
        { model = Just name
        , ..
        }

currentModel :: ResponseCreateParams -> Text
currentModel params =
    fromMaybe "(unset)" params.model
