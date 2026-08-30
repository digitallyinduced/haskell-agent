-- | Grok Build monitor tool for long-lived observation commands.
module Agent.GrokBuild.Dialect.Monitor
    ( monitorTool
    ) where

import qualified Agent.Json.Decode as Json
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch (typedTool)
import Agent.GrokBuild.Dialect.Common (jsonTool)
import Agent.GrokBuild.Dialect.Json (optionalBool, optionalIntOrString)
import Agent.GrokBuild.Dialect.Shell
    ( GrokSession
    , startMonitor
    )
import Agent.Tools.Types (AppTool, ToolExecutionPolicy(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

data MonitorArgs = MonitorArgs
    { command :: !Text
    , description :: !Text
    , timeoutMs :: !(Maybe Int)
    , persistent :: !Bool
    }

monitorArgsDecoder :: Json.Decoder MonitorArgs
monitorArgsDecoder = Json.object $
    MonitorArgs
        <$> Json.atKey "command" Json.text
        <*> Json.atKey "description" Json.text
        <*> optionalIntOrString "timeout_ms"
        <*> (fromMaybe False <$> optionalBool "persistent")

monitorTool :: GrokSession -> AppTool
monitorTool session = jsonTool "monitor" monitorDescription
    [ PropertySchema "command" PropertyString True $ Just
        "Shell command or script. Each stdout line is an event; exit ends the watch."
    , PropertySchema "description" PropertyString True $ Just
        "Short human-readable description of what you are monitoring."
    , PropertySchema "timeout_ms" PropertyInteger False $ Just
        "Kill the monitor after this deadline in milliseconds. Default and maximum: 36000000 (10 hours). Ignored when persistent is true."
    , PropertySchema "persistent" PropertyBoolean False $ Just
        "Run for the lifetime of the session with no timeout. Stop it with kill_command_or_subagent."
    ]
    False
    TurnSequential
    (typedTool "monitor" monitorArgsDecoder (runMonitor session))

monitorDescription :: Text
monitorDescription =
    "Start a background monitor that streams events from a long-running script. Each stdout line is an event; exit ends the watch.\n\n\
    \Print only meaningful status changes. Use line-buffered commands in pipes so events are not delayed.\n\n\
    \Use `$TMPDIR` for temporary files; literal `/tmp` and `/private/tmp` paths are rejected.\n\n\
    \Set persistent=true for session-length watches. Otherwise the monitor stops at timeout_ms (default 10h)."

runMonitor :: GrokSession -> MonitorArgs -> IO (Either Text Text)
runMonitor session args
    | Text.null (Text.strip args.description) =
        pure (Left "Missing parameter: description")
    | not args.persistent
    , Just timeout <- args.timeoutMs
    , timeout > maxMonitorTimeoutMs =
        pure $ Left $
            "timeout_ms exceeds maximum of "
                <> Text.pack (show maxMonitorTimeoutMs)
                <> ". Set persistent=true for a session-length monitor."
    | otherwise = do
        let timeout
                | args.persistent = Nothing
                | otherwise =
                    Just
                        (max 1
                            (fromMaybe
                                maxMonitorTimeoutMs
                                args.timeoutMs))
        startMonitor session args.command timeout >>= \case
            Left err -> pure (Left err)
            Right output ->
                pure $ Right $
                    output
                        <> "\ndescription: "
                        <> Text.strip args.description
                        <> "\ntimeout_ms: "
                        <> Text.pack (show (fromMaybe 0 timeout))
                        <> "\npersistent: "
                        <> if args.persistent then "true" else "false"

maxMonitorTimeoutMs :: Int
maxMonitorTimeoutMs = 36000000
