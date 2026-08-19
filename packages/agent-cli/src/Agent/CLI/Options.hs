-- | Command-line flags for @agent-cli@.
module Agent.CLI.Options
    ( ApprovalPolicy(..)
    , CliOptions(..)
    , Command(..)
    , defaultCliOptions
    , isOneShot
    , parseArgs
    , parseEffort
    , resolveApprovalPolicy
    , usage
    ) where

import Agent.Provider (Provider(..), parseProvider)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text

data Command
    = ShowHelp
    | ShowVersion
    | RunAgent CliOptions
    deriving (Eq, Show)

data ApprovalPolicy
    = ApproveAll
    | DenyMutating
    | PromptMutating
    deriving (Eq, Show)

data CliOptions = CliOptions
    { optProvider :: !(Maybe Provider)
    , optModel :: !(Maybe Text)
    , optCwd :: !(Maybe FilePath)
    , optYolo :: !Bool
    , optNoYolo :: !Bool
    , optMaxTurns :: !Int
    , optEffort :: !Text
    , optPrompt :: !(Maybe Text)
    , optPromptFile :: !(Maybe FilePath)
    , optShowReasoning :: !Bool
    } deriving (Eq, Show)

defaultCliOptions :: CliOptions
defaultCliOptions = CliOptions
    { optProvider = Nothing
    , optModel = Nothing
    , optCwd = Nothing
    , optYolo = False
    , optNoYolo = False
    , optMaxTurns = 50
    , optEffort = "low"
    , optPrompt = Nothing
    , optPromptFile = Nothing
    , optShowReasoning = False
    }

isOneShot :: CliOptions -> Bool
isOneShot options = isJust options.optPrompt || isJust options.optPromptFile

-- | One-shot without a TTY auto-approves so scripts do not hang, unless
-- @--no-yolo@ is set. Interactive sessions prompt on mutating tools.
resolveApprovalPolicy :: CliOptions -> Bool -> ApprovalPolicy
resolveApprovalPolicy options isTty
    | options.optYolo && not options.optNoYolo = ApproveAll
    | options.optNoYolo && not isTty = DenyMutating
    | not isTty = ApproveAll
    | otherwise = PromptMutating

parseArgs :: [String] -> Either String Command
parseArgs args
    | any (`elem` ["--help", "-h"]) args = Right ShowHelp
    | "--version" `elem` args = Right ShowVersion
    | take 1 args == ["login"] = Left loginHint
    | otherwise = RunAgent <$> parseOptions defaultCliOptions args

parseOptions :: CliOptions -> [String] -> Either String CliOptions
parseOptions options = \case
    [] -> validate options
    "-h" : _ -> Left usage
    "--help" : _ -> Left usage
    "--version" : _ -> Left "agent-cli 0.1.0.0"
    "--provider" : value : rest -> do
        provider <- case parseProvider (Text.pack value) of
            Just parsed -> Right parsed
            Nothing -> Left ("unknown provider: " <> value <> " (use openai or xai)")
        parseOptions options { optProvider = Just provider } rest
    "--model" : value : rest ->
        parseOptions options { optModel = Just (Text.pack value) } rest
    "--cwd" : value : rest ->
        parseOptions options { optCwd = Just value } rest
    "--yolo" : rest ->
        parseOptions options { optYolo = True, optNoYolo = False } rest
    "--no-yolo" : rest ->
        parseOptions options { optNoYolo = True, optYolo = False } rest
    "--max-turns" : value : rest -> do
        turns <- parseInt "--max-turns" value
        parseOptions options { optMaxTurns = turns } rest
    "--effort" : value : rest -> do
        effort <- parseEffort (Text.pack value)
        parseOptions options { optEffort = effort } rest
    "--show-reasoning" : rest ->
        parseOptions options { optShowReasoning = True } rest
    "-p" : value : rest ->
        parseOptions options { optPrompt = Just (Text.pack value) } rest
    "--prompt" : value : rest ->
        parseOptions options { optPrompt = Just (Text.pack value) } rest
    "--prompt-file" : value : rest ->
        parseOptions options { optPromptFile = Just value } rest
    flag : _
        | flag == "openai-base-url" ->
            Left "openai-base-url was removed; run agent-cli --help"
        | "-" `Text.isPrefixOf` Text.pack flag ->
            Left ("unknown flag: " <> flag <> "\n" <> usage)
        | otherwise ->
            Left ("unexpected argument: " <> flag <> "\n" <> usage)

validate :: CliOptions -> Either String CliOptions
validate options
    | isJust options.optPrompt && isJust options.optPromptFile =
        Left "use either -p/--prompt or --prompt-file, not both"
    | options.optMaxTurns < 1 =
        Left "--max-turns must be at least 1"
    | otherwise = Right options

parseInt :: String -> String -> Either String Int
parseInt flag value = case reads value of
    [(n, "")] | n >= 1 -> Right n
    _ -> Left (flag <> " expects a positive integer, got " <> value)

parseEffort :: Text -> Either String Text
parseEffort raw = case Text.toLower (Text.strip raw) of
    "low" -> Right "low"
    "medium" -> Right "medium"
    "high" -> Right "high"
    "xhigh" -> Right "xhigh"
    other ->
        Left ("effort must be low, medium, high, or xhigh (got " <> Text.unpack other <> ")")

loginHint :: String
loginHint =
    "login is not in this slice. Place credentials in ~/.codex/auth.json \
    \or ~/.grok/auth.json, or set CODEX_ACCESS_TOKEN / GROK_ACCESS_TOKEN."

usage :: String
usage = unlines
    [ "Usage: agent-cli [OPTIONS]"
    , ""
    , "  -p, --prompt TEXT       Run one prompt and exit"
    , "      --prompt-file FILE  Read the one-shot prompt from a file"
    , "      --provider NAME     openai or xai (default: detect from auth)"
    , "      --model NAME        Override the provider default model"
    , "      --cwd DIR           Working directory for tools (default: current)"
    , "      --yolo              Auto-approve every tool"
    , "      --no-yolo           Never auto-approve; deny mutating tools without a TTY"
    , "      --max-turns N       Stop after N model turns (default: 50)"
    , "      --effort LEVEL      Reasoning effort: low, medium, high, xhigh (default: low)"
    , "      --show-reasoning    Print reasoning deltas on stderr"
    , "      --version           Print the agent-cli version"
    , "      --help              Show this help"
    , ""
    , "Without -p/--prompt-file, start a REPL. /effort [LEVEL] changes"
    , "reasoning effort. Ctrl-D or :q exits."
    ]
