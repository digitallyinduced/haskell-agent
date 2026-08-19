module Main (main) where

import Agent.OpenAI.Client (defaultCodexBaseUrl)
import qualified Data.Text.IO as Text
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        [] -> putStr usage
        ["--help"] -> putStr usage
        ["--version"] -> putStrLn "agent-cli 0.1.0.0"
        ["openai-base-url"] -> Text.putStrLn defaultCodexBaseUrl
        _ -> die usage

usage :: String
usage = unlines
    [ "Usage: agent-cli COMMAND"
    , ""
    , "Commands:"
    , "  openai-base-url  Print the default OpenAI Responses endpoint"
    , "  --version        Print the agent-cli version"
    , "  --help           Show this help"
    ]
