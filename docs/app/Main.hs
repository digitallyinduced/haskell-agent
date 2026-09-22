module Main (main) where

import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Documentation.Application (loadApplication)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Paths_haskell_agent_documentation (getDataFileName)
import System.Environment (lookupEnv)
import System.Exit (die)
import Text.Read (readMaybe)

main :: IO ()
main = do
    host <- fromMaybe "127.0.0.1" <$> lookupEnv "DOCS_HOST"
    configuredPort <- fromMaybe "4321" <$> lookupEnv "DOCS_PORT"
    port <- case readMaybe configuredPort of
        Just value | value > 0 && value <= 65535 -> pure value
        _ -> die "DOCS_PORT must be an integer between 1 and 65535"
    configuredDirectory <- lookupEnv "DOCS_ASSET_DIRECTORY"
    directory <- maybe (getDataFileName "public") pure configuredDirectory
    application <- loadApplication directory
    putStrLn ("Serving documentation on " <> host <> ":" <> show port)
    runSettings (setHost (fromString host) (setPort port defaultSettings)) application
