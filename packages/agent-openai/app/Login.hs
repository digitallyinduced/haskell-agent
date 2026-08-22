module Main where

import qualified Agent.OpenAI.Login as Login
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Directory.OsPath (getHomeDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)
import System.OsPath (decodeFS, unsafeEncodeUtf, (</>))

main :: IO ()
main = do
    home <- getHomeDirectory
    args <- getArgs
    oauthClientId <- lookupEnv "OPENAI_OAUTH_CLIENT_ID" >>= \case
        Just value | not (null value) -> pure (Text.pack value)
        _ -> die "OPENAI_OAUTH_CLIENT_ID must be set"
    let loginOptions = Login.defaultLoginOptions oauthClientId
    output <- case args of
        [] -> pure
            (home </> unsafeEncodeUtf ".codex" </> unsafeEncodeUtf "auth.json")
        ["--output", path] -> pure (unsafeEncodeUtf path)
        _ -> die "usage: agent-openai-login [--output PATH]"
    requested <- Login.requestDeviceCode loginOptions
    deviceCode <- either (die . show) pure requested
    putStrLn "Open this URL in a browser and sign in:"
    putStrLn deviceCode.verificationUrl
    putStrLn "Then enter this one-time code:"
    TextIO.putStrLn deviceCode.userCode
    putStrLn "Waiting for authorization..."
    completed <- Login.completeDeviceCodeLogin loginOptions deviceCode
    auth <- either (die . show) pure completed
    Login.writeAuthFile output auth
    outputPath <- decodeFS output
    putStrLn ("Login successful. Credentials written to " <> outputPath)
