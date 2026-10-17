#!/usr/bin/env runghc
{-# LANGUAGE BlockArguments, LambdaCase, NoFieldSelectors, OverloadedRecordDot, OverloadedStrings #-}

-- Compare optimized CLI executables through independent, real tmux terminals.
module Main (main) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (wait, withAsyncWithUnmask)
import Control.Exception.Safe (finally)
import Control.Monad (forM, forM_, unless, void, when)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Lazy as ByteString
import Data.Char (isDigit)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesPathExist, getHomeDirectory, makeAbsolute)
import System.Environment (getArgs, getEnvironment)
import System.Exit (ExitCode(..), die)
import System.FilePath ((</>))
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)
import Text.Read (readMaybe)

data HomeSelection = UnspecifiedHome | CurrentHome | DedicatedHome FilePath

data Options = Options
    { beforeExecutable :: FilePath
    , afterExecutable :: FilePath
    , socketPath :: FilePath
    , projectDirectory :: FilePath
    , outputDirectory :: FilePath
    , homeSelection :: HomeSelection
    , sampleCount :: Int
    , warmupCount :: Int
    , timeoutSeconds :: Int
    , cliArguments :: [String]
    }

data Observation = Observation
    { firstFrameMilliseconds :: Maybe Double
    , editableMilliseconds :: Maybe Double
    , readyMilliseconds :: Maybe Double
    , internalReadyMilliseconds :: Maybe Double
    }

usage :: Text
usage = Text.unlines
    [ "Usage: BenchmarkCliStartup.hs BEFORE AFTER SOCKET PROJECT OUTPUT"
    , "         (--home HOME | --current-home) [--samples N] [--warmups N]"
    , "         [--timeout SECONDS] [-- CLI_FLAGS]"
    , "Use optimized executables and an unused, short socket path."
    , "Runs fullscreen without submitting a model turn; captures may contain session text."
    ]

parseArguments :: [String] -> Either Text Options
parseArguments (before : after : socket : project : output : arguments) =
    parseOptions (Options before after socket project output UnspecifiedHome 7 1 45 []) arguments
  where
    parseOptions options [] = validate options
    parseOptions options ("--" : flags) = validate options { cliArguments = flags }
    parseOptions options ("--home" : home : remaining) = case options.homeSelection of
        UnspecifiedHome -> parseOptions options { homeSelection = DedicatedHome home } remaining
        _ -> Left "Select exactly one HOME mode."
    parseOptions options ("--current-home" : remaining) = case options.homeSelection of
        UnspecifiedHome -> parseOptions options { homeSelection = CurrentHome } remaining
        _ -> Left "Select exactly one HOME mode."
    parseOptions options ("--samples" : value : remaining)
        | Just count <- readMaybe value = parseOptions options { sampleCount = count } remaining
    parseOptions options ("--warmups" : value : remaining)
        | Just count <- readMaybe value = parseOptions options { warmupCount = count } remaining
    parseOptions options ("--timeout" : value : remaining)
        | Just seconds <- readMaybe value = parseOptions options { timeoutSeconds = seconds } remaining
    parseOptions _ _ = Left "Invalid benchmark option."
    validate options
        | options.sampleCount < 1 || options.warmupCount < 0 || options.timeoutSeconds < 1 =
            Left "Provide positive samples/timeout and nonnegative warmups."
        | any initiatesTurn options.cliArguments =
            Left "Prompt options initiate model turns and cannot be benchmarked here."
        | otherwise = case options.homeSelection of
            UnspecifiedHome -> Left "Select --home HOME or --current-home explicitly."
            _ -> Right options
    initiatesTurn argument = any (`Text.isPrefixOf` Text.pack argument)
        ["-p", "--prompt", "--managed-turn-file"]
parseArguments _ = Left "Provide both executables, socket, project and output directory."

shellQuote :: String -> String
shellQuote value = "'" <> concatMap (\character -> if character == '\'' then "'\\''" else [character]) value <> "'"

-- Intermediate checkpoints such as "workspace scopes ready" are not readiness.
parseReadyMilliseconds :: Text -> Maybe Double
parseReadyMilliseconds pane = case
    [ milliseconds
    | (previous, name, duration) <- zip3 tokens (drop 1 tokens) (drop 2 tokens)
    , previous == "·" || previous == "startup:"
    , name == "ready"
    , let (number, unit) = Text.span (\character -> isDigit character || character == '.') duration
    , Just value <- [readMaybe (Text.unpack number) :: Maybe Double]
    , value >= 0
    , Just milliseconds <- [case unit of "ms" -> Just value; "s" -> Just (1000 * value); _ -> Nothing]
    ] of
        value : _ -> Just value
        [] -> Nothing
  where
    tokens = Text.words (snd (Text.breakOn "startup:" pane))

median :: [Double] -> Double
median samples =
    let values = sort samples
        middle = length values `div` 2
    in if odd (length values)
        then values !! middle
        else (values !! (middle - 1) + values !! middle) / 2

runTmux :: Options -> [(String, String)] -> [String] -> IO (Maybe (ExitCode, Text, Text))
runTmux options environment arguments = do
    result <- timeout 2000000 $
        readCreateProcessWithExitCode
            (proc "tmux" (["-f", "/dev/null", "-S", options.socketPath] <> arguments))
                { env = Just environment }
            ""
    pure ((\(status, output, errors) -> (status, Text.pack output, Text.pack errors)) <$> result)

requireTmux :: Options -> [(String, String)] -> [String] -> IO Text
requireTmux options environment arguments = runTmux options environment arguments >>= \case
    Just (ExitSuccess, output, _) -> pure output
    Just (_, _, errors) -> die ("tmux failed: " <> Text.unpack errors)
    Nothing -> die "tmux exceeded its two-second command timeout."

observe :: Options -> [(String, String)] -> Text -> FilePath -> IO Observation
observe options environment identifier executable = do
    started <- getMonotonicTimeNSec
    let session = Text.unpack identifier
        command = "HASKELL_AGENT_STARTUP_TIMING=1 "
            <> unwords (map shellQuote (executable : "--fullscreen" : options.cliArguments))
            <> "; status=$?; printf '\\nCLI_EXIT_STATUS=%s\\n' \"$status\"; exec /bin/sh"
        capture = requireTmux options environment ["capture-pane", "-p", "-t", session]
        output = options.outputDirectory </> session <> ".txt"
        elapsed = do
            now <- getMonotonicTimeNSec
            pure (fromIntegral (now - started) / 1000000 :: Double)
        loop probeSent observation = do
            pane <- capture
            milliseconds <- elapsed
            let visible = "❯" `Text.isInfixOf` pane
                internalReady = observation.internalReadyMilliseconds <|> parseReadyMilliseconds pane
                record previous condition = previous <|> if condition then Just milliseconds else Nothing
                next = Observation
                    (record observation.firstFrameMilliseconds visible)
                    (record observation.editableMilliseconds (probeSent && "❯ startup-probe" `Text.isInfixOf` pane))
                    (record observation.readyMilliseconds (isJust internalReady))
                    internalReady
            when ("CLI_EXIT_STATUS=" `Text.isInfixOf` pane) $
                Text.writeFile output pane >> die ("CLI exited before startup completed; inspect " <> output)
            when (milliseconds > fromIntegral options.timeoutSeconds * 1000) $
                Text.writeFile output pane >> die ("Startup timed out; inspect " <> output)
            when (not probeSent && visible) $
                void (requireTmux options environment ["send-keys", "-t", session, "-l", "startup-probe"])
            if all isJust [next.firstFrameMilliseconds, next.editableMilliseconds, next.readyMilliseconds]
                then Text.writeFile output pane >> pure next
                else threadDelay 10000 >> loop (probeSent || visible) next
        cleanup = do
            void (runTmux options environment ["send-keys", "-t", session, "C-u", "C-d"])
            let awaitExit = do
                    result <- runTmux options environment ["capture-pane", "-p", "-t", session]
                    case result of
                        Just (ExitSuccess, pane, _) | not ("CLI_EXIT_STATUS=" `Text.isInfixOf` pane) ->
                            threadDelay 20000 >> awaitExit
                        _ -> pure ()
            void (timeout 3000000 awaitExit)
            void (runTmux options environment ["kill-session", "-t", session])
            void (runTmux options environment ["kill-server"])
        run = do
            void (requireTmux options environment
                [ "new-session", "-d", "-s", session, "-x", "120", "-y", "40"
                , "-c", options.projectDirectory, "/bin/sh", "-c", command
                ])
            loop False (Observation Nothing Nothing Nothing Nothing)
    -- Safe.finally masks its release; deadlines execute in a scoped unmasked child.
    run `finally` withAsyncWithUnmask (\unmask -> unmask cleanup) wait

prepareEnvironment :: Options -> IO [(String, String)]
prepareEnvironment options = do
    inherited <- Map.fromList <$> getEnvironment
    let environment = Map.delete "TMUX" (Map.delete "STY" inherited)
    case options.homeSelection of
        CurrentHome -> pure (Map.toList environment)
        DedicatedHome directory -> do
            home <- canonicalizePath directory
            currentHome <- getHomeDirectory >>= canonicalizePath
            exists <- doesDirectoryExist home
            unless (exists && home /= currentHome) $
                die "--home must name an existing, dedicated benchmark HOME."
            pure (Map.toList (Map.fromList
                [ ("HOME", home), ("XDG_CONFIG_HOME", home </> ".config")
                , ("XDG_CACHE_HOME", home </> ".cache"), ("XDG_DATA_HOME", home </> ".local/share")
                , ("XDG_STATE_HOME", home </> ".local/state")
                ] `Map.union` environment))
        UnspecifiedHome -> die "A HOME mode is required."

runBenchmark :: Options -> IO ()
runBenchmark supplied = do
    before <- canonicalizePath supplied.beforeExecutable
    after <- canonicalizePath supplied.afterExecutable
    socket <- makeAbsolute supplied.socketPath
    project <- canonicalizePath supplied.projectDirectory
    output <- makeAbsolute supplied.outputDirectory
    let options = supplied
            { beforeExecutable = before, afterExecutable = after, socketPath = socket
            , projectDirectory = project, outputDirectory = output }
    socketExists <- doesPathExist socket
    outputExists <- doesPathExist output
    when (socketExists || outputExists) $
        die "Socket and output paths must be unused."
    when (StrictByteString.length (Text.encodeUtf8 (Text.pack socket)) > 100) $
        die "Use a socket path of at most 100 bytes."
    environment <- prepareEnvironment options
    createDirectoryIfMissing True output
    let emit value = do
            let bytes = encode value <> "\n"
            ByteString.putStr bytes
            ByteString.appendFile (output </> "samples.jsonl") bytes
    rows <- fmap concat $ forM [-options.warmupCount .. options.sampleCount - 1] \index -> do
        let pair = [("before", before), ("after", after)]
            commands = if even index then pair else reverse pair
        forM commands \(label, executable) -> do
            let identifier = label <> "_" <> Text.pack (show index)
            observation <- observe options environment identifier executable
            emit (object
                [ "sample" .= index, "executable" .= (label :: Text)
                , "first_frame_ms" .= observation.firstFrameMilliseconds
                , "editable_ms" .= observation.editableMilliseconds
                , "ready_ms" .= observation.readyMilliseconds
                , "internal_ready_ms" .= observation.internalReadyMilliseconds
                ])
            pure (label, index, observation)
    forM_ ["before", "after"] \label -> do
        let samples = [sample | (name, index, sample) <- rows, name == label, index >= 0]
            values select = median [value | sample <- samples, Just value <- [select sample]]
        emit (object
            [ "executable" .= (label :: Text), "samples" .= options.sampleCount
            , "median_first_frame_ms" .= values (.firstFrameMilliseconds)
            , "median_editable_ms" .= values (.editableMilliseconds)
            , "median_ready_ms" .= values (.readyMilliseconds)
            , "median_internal_ready_ms" .= values (.internalReadyMilliseconds)
            ])

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    arguments <- getArgs
    case arguments of
        ["--help"] -> Text.putStrLn usage
        _ -> case parseArguments arguments of
            Left message -> die (Text.unpack (message <> "\n" <> usage))
            Right options -> runBenchmark options
