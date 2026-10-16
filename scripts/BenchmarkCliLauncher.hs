#!/usr/bin/env runghc
{-# LANGUAGE BlockArguments, LambdaCase, NoFieldSelectors, OverloadedStrings, OverloadedRecordDot #-}

-- Compare shell and compiled launchers around the same optimized executable.
module Main (main) where

import Control.Concurrent.Async (concurrently)
import Control.Monad (forM, forM_, unless)
import Data.Aeson (FromJSON(..), ToJSON(..), eitherDecodeFileStrict', encode, object, withObject, (.:), (.=))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (canonicalizePath)
import System.Environment (getArgs, getEnvironment)
import System.Exit (ExitCode(..), die)
import System.FilePath ((</>))
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import System.Process (CreateProcess(..), StdStream(..), proc, waitForProcess, withCreateProcess)
import System.Timeout (timeout)
import Text.Printf (printf)
import Text.Read (readMaybe)

type Environment = Map.Map Text Text

data BenchmarkOptions = BenchmarkOptions
    { comparisonDirectory :: FilePath
    , sampleCount :: Int
    , warmupCount :: Int
    , outputFile :: Maybe FilePath
    }

data Command = Benchmark BenchmarkOptions | Check FilePath | Help

data Launcher = ShellLauncher | CompiledLauncher deriving (Eq)

launcherName :: Launcher -> Text
launcherName ShellLauncher = "shell"
launcherName CompiledLauncher = "native"

data Sample = Sample
    { workloadName :: Text
    , sampleLauncher :: Launcher
    , sampleIndex :: Int
    , elapsedMilliseconds :: Double
    , pathBytes :: Int
    }

instance ToJSON Sample where
    toJSON sample = object
        [ "workload" .= sample.workloadName
        , "launcher" .= launcherName sample.sampleLauncher
        , "sample" .= sample.sampleIndex
        , "elapsed_ms" .= sample.elapsedMilliseconds
        , "path_bytes" .= sample.pathBytes
        ]

data ProcessOutput = ProcessOutput
    { processExitCode :: ExitCode
    , processStdout :: ByteString.ByteString
    , processStderr :: ByteString.ByteString
    }

data CheckConfiguration = CheckConfiguration
    { commandDirectories :: [Text]
    , pluginDirectories :: [Text]
    , syntaxDirectory :: Text
    , postgresBin :: Text
    , shellEnvironmentProgram :: FilePath
    , nativeEnvironmentProgram :: FilePath
    , shellCliProgram :: FilePath
    , nativeCliProgram :: FilePath
    }

instance FromJSON CheckConfiguration where
    parseJSON = withObject "launcher check configuration" \value ->
        CheckConfiguration
            <$> value .: "commandDirectories"
            <*> value .: "pluginDirectories"
            <*> value .: "syntaxDirectory"
            <*> value .: "postgresBin"
            <*> value .: "shellEnvironmentProgram"
            <*> value .: "nativeEnvironmentProgram"
            <*> value .: "shellCliProgram"
            <*> value .: "nativeCliProgram"

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    arguments <- getArgs
    case parseArguments arguments of
        Left message -> die (Text.unpack (message <> "\n" <> usage))
        Right Help -> Text.putStrLn usage
        Right (Benchmark options) -> runBenchmark options
        Right (Check path) -> runChecks path

usage :: Text
usage = Text.unlines
    [ "Usage: BenchmarkCliLauncher.hs COMPARISON [--samples N] [--warmups N] [--output FILE]"
    , "       BenchmarkCliLauncher.hs --check CONFIGURATION"
    , "Compare the launchers produced by nix/benchmarks/cli-wrapper.nix."
    ]

parseArguments :: [String] -> Either Text Command
parseArguments ["--help"] = Right Help
parseArguments ["--check", path] = Right (Check path)
parseArguments (directory : arguments)
    | not (Text.isPrefixOf "--" (Text.pack directory)) =
        Benchmark <$> parseOptions (BenchmarkOptions directory 7 1 Nothing) arguments
  where
    parseOptions :: BenchmarkOptions -> [String] -> Either Text BenchmarkOptions
    parseOptions options []
        | options.sampleCount > 0 && options.warmupCount >= 0 = Right options
        | otherwise = Left "Provide positive samples and nonnegative warmups."
    parseOptions options ("--samples" : count : rest)
        | Just parsed <- readMaybe count =
            parseOptions options { sampleCount = parsed } rest
    parseOptions options ("--warmups" : count : rest)
        | Just parsed <- readMaybe count =
            parseOptions options { warmupCount = parsed } rest
    parseOptions options ("--output" : path : rest) =
        parseOptions options { outputFile = Just path } rest
    parseOptions _ _ = Left "Invalid benchmark option."
parseArguments _ = Left "Provide a comparison directory or a check configuration."

loadEnvironment :: IO Environment
loadEnvironment = Map.fromList . map (\(name, value) -> (Text.pack name, Text.pack value))
    <$> getEnvironment

processEnvironment :: Environment -> [(String, String)]
processEnvironment = map (\(name, value) -> (Text.unpack name, Text.unpack value)) . Map.toList

runProcess :: CreateProcess -> IO ProcessOutput
runProcess command = do
    result <- timeout 30000000 $
        withCreateProcess command
            { std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe }
            \_ output errors process -> case (output, errors) of
                (Just outputHandle, Just errorHandle) -> do
                    (outputBytes, errorBytes) <- concurrently
                        (ByteString.hGetContents outputHandle)
                        (ByteString.hGetContents errorHandle)
                    exitCode <- waitForProcess process
                    pure (ProcessOutput exitCode outputBytes errorBytes)
                _ -> die "Launcher output pipes were not created."
    maybe (die "Launcher exceeded the 30-second timeout.") pure result

require :: Bool -> Text -> IO ()
require condition message = unless condition (die (Text.unpack message))

median :: [Double] -> Double
median samples =
    let values = sort samples
        middle = length values `div` 2
    in if odd (length values)
        then values !! middle
        else (values !! (middle - 1) + values !! middle) / 2

runBenchmark :: BenchmarkOptions -> IO ()
runBenchmark options = do
    comparison <- canonicalizePath options.comparisonDirectory
    inherited <- loadEnvironment
    expectedOutput <- newIORef Nothing
    let synthetic count = Text.intercalate ":"
            [ Text.pack (printf "/nix/store/%032x-startup-command/bin" index)
            | index <- [0 .. count - 1] :: [Int]
            ]
        workloads =
            [ (Text.pack (show count), Map.insert "PATH" (synthetic count) inherited)
            | count <- [1, 64, 128, 256]
            ] <> [ ("inherited", inherited)
                 , ("256-repeat", Map.insert "PATH" (synthetic 256) inherited)
                 ]
    rows <- fmap concat $ forM workloads \(workload, environment) -> do
        let childEnvironment = Just (processEnvironment environment)
            bytes = ByteString.length (Text.encodeUtf8 (Map.findWithDefault "" "PATH" environment))
        samples <- fmap (catMaybes . concat) $
            forM [negate options.warmupCount .. options.sampleCount - 1] \index -> do
                let order = if even index
                        then [ShellLauncher, CompiledLauncher]
                        else [CompiledLauncher, ShellLauncher]
                forM order \launcher -> do
                    let executable = comparison </> Text.unpack (launcherName launcher) </> "bin/agent-cli"
                        command = (proc executable ["--version"]) { env = childEnvironment }
                    started <- getMonotonicTimeNSec
                    output <- runProcess command
                    finished <- getMonotonicTimeNSec
                    require (output.processExitCode == ExitSuccess) "Launcher exited unsuccessfully."
                    readIORef expectedOutput >>= \case
                        Nothing -> writeIORef expectedOutput (Just output.processStdout)
                        Just expected -> require (expected == output.processStdout)
                            "Launchers returned different version output."
                    let elapsed = fromIntegral (finished - started) / 1000000
                    pure if index < 0 then Nothing else
                        Just (Sample workload launcher index elapsed bytes)
        forM_ [ShellLauncher, CompiledLauncher] \launcher ->
            LazyByteString.putStr (encode (object
                [ "workload" .= workload
                , "launcher" .= launcherName launcher
                , "path_bytes" .= bytes
                , "median_ms" .= median
                    [ sample.elapsedMilliseconds
                    | sample <- samples, sample.sampleLauncher == launcher
                    ]
                ]) <> "\n")
        pure samples
    forM_ options.outputFile \path ->
        LazyByteString.writeFile path (encode rows <> "\n")

runChecks :: FilePath -> IO ()
runChecks path = do
    configuration <- (eitherDecodeFileStrict' path :: IO (Either String CheckConfiguration))
        >>= either die pure
    inherited <- loadEnvironment
    let variables =
            [ "PATH", "GST_PLUGIN_SYSTEM_PATH_1_0", "AGENT_SYNTAX_DIR"
            , "AGENT_POSTGRES_BIN", "AGENT_MEDIA_HELPER", "HASKELL_AGENT_APPLE_SESSION_TITLE"
            ]
        cases =
            [ Map.empty
            , Map.fromList [(name, "") | name <- variables]
            , Map.fromList [(name, "/custom/location with spaces") | name <- variables]
            , Map.fromList
                [ ("PATH", Text.intercalate ":"
                    (["/custom/bin"] <> configuration.commandDirectories <> ["/usr/bin"]))
                , ("GST_PLUGIN_SYSTEM_PATH_1_0", Text.intercalate ":"
                    ("/custom/plugins" : configuration.pluginDirectories))
                ]
            , Map.fromList
                [ ("PATH", Text.intercalate ":" (reverse configuration.commandDirectories))
                , ("GST_PLUGIN_SYSTEM_PATH_1_0", Text.intercalate ":" (reverse configuration.pluginDirectories))
                ]
            ]
        readVariables executable overrides = do
            let environment = Map.union overrides (foldr Map.delete inherited variables)
            output <- runProcess (proc executable ["-0"])
                { env = Just (processEnvironment environment) }
            require (output.processExitCode == ExitSuccess) "Environment reporter exited unsuccessfully."
            let entries = Map.fromList
                    [ (name, ByteString.drop 1 rest)
                    | entry <- ByteString.split 0 output.processStdout
                    , let (name, rest) = ByteString.break (== 61) entry
                    , not (ByteString.null rest)
                    ]
            pure (Map.fromList [(name, Map.lookup (Text.encodeUtf8 name) entries) | name <- variables])
        checkPrefix actual name directories = require
            (fmap (take (length directories) . ByteString.split 58) (Map.findWithDefault Nothing name actual)
                == Just (map Text.encodeUtf8 directories))
            ("Launcher directory prefix differs for " <> name)
    forM_ (zip [0 :: Int ..] cases) \(index, overrides) -> do
        shell <- readVariables configuration.shellEnvironmentProgram overrides
        native <- readVariables configuration.nativeEnvironmentProgram overrides
        checkPrefix native "PATH" configuration.commandDirectories
        checkPrefix native "GST_PLUGIN_SYSTEM_PATH_1_0" configuration.pluginDirectories
        forM_ [("AGENT_SYNTAX_DIR", configuration.syntaxDirectory), ("AGENT_POSTGRES_BIN", configuration.postgresBin)]
            \(name, fallback) -> require
                (Map.lookup name native == Just (Just (Text.encodeUtf8 (Map.findWithDefault fallback name overrides))))
                ("Launcher default differs for " <> name)
        -- Nix's Bash supplies an inert default when PATH is unset.
        let normalizedShell
                | Map.member "PATH" overrides = shell
                | otherwise = Map.adjust (fmap \value ->
                    fromMaybe value (ByteString.stripSuffix ":/no-such-path" value)) "PATH" shell
        forM_ variables \name -> require (Map.lookup name normalizedShell == Map.lookup name native)
            ("Launcher environments differ in case " <> Text.pack (show index) <> " for " <> name)
    Text.putStrLn "Launcher environments agree for all five cases"
    forM_
        [ (["--version"], ExitSuccess)
        , (["--help"], ExitSuccess)
        , (["--invalid-launcher-option"], ExitFailure 1)
        , (["+RTS", "-N2", "-RTS", "--version"], ExitSuccess)
        ] \(arguments, expectedStatus) -> do
            shell <- runProcess (proc configuration.shellCliProgram arguments)
            native <- runProcess (proc configuration.nativeCliProgram arguments)
            require
                (native.processExitCode == expectedStatus && shell.processExitCode == expectedStatus
                    && native.processStdout == shell.processStdout
                    && native.processStderr == shell.processStderr)
                "Launcher arguments, output or exit statuses differ."
    Text.putStrLn "Launcher arguments, output and exit statuses agree"
