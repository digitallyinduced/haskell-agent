{-# LANGUAGE OverloadedStrings #-}
-- Run from the repository root in nix develop. Current decoder modules are
-- compiled against dependencies from --runtime-package, never replaced by them.
module Main where

import Control.Monad (forM_, unless, when)
import Data.Char (isDigit)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory
import System.Environment (getArgs, getEnv)
import System.Exit (die)
import System.FilePath
import System.IO.Temp (withTempDirectory)
import System.Process (callProcess, readProcess)

main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        ["--help"] -> putStrLn usage
        ["--self-test"] -> selfTest
        _ -> do
            (runtimeArgument, examplesArgument) <- either die pure (parseArguments arguments)
            root <- findRoot =<< getCurrentDirectory
            runtime <- canonicalizePath runtimeArgument
            examples <- canonicalizePath examplesArgument
            withCurrentDirectory root $ do
                exists <- doesFileExist (examples </> "manifest.json")
                unless exists (die "--examples must contain the exported manifest.json")
                version <- Text.strip . Text.pack <$> readProcess "ghc" ["--numeric-version"] ""
                closure <- lines <$> readProcess "nix-store" ["-qR", runtime] ""
                temporaryRoot <- getEnv "TMPDIR"
                withTempDirectory temporaryRoot "documentation-decoders-" $ \build -> do
                    let database = build </> "package.conf.d"
                    createDirectory database
                    forM_ closure $ \package ->
                        unless (compilerPackage (Text.pack (takeFileName package))) $ do
                            let source = package </> "lib" </> ("ghc-" <> Text.unpack version) </> "lib" </> "package.conf.d"
                            available <- doesDirectoryExist source
                            when available $ do
                                registrations <- filter ((== ".conf") . takeExtension) <$> listDirectory source
                                forM_ registrations $ \name -> do
                                    original <- Text.readFile (source </> name)
                                    let contents
                                            | "agent-runtime-" `Text.isPrefixOf` Text.pack name =
                                                exposePaths original
                                            | otherwise = original
                                        destination = database </> name
                                    present <- doesFileExist destination
                                    when present $ do
                                        existing <- Text.readFile destination
                                        unless (existing == contents) (die ("Conflicting package registration: " <> name))
                                    Text.writeFile destination contents
                    registrations <- listDirectory database
                    unless (any (Text.isPrefixOf "agent-runtime-" . Text.pack) registrations) $
                        die ("Runtime package registration not found for active GHC " <> Text.unpack version)
                    callProcess "ghc-pkg" ["recache", "--package-db", database]
                    let executable = build </> "verify"
                        extensions = ["GHC2021", "BlockArguments", "OverloadedStrings", "OverloadedRecordDot",
                            "DuplicateRecordFields", "NoFieldSelectors", "LambdaCase", "RecordWildCards", "TypeApplications"]
                    callProcess "ghc" $
                        ["--make", "-O0", "-outputdir", build, "-o", executable,
                        "-main-is", "VerifyConfigurationExamples", "-package-db", database,
                        "-package", "agent-runtime (Paths_agent_runtime)", "-ipackages/agent-runtime/src"]
                        <> map ("-X" <>) extensions <> ["docs/scripts/VerifyConfigurationExamples.hs"]
                    callProcess executable ["--self-test"]
                    callProcess executable [examples]

usage :: String
usage = "RunConfigurationExamples.hs --runtime-package PATH --examples PATH\nRun inside nix develop; examples must contain manifest.json and TMPDIR must be set. Use --self-test for metadata fixtures."

parseArguments :: [String] -> Either String (FilePath, FilePath)
parseArguments = go Nothing Nothing
  where
    go (Just runtime) (Just examples) [] = Right (runtime, examples)
    go _ examples ("--runtime-package" : runtime : rest) = go (Just runtime) examples rest
    go runtime _ ("--examples" : examples : rest) = go runtime (Just examples) rest
    go _ _ _ = Left usage

findRoot :: FilePath -> IO FilePath
findRoot directory = do
    found <- doesFileExist (directory </> "docs/scripts/VerifyConfigurationExamples.hs")
    if found then pure directory
    else if takeDirectory directory == directory then die "Run from inside the repository"
    else findRoot (takeDirectory directory)

compilerPackage :: Text.Text -> Bool
compilerPackage name = any startsWithDigit (drop 1 (Text.splitOn "-ghc-" name))
  where
    startsWithDigit suffix = maybe False (isDigit . fst) (Text.uncons suffix)

-- Package registration is textual Cabal metadata. Remove only the complete
-- generated module token; keep all other fields byte-for-byte.
exposePaths :: Text.Text -> Text.Text
exposePaths contents =
    let stripped = removeToken contents
        (before, after) = Text.breakOn "exposed-modules:" stripped
    in if Text.null after then stripped
       else before <> "exposed-modules:\n    Paths_agent_runtime" <> Text.drop (Text.length "exposed-modules:") after
  where
    removeToken input = Text.concat (map removeWord (Text.groupBy sameClass input))
    sameClass left right = isWord left == isWord right
    isWord character = character == '_' || character >= 'a' && character <= 'z'
        || character >= 'A' && character <= 'Z' || character >= '0' && character <= '9'
    removeWord word | word == "Paths_agent_runtime" = ""
                    | otherwise = word

selfTest :: IO ()
selfTest = do
    let input = "name: agent-runtime\nexposed-modules: Agent.Runtime Paths_agent_runtime\nhidden-modules: Paths_agent_runtime\n"
        expected = "name: agent-runtime\nexposed-modules:\n    Paths_agent_runtime Agent.Runtime \nhidden-modules: \n"
    unless (exposePaths input == expected) (die "Generated Paths metadata fixture failed")
    unless (exposePaths "exposed-modules: Paths_agent_runtime_extra" ==
        "exposed-modules:\n    Paths_agent_runtime Paths_agent_runtime_extra") (die "Module token boundaries were not preserved")
    unless (parseArguments ["--examples", "examples", "--runtime-package", "runtime"] ==
        Right ("runtime", "examples")) (die "Argument order fixture failed")
    putStrLn "Decoder launcher metadata and argument fixtures passed."
