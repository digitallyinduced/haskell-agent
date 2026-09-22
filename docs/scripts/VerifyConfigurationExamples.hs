{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FieldSelectors #-}
module VerifyConfigurationExamples (main, verifyExamples, selfTest) where

-- Load alongside the runtime library, not the small documentation shell:
-- Reproducible static-library build runner: verify_configuration_examples.py
-- (see its module documentation for the Nix commands).
--
-- nix develop -c cabal repl agent-runtime:lib:agent-runtime
-- :add docs/scripts/VerifyConfigurationExamples.hs
-- VerifyConfigurationExamples.selfTest
-- VerifyConfigurationExamples.verifyExamples "/path/under/TMPDIR/documentation-examples"
--
-- The Python exporter supplies manifest.json and exact rendered JSON bytes.
-- No user's configuration, provider credentials or network service is used.
-- Run from the repository root. Model examples are user overlays, merged with
-- the checked-in defaults through the same public merge function as startup.
-- Successful decoding does not verify executables, credentials or endpoints.

import Agent.Runtime.Config (loadHarnessConfig)
import Agent.Runtime.ModelConfig (mergeModelConfigs)
import Agent.Runtime.Project (loadProjectSettings)
import Control.Exception.Safe (bracket)
import Control.Monad (forM_, unless, when)
import Data.Aeson (FromJSON (..), Value (..), toJSON, eitherDecode, withObject, (.:))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as ByteString
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (canonicalizePath, createDirectory, removePathForcibly)
import System.Environment (getArgs, getEnv)
import System.FilePath ((</>), addTrailingPathSeparator, takeFileName)
import System.OsPath (encodeUtf)
import System.Posix.Temp (mkdtemp)

data Example = Example
    { exampleSchema :: Text
    , exampleFile :: FilePath
    , exampleRoute :: Text
    }

instance FromJSON Example where
    parseJSON = withObject "documentation example" $ \object ->
        Example <$> object .: "schema" <*> object .: "file" <*> object .: "route"

main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        ["--self-test"] -> selfTest
        [directory] -> verifyExamples directory
        _ -> fail "Expected exported example directory or --self-test"

verifyExamples :: FilePath -> IO ()
verifyExamples directory = do
    examples <- ByteString.readFile (directory </> "manifest.json")
        >>= either fail pure . (eitherDecode :: ByteString.ByteString -> Either String [Example])
    when (null examples) $ fail "Refusing to validate an empty example manifest"
    forM_ examples $ \example -> do
        unless (takeFileName (exampleFile example) == exampleFile example) $
            fail "Manifest filenames must not contain directory components"
        contents <- ByteString.readFile (directory </> exampleFile example)
        result <- validate (exampleSchema example) contents
        case result of
            Left message -> fail $
                Text.unpack (exampleRoute example) <> " / " <> exampleFile example
                    <> ": " <> Text.unpack message
            Right () -> putStrLn $
                "Accepted " <> exampleFile example <> " from "
                    <> Text.unpack (exampleRoute example)
    putStrLn $ "Product decoders accepted " <> show (length examples)
        <> " rendered configuration examples. No integration behavior was exercised."

validate :: Text -> ByteString.ByteString -> IO (Either Text ())
validate "models" contents = do
    defaults <- ByteString.readFile "packages/agent-runtime/config/models.default.json"
    pure $ () <$ mergeModelConfigs ("checked-in defaults", defaults)
        (Just ("documentation example", contents))
validate "harness" contents = do
    root <- getEnv "TMPDIR" >>= canonicalizePath
    bracket (mkdtemp (addTrailingPathSeparator root <> "documentation-config-"))
        removePathForcibly $ \home -> do
            createDirectory (home </> ".haskell-agent")
            ByteString.writeFile (home </> ".haskell-agent" </> "config.json") contents
            encodedHome <- encodeUtf home
            fmap (fmap (const ())) (loadHarnessConfig encodedHome)
validate "settings" contents = do
    root <- getEnv "TMPDIR" >>= canonicalizePath
    bracket (mkdtemp (addTrailingPathSeparator root <> "documentation-settings-"))
        removePathForcibly $ \home -> do
            createDirectory (home </> ".haskell-agent")
            ByteString.writeFile (home </> ".haskell-agent" </> "settings.json") contents
            encodedHome <- encodeUtf home
            actual <- toJSON <$> loadProjectSettings encodedHome
            -- The public loader deliberately falls back on malformed input.
            -- Require every illustrated field to survive loading, including
            -- nested selections, rather than treating "did not throw" as success.
            pure $ case eitherDecode contents of
                Right expected@Object{}
                    | preserves expected actual -> Right ()
                    | otherwise -> Left "Settings fields were defaulted, normalized or discarded"
                _ -> Left "Settings example must be a JSON object"
validate schema _ = pure $ Left ("Unknown example schema: " <> schema)

preserves :: Value -> Value -> Bool
preserves (Object expected) (Object actual) =
    all (\(key, value) -> maybe False (preserves value) (KeyMap.lookup key actual))
        (KeyMap.toList expected)
preserves expected actual = expected == actual

selfTest :: IO ()
selfTest = do
    forM_
        [ ("harness", "{\"version\":1,\"maxConcurrentAgents\":2}")
        , ("models", "{\"version\":1,\"connections\":{},\"models\":[]}")
        , ("settings", "{\"version\":1,\"autoApprove\":true,\"maxConcurrentAgents\":2}")
        ] $ \(schema, contents) -> do
            result <- validate schema contents
            unless (isRight result) $ fail ("Valid fixture rejected: " <> show result)
    forM_
        [ ("harness", "{\"version\":1,\"maxConcurrentAgents\":0}")
        , ("harness", "{\"version\":1,\"maxConcurrentAgents\":2.0}")
        , ("harness", "{\"lsp\":{\"servers\":{\"test\":{\"command\":\"unused\",\"transport\":\"http\"}}}}")
        , ("models", "{\"version\":2,\"connections\":{},\"models\":[]}")
        , ("models", "{\"version\":1,\"connections\":{},\"models\":[{\"id\":\"bad\",\"connection\":\"absent\",\"dialect\":\"codex\"}]}")
        , ("settings", "{\"autoApprove\":\"yes\"}")
        , ("settings", "{\"autoApprove\":true,\"maxConcurrentAgents\":2.0}")
        , ("settings", "{\"lastModel\":{\"provider\":\"unknown\",\"model\":\"bad\"}}")
        ] $ \(schema, contents) -> do
            result <- validate schema contents
            unless (isLeft result) $ fail "Invalid fixture unexpectedly accepted"
    putStrLn "Product decoder positive/negative fixtures passed."
