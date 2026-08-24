module Main (main) where

import Agent.CLI.Clipboard
    ( ClipboardContent(..)
    , readClipboard
    , readClipboardImages
    , readClipboardImagesForPaste
    )
import Agent.Loop (ImageAttachment(..))
import Control.Exception.Safe (bracket)
import Control.Monad (when)
import Data.List (sort)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import System.CPUTime (getCPUTime)
import System.Directory
    ( Permissions(executable)
    , createDirectory
    , doesDirectoryExist
    , getPermissions
    , getTemporaryDirectory
    , removePathForcibly
    , setPermissions
    )
import System.Environment (getArgs, getEnv, setEnv)
import System.FilePath ((</>), searchPathSeparator)
import System.Info (os)

data Sample = Sample
    { sampleElapsedMillis :: !Double
    , sampleCpuMillis :: !Double
    }

main :: IO ()
main = do
    when (os /= "darwin") $
        fail "clipboard-latency-bench currently models the macOS paste path"
    samples <- parseSamples <$> getArgs
    withFakeClipboard do
        putStrLn
            ("text clipboard failure path, samples=" <> show samples
                <> ", fake osascript delay=10ms")
        benchmark "old-repeat-probes" samples oldFailureProbe
        benchmark "new-single-pass" samples newFailureProbe

parseSamples :: [String] -> Int
parseSamples = \case
    [samples] -> max 1 (read samples)
    _ -> 7

benchmark :: String -> Int -> IO Int -> IO ()
benchmark label count action = do
    measured <- mapM (const (measure action)) [1 .. count]
    let elapsed = median (map (.sampleElapsedMillis) measured)
        cpu = median (map (.sampleCpuMillis) measured)
    putStrLn
        (label
            <> " elapsed-ms=" <> show elapsed
            <> " cpu-ms=" <> show cpu)

measure :: IO Int -> IO Sample
measure action = do
    beforeCpu <- getCPUTime
    beforeElapsed <- getMonotonicTimeNSec
    checksum <- action
    afterElapsed <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    checksum `seq`
        pure Sample
            { sampleElapsedMillis =
                fromIntegral (afterElapsed - beforeElapsed) / 1_000_000
            , sampleCpuMillis =
                fromIntegral (afterCpu - beforeCpu) / 1_000_000_000
            }

-- Baseline matching the replaced call sites: try images, then call the richer
-- general clipboard reader after failure, repeating image and path probes.
oldFailureProbe :: IO Int
oldFailureProbe =
    readClipboardImages >>= \case
        Right images -> pure (length images)
        Left _ -> clipboardContentChecksum <$> readClipboard

newFailureProbe :: IO Int
newFailureProbe =
    readClipboardImagesForPaste >>= \case
        Right images -> pure (length images)
        Left err -> pure (Text.length err)

clipboardContentChecksum :: ClipboardContent -> Int
clipboardContentChecksum = \case
    ClipboardImage image -> Text.length image.imageMime
    ClipboardText text -> Text.length text
    ClipboardPaths paths -> sum (map length paths)
    ClipboardEmpty -> 0

withFakeClipboard :: IO a -> IO a
withFakeClipboard action = do
    tmp <- getTemporaryDirectory
    let binDir = tmp </> "agent-clipboard-latency-bench"
    originalPath <- getEnv "PATH"
    bracket
        (do
            removeDirectoryIfPresent binDir
            createDirectory binDir
            writeExecutable
                (binDir </> "osascript")
                "#!/bin/sh\n/bin/sleep 0.01\nexit 1\n"
            writeExecutable
                (binDir </> "pbpaste")
                "#!/bin/sh\nprintf 'benchmark clipboard text'\n"
            setEnv
                "PATH"
                (binDir <> [searchPathSeparator] <> originalPath))
        (\_ -> do
            setEnv "PATH" originalPath
            removeDirectoryIfPresent binDir)
        (const action)

writeExecutable :: FilePath -> String -> IO ()
writeExecutable path contents = do
    writeFile path contents
    permissions <- getPermissions path
    setPermissions path permissions { executable = True }

removeDirectoryIfPresent :: FilePath -> IO ()
removeDirectoryIfPresent path = do
    exists <- doesDirectoryExist path
    when exists (removePathForcibly path)

median :: Ord a => [a] -> a
median values =
    sort values !! (length values `div` 2)
