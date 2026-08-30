{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Agent.Tools.OutputArtifact
    ( ReadArgs(..)
    , SearchArgs(..)
    , outputArtifactMetadata
    , readOutputArtifact
    , readToolOutput
    , searchToolOutput
    , writeOutputArtifactDetailed
    )
import Agent.Tools.Types
    ( defaultToolEnv
    , setToolSessionTmp
    )
import Control.Exception (evaluate)
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import GHC.Stats
    ( RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import System.Directory
    ( createDirectory
    , doesDirectoryExist
    , getTemporaryDirectory
    , removeDirectoryRecursive
    )
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import System.Mem (performGC)
import System.OsPath (unsafeEncodeUtf)
import Text.Printf (printf)

data Sample = Sample
    { allocatedBytes :: !Integer
    , liveBytes :: !Integer
    }

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if not enabled
        then die "RTS statistics are disabled; run with +RTS -T"
        else pure ()
    _ <- getArgs
    tmp <- getTemporaryDirectory
    let root = tmp </> "agent-output-artifact-bench"
        session = root </> "session"
    exists <- doesDirectoryExist root
    if exists then removeDirectoryRecursive root else pure ()
    createDirectory root
    createDirectory session
    env <- defaultToolEnv (unsafeEncodeUtf root)
    setToolSessionTmp env (Just (unsafeEncodeUtf session))
    let payload =
            ByteString.concat
                [ ByteString.replicate (32 * 1024 * 1024) 97
                , "\nneedle\n"
                ]
    writeOutputArtifactDetailed env payload >>= \case
        Left err -> die (Text.unpack err)
        Right artifact -> do
            run "bounded-read" $
                readToolOutput env
                    (ReadArgs artifact.artifactHandle (Just 1) (Just 1))
            run "bounded-search" $
                searchToolOutput env
                    (SearchArgs artifact.artifactHandle "needle" False (Just 5))
            run "metadata" $
                outputArtifactMetadata env artifact.artifactHandle
            run "full-read-baseline" $
                readOutputArtifact env artifact.artifactHandle
    removeDirectoryRecursive root
  where
    run label action = do
        sample <- measure action
        printf "%s,allocated=%d,live-after-gc=%d\n"
            label sample.allocatedBytes sample.liveBytes

measure :: IO a -> IO Sample
measure action = do
    performGC
    before <- getRTSStats
    result <- action
    _ <- evaluate result
    after <- getRTSStats
    performGC
    settled <- getRTSStats
    pure Sample
        { allocatedBytes =
            fromIntegral (after.allocated_bytes - before.allocated_bytes)
        , liveBytes = fromIntegral settled.live_bytes
        }
