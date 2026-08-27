{-# LANGUAGE TemplateHaskell #-}

module Agent.Tools.CodeMode.Host.Worker
    ( bundledCodeModeWorkerPath
    , codeModeWorkerPath
    ) where

import Paths_agent_core (getDataFileName)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word64)
import qualified Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH.Syntax (makeRelativeToProject, qAddDependentFile, runIO)
import System.Directory
    ( canonicalizePath
    , doesFileExist
    , getTemporaryDirectory
    , renameFile
    )
import System.FilePath ((</>))
import System.Posix.Process (getProcessID)

bundledCodeModeWorkerPath :: IO FilePath
bundledCodeModeWorkerPath = do
    installedPath <- getDataFileName workerRelativePath
    installedExists <- doesFileExist installedPath
    if installedExists
        then pure installedPath
        else materializeEmbeddedWorker
  where
    workerRelativePath = "data/code-mode/worker.mjs"

codeModeWorkerPath :: IO FilePath
codeModeWorkerPath = bundledCodeModeWorkerPath

embeddedCodeModeWorkerSource :: Text
embeddedCodeModeWorkerSource =
    Text.pack
        $(do
            path <- makeRelativeToProject "data/code-mode/worker.mjs"
            qAddDependentFile path
            contents <- runIO (readFile path)
            TH.lift contents
        )

materializeEmbeddedWorker :: IO FilePath
materializeEmbeddedWorker = do
    tmpDir <- getTemporaryDirectory >>= canonicalizePath
    let bytes = Text.encodeUtf8 embeddedCodeModeWorkerSource
        target =
            tmpDir
                </> ("haskell-agent-code-mode-worker-"
                    <> embeddedWorkerFingerprint bytes
                    <> ".mjs")
    exists <- doesFileExist target
    if exists
        then pure target
        else do
            processId <- getProcessID
            let staging = target <> "." <> show processId <> ".tmp"
            BS.writeFile staging bytes
            renameFile staging target
            pure target

embeddedWorkerFingerprint :: BS.ByteString -> String
embeddedWorkerFingerprint bytes =
    show (BS.length bytes) <> "-" <> show (BS.foldl' step seed bytes)
  where
    seed = 14695981039346656037 :: Word64
    step acc byte =
        (acc `xor` fromIntegral byte) * 1099511628211
