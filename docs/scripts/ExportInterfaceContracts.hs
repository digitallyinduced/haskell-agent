{-# LANGUAGE CPP #-}
module Main (main) where

import Control.Monad (unless)
import qualified Data.ByteString as ByteString
import System.Directory (canonicalizePath, doesFileExist)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>), takeDirectory)

-- Export the canonical contract verbatim; --check never writes.
main :: IO ()
main = do
    args <- getArgs
    unless (args == [] || args == ["--check"]) $
        die "Usage: runghc docs/scripts/ExportInterfaceContracts.hs [--check]"
    root <- takeDirectory . takeDirectory . takeDirectory <$> canonicalizePath __FILE__
    let name = "agent-server-openapi.json"
        source = "packages/agent-server/openapi.json"
        target = root </> "docs/public" </> name
    content <- ByteString.readFile (root </> source)
    if args == ["--check"] then do
        exists <- doesFileExist target
        unless exists $ die ("Stale interface reference: " <> target)
        actual <- ByteString.readFile target
        unless (actual == content) $ die ("Stale interface reference: " <> target)
    else ByteString.writeFile target content
    putStrLn (name <> ": " <> show (ByteString.length content) <> " bytes match " <> source)
