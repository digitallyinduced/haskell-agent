module Agent.CLI.WorktreeIgnoredSpec (spec) where

import Agent.CLI.Worktree.Ignored (checkIgnoredPath)
import Control.Monad (forM_)
import qualified System.Directory as Directory
import System.FilePath ((</>), takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import qualified System.Posix.Files as Posix
import Test.Hspec

spec :: Spec
spec = describe "ignored worktree paths" $ do
    it "accepts only an exact primary-checkout settings duplicate" $
        fixture $ \primary checkout check -> do
            copies primary checkout ".haskell-agent/settings.json" "{}"
            check ".haskell-agent/" `shouldReturn` ()
            writeFile (checkout </> ".haskell-agent/settings.json") "{\"model\":\"other\"}"
            check ".haskell-agent/" `shouldThrow` anyIOException
    it "retains additional agent-directory state" $
        fixture $ \primary checkout check -> do
            copies primary checkout ".haskell-agent/settings.json" "{}"
            writeFile (checkout </> ".haskell-agent/notes") "unique"
            check ".haskell-agent/" `shouldThrow` anyIOException
    it "retains settings when the primary copy is absent" $
        fixture $ \_ checkout check -> do
            Directory.createDirectory (checkout </> ".haskell-agent")
            writeFile (checkout </> ".haskell-agent/settings.json") "{}"
            check ".haskell-agent/" `shouldThrow` anyIOException
    it "accepts duplicate generated provider data but retains differences" $
        fixture $ \primary checkout check -> do
            forM_ ["models.json", "prompt.md"] $ \name -> do
                let relative = "packages/agent-openai/data/" <> name
                copies primary checkout relative "data"
                check relative `shouldReturn` ()
                writeFile (checkout </> relative) "unique"
                check relative `shouldThrow` anyIOException
    it "rejects symlinked duplicate files" $
        fixture $ \primary checkout check -> do
            copies primary checkout ".haskell-agent/settings.json" "{}"
            Directory.removeFile (checkout </> ".haskell-agent/settings.json")
            Posix.createSymbolicLink (primary </> ".haskell-agent/settings.json")
                (checkout </> ".haskell-agent/settings.json")
            check ".haskell-agent/" `shouldThrow` anyIOException
    it "rejects symlinked parent directories" $
        fixture $ \primary checkout check -> do
            copies primary primary ".haskell-agent/settings.json" "{}"
            Posix.createSymbolicLink (primary </> ".haskell-agent") (checkout </> ".haskell-agent")
            check ".haskell-agent/" `shouldThrow` anyIOException
    it "accepts a Nix result symlink but rejects other targets and regular files" $
        fixture $ \_ checkout check -> do
            let path = checkout </> "result"
            Posix.createSymbolicLink ("/nix/store/" <> replicate 32 'a' <> "-output") path
            check "result" `shouldReturn` ()
            Directory.removeFile path
            Posix.createSymbolicLink "../valuable" path
            check "result" `shouldThrow` anyIOException
            Directory.removeFile path
            writeFile path "unique"
            check "result" `shouldThrow` anyIOException
    it "retains unknown ignored files and release artifacts" $
        fixture $ \_ _ check -> do
            check ".env" `shouldThrow` anyIOException
            check "release-output/" `shouldThrow` anyIOException
    it "retains existing recognized cache-directory behavior" $
        fixture $ \_ checkout check -> do
            Directory.createDirectory (checkout </> "dist-newstyle")
            check "dist-newstyle/" `shouldReturn` ()
    it "rejects global ignore evidence even for recognized paths" $
        fixture $ \primary checkout _ -> do
            Directory.createDirectory (checkout </> "dist-newstyle")
            checkIgnoredPath (response primary "/global/excludes") checkout "dist-newstyle/"
                `shouldThrow` anyIOException

fixture :: (FilePath -> FilePath -> (FilePath -> IO ()) -> IO ()) -> IO ()
fixture action = withSystemTempDirectory "worktree-ignored-spec-" $ \root -> do
    let primary = root </> "primary"
        checkout = root </> "checkout"
    Directory.createDirectory primary
    Directory.createDirectory checkout
    Directory.createDirectory (primary </> ".git")
    action primary checkout (checkIgnoredPath (response primary ".gitignore") checkout)

response :: FilePath -> FilePath -> [String] -> String -> IO String
response primary source arguments input
    | take 1 arguments == ["check-ignore"] = pure (source <> "\0" <> "1\0*\0" <> input)
    | take 1 arguments == ["rev-parse"] = pure (primary </> ".git" <> "\n")
    | otherwise = fail "unexpected Git command"

copies :: FilePath -> FilePath -> FilePath -> String -> IO ()
copies primary checkout relative contents =
    forM_ [primary, checkout] $ \root -> do
        Directory.createDirectoryIfMissing True (takeDirectory (root </> relative))
        writeFile (root </> relative) contents
