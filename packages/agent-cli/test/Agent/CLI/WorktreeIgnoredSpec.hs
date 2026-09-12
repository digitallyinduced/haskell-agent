module Agent.CLI.WorktreeIgnoredSpec (spec) where

import Agent.CLI.Worktree.Ignored (checkIgnoredPath, inspectCabalArtifacts, removeCabalArtifacts)
import qualified Data.ByteString as BS
import System.Process (readProcessWithExitCode, readCreateProcessWithExitCode, proc, CreateProcess(..))
import System.Exit (ExitCode(..))
import Control.Monad (forM_, unless)
import qualified System.Directory as Directory
import System.FilePath ((</>), takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import qualified System.Posix.Files as Posix
import Test.Hspec

spec :: Spec
spec = describe "ignored worktree paths" $ do
    describe "independent Cabal artifact cleanup" $ do
        it "recognizes component executables and dynamic libraries but not arbitrary binary data" $
            artifactFixture $ \repository git -> do
                let component = "dist-newstyle/build/aarch64-osx/ghc-9.10.3/example-0.1/x/example/build/example"
                Directory.createDirectoryIfMissing True (repository </> component)
                forM_ [component </> "example", component </> "libexample.dylib", "dist-newstyle/build/unrelated"] $ \relative ->
                    BS.writeFile (repository </> relative) (BS.pack [0xcf, 0xfa, 0xed, 0xfe, 0, 0, 0, 0])
                inventory <- inspectCabalArtifacts git repository
                map fst inventory `shouldContain` [component </> "example", component </> "libexample.dylib"]
                map fst inventory `shouldNotContain` ["dist-newstyle/build/unrelated"]
        it "supports an ordinary Cabal rebuild after artifact removal" $
            artifactFixture $ \repository git -> do
                writeFile (repository </> "example.cabal") "cabal-version: 3.0\nname: example\nversion: 0.1\nbuild-type: Simple\nexecutable example\n  main-is: Main.hs\n  build-depends: base\n  default-language: Haskell2010\n"
                writeFile (repository </> "cabal.project") "packages: .\nactive-repositories: :none\n"
                writeFile (repository </> "Main.hs") "main = putStrLn \"rebuilt\"\n"
                -- Nix tests have an unwritable HOME. Keep Cabal configuration
                -- and caches inside the fixture, independent of user state.
                let cabalConfig = repository </> "cabal-test.config"
                writeFile cabalConfig $ unlines
                    [ "store-dir: " <> repository </> "cabal-store"
                    , "remote-repo-cache: " <> repository </> "cabal-repositories"
                    , "logs-dir: " <> repository </> "cabal-logs"
                    ]
                let cabal arguments = do
                        (code, output, errors) <- readCreateProcessWithExitCode
                            ((proc "cabal" (["--config-file=" <> cabalConfig] <> arguments)) { cwd = Just repository }) ""
                        unless (code == ExitSuccess) (fail (output <> errors))
                        pure output
                _ <- cabal ["build", "--offline", "exe:example"]
                executable <- head . lines <$> cabal ["list-bin", "exe:example"]
                inventory <- inspectCabalArtifacts git repository
                inventory `shouldSatisfy` (not . null)
                removeCabalArtifacts git repository inventory
                Directory.doesFileExist executable `shouldReturn` False
                _ <- cabal ["build", "--offline", "exe:example"]
                Directory.doesFileExist executable `shouldReturn` True
        it "reports bytes without deleting and removes only recognized ignored objects" $
            artifactFixture $ \repository git -> do
                inventory <- inspectCabalArtifacts git repository
                inventory `shouldBe` [("dist-newstyle/build/Example.o", 8)]
                Directory.doesFileExist (repository </> "dist-newstyle/build/Example.o") `shouldReturn` True
                removeCabalArtifacts git repository inventory
                Directory.doesFileExist (repository </> "dist-newstyle/build/Example.o") `shouldReturn` False
                forM_ ["dist-newstyle/build/notes.o", "dist-newstyle/build/Source.hs", ".haskell-agent/settings.json", ".build/checkouts/Dependency/Source.swift", "dist-newstyle/src/Dependency.o"] $ \relative ->
                    Directory.doesFileExist (repository </> relative) `shouldReturn` True
        it "rejects force-added tracked output" $
            artifactFixture $ \repository git -> do
                _ <- git ["add", "-f", "dist-newstyle/build/Example.o"] ""
                inspectCabalArtifacts git repository `shouldThrow` anyIOException
        it "refuses unknown build-monitor data without removing binary output" $
            artifactFixture $ \repository git -> do
                let component = "dist-newstyle/build/aarch64-osx/ghc-9.10.3/example-0.1"
                Directory.createDirectoryIfMissing True (repository </> component </> "build")
                Directory.createDirectoryIfMissing True (repository </> component </> "cache")
                BS.writeFile (repository </> component </> "build/Example.o") (BS.pack [0xcf, 0xfa, 0xed, 0xfe, 0, 0, 0, 0])
                writeFile (repository </> component </> "cache/build") "user data"
                inspectCabalArtifacts git repository `shouldThrow` anyIOException
                readFile (repository </> component </> "cache/build") `shouldReturn` "user data"
                Directory.doesFileExist (repository </> component </> "build/Example.o") `shouldReturn` True
        it "preserves symlinked files and never traverses symlink directories" $
            artifactFixture $ \repository git -> do
                Directory.removeFile (repository </> "dist-newstyle/build/Example.o")
                Posix.createSymbolicLink "../src/Dependency.o" (repository </> "dist-newstyle/build/Example.o")
                Posix.createSymbolicLink "../src" (repository </> "dist-newstyle/build/dependency")
                inspectCabalArtifacts git repository `shouldReturn` []
        it "rejects symlinked output roots" $
            artifactFixture $ \repository git -> do
                Directory.renameDirectory (repository </> "dist-newstyle") (repository </> "output")
                Posix.createSymbolicLink "output" (repository </> "dist-newstyle")
                inspectCabalArtifacts git repository `shouldThrow` anyIOException
        it "preserves hard-linked objects" $
            artifactFixture $ \repository git -> do
                Posix.createLink (repository </> "dist-newstyle/build/Example.o") (repository </> "retained.o")
                inspectCabalArtifacts git repository `shouldReturn` []
        it "refuses changed inventories before deleting anything" $
            artifactFixture $ \repository git -> do
                inventory <- inspectCabalArtifacts git repository
                writeFile (repository </> "dist-newstyle/build/Example.o") "changed"
                removeCabalArtifacts git repository inventory `shouldThrow` anyIOException
                readFile (repository </> "dist-newstyle/build/Example.o") `shouldReturn` "changed"
        it "does not accept global or negated ignore rules" $
            artifactFixture $ \repository _ -> do
                let evidence source patternText arguments input = pure $ if take 1 arguments == ["ls-files"] then "" else source <> "\0" <> "1\0" <> patternText <> "\0" <> input
                inspectCabalArtifacts (evidence "/global/.gitignore" "*") repository `shouldReturn` []
                inspectCabalArtifacts (evidence ".gitignore" "!*.o") repository `shouldReturn` []
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

artifactFixture :: (FilePath -> ([String] -> String -> IO String) -> IO ()) -> IO ()
artifactFixture action = withSystemTempDirectory "worktree-artifact-spec-" $ \repository -> do
    let git arguments input = do
            (code, output, errors) <- readProcessWithExitCode "git" (["-C", repository] <> arguments) input
            case code of
                ExitSuccess -> pure output
                ExitFailure 1 | take 1 arguments == ["check-ignore"] -> pure ""
                _ -> fail errors
    _ <- git ["init", "-q"] ""
    writeFile (repository </> ".gitignore") "dist-newstyle/\n.build/\n.haskell-agent/\n"
    forM_ ["dist-newstyle/build", "dist-newstyle/src", ".haskell-agent", ".build/checkouts/Dependency"] $ \relative ->
        Directory.createDirectoryIfMissing True (repository </> relative)
    forM_ ["dist-newstyle/build/Example.o", "dist-newstyle/src/Dependency.o"] $ \relative ->
        BS.writeFile (repository </> relative) (BS.pack [0xcf, 0xfa, 0xed, 0xfe, 0, 0, 0, 0])
    forM_ ["dist-newstyle/build/notes.o", "dist-newstyle/build/Source.hs", ".haskell-agent/settings.json", ".build/checkouts/Dependency/Source.swift"] $ \relative ->
        writeFile (repository </> relative) "retained source or configuration"
    action repository git

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
