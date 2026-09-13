module Agent.CLI.DialectsSpec (spec) where

import Agent.CLI.Options (defaultCliOptions)
import Agent.CLI.StartupContext
    ( AgentsContextNotice(SuppressAgentsContextLoaded)
    , loadAgentsContext
    , loadAgentsContextWithPreload
    , preloadAgentsContext
    )
import Agent.Dialect (codexDialect)
import Control.Exception.Safe (bracket)
import qualified Data.ByteString as BS
import Data.IORef (IORef, readIORef)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Directory
    ( createDirectoryIfMissing
    , getTemporaryDirectory
    , removeDirectoryRecursive
    )
import System.FilePath ((</>))
import System.IO (Handle, IOMode(WriteMode), hClose, openFile)
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "CLI dialect startup context" do
    it "keeps synchronous and preloaded AGENTS context equivalent" do
        withTempDirectory \directory -> do
            let home = directory </> "home"
                project = directory </> "project"
                nested = project </> "nested"
                homePath = unsafeEncodeUtf home
                nestedPath = unsafeEncodeUtf nested
                extraContext = Just "<environment_context>fixture</environment_context>"
            createDirectoryIfMissing True home
            createDirectoryIfMissing True (project </> ".git")
            createDirectoryIfMissing True nested
            writeFile (project </> "AGENTS.md") "root instructions\n"
            -- Invalid UTF-8 exercises deferred warning reporting as well as
            -- successful parent-directory instruction formatting.
            BS.writeFile (nested </> "AGENTS.md") (BS.pack [0xff, 0xfe])

            preloaded <-
                preloadAgentsContext
                    defaultCliOptions
                    codexDialect
                    homePath
                    nestedPath
            synchronous <-
                captureAgentsContext
                    (directory </> "synchronous.stderr")
                    \stderrHandle ->
                        loadAgentsContext
                            stderrHandle
                            Nothing
                            SuppressAgentsContextLoaded
                            defaultCliOptions
                            codexDialect
                            homePath
                            nestedPath
                            []
                            Nothing
                            extraContext
            fromPreload <-
                captureAgentsContext
                    (directory </> "preloaded.stderr")
                    \stderrHandle ->
                        loadAgentsContextWithPreload
                            stderrHandle
                            Nothing
                            SuppressAgentsContextLoaded
                            defaultCliOptions
                            codexDialect
                            homePath
                            nestedPath
                            []
                            Nothing
                            extraContext
                            preloaded

            fromPreload `shouldBe` synchronous
            fst fromPreload
                `shouldSatisfy`
                    maybe False (Text.isInfixOf "root instructions")
            snd fromPreload
                `shouldSatisfy`
                    Text.isInfixOf "agents.md ignored:"

    it "discards an unused AGENTS preload without reporting its warnings" do
        withTempDirectory \directory -> do
            let home = directory </> "home"
                project = directory </> "project"
                homePath = unsafeEncodeUtf home
                projectPath = unsafeEncodeUtf project
            createDirectoryIfMissing True home
            createDirectoryIfMissing True (project </> ".git")
            BS.writeFile (project </> "AGENTS.md") (BS.pack [0xff])
            preloaded <-
                preloadAgentsContext
                    defaultCliOptions
                    codexDialect
                    homePath
                    projectPath
            (context, warnings) <-
                captureAgentsContext
                    (directory </> "discarded.stderr")
                    \stderrHandle ->
                        loadAgentsContextWithPreload
                            stderrHandle
                            Nothing
                            SuppressAgentsContextLoaded
                            defaultCliOptions
                            codexDialect
                            homePath
                            projectPath
                            []
                            (Just "response-1")
                            Nothing
                            preloaded
            context `shouldBe` Nothing
            warnings `shouldBe` ""

withTempDirectory :: (FilePath -> IO a) -> IO a
withTempDirectory action = do
    root <- getTemporaryDirectory
    bracket
        (mkdtemp (root </> "agent-cli-dialects-"))
        removeDirectoryRecursive
        action

captureAgentsContext
    :: FilePath
    -> (Handle -> IO (IORef (Maybe Text.Text)))
    -> IO (Maybe Text.Text, Text.Text)
captureAgentsContext outputPath action = do
    contextRef <-
        bracket
            (openFile outputPath WriteMode)
            hClose
            action
    context <- readIORef contextRef
    output <- TextIO.readFile outputPath
    pure (context, output)
