module Agent.CLI.DialectsSpec (spec) where

import Agent.CLI.Dialects
    ( CodingTools(..)
    , codingToolsFor
    , filterBashTools
    , filterGhciTools
    , formatAgentsMdForDialect
    , globalAgentsHomeDir
    )
import Agent.CLI.Options (defaultCliOptions)
import Agent.CLI.StartupContext
    ( AgentsContextNotice(SuppressAgentsContextLoaded)
    , loadAgentsContext
    , loadAgentsContextWithPreload
    , preloadAgentsContext
    )
import Agent.Dialect
    ( claudeCodeDialect
    , codexDialect
    , genericResponsesDialect
    , grokBuildDialect
    )
import Agent.ProjectInstructions (InstructionFile(..), LoadedAgentsMd(..))
import Agent.ToolDispatch (noArgsTool)
import Agent.Tools.Secret (SecretPromptHooks(..))
import Agent.Tools.ShowImage (ImageDisplayHooks(..))
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(AlwaysReadOnly)
    , ToolEnv
    , appToolsFromGroups
    , defaultToolEnv
    , executionToolsFromGroups
    , hostToolsFromGroups
    , jsonAppTool
    )
import Control.Exception.Safe (bracket, finally)
import Control.Monad (forM_)
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
spec = describe "Agent.CLI.Dialects" do
    it "dispatches project-instruction formatting by dialect" do
        let cwd = unsafeEncodeUtf "/repo"
            loaded = LoadedAgentsMd
                { loadedGlobal = Nothing
                , loadedProject =
                    [InstructionFile (unsafeEncodeUtf "/repo/AGENTS.md") "rules"]
                , loadedWarnings = []
                }
        formatAgentsMdForDialect codexDialect cwd loaded
            `shouldSatisfy`
                maybe False (Text.isPrefixOf "# AGENTS.md instructions")
        formatAgentsMdForDialect grokBuildDialect cwd loaded
            `shouldSatisfy`
                maybe False (Text.isInfixOf "<system-reminder>")
        formatAgentsMdForDialect genericResponsesDialect cwd loaded
            `shouldSatisfy`
                maybe False (Text.isInfixOf "<system-reminder>")
        formatAgentsMdForDialect claudeCodeDialect cwd loaded
            `shouldSatisfy`
                maybe False (Text.isPrefixOf "# AGENTS.md instructions")

    it "uses each dialect's compatibility instruction home" do
        let home = unsafeEncodeUtf "/home/u"
        globalAgentsHomeDir codexDialect home
            `shouldBe` unsafeEncodeUtf "/home/u/.codex"
        globalAgentsHomeDir grokBuildDialect home
            `shouldBe` unsafeEncodeUtf "/home/u/.grok"
        globalAgentsHomeDir genericResponsesDialect home
            `shouldBe` unsafeEncodeUtf "/home/u/.haskell-agent"
        globalAgentsHomeDir claudeCodeDialect home
            `shouldBe` unsafeEncodeUtf "/home/u/.claude"

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

    it "allocates only the AskUserQuestion MCP fallback for Claude Code" do
        env <- defaultToolEnv (unsafeEncodeUtf "/tmp")
        coding <- codingToolsFor claudeCodeDialect env Nothing Nothing Nothing Nothing
        map (.appToolName) coding.codingAppTools
            `shouldBe` ["ask_user_question"]
        coding.codingClose

    it "registers ask_secret only when root prompt hooks are supplied" do
        withTempToolEnv \env ->
            forM_ [codexDialect, grokBuildDialect] \dialect -> do
                withoutSecret <-
                    codingToolsFor dialect env Nothing Nothing Nothing Nothing
                map (.appToolName) withoutSecret.codingAppTools
                    `shouldNotContain` ["ask_secret"]
                withoutSecret.codingClose

                let hooks = SecretPromptHooks
                        (const (pure (Right Nothing)))
                withSecret <-
                    codingToolsFor dialect env Nothing (Just hooks) Nothing Nothing
                (map (.appToolName) withSecret.codingAppTools
                    `shouldContain` ["ask_secret"])
                    `finally` withSecret.codingClose

    it "registers show_image only when image display hooks are supplied" do
        withTempToolEnv \env ->
            forM_ [codexDialect, grokBuildDialect] \dialect -> do
                withoutImages <-
                    codingToolsFor dialect env Nothing Nothing Nothing Nothing
                map (.appToolName) withoutImages.codingAppTools
                    `shouldNotContain` ["show_image"]
                withoutImages.codingClose

                let hooks = ImageDisplayHooks (const (pure (Right ())))
                withImages <-
                    codingToolsFor dialect env Nothing Nothing (Just hooks) Nothing
                (map (.appToolName) withImages.codingAppTools
                    `shouldContain` ["show_image"])
                    `finally` withImages.codingClose

    it "registers bounded artifact readers on coding tool surfaces" do
        withTempToolEnv \env ->
            forM_ [codexDialect, grokBuildDialect] \dialect -> do
                coding <- codingToolsFor dialect env Nothing Nothing Nothing Nothing
                let names = map (.appToolName) coding.codingAppTools
                names `shouldContain`
                    ["read_tool_output", "search_tool_output"]
                names `shouldNotContain` ["analyze_tool_output"]
                coding.codingClose

    it "partitions execution tools from host services when constructed" do
        withTempToolEnv \env ->
            forM_
                [ (codexDialect, "shell_command")
                , (grokBuildDialect, "run_terminal_cmd")
                ]
                \(dialect, shellName) -> do
                    coding <-
                        codingToolsFor
                            dialect env Nothing Nothing Nothing Nothing
                    let names = map (.appToolName)
                        executionNames =
                            names
                                (executionToolsFromGroups
                                    coding.codingAppToolGroups)
                        hostNames =
                            names
                                (hostToolsFromGroups
                                    coding.codingAppToolGroups)
                        assertions = do
                            names
                                (appToolsFromGroups
                                    coding.codingAppToolGroups)
                                `shouldBe` names coding.codingAppTools
                            forM_
                                [ "run_ghci"
                                , "read_file"
                                , shellName
                                , "read_tool_output"
                                , "search_tool_output"
                                ]
                                \name ->
                                    executionNames `shouldContain` [name]
                            executionNames
                                `shouldNotContain` ["ask_user_question"]
                            hostNames `shouldContain` ["ask_user_question"]
                            forM_
                                ["read_file", shellName, "read_tool_output"]
                                \name -> hostNames `shouldNotContain` [name]
                    assertions `finally` coding.codingClose

    it "filters shell and ghci tools independently" do
        let tools = map fakeTool
                [ "run_ghci"
                , "read_file"
                , "shell_command"
                , "write_stdin"
                , "run_terminal_cmd"
                ]
            names = map (.appToolName)
        names (filterBashTools False tools)
            `shouldBe` ["run_ghci", "read_file"]
        names (filterGhciTools False tools)
            `shouldBe`
                [ "read_file"
                , "shell_command"
                , "write_stdin"
                , "run_terminal_cmd"
                ]

withTempToolEnv :: (ToolEnv -> IO a) -> IO a
withTempToolEnv action = do
    withTempDirectory \directory ->
        defaultToolEnv (unsafeEncodeUtf directory) >>= action

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

fakeTool :: Text.Text -> AppTool
fakeTool name =
    jsonAppTool name "" [] AlwaysReadOnly
        (noArgsTool name (pure (Right "")))
