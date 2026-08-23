module Agent.CLI.DialectsSpec (spec) where

import Agent.CLI.Dialects
    ( CodingTools(..)
    , codingToolsFor
    , formatAgentsMdForDialect
    , globalAgentsHomeDir
    )
import Agent.Dialect
    ( codexDialect
    , genericResponsesDialect
    , grokBuildDialect
    )
import Agent.ProjectInstructions (InstructionFile(..), LoadedAgentsMd(..))
import Agent.Tools.Secret (SecretPromptHooks(..))
import Agent.Tools.Types (AppTool(..), ToolEnv, defaultToolEnv)
import Control.Exception.Safe (bracket, finally)
import Control.Monad (forM_)
import qualified Data.Text as Text
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
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

    it "uses each dialect's compatibility instruction home" do
        let home = unsafeEncodeUtf "/home/u"
        globalAgentsHomeDir codexDialect home
            `shouldBe` unsafeEncodeUtf "/home/u/.codex"
        globalAgentsHomeDir grokBuildDialect home
            `shouldBe` unsafeEncodeUtf "/home/u/.grok"
        globalAgentsHomeDir genericResponsesDialect home
            `shouldBe` unsafeEncodeUtf "/home/u/.haskell-agent"

    it "registers ask_secret only when root prompt hooks are supplied" do
        withTempToolEnv \env ->
            forM_ [codexDialect, grokBuildDialect] \dialect -> do
                withoutSecret <-
                    codingToolsFor dialect env Nothing Nothing Nothing
                map (.appToolName) withoutSecret.codingAppTools
                    `shouldNotContain` ["ask_secret"]
                withoutSecret.codingClose

                let hooks = SecretPromptHooks
                        (const (pure (Right Nothing)))
                withSecret <-
                    codingToolsFor dialect env Nothing (Just hooks) Nothing
                (map (.appToolName) withSecret.codingAppTools
                    `shouldContain` ["ask_secret"])
                    `finally` withSecret.codingClose

withTempToolEnv :: (ToolEnv -> IO a) -> IO a
withTempToolEnv action = do
    root <- getTemporaryDirectory
    bracket
        (mkdtemp (root </> "agent-cli-dialects-"))
        removeDirectoryRecursive
        (\directory ->
            defaultToolEnv (unsafeEncodeUtf directory) >>= action)
