module Agent.CLI.DialectsSpec (spec) where

import Agent.CLI.Dialects
    ( CodingTools(..)
    , codingToolsFor
    , formatAgentsMdForDialect
    , globalAgentsHomeDir
    )
import Agent.Dialect
    ( claudeCodeDialect
    , codexDialect
    , genericResponsesDialect
    , grokBuildDialect
    )
import Agent.ProjectInstructions (InstructionFile(..), LoadedAgentsMd(..))
import Agent.Tools.Types (defaultToolEnv)
import qualified Data.Text as Text
import System.OsPath (unsafeEncodeUtf)
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

    it "does not allocate host tools for Claude Code" do
        env <- defaultToolEnv (unsafeEncodeUtf "/tmp")
        coding <- codingToolsFor claudeCodeDialect env Nothing Nothing
        length coding.codingAppTools `shouldBe` 0
        coding.codingClose
