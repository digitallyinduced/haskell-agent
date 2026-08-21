module Agent.CLI.SkillsSpec (spec) where

import Agent.CLI.Command (SkillCommand(..))
import Agent.CLI.Options (CliOptions(..), defaultCliOptions)
import Agent.CLI.Skills
import Agent.OsPath (fromFilePath)
import Agent.Skills
import Data.IORef (newIORef, readIORef)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.Skills" do
    it "disables discovery with --no-skills" do
        catalog <- loadSkillsCatalog
            defaultCliOptions { optSkills = False }
            (fromFilePath "/tmp")
            (fromFilePath "/tmp")
            (fromFilePath "/tmp")
            False
        catalog `shouldBe` SkillCatalog [] []

    it "queues skill metadata after existing startup context" do
        context <- newIORef (Just "agents")
        queueSkillCatalogContext context (SkillCatalog [fakeSkill] [])
        readIORef context >>= \case
            Nothing -> expectationFailure "expected startup context"
            Just text -> do
                text `shouldSatisfy` Text.isPrefixOf "agents\n\n## Skills"
                text `shouldSatisfy` Text.isInfixOf "/tmp/deploy/SKILL.md"

    it "maps invocation metadata into a slash command" do
        let invocation = SkillInvocation "deploy" fakeSkill True
        skillInvocationCommand invocation `shouldBe` SkillCommand
            { skillCommandName = "deploy"
            , skillCommandSummary = "Deploy the service"
            , skillCommandArgumentHint = Just "<environment>"
            , skillCommandSource = "user · agents"
            }

    it "installs a deferred catalog, invocations, and startup context together" do
        context <- newIORef (Just "agents")
        catalogRef <- newIORef (SkillCatalog [] [])
        invocationsRef <- newIORef []
        let catalog = SkillCatalog [fakeSkill] []
        installSkillCatalog
            ["help"] True context catalogRef invocationsRef catalog
        readIORef catalogRef `shouldReturn` catalog
        readIORef invocationsRef `shouldReturn`
            [SkillInvocation "deploy" fakeSkill True]
        readIORef context >>= \case
            Nothing -> expectationFailure "expected startup context"
            Just text ->
                text `shouldSatisfy` Text.isPrefixOf "agents\n\n## Skills"

fakeSkill :: Skill
fakeSkill = Skill
    { skillName = "deploy"
    , skillDescription = "Deploy the service"
    , skillDisplayName = Nothing
    , skillShortDescription = Nothing
    , skillDefaultPrompt = Nothing
    , skillWhenToUse = Nothing
    , skillArgumentHint = Just "<environment>"
    , skillUserInvocable = True
    , skillModelInvocable = True
    , skillAllowedTools = []
    , skillModelOverride = Nothing
    , skillEffortOverride = Nothing
    , skillLicense = Nothing
    , skillCompatibility = Nothing
    , skillMetadata = mempty
    , skillPath = fromFilePath "/tmp/deploy/SKILL.md"
    , skillDirectory = fromFilePath "/tmp/deploy"
    , skillBody = "Deploy."
    , skillFileText = "---\nname: deploy\ndescription: Deploy the service\n---\nDeploy."
    , skillScope = UserSkill
    , skillOrigin = AgentSkills
    }
