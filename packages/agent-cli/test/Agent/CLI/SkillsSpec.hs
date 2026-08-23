module Agent.CLI.SkillsSpec (spec) where

import Agent.CLI.Command (SkillCommand(..))
import Agent.CLI.Options (CliOptions(..), defaultCliOptions)
import Agent.CLI.SessionState (SessionAccountState(..), SessionState(..))
import Agent.CLI.Skills
import Agent.CLI.TurnState (ConversationState(..))
import Agent.Loop (emptyTokenUsage)
import System.OsPath (unsafeEncodeUtf)
import Agent.Skills
import Data.IORef (newIORef, readIORef)
import qualified Data.Text as Text
import Test.Hspec

fromFilePath = unsafeEncodeUtf

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

    it "loads the packaged Telegram setup skill" do
        catalog <- loadSkillsCatalog
            defaultCliOptions
            (fromFilePath "/tmp")
            (fromFilePath "/tmp")
            (fromFilePath "/tmp")
            False
        let matching =
                filter ((== "telegram-agent") . (.skillName))
                    catalog.catalogSkills
        map (.skillScope) matching `shouldBe` [BuiltinSkill]
        map (.skillModelInvocable) matching `shouldBe` [True]

    it "loads the packaged add-model skill" do
        catalog <- loadSkillsCatalog
            defaultCliOptions
            (fromFilePath "/tmp")
            (fromFilePath "/tmp")
            (fromFilePath "/tmp")
            False
        let matching =
                filter ((== "add-model") . (.skillName))
                    catalog.catalogSkills
        map (.skillScope) matching `shouldBe` [BuiltinSkill]
        map (.skillModelInvocable) matching `shouldBe` [True]

    it "queues skill metadata after existing startup context" do
        context <- newIORef (testSessionState (Just "agents"))
        _ <- queueSkillCatalogContextWithOmissions
            context
            (SkillCatalog [fakeSkill] [])
        (.sessionConversation.conversationStartupContext)
            <$> readIORef context >>= \case
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
        context <- newIORef (testSessionState (Just "agents"))
        let catalog = SkillCatalog [fakeSkill] []
        _ <- installSkillCatalogWithOmissions
            ["help"] True context catalog
        state <- readIORef context
        state.sessionSkillCatalog `shouldBe` catalog
        state.sessionSkillInvocations `shouldBe`
            [SkillInvocation "deploy" fakeSkill True]
        case state.sessionConversation.conversationStartupContext of
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

testSessionState :: Maybe Text.Text -> SessionState
testSessionState startup =
    SessionState
        { sessionConversation = ConversationState
            { conversationPreviousResponseId = Nothing
            , conversationTranscript = []
            , conversationStartupContext = startup
            , conversationUsage = emptyTokenUsage
            , conversationLastAssistant = Nothing
            }
        , sessionSkillCatalog = SkillCatalog [] []
        , sessionSkillInvocations = []
        , sessionAccount = SessionAccountState
            { accountLabel = ""
            , accountId = ""
            , accountSelectionId = ""
            }
        }
