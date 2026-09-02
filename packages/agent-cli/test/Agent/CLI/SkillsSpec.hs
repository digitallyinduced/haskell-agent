module Agent.CLI.SkillsSpec (spec) where

import Agent.CLI.Command (SkillCommand(..))
import Agent.CLI.Options (CliOptions(..), defaultCliOptions)
import Agent.CLI.Skills
import Agent.Skills
import Agent.Tools.IO (resolveForRead, resolveUnderCwd)
import Agent.Tools.Types (defaultToolEnv)
import Data.Either (isLeft, isRight)
import Data.IORef (newIORef, readIORef)
import qualified Data.Text as Text
import System.Directory (doesFileExist, findExecutable, getCurrentDirectory)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode(..))
import qualified System.FilePath as FilePath
import System.OsPath (takeDirectory, unsafeEncodeUtf, (</>))
import System.Process (readProcessWithExitCode)
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

    it "allows packaged skills and their shared resume reader but not the parent directory" do
        catalog <- loadSkillsCatalog
            defaultCliOptions
            (fromFilePath "/tmp")
            (fromFilePath "/tmp")
            (fromFilePath "/tmp")
            False
        env <- defaultToolEnv (fromFilePath "/tmp")
        installSkillToolRoots env catalog
        telegram <- case
                filter ((== "telegram-agent") . (.skillName))
                    catalog.catalogSkills of
            [skill] -> pure skill
            skills ->
                expectationFailure
                    ("expected one packaged Telegram skill, got "
                        <> show (length skills))
                    >> fail "unreachable"
        resolveForRead env telegram.skillPath
            >>= (`shouldSatisfy` isRight)
        resolveForRead env (takeDirectory telegram.skillDirectory)
            >>= (`shouldSatisfy` isLeft)
        resolveUnderCwd env telegram.skillPath
            >>= (`shouldSatisfy` isLeft)
        let sharedResumeDirectory =
                takeDirectory telegram.skillDirectory
                    </> fromFilePath "shared/resume-session"
        resolveForRead env (sharedResumeDirectory </> fromFilePath "CORE.md")
            >>= (`shouldSatisfy` isRight)
        resolveForRead
            env
            (sharedResumeDirectory </> fromFilePath "session_reader.py")
            >>= (`shouldSatisfy` isRight)

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

    it "loads the packaged learn-about-user skill" do
        catalog <- loadSkillsCatalog
            defaultCliOptions
            (fromFilePath "/tmp")
            (fromFilePath "/tmp")
            (fromFilePath "/tmp")
            False
        let matching =
                filter ((== "learn-about-user") . (.skillName))
                    catalog.catalogSkills
        map (.skillScope) matching `shouldBe` [BuiltinSkill]
        map (.skillModelInvocable) matching `shouldBe` [True]
        map (.skillUserInvocable) matching `shouldBe` [True]

    it "loads the packaged external session resume skills" do
        catalog <- loadSkillsCatalog
            defaultCliOptions
            (fromFilePath "/tmp")
            (fromFilePath "/tmp")
            (fromFilePath "/tmp")
            False
        let resumeNames =
                [ "resume-claude"
                , "resume-codex"
                , "resume-cursor"
                , "resume-grok"
                ]
            matching =
                filter ((`elem` resumeNames) . (.skillName))
                    catalog.catalogSkills
        map (.skillName) matching `shouldMatchList` resumeNames
        map (.skillScope) matching `shouldBe` replicate 4 BuiltinSkill
        map (.skillModelInvocable) matching `shouldBe` replicate 4 True
        map (.skillUserInvocable) matching `shouldBe` replicate 4 True
        let commands =
                filter
                    ((`elem` resumeNames) . (.skillCommandName))
                    ( map skillInvocationCommand
                        (buildSkillInvocations reservedSlashNames catalog)
                    )
        map (.skillCommandName) commands `shouldMatchList` resumeNames

    it "passes the external session reader regression suite" do
        python <- findExecutable "python3" >>= \case
            Nothing -> expectationFailure "python3 is required by resume skills"
                >> fail "unreachable"
            Just executable -> pure executable
        testPath <- findSessionReaderTest
        (exitCode, output, errors) <-
            readProcessWithExitCode python ["-B", testPath] ""
        case exitCode of
            ExitSuccess -> pure ()
            ExitFailure code ->
                expectationFailure
                    ( "session reader regression suite failed with exit code "
                        <> show code
                        <> "\n"
                        <> output
                        <> errors
                    )

    it "loads the packaged post-task review as always-active context" do
        catalog <- loadSkillsCatalog
            defaultCliOptions
            (fromFilePath "/tmp")
            (fromFilePath "/tmp")
            (fromFilePath "/tmp")
            False
        let matching =
                filter ((== "post-task-learning-review") . (.skillName))
                    catalog.catalogSkills
        map (.skillScope) matching `shouldBe` [BuiltinSkill]
        map (.skillContextMode) matching `shouldBe` [SkillContextAlways]
        case formatSkillCatalogContext 8000 catalog of
            (Just context, _) -> do
                context `shouldSatisfy`
                    Text.isInfixOf
                        "Always-active skill: post-task-learning-review"
                context `shouldSatisfy`
                    Text.isInfixOf "Before the final response"
            other -> expectationFailure ("unexpected skill context: " <> show other)

    it "queues skill metadata after existing startup context" do
        context <- newIORef (Just "agents")
        _ <- queueSkillCatalogContextWithOmissions
            context
            (SkillCatalog [fakeSkill] [])
        readIORef context >>= \case
            Nothing -> expectationFailure "expected startup context"
            Just text -> do
                text `shouldSatisfy` Text.isPrefixOf "agents\n\n## Skills"
                text `shouldSatisfy` Text.isInfixOf
                    "call `view_skill` with the listed name"
                text `shouldSatisfy` Text.isInfixOf "$deploy: Deploy the service"
                text `shouldSatisfy`
                    (not . Text.isInfixOf "/tmp/deploy/SKILL.md")

    it "maps invocation metadata into a slash command" do
        let invocation = SkillInvocation "deploy" fakeSkill True
        skillInvocationCommand invocation `shouldBe` SkillCommand
            { skillCommandName = "deploy"
            , skillCommandSummary = "Deploy the service"
            , skillCommandArgumentHint = Just "<environment>"
            , skillCommandSource = "user · agents"
            }

    it "lists Codex dollar syntax before the slash compatibility alias" do
        let invocation = SkillInvocation "deploy" fakeSkill True
            listing =
                formatSkillsListing
                    False
                    (SkillCatalog [fakeSkill] [])
                    [invocation]
        listing `shouldSatisfy` Text.isInfixOf "$deploy, /deploy"

    it "does not interpret dollar-prefixed SQL parameters in pasted prompts" do
        let invocations = [SkillInvocation "deploy" fakeSkill True]
            pastedSql = "WHERE listings.agent_id = $1"
        resolvePromptSkillMentions True invocations pastedSql
            `shouldBe` Right []
        resolvePromptSkillMentions False invocations pastedSql
            `shouldSatisfy` isLeft
        resolvePromptSkillMentions False invocations "please $deploy"
            `shouldBe` Right invocations

    it "installs a deferred catalog, invocations, and startup context together" do
        context <- newIORef (Just "agents")
        catalogRef <- newIORef (SkillCatalog [] [])
        invocationsRef <- newIORef []
        let catalog = SkillCatalog [fakeSkill] []
        _ <- installSkillCatalogWithOmissions
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
    , skillContextMode = SkillContextOnDemand
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

findSessionReaderTest :: IO FilePath
findSessionReaderTest = do
    cwd <- getCurrentDirectory
    executable <- getExecutablePath
    firstExisting
        [ candidate
        | root <- ancestors cwd <> ancestors (FilePath.takeDirectory executable)
        , candidate <-
            [ root FilePath.</> "test" FilePath.</> "session_reader_test.py"
            , root
                FilePath.</> "packages"
                FilePath.</> "agent-cli"
                FilePath.</> "test"
                FilePath.</> "session_reader_test.py"
            ]
        ]
  where
    ancestors path =
        let parent = FilePath.takeDirectory path
        in path : if parent == path then [] else ancestors parent
    firstExisting [] =
        expectationFailure "could not locate test/session_reader_test.py"
            >> fail "unreachable"
    firstExisting (path : paths) =
        doesFileExist path >>= \case
            True -> pure path
            False -> firstExisting paths
