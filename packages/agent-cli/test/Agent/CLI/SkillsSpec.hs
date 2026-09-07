module Agent.CLI.SkillsSpec (spec) where

import Agent.CLI.Command (SkillCommand(..))
import Agent.CLI.Options (CliOptions(..), defaultCliOptions)
import Agent.CLI.Skills
import Agent.Json (rawJsonFromEncoding)
import Agent.MCP.Types
    ( McpResourceContent(..)
    , McpSkillEntry(..)
    , McpSkillResource(..)
    , McpSkillResources(..)
    )
import Agent.Skills
import Agent.Tools.IO (resolveForRead, resolveUnderCwd)
import Agent.Tools.Types (defaultToolEnv)
import Data.Aeson qualified as Aeson
import Data.Either (isLeft, isRight)
import Data.IORef (newIORef, readIORef)
import qualified Data.Text as Text
import System.OsPath (OsPath, takeDirectory, unsafeEncodeUtf, (</>))
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
        map skillScopeOf matching `shouldBe` [BuiltinSkill]
        map (.skillModelInvocable) matching `shouldBe` [True]

    it "allows packaged skills and their shared resume guidance but not the parent directory" do
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
        let (telegramPath, telegramDirectory) = filesystemPaths telegram
        resolveForRead env telegramPath
            >>= (`shouldSatisfy` isRight)
        resolveForRead env (takeDirectory telegramDirectory)
            >>= (`shouldSatisfy` isLeft)
        resolveUnderCwd env telegramPath
            >>= (`shouldSatisfy` isLeft)
        let sharedResumeDirectory =
                takeDirectory telegramDirectory
                    </> fromFilePath "shared/resume-session"
        resolveForRead env (sharedResumeDirectory </> fromFilePath "CORE.md")
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
        map skillScopeOf matching `shouldBe` [BuiltinSkill]
        map (.skillModelInvocable) matching `shouldBe` [True]

    it "loads the packaged wait-for-ci skill" do
        catalog <- loadSkillsCatalog
            defaultCliOptions
            (fromFilePath "/tmp")
            (fromFilePath "/tmp")
            (fromFilePath "/tmp")
            False
        let matching =
                filter ((== "wait-for-ci") . (.skillName))
                    catalog.catalogSkills
        map skillScopeOf matching `shouldBe` [BuiltinSkill]
        map (.skillModelInvocable) matching `shouldBe` [True]
        map (.skillUserInvocable) matching `shouldBe` [True]
        map (.skillWhenToUse) matching `shouldBe`
            [ Just
                "Apply after a push or pull-request update when CI checks are running, queued, or expected and the task depends on their result."
            ]

    it "loads the packaged skill-installer skill" do
        catalog <- loadSkillsCatalog
            defaultCliOptions
            (fromFilePath "/tmp")
            (fromFilePath "/tmp")
            (fromFilePath "/tmp")
            False
        let matching =
                filter ((== "skill-installer") . (.skillName))
                    catalog.catalogSkills
        map skillScopeOf matching `shouldBe` [BuiltinSkill]
        map (.skillModelInvocable) matching `shouldBe` [True]
        map (.skillUserInvocable) matching `shouldBe` [True]
        map (.skillWhenToUse) matching `shouldBe`
            [ Just
                "Use when the user asks to install a skill, add a SKILL.md from GitHub or a gist, list installable skills, or copy a skill into the local skills directory."
            ]

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
        map skillScopeOf matching `shouldBe` [BuiltinSkill]
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
        map skillScopeOf matching `shouldBe` replicate 4 BuiltinSkill
        map (.skillModelInvocable) matching `shouldBe` replicate 4 True
        map (.skillUserInvocable) matching `shouldBe` replicate 4 True
        let commands =
                filter
                    ((`elem` resumeNames) . (.skillCommandName))
                    ( map skillInvocationCommand
                        (buildSkillInvocations reservedSlashNames catalog)
                    )
        map (.skillCommandName) commands `shouldMatchList` resumeNames

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
        map skillScopeOf matching `shouldBe` [BuiltinSkill]
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
            `shouldBe` Right []
        resolvePromptSkillMentions False invocations "please $deploy"
            `shouldBe` Right invocations

    it "warns about unknown skill mentions without rejecting the prompt" do
        let invocations = [SkillInvocation "deploy" fakeSkill True]
        resolvePromptSkillMentionsWithWarnings
            False
            invocations
            "compare $missing with $deploy"
            `shouldBe`
                ( [ "unknown skill: missing"
                        <> " (available: deploy)"
                  ]
                , invocations
                )

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

    it "keeps local bare-name precedence and qualifies MCP skills" do
        let catalog =
                mergeSkillCatalogs
                    (SkillCatalog [fakeSkill] [])
                    (SkillCatalog [remoteSkill] [])
            invocations = buildSkillInvocations reservedSlashNames catalog
        map (.invocationName) invocations
            `shouldMatchList`
                ["deploy", "user:deploy", "mcp-demo-server:deploy"]
        map (.invocationName)
            (filter (.invocationBare) invocations)
            `shouldBe` ["deploy"]

    it "does not resolve MCP skill instructions without a live fleet" do
        resolveSkillContent Nothing remoteSkill
            `shouldReturn` Left "MCP skill server is unavailable"

    it "verifies MCP skill identity, size, digest, and complete document" do
        case verifyMcpSkillContent
                remoteSkill
                "demo server"
                remoteEntry
                [remoteContent] of
            Left err -> expectationFailure (Text.unpack err)
            Right content -> do
                content.skillContentBody `shouldBe` "Deploy remotely."
                content.skillContentFile `shouldBe`
                    "skill://demo/deploy/SKILL.md"
                content.skillContentDirectory `shouldBe` Nothing
                content.skillContentResourceUris `shouldBe`
                    ["skill://demo/deploy/SKILL.md"]

    it "rejects MCP skill content whose digest changed after discovery" do
        verifyMcpSkillContent
            remoteSkill
            "demo server"
            remoteEntry
            [ remoteContent
                { mcpResourceText =
                    Just
                        (Text.replace
                            "Deploy remotely."
                            "deploy remotely."
                            remoteDocument)
                }
            ]
            `shouldBe`
                Left "MCP SKILL.md SHA-256 digest does not match its manifest"

    it "rejects duplicate SKILL.md manifest entries" do
        let duplicateEntry =
                remoteEntry
                    { mcpSkillResources =
                        case remoteEntry.mcpSkillResources of
                            McpSkillResourcesListed resources ->
                                McpSkillResourcesListed (resources <> resources)
                            McpSkillResourcesDynamic ->
                                error "remoteEntry must use listed resources"
                    }
        verifyMcpSkillContent
            remoteSkill
            "demo server"
            duplicateEntry
            [remoteContent]
            `shouldBe`
                Left "MCP skill manifest contains duplicate SKILL.md entries"

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
    , skillSource = FilesystemSkillSource
        { skillPath = fromFilePath "/tmp/deploy/SKILL.md"
        , skillDirectory = fromFilePath "/tmp/deploy"
        , skillBody = "Deploy."
        , skillFileText =
            "---\nname: deploy\ndescription: Deploy the service\n---\nDeploy."
        , skillScope = UserSkill
        , skillOrigin = AgentSkills
        }
    }

remoteSkill :: Skill
remoteSkill =
    fakeSkill
        { skillArgumentHint = Nothing
        , skillSource =
            McpSkillSource
                "demo server"
                "skill://demo/deploy/SKILL.md"
                ["skill://demo/deploy/SKILL.md"]
        }

remoteDocument :: Text.Text
remoteDocument =
    "---\nname: deploy\ndescription: Deploy the service\n---\nDeploy remotely.\n"

remoteEntry :: McpSkillEntry
remoteEntry = McpSkillEntry
    { mcpSkillUri = "skill://demo/deploy/SKILL.md"
    , mcpSkillFrontmatter =
        rawJsonFromEncoding . Aeson.toEncoding $
            Aeson.object
                [ "name" Aeson..= ("deploy" :: Text.Text)
                , "description" Aeson..= ("Deploy the service" :: Text.Text)
                ]
    , mcpSkillResources =
        McpSkillResourcesListed
            [ McpSkillResource
                { mcpSkillResourceUri = "skill://demo/deploy/SKILL.md"
                , mcpSkillResourceDigest =
                    "sha256:8ca7544fd7ed7665f7a73f12e29664dc321b2d0e8bdb3a6c56d7869712e7368e"
                , mcpSkillResourceSize = 70
                }
            ]
    }

remoteContent :: McpResourceContent
remoteContent = McpResourceContent
    { mcpResourceUri = "skill://demo/deploy/SKILL.md"
    , mcpResourceMimeType = Just "text/markdown"
    , mcpResourceText = Just remoteDocument
    , mcpResourceBlob = Nothing
    }

skillScopeOf :: Skill -> SkillScope
skillScopeOf skill =
    case skill.skillSource of
        FilesystemSkillSource{skillScope} -> skillScope
        McpSkillSource{} -> error "expected filesystem skill"

filesystemPaths :: Skill -> (OsPath, OsPath)
filesystemPaths skill =
    case skill.skillSource of
        FilesystemSkillSource{skillPath, skillDirectory} ->
            (skillPath, skillDirectory)
        McpSkillSource{} -> error "expected filesystem skill"
