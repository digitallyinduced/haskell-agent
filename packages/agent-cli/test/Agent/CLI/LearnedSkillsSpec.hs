module Agent.CLI.LearnedSkillsSpec (spec) where

import Agent.CLI (learnAboutUserOnboardingPrompt)
import Agent.CLI.LearnedSkills
import Agent.Store.Postgres.Scope
    ( Scope(..)
    , ScopeKind(..)
    , mkScopeId
    )
import Agent.Store.Postgres.Skill
    ( LearnedSkill(..)
    , LearnedSkillActivation(..)
    , LearnedSkillStatus(..)
    )
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolDispatchConfig(..)
    , dispatchToolCall
    , functionToolCall
    )
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , appToolHandlers
    )
import Control.Exception.Safe (displayException)
import Data.Aeson (object, (.=))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import Test.Hspec

spec :: Spec
spec = do
    describe "learnedSkillTools" do
        it "registers read-only search/read and approved sequential mutations" do
            let tools = learnedSkillTools testEnv
            map (.appToolName) tools `shouldBe`
                [ "skill_search"
                , "skill_read"
                , "skill_create"
                , "skill_update"
                , "skill_archive"
                , "skill_rollback"
                ]
            map (approvalLabel . (.appToolApproval)) tools `shouldBe`
                [ "read-only"
                , "read-only"
                , "prompt"
                , "prompt"
                , "prompt"
                , "prompt"
                ]
            map (.appToolExecution) tools `shouldBe`
                [ ParallelSafe
                , ParallelSafe
                , TurnSequential
                , TurnSequential
                , TurnSequential
                , TurnSequential
                ]

        it "dispatches search and create requests to the environment" do
            searchSeen <- newIORef Nothing
            createSeen <- newIORef Nothing
            let env = testEnv
                    { learnedSkillSearch = \query limit -> do
                        writeIORef searchSeen (Just (query, limit))
                        pure (Right (object ["matches" .= ([] :: [Text])]))
                    , learnedSkillCreate = \request -> do
                        writeIORef createSeen (Just request)
                        pure (Right (object ["status" .= ("applied" :: Text)]))
                    }
            searchResult <- dispatchToolCall dispatchConfig
                (appToolHandlers (learnedSkillTools env))
                (functionToolCall "call-1" "skill_search"
                    "{\"query\":\"postgres session\",\"limit\":4}")
            createResult <- dispatchToolCall dispatchConfig
                (appToolHandlers (learnedSkillTools env))
                (functionToolCall "call-2" "skill_create"
                    "{\"scope\":\"repository\",\
                        \\"slug\":\"postgres-session\",\
                        \\"title\":\"Postgres sessions\",\
                        \\"description\":\"Use typed durable tables\",\
                        \\"applies_when\":\"Changing session persistence\",\
                        \\"instructions\":\"Keep outputs as text\",\
                        \\"change_summary\":\"Initial lesson\",\
                        \\"evidence\":\"The user requested this in the session\"}")
            readIORef searchSeen `shouldReturn`
                Just ("postgres session", 4)
            fmap (.createRequestSlug) <$> readIORef createSeen
                `shouldReturn` Just "postgres-session"
            searchResult.output `shouldContainText` "\"matches\""
            createResult.output `shouldContainText` "\"applied\""

        it "rejects invalid mutations before calling storage" do
            called <- newIORef False
            let env = testEnv
                    { learnedSkillCreate = \_ -> do
                        writeIORef called True
                        pure (Right (object []))
                    , learnedSkillUpdate = \_ -> do
                        writeIORef called True
                        pure (Right (object []))
                    }
            badSlug <- dispatchToolCall dispatchConfig
                (appToolHandlers (learnedSkillTools env))
                (functionToolCall "call-3" "skill_create"
                    "{\"scope\":\"user\",\
                    \\"slug\":\"Bad Slug\",\
                    \\"title\":\"Title\",\
                    \\"description\":\"Description\",\
                    \\"applies_when\":\"Always\",\
                    \\"instructions\":\"Do this\",\
                    \\"change_summary\":\"Learned\",\
                    \\"evidence\":\"Evidence\"}")
            readIORef called `shouldReturn` False
            badSlug.output `shouldContainText` "lowercase ASCII"

            emptyEvidence <- dispatchToolCall dispatchConfig
                (appToolHandlers (learnedSkillTools env))
                (functionToolCall "call-4" "skill_create"
                    "{\"scope\":\"user\",\
                    \\"slug\":\"valid-skill\",\
                    \\"title\":\"Title\",\
                    \\"description\":\"Description\",\
                    \\"applies_when\":\"Always\",\
                    \\"instructions\":\"Do this\",\
                    \\"change_summary\":\"Learned\",\
                    \\"evidence\":\"  \"}")
            readIORef called `shouldReturn` False
            emptyEvidence.output `shouldContainText` "evidence must not be empty"

            noOp <- dispatchToolCall dispatchConfig
                (appToolHandlers (learnedSkillTools env))
                (functionToolCall "call-5" "skill_update"
                    "{\"scope\":\"user\",\
                    \\"slug\":\"valid-skill\",\
                    \\"expected_revision\":1,\
                    \\"title\":null,\
                    \\"description\":null,\
                    \\"applies_when\":null,\
                    \\"instructions\":null,\
                    \\"activation\":null,\
                    \\"priority\":null,\
                    \\"change_summary\":\"No change\",\
                    \\"evidence\":\"Evidence\"}")
            readIORef called `shouldReturn` False
            noOp.output `shouldContainText` "must change at least one field"

    describe "formatLearnedSkillContext" do
        it "includes always instructions but only indexes relevant/manual skills" do
            let (context, omitted) =
                    formatLearnedSkillContext 8000
                        [ skill "always-skill" SkillAlways
                        , skill "relevant-skill" SkillRelevant
                        , skill "manual-skill" SkillManual
                        ]
            context `shouldNotBe` Nothing
            let rendered = maybe "" id context
            rendered `shouldContainText` "always-skill"
            rendered `shouldContainText` "Always instructions"
            rendered `shouldContainText` "relevant-skill"
            rendered `shouldContainText` "manual-skill"
            rendered `shouldNotContainText` "Relevant instructions"
            rendered `shouldNotContainText` "Manual instructions"
            rendered `shouldContainText` "[repository]"
            omitted `shouldBe` 0

        it "omits entries that do not fit the context budget" do
            let (context, omitted) =
                    formatLearnedSkillContext 600
                        [ skill "first" SkillAlways
                        , skill "second" SkillRelevant
                        , skill "third" SkillManual
                        ]
            omitted `shouldSatisfy` (> 0)
            Text.length (maybe "" id context) `shouldSatisfy` (< 601)

        it "uses the narrowest applicable scope for duplicate slugs" do
            let
                userSkill =
                    (skill "scope-overlay" SkillAlways)
                        { learnedSkillScope = testScope UserScope
                        , learnedSkillInstructions = "Broad instructions"
                        }
                checkoutSkill =
                    (skill "scope-overlay" SkillAlways)
                        { learnedSkillScope = testScope CheckoutScope
                        , learnedSkillInstructions = "Narrow instructions"
                        }
                (context, omitted) =
                    formatLearnedSkillContext
                        8000
                        [userSkill, checkoutSkill]
                rendered = maybe "" id context
            rendered `shouldContainText` "[checkout]"
            rendered `shouldContainText` "Narrow instructions"
            rendered `shouldNotContainText` "Broad instructions"
            omitted `shouldBe` 0

        it "injects an always-loaded user technical profile in startup context" do
            let
                profile =
                    (skill "user-technical-profile" SkillAlways)
                        { learnedSkillScope = testScope UserScope
                        , learnedSkillInstructions =
                            "Prefer Nix flakes and fast GHCi iteration."
                        }
                (context, omitted) =
                    formatLearnedSkillContext 8000 [profile]
                rendered = maybe "" id context
            rendered `shouldContainText` "user-technical-profile"
            rendered `shouldContainText`
                "Prefer Nix flakes and fast GHCi iteration."
            omitted `shouldBe` 0

    describe "learnAboutUserOnboardingPrompt" do
        it "starts the skill when no technical profile exists" do
            learnAboutUserOnboardingPrompt []
                `shouldSatisfy` maybe False (Text.isInfixOf "$learn-about-user")

        it "does not repeat onboarding when the profile already exists" do
            let profile =
                    (skill "user-technical-profile" SkillAlways)
                        { learnedSkillScope = testScope UserScope
                        }
            learnAboutUserOnboardingPrompt [profile] `shouldBe` Nothing

testEnv :: LearnedSkillToolsEnv
testEnv = LearnedSkillToolsEnv
    { learnedSkillSearch = \_ _ -> pure (Right (object []))
    , learnedSkillRead = \_ _ _ -> pure (Right (object []))
    , learnedSkillCreate = \_ -> pure (Right (object []))
    , learnedSkillUpdate = \_ -> pure (Right (object []))
    , learnedSkillArchive = \_ -> pure (Right (object []))
    , learnedSkillRollback = \_ -> pure (Right (object []))
    }

dispatchConfig :: ToolDispatchConfig
dispatchConfig = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "unknown: " <> name
    , toolDispatchFormatResult = either ("error: " <>) id
    , toolDispatchFormatException = \name exception ->
        name <> ": " <> Text.pack (displayException exception)
    , toolDispatchOnException = \_ _ -> pure ()
    , toolDispatchOnOutput = \_ _ -> pure ()
    }

skill :: Text -> LearnedSkillActivation -> LearnedSkill
skill slug activation = LearnedSkill
    { learnedSkillId = slug <> "-id"
    , learnedSkillScope = testScope RepositoryScope
    , learnedSkillSlug = slug
    , learnedSkillTitle = slug
    , learnedSkillDescription = "Reusable description"
    , learnedSkillAppliesWhen = "When this task matches"
    , learnedSkillInstructions = case activation of
        SkillAlways -> "Always instructions"
        SkillRelevant -> "Relevant instructions"
        SkillManual -> "Manual instructions"
    , learnedSkillActivation = activation
    , learnedSkillPriority = 0
    , learnedSkillStatus = SkillActive
    , learnedSkillRevision = 1
    , learnedSkillCreatedAt = timestamp
    , learnedSkillUpdatedAt = timestamp
    }

testScope :: ScopeKind -> Scope
testScope kind =
    Scope kind $
        either (error . Text.unpack) id $
            mkScopeId "11111111111111111111111111111111"

timestamp :: UTCTime
timestamp = read "2026-08-24 15:00:00 UTC"

shouldContainText :: Text -> Text -> Expectation
shouldContainText actual expected =
    actual `shouldSatisfy` Text.isInfixOf expected

shouldNotContainText :: Text -> Text -> Expectation
shouldNotContainText actual expected =
    actual `shouldSatisfy` (not . Text.isInfixOf expected)

approvalLabel :: ApprovalRule -> Text
approvalLabel = \case
    AlwaysReadOnly -> "read-only"
    AlwaysPrompt -> "prompt"
    ClassifyReadOnly _ -> "classified"
