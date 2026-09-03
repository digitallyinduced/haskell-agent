{-# LANGUAGE OverloadedStrings #-}

module Agent.CLI.ResourceAdminSpec (spec) where

import Agent.CLI.ResourceAdmin
import Agent.Store.Postgres.Skill (LearnedSkillActivation(..))
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "typed learned-resource administration validation" do
    it "accepts the three documented scopes and canonical slugs" do
        validateResourceSlug "remember-user-preference" `shouldBe`
            Right "remember-user-preference"

    it "rejects malformed slugs without exposing input values" do
        validateResourceSlug "Bad/secret" `shouldBe`
            Left (ResourceAdminInvalid
                "slug must use lower-case ASCII words separated by single hyphens")

    it "enforces required content and bounded fields" do
        validateResourceSkillDraft validDraft `shouldBe` Right validDraft
        validateResourceSkillDraft
            validDraft { resourceDraftInstructions = "" }
            `shouldBe` Left (ResourceAdminInvalid
                "instructions must not be empty")

    it "rejects out-of-range priorities" do
        validateResourceSkillDraft
            validDraft { resourceDraftPriority = 101 }
            `shouldBe` Left (ResourceAdminInvalid
                "priority must be between -100 and 100")

    it "enforces exact store character bounds" do
        validateResourceSkillDraft
            validDraft { resourceDraftTitle = Text.replicate 201 "ü" }
            `shouldBe` Left (ResourceAdminInvalid
                "title must contain at most 200 characters")
        validateResourceSummary (Text.replicate 1001 "x")
            `shouldBe` Left (ResourceAdminInvalid
                "change summary must contain at most 1000 characters")

    it "rejects NUL and noncanonical slug forms" do
        validateResourceSkillDraft
            validDraft { resourceDraftDescription = "safe\NULsecret" }
            `shouldBe` Left (ResourceAdminInvalid
                "description must not contain NUL")
        map validateResourceSlug ["double--dash", "-leading", "trailing-"]
            `shouldSatisfy` all isLeft

    it "short-circuits invalid read keys before accessing the store" do
        readResourceSkill
            unreachableStorageContext
            unreachableStorageContext
            ResourceUserScope
            ""
            Nothing
            `shouldReturn` Left
                (ResourceAdminInvalid "slug must not be empty")
        readResourceSkill
            unreachableStorageContext
            unreachableStorageContext
            ResourceUserScope
            "remember-user-preference"
            (Just 0)
            `shouldReturn` Left
                (ResourceAdminInvalid "revision must be positive")

    it "preserves validation precedence across mutation inputs" do
        createResourceSkill
            unreachableStorageContext
            unreachableStorageContext
            ResourceUserScope
            ""
            invalidDraft
            ""
            `shouldReturn` Left
                (ResourceAdminInvalid "slug must not be empty")
        updateResourceSkill
            unreachableStorageContext
            unreachableStorageContext
            ResourceUserScope
            "remember-user-preference"
            1
            invalidDraft
            ""
            `shouldReturn` Left
                (ResourceAdminInvalid "title must not be empty")
        rollbackResourceSkill
            unreachableStorageContext
            unreachableStorageContext
            ResourceUserScope
            "remember-user-preference"
            1
            0
            ""
            `shouldReturn` Left
                (ResourceAdminInvalid "target revision must be positive")

    it "validates list and history limits before accessing the store" do
        listResourceSkills
            unreachableStorageContext
            unreachableStorageContext
            Nothing
            0
            `shouldReturn` Left
                (ResourceAdminInvalid
                    "list limit must be between 1 and 1000")
        historyResourceSkill
            unreachableStorageContext
            unreachableStorageContext
            ResourceUserScope
            ""
            0
            `shouldReturn` Left
                (ResourceAdminInvalid
                    "history limit must be between 1 and 1000")

validDraft :: ResourceSkillDraft
validDraft = ResourceSkillDraft
    { resourceDraftTitle = "Remember"
    , resourceDraftDescription = "A preference"
    , resourceDraftAppliesWhen = ""
    , resourceDraftInstructions = "Use the preference."
    , resourceDraftActivation = SkillRelevant
    , resourceDraftPriority = 0
    }

invalidDraft :: ResourceSkillDraft
invalidDraft = validDraft { resourceDraftTitle = "" }

unreachableStorageContext :: value
unreachableStorageContext =
    error "resource administration accessed storage after invalid input"

isLeft :: Either error value -> Bool
isLeft = either (const True) (const False)
