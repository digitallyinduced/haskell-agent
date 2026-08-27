module Agent.Tools.SchedulingSpec (spec) where

import Agent.Tools.Scheduling
    ( ToolAccess(..)
    , ToolResource(..)
    , ToolResourceClaim(..)
    , ToolSchedulingPlan(..)
    , pathUsesSchedulingTrie
    , toolSchedulingWaves
    , toolSchedulingWavesLegacy
    )
import Data.Text (Text)
import qualified Data.Text as Text
import System.Info (os)
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
    ( Gen
    , chooseInt
    , counterexample
    , elements
    , forAll
    , listOf
    , vectorOf
    )

spec :: Spec
spec = describe "toolSchedulingWaves" do
    modifyMaxSuccess (const 200) $
        prop "matches the legacy wave partition for generated plans" $
            forAll schedulingPlans \plans ->
                let indexed = toolSchedulingWaves (zip [0 :: Int ..] plans)
                    legacy = toolSchedulingWavesLegacy (zip [0 :: Int ..] plans)
                in counterexample
                    ("indexed: " <> show indexed <> "\nlegacy: " <> show legacy)
                    (indexed == legacy)

    it "handles all plan and resource categories in one schedule" do
        let plans =
                [ ToolUnconstrained
                , claims [named ToolRead "cache"]
                , claims [named ToolWrite "cache"]
                , claims [path ToolRead "/workspace/src/A.hs"]
                , claims [tree ToolWrite "/workspace/src"]
                , claims [ToolResourceClaim ToolRead ToolAllPaths]
                , ToolExclusive
                , ToolUnconstrained
                ]
            input = zip [0 :: Int ..] plans
        toolSchedulingWaves input
            `shouldBe` toolSchedulingWavesLegacy input

    it "disables case-sensitive path trie keys on Windows" do
        let normalizedAbsolute =
                unsafeEncodeUtf $
                    if os == "mingw32"
                        then "C:\\workspace\\src"
                        else "/workspace/src"
        pathUsesSchedulingTrie normalizedAbsolute
            `shouldBe` (os /= "mingw32")

    it "matches legacy handling of Windows case variants" do
        let plans =
                [ claims [tree ToolRead "C:\\Work\\src"]
                , claims [path ToolWrite "c:\\work\\src\\Main.hs"]
                ]
            input = zip [0 :: Int ..] plans
        toolSchedulingWaves input
            `shouldBe` toolSchedulingWavesLegacy input

    it "preserves path semantics for fallback path representations" do
        let paths =
                [ "/workspace/src"
                , "/workspace/src/"
                , "/workspace/./src"
                , "workspace/src"
                , "./workspace/src"
                , "workspace/src/"
                , "C:\\workspace\\src"
                , "C:\\workspace\\src\\"
                ]
            plans =
                concatMap
                    (\raw ->
                        [ claims [tree ToolRead raw]
                        , claims [path ToolWrite (raw <> "/Main.hs")]
                        ])
                    paths
            input = zip [0 :: Int ..] plans
        toolSchedulingWaves input
            `shouldBe` toolSchedulingWavesLegacy input

schedulingPlans :: Gen [ToolSchedulingPlan]
schedulingPlans = do
    size <- chooseInt (0, 25)
    vectorOf size schedulingPlan

schedulingPlan :: Gen ToolSchedulingPlan
schedulingPlan =
    elements [0 :: Int .. 9] >>= \case
        0 -> pure ToolUnconstrained
        1 -> pure ToolExclusive
        _ -> do
            claimCount <- chooseInt (1, 3)
            ToolResourceClaims <$> vectorOf claimCount resourceClaim

resourceClaim :: Gen ToolResourceClaim
resourceClaim = do
    access <- elements [ToolRead, ToolWrite]
    resource <- elements [0 :: Int .. 3] >>= \case
        0 -> ToolNamedResource <$> resourceName
        1 -> ToolPath . unsafeEncodeUtf <$> filePath
        2 -> ToolPathTree . unsafeEncodeUtf <$> treePath
        _ -> pure ToolAllPaths
    pure (ToolResourceClaim access resource)

resourceName :: Gen Text
resourceName =
    Text.pack . ("resource-" <>) . show <$> chooseInt (0, 8)

treePath :: Gen FilePath
treePath = do
    variant <- elements [0 :: Int .. 6]
    components <- listOf (elements ["src", "test", "lib", "nested"])
    let body = foldPath components
    pure $ case variant of
        0 -> "/workspace/" <> body
        1 -> "/workspace/./" <> body
        2 -> "/workspace/" <> body <> "/"
        3 -> "workspace/" <> body
        4 -> "./workspace/" <> body
        5 -> "workspace/" <> body <> "/"
        _ -> "C:\\workspace\\" <> body

filePath :: Gen FilePath
filePath = do
    root <- treePath
    file <- elements ["A.hs", "B.hs", "Main.hs", "Spec.hs"]
    pure (root <> "/" <> file)

foldPath :: [FilePath] -> FilePath
foldPath = \case
    [] -> "."
    first : rest -> foldl (\left right -> left <> "/" <> right) first rest

claims :: [ToolResourceClaim] -> ToolSchedulingPlan
claims = ToolResourceClaims

named :: ToolAccess -> Text -> ToolResourceClaim
named access = ToolResourceClaim access . ToolNamedResource

path :: ToolAccess -> FilePath -> ToolResourceClaim
path access = ToolResourceClaim access . ToolPath . unsafeEncodeUtf

tree :: ToolAccess -> FilePath -> ToolResourceClaim
tree access = ToolResourceClaim access . ToolPathTree . unsafeEncodeUtf
