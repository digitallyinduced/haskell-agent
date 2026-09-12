{-# LANGUAGE OverloadedStrings #-}

module Agent.CLI.TUIComposerUndoSpec (spec) where

import Agent.CLI.TUI.Composer.Undo
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "compact composer undo" $ do
    it "pops an empty history" $
        popUndoSnapshot [] `shouldBe` Nothing

    it "retains snapshot text and cursor independently of the current draft" $
        drain (pushUndoSnapshot 200 "before dictation" 3 []) `shouldBe`
            [("before dictation", 3)]

    it "restores insertions, deletions, replacements, empty text and Unicode" $ do
        let snapshots =
                [ ("", 0), ("Café🙂e\x0301\n", 7)
                , ("Café日本🙂e\x0301\n", 6), ("日本🙂", 2)
                , ("[image 1] 日本🙂", 10), ("[image 2] 日本🙂", 2)
                , ("totally different\nprompt", 4), ("", 0)
                ]
        drain (history 200 snapshots) `shouldBe` reverse snapshots

    it "does not overlap repeated common prefixes and suffixes" $ do
        let snapshots = [("aaaaa", 3), ("aaaa", 2), ("aaa", 1), ("aaaaaa", 6)]
        drain (history 200 snapshots) `shouldBe` reverse snapshots

    it "preserves exactly the latest 200 steps" $ do
        let snapshots = [(Text.replicate n "🙂", n) | n <- [0 .. 250]]
        drain (history 200 snapshots) `shouldBe` take 200 (reverse snapshots)

    it "supports branching after undo without depending on a newer draft" $ do
        let original = history 200 [("start", 1), ("start x", 7), ("start xy", 8)]
        case popUndoSnapshot original of
            Nothing -> expectationFailure "missing undo snapshot"
            Just (_, rest) ->
                drain (pushUndoSnapshot 200 "external replacement" 4 rest)
                    `shouldBe` [("external replacement", 4), ("start x", 7), ("start", 1)]

    it "matches the snapshot model for every pair of short Unicode texts" $ do
        let texts = "" : [Text.pack [a] | a <- alphabet]
                <> [Text.pack [a,b] | a <- alphabet, b <- alphabet]
            alphabet = ['a', 'b', '🙂', '🙃', 'é', 'ê', 'ũ', '\x0301', '\n']
        mapM_ (\(a, b) ->
            drain (history 200 [(a, 1), (b, 2)])
                `shouldBe` [(b, 2), (a, 1)])
            [(a, b) | a <- texts, b <- texts]

    it "handles nonzero backing-array offsets and shared partial UTF-8 bytes" $ do
        let texts = map (Text.drop 3 . ("日本🙂" <>))
                ["é", "ê", "ũ", "a🙂z", "a🙃z", "", "🙂🙂"]
        mapM_ (\(a, b) ->
            drain (history 200 [(a, 1), (b, 2)])
                `shouldBe` [(b, 2), (a, 1)])
            [(a, b) | a <- texts, b <- texts]

    it "handles zero and single-entry limits" $ do
        history 0 [("a", 1)] `shouldBe` []
        drain (history 1 [("a", 1), ("b", 2)]) `shouldBe` [("b", 2)]

    it "restores edits around byte comparison chunk boundaries" $ do
        mapM_ (\n -> do
            let prefix = Text.replicate n "a"
                suffix = Text.replicate n "🙂"
                snapshots = [(prefix <> middle <> suffix, 4)
                    | middle <- ["é", "ê", "ũ", "🙂", "🙃", "", "xyz"]]
            drain (history 200 snapshots) `shouldBe` reverse snapshots)
            [1021 .. 1027]

history :: Int -> [(Text, Int)] -> [UndoEntry]
history limit = foldl' (\entries (text, cursor) ->
    pushUndoSnapshot limit text cursor entries) []

drain :: [UndoEntry] -> [(Text, Int)]
drain entries = case popUndoSnapshot entries of
    Nothing -> []
    Just (snapshot, rest) -> snapshot : drain rest
