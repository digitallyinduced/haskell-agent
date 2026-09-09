{-# LANGUAGE OverloadedStrings #-}

module Agent.Tools.OutputArtifact.RetrievalSpec (spec) where

import Agent.Tools.OutputArtifact.Retrieval
import Data.Aeson (Value(..), encode)
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as ByteString
import Data.Foldable (toList)
import Data.List (findIndex, isPrefixOf, tails)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Test.Hspec
import qualified Test.QuickCheck as QuickCheck

spec :: Spec
spec = describe "storage-independent artifact retrieval" $ do
    it "paginates an entire single-line Unicode document without loss" $ do
        let source = LazyText.fromStrict (Text.replicate 5000 "a😀ß")
            collect cursor =
                let page = expectRight (readArtifactChunk source cursor 4096)
                    text = case field "text" page of
                        String value -> value
                        _ -> error "invalid chunk"
                in text <> case field "next_cursor" page of
                    Null -> ""
                    Number next -> collect (round next)
                    _ -> error "invalid cursor"
        collect 0 `shouldBe` LazyText.toStrict source

    it "returns context around a match beyond the old line preview" $ do
        let source = LazyText.replicate 100000 "x" <> "target:end"
        let result = expectRight (searchArtifactOccurrences source "target" False 0 50 4)
        let entry = case entries result of
                [value] -> value
                _ -> error "expected one occurrence"
        field "start" entry `shouldBe` Number 100000
        field "text" entry `shouldBe` String "xxxxtarget:end"

    it "returns every occurrence on a single line across pages" $ do
        let first = expectRight (searchArtifactOccurrences "a a a" "a" False 0 2 0)
            second = expectRight (searchArtifactOccurrences "a a a" "a" False 3 2 0)
        map (field "start") (entries first) `shouldBe` [Number 0, Number 2]
        field "next_cursor" first `shouldBe` Number 3
        map (field "start") (entries second) `shouldBe` [Number 4]
        field "next_cursor" second `shouldBe` Null

    it "preserves original Unicode offsets for expanding case folds" $ do
        let result = expectRight (searchArtifactOccurrences "😀 Straße STRASSE ß" "ss" True 0 20 0)
        map (field "start") (entries result) `shouldBe` map Number [6, 13, 17]
        map (field "end") (entries result) `shouldBe` map Number [7, 15, 18]
        map (field "text") (entries result) `shouldBe` map String ["ß", "SS", "ß"]

    it "finds a literal across lazy text chunks" $ do
        let result = expectRight (searchArtifactOccurrences (LazyText.fromChunks ["abc", "def"]) "cde" False 0 2 0)
        map (field "text") (entries result) `shouldBe` [String "cde"]

    it "bounds encoded output and resumes without skipping a budget-limited occurrence" $ do
        let source = LazyText.replicate 5000 "\NUL"
            pattern = Text.replicate 100 "\NUL"
            collect cursor =
                let page = expectRight (searchArtifactOccurrences source pattern False cursor 200 256)
                in (page, entries page) : case field "next_cursor" page of
                    Null -> []
                    Number next -> collect (round next)
                    _ -> error "invalid cursor"
            pages = collect 0
        map (field "start") (concatMap snd pages) `shouldBe` map (Number . fromIntegral) [0, 100 .. 4900 :: Int]
        map (ByteString.length . encode . fst) pages `shouldSatisfy` all (< 40 * 1024)

    it "rejects empty and oversized patterns" $ do
        searchArtifactOccurrences "abc" "" False 0 1 0 `shouldSatisfy` isLeft
        searchArtifactOccurrences "abc" (Text.replicate 1025 "a") False 0 1 0 `shouldSatisfy` isLeft

    it "rejects negative cursors and invalid retrieval bounds" $ do
        readArtifactChunk "abc" (-1) 1 `shouldSatisfy` isLeft
        readArtifactChunk "abc" 0 0 `shouldSatisfy` isLeft
        searchArtifactOccurrences "abc" "a" False (-1) 1 0 `shouldSatisfy` isLeft
        searchArtifactOccurrences "abc" "a" False 0 0 0 `shouldSatisfy` isLeft
        searchArtifactOccurrences "abc" "a" False 0 1 (-1) `shouldSatisfy` isLeft

    it "finds matches across search-window boundaries with original context" $ do
        let source = LazyText.replicate 32766 "x" <> "Straße:end"
            result = expectRight (searchArtifactOccurrences source "STRASSE" True 0 10 4)
        map (field "text") (entries result) `shouldBe` [String "xxxxStraße:end"]
        map (field "start") (entries result) `shouldBe` [Number 32766]

    it "agrees with a tagged-character reference across varied Unicode and search pages" $
        QuickCheck.withMaxSuccess 500 $
            QuickCheck.forAll unicodeSearchCase $ \(source, pattern, insensitive) ->
                let content = LazyText.fromChunks (map Text.singleton source)
                    collect cursor =
                        let page = expectRight (searchArtifactOccurrences content (Text.pack pattern) insensitive cursor 3 2)
                            coordinates =
                                [ (round start, round end)
                                | entry <- entries page
                                , Number start <- [field "start" entry]
                                , Number end <- [field "end" entry]
                                ]
                        in coordinates <> case field "next_cursor" page of
                            Null -> []
                            Number next -> collect (round next)
                            _ -> error "invalid search cursor"
                in collect 0 QuickCheck.=== referenceOccurrences source pattern insensitive

field :: Key -> Value -> Value
field key (Object object) = maybe Null id (KeyMap.lookup key object)
field _ _ = Null

entries :: Value -> [Value]
entries page = case field "matches" page of
    Array values -> toList values
    _ -> []

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

expectRight :: Either Text.Text a -> a
expectRight (Right value) = value
expectRight (Left message) = error (Text.unpack message)

unicodeSearchCase :: QuickCheck.Gen (String, String, Bool)
unicodeSearchCase = do
    let character = QuickCheck.elements "aAbBsSßﬃİiΣσς😀\n\NUL"
    source <- QuickCheck.listOf character
    count <- QuickCheck.chooseInt (1, 4)
    pattern <- QuickCheck.vectorOf count character
    insensitive <- QuickCheck.arbitrary
    pure (source, pattern, insensitive)

-- Intentionally simple reference: tag every folded character with its
-- original position, then search ordinary lists and round matches outward.
referenceOccurrences :: String -> String -> Bool -> [(Int, Int)]
referenceOccurrences source pattern insensitive = collect tagged
  where
    foldCharacters value =
        if insensitive then Text.unpack (Text.toCaseFold (Text.pack value)) else value
    needle = foldCharacters pattern
    tagged =
        [ (folded, position)
        | (position, character) <- zip [0 ..] source
        , folded <- foldCharacters [character]
        ]
    collect remaining =
        case findIndex (isPrefixOf needle) (tails (map fst remaining)) of
            Nothing -> []
            Just index ->
                let occurrence = take (length needle) (drop index remaining)
                in case (occurrence, reverse occurrence) of
                    ((_, start) : _, (_, lastPosition) : _) ->
                        let end = lastPosition + 1
                        in (start, end) : collect (dropWhile ((< end) . snd) remaining)
                    _ -> error "empty reference occurrence"
