module Agent.TUI.Markdown.InlineStreamSpec (spec) where

import Agent.TUI.Markdown.Inline
import Control.Monad (forM_)
import qualified Data.Text as Text
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (elements, forAll, listOf, resize, withMaxSuccess)

spec :: Spec
spec = describe "streaming inline Markdown" do
    prop "matches the batch parser for every generated fragment prefix" $
        withMaxSuccess 2000 $ forAll
            (resize 30 (listOf (elements
                [ "plain", " ", "\n", "\r", "\r\n", "*", "**", "***"
                , "_", "__", "`", "``", "\\", "[", "]", "(", ")"
                , "http", "https://", "://", "h", "ttp://", "a.test"
                , ".", "!", ",", "中", "é", "\\*", "[label](url)"
                , "*simple* ", "**strong** ", "_emphasis_ ", "__strong__ "
                ]))) \fragments ->
                    map inlineStreamSnapshot
                        (scanl feedInlineStream emptyInlineStreamState fragments)
                        == map parseInline (scanl (<>) "" fragments)

    it "preserves all character prefixes and binary partitions" do
        let documents =
                [ "before **strong** after *emphasis* tail"
                , "before __strong__ after _emphasis_word"
                , "***nested **strong** emphasis***"
                , "*a ` apparent* closer` end"
                , "*a [apparent* closer](url) end"
                , "escaped \\* text \\\\*style*"
                , "a `code`` not a closer` following"
                , "[nested [label]](https://example.test/a(b)c) tail"
                , "[label](url\\)part) [unfinished"
                , "https://example.test/a(b)). next http://a.test."
                , "wordhttp://example.test https://x.test/中"
                , "plain h\nhttp://example.test\r\n*unfinished\nnext"
                , "_a_ https://example.test/path_(next)."
                , "**unfinished [label (with nesting) plain text and `code` "
                , "`literal http://example.test *style*` done"
                , "***literal ___literal"
                ]
        forM_ documents \document -> do
            let fragments = Text.foldr (\character rest -> Text.singleton character : rest) [] document
            forM_ (zip
                (scanl (<>) "" fragments)
                (scanl feedInlineStream emptyInlineStreamState fragments)) \(prefix, state) ->
                    inlineStreamSnapshot state `shouldBe` parseInline prefix
            forM_ [0 .. Text.length document] \offset -> do
                let state = feedInlineStream
                        (feedInlineStream emptyInlineStreamState (Text.take offset document))
                        (Text.drop offset document)
                finishInlineStream state `shouldBe` parseInline document
                feedInlineStream state "" `shouldBe` state

    it "resumes long pending constructs and resolves split closing syntax" do
        forM_
            [ (["``"], ["`", "`", "x", "``", " tail"])
            , (["[outer ["], ["]", "]", "(", "path(", "x", ")", "\\", ")", ")", " tail"])
            , (["[label](path("], [")", "\\", ")", ")", " tail"])
            , (["https://"], [".", ")", " ", "tail"])
            , (["**"], ["*", "*", " ", "tail"])
            ] \(opening, closing) -> do
                let fragments = opening <> replicate 64 "ordinarytext" <> closing
                    prefixes = scanl (<>) "" fragments
                    states = scanl feedInlineStream emptyInlineStreamState fragments
                forM_ (zip prefixes states) \(prefix, state) ->
                    inlineStreamSnapshot state `shouldBe` parseInline prefix
