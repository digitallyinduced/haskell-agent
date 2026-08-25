module Agent.TUI.TextWidthSpec (spec) where

import Agent.TUI.TextWidth
import qualified Data.Text as Text
import qualified Graphics.Vty as V
import Test.Hspec

spec :: Spec
spec = describe "terminal character width" do
    it "classifies ASCII, CJK, emoji, combining, and ambiguous characters" do
        charCellWidth 'a' `shouldBe` 1
        charCellWidth '界' `shouldBe` 2
        charCellWidth '🙂' `shouldBe` 2
        charCellWidth '\x0301' `shouldBe` 0
        charCellWidth '·' `shouldBe` 1

    it "keeps raw controls zero-width but display placeholders one cell wide" do
        charCellWidth '\BEL' `shouldBe` 0
        charCellWidth '\x200d' `shouldBe` 1
        displayCharCellWidth '\BEL' `shouldBe` 1
        displayCharCellWidth '\t' `shouldBe` 1
        displayCharCellWidth '\x200d' `shouldBe` 1
        displayTerminalText "\ESC]0;owned\BEL\t\r"
            `shouldBe` "␛]0;owned␇⇥↵"

    it "exposes the shared wide-character classification" do
        isWideCharacter '界' `shouldBe` True
        isWideCharacter '🙂' `shouldBe` True
        isWideCharacter 'a' `shouldBe` False
        isWideCharacter '\x0301' `shouldBe` False
        isWideCharacter '·' `shouldBe` False

    it "keeps common emoji graphemes indivisible and two cells wide" do
        let womanTechnologist =
                Text.pack ['\x1f469', '\x200d', '\x1f4bb']
            usFlag =
                Text.pack ['\x1f1fa', '\x1f1f8']
            thumbsUpMedium =
                Text.pack ['\x1f44d', '\x1f3fd']
            keycapOne =
                Text.pack ['1', '\xfe0f', '\x20e3']
        graphemeClusters
            (womanTechnologist <> usFlag <> thumbsUpMedium <> keycapOne)
            `shouldBe`
                [ womanTechnologist
                , usFlag
                , thumbsUpMedium
                , keycapOne
                ]
        map graphemeCellWidth
            [womanTechnologist, usFlag, thumbsUpMedium, keycapOne]
            `shouldBe` [2, 2, 2, 2]

    it "does not attach emoji modifiers or tags to ordinary text" do
        let modifier = Text.singleton '\x1f3fd'
            tag = Text.singleton '\xe0067'
        graphemeClusters ("a" <> modifier <> "b" <> tag)
            `shouldBe` ["a", modifier, "b", tag]

    it "moves and clamps cursors at grapheme boundaries" do
        let womanTechnologist =
                Text.pack ['\x1f469', '\x200d', '\x1f4bb']
            text = "a" <> womanTechnologist <> "b"
        clampGraphemeCursor text 2 `shouldBe` 1
        previousGraphemeBoundary text 4 `shouldBe` 1
        nextGraphemeBoundary text 1 `shouldBe` 4
        nextGraphemeBoundary text 2 `shouldBe` 4

    it "holds only terminal graphemes that may extend across stream chunks" do
        let woman = Text.singleton '\x1f469'
            trailingJoin = woman <> Text.singleton '\x200d'
        splitTerminalGraphemeSuffix "plain text"
            `shouldBe` ("plain text", "")
        splitTerminalGraphemeSuffix ("hello " <> woman)
            `shouldBe` ("hello ", woman)
        splitTerminalGraphemeSuffix trailingJoin
            `shouldBe` ("", trailingJoin)
        splitTerminalGraphemeSuffix "version 1"
            `shouldBe` ("version ", "1")

    it "declares modern emoji sequence widths correctly to Vty" do
        let family =
                Text.pack
                    [ '\x1f468'
                    , '\x200d'
                    , '\x1f469'
                    , '\x200d'
                    , '\x1f467'
                    , '\x200d'
                    , '\x1f466'
                    ]
            keycapOne =
                Text.pack ['1', '\xfe0f', '\x20e3']
        V.imageWidth (terminalTextImage V.defAttr family)
            `shouldBe` 2
        V.imageWidth (terminalTextImage V.defAttr keycapOne)
            `shouldBe` 2
        displayTerminalText family
            `shouldBe` "？"
        displayTerminalText keycapOne
            `shouldBe` "１"

    it "substitutes every emoji presentation that Vty under-measures" do
        let heart =
                Text.pack ['\x2665', '\xfe0f']
        displayTerminalText "🙂" `shouldBe` "？"
        displayTerminalText heart `shouldBe` "？"
        V.imageWidth (terminalTextImage V.defAttr "🙂")
            `shouldBe` 2
        V.imageWidth (terminalTextImage V.defAttr heart)
            `shouldBe` 2

    it "does not classify invalid keycaps or standalone tags as emoji" do
        let invalidKeycap =
                Text.pack ['a', '\x20e3']
            standaloneTag =
                Text.singleton '\xe0067'
        graphemeCellWidth invalidKeycap `shouldBe` 1
        displayTerminalText invalidKeycap `shouldBe` "a�"
        graphemeCellWidth standaloneTag `shouldBe` 0
        displayTerminalText standaloneTag `shouldBe` "�"

    it "preserves valid emoji formatting but neutralizes standalone formats" do
        let womanTechnologist =
                Text.pack ['\x1f469', '\x200d', '\x1f4bb']
        displayTerminalText womanTechnologist
            `shouldBe` womanTechnologist
        displayTerminalText (Text.singleton '\x200d')
            `shouldBe` "�"

    it "drops variation selectors that could attach across widget boundaries" do
        let emojiCheck =
                Text.pack ['\x2713', '\xfe0f']
            ordinaryVariation =
                Text.pack ['w', '\xfe0f']
        displayTerminalText emojiCheck `shouldBe` "？"
        displayTerminalText ordinaryVariation `shouldBe` "w"
        displayTerminalText (Text.singleton '\xfe0f') `shouldBe` ""
        V.safeWctwidth (displayTerminalText ordinaryVariation)
            `shouldBe` graphemeCellWidth ordinaryVariation
