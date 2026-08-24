module Agent.CLI.TUIComposerSpec (spec) where

import Agent.CLI.Input (ReplLine(..))
import Agent.CLI.TUI.Composer
import Agent.CLI.TUI.Types
import Control.Concurrent.STM (atomically)
import qualified Data.ByteString as ByteString
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = describe "fullscreen composer" do
    it "cancels a running turn even when the slash menu is open" do
        composerEscapeAction False True
            `shouldBe` EscapeCancelTurn

    it "dismisses slash completion or clears the draft while idle" do
        composerEscapeAction True True
            `shouldBe` EscapeDismissSlashMenu
        composerEscapeAction True False
            `shouldBe` EscapeClearDraft

    it "tracks the multiline cursor location" do
        draftCursorLocation "one\ntwo" 6 `shouldBe` (1, 2)

    it "soft-wraps long unbroken drafts at the composer width" do
        wrapDraft 5 "abcdefgh" 8
            `shouldBe` (["abcde", "fgh"], (1, 3))

    it "places the cursor on the continuation row at a wrap boundary" do
        wrapDraft 5 "abcdefgh" 5
            `shouldBe` (["abcde", "fgh"], (1, 0))
        wrapDraft 5 "abcde" 5
            `shouldBe` (["abcde", ""], (1, 0))

    it "combines explicit newlines with visual wrapping" do
        wrapDraft 5 "abc\ndefghi" 10
            `shouldBe` (["abc", "defgh", "i"], (2, 1))

    it "wraps using terminal columns for wide characters" do
        wrapDraft 3 "a界b" 2
            `shouldBe` (["a界", "b"], (1, 0))

    it "keeps combining marks attached at a visual boundary" do
        wrapDraft 1 "e\x0301x" 2
            `shouldBe` (["e\x0301", "x"], (1, 0))

    it "filters control characters from bracketed paste text" do
        decodePaste (ByteString.pack [97, 0, 10, 9, 27, 98])
            `shouldBe` "a\n\tb"

    it "inserts bracketed paste immediately while a turn is running" do
        prepareBracketedPaste False "next message" 4 " pasted"
            `shouldBe` ("next pasted message", 11, Nothing)

    it "defers clipboard classification only while the REPL is awaiting input" do
        prepareBracketedPaste True "next message" 4 " pasted"
            `shouldBe`
                ( "next message"
                , 4
                , Just
                    (ReplClipboardPasteOrText
                        "next message"
                        " pasted"
                        "next pasted message")
                )

    it "keeps empty paste events available for native clipboard images" do
        prepareBracketedPaste False "next message" 4 ""
            `shouldBe`
                ( "next message"
                , 4
                , Just (ReplClipboardPaste "next message" Nothing)
                )

    it "keeps clipboard preludes immediately before promoted input" do
        buffer <- newFullscreenInputBuffer
        atomically do
            appendFullscreenInput buffer (input (ReplText "queued"))
            appendFullscreenInput
                buffer
                (input (ReplClipboardPaste "draft" Nothing))
            appendFullscreenInput
                buffer
                (input
                    (ReplClipboardPasteOrText
                        "before"
                        "/path.png"
                        "before/path.png"))
            promoteFullscreenInput buffer (input (ReplText "urgent"))
        queued <- atomically (readFullscreenInputs buffer)
        map (.fullscreenInputLine) (toList queued)
            `shouldBe`
                [ ReplClipboardPaste "draft" Nothing
                , ReplClipboardPasteOrText
                    "before"
                    "/path.png"
                    "before/path.png"
                , ReplText "urgent"
                , ReplText "queued"
                ]

    it "only exposes displays for queued prompts" do
        buffer <- newFullscreenInputBuffer
        atomically do
            appendFullscreenInput buffer FullscreenInput
                { fullscreenInputLine = ReplText "active"
                , fullscreenInputQueued = False
                , fullscreenInputDisplay = Just "active"
                }
            appendFullscreenInput buffer FullscreenInput
                { fullscreenInputLine = ReplText "queued"
                , fullscreenInputQueued = True
                , fullscreenInputDisplay = Just "queued"
                }
        queuedFullscreenInputDisplays buffer
            `shouldReturn` Seq.singleton "queued"

    it "prefers an already-submitted prompt over a simultaneous wakeup" do
        buffer <- newFullscreenInputBuffer
        atomically $
            appendFullscreenInput buffer (input (ReplText "submitted"))
        result <- atomically $
            takeFullscreenInputOr
                buffer
                (pure ("provider unavailable" :: Text))
        fmap (.fullscreenInputLine) result
            `shouldBe` Right (ReplText "submitted")
  where
    input replLine = FullscreenInput
        { fullscreenInputLine = replLine
        , fullscreenInputQueued = True
        , fullscreenInputDisplay = Nothing
        }
