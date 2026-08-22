module Agent.CLI.TUIComposerSpec (spec) where

import Agent.CLI.Input (ReplLine(..))
import Agent.CLI.TUI.Composer
import Agent.CLI.TUI.Types
import Control.Concurrent.STM (atomically)
import qualified Data.ByteString as ByteString
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Test.Hspec

spec :: Spec
spec = describe "fullscreen composer" do
    it "tracks the multiline cursor location" do
        draftCursorLocation "one\ntwo" 6 `shouldBe` (1, 2)

    it "filters control characters from bracketed paste text" do
        decodePaste (ByteString.pack [97, 0, 10, 9, 27, 98])
            `shouldBe` "a\n\tb"

    it "keeps clipboard preludes immediately before promoted input" do
        buffer <- newFullscreenInputBuffer
        atomically do
            appendFullscreenInput buffer (input (ReplText "queued"))
            appendFullscreenInput
                buffer
                (input (ReplClipboardPaste "draft" Nothing))
            promoteFullscreenInput buffer (input (ReplText "urgent"))
        queued <- atomically (readFullscreenInputs buffer)
        map (.fullscreenInputLine) (toList queued)
            `shouldBe`
                [ ReplClipboardPaste "draft" Nothing
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
  where
    input replLine = FullscreenInput
        { fullscreenInputLine = replLine
        , fullscreenInputQueued = True
        , fullscreenInputDisplay = Nothing
        }
