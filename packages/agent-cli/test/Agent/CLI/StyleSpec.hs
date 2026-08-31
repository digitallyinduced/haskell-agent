module Agent.CLI.StyleSpec (spec) where

import Agent.CLI.Style
import Agent.CLI.WindowTitle
    ( WindowTitleController(..)
    , busyWindowTitle
    , newWindowTitleController
    , oscWindowTitleBytes
    )
import Agent.TUI.Motion (MotionMode(..))
import Data.IORef (modifyIORef', newIORef, readIORef)
import System.OsPath (unsafeEncodeUtf)
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Test.Hspec

fromFilePath = unsafeEncodeUtf

spec :: Spec
spec = do
    describe "style" do
        it "leaves text unchanged when color is off" do
            style False [] "plain" `shouldBe` "plain"

        it "wraps text in SGR codes when color is on" do
            let out = rolePrompt True "λ"
            out `shouldSatisfy` Text.isInfixOf "λ"
            out `shouldSatisfy` Text.isInfixOf "\ESC["
            out `shouldSatisfy` (not . Text.isPrefixOf "λ")

        it "uses the terminal default background after nested styling" do
            let out = styleBase True agentBackground [] "hi"
            out `shouldSatisfy` (not . Text.isInfixOf "48;")

    describe "paintBackgroundLines" do
        it "leaves text unchanged when color is off" do
            paintBackgroundLines False agentBackground "a\nb" `shouldBe` "a\nb"

        it "leaves lines on the terminal theme's default background" do
            paintBackgroundLines True agentBackground "a\nb\n"
                `shouldBe` "a\nb\n"

        it "does not erase with the terminal default background" do
            paintBackgroundLines True agentBackground "text"
                `shouldSatisfy` (not . Text.isInfixOf "\ESC[K")

    describe "roles" do
        it "keeps tool labels readable with color off" do
            roleToolName False "Read" `shouldBe` "Read"
            roleToolPath False "src/A.hs" `shouldBe` "src/A.hs"
            roleToolCommand False "git status" `shouldBe` "git status"
            roleError False "boom" `shouldBe` "boom"
            roleMuted False "session: 1" `shouldBe` "session: 1"

        it "uses the terminal cyan slot without forcing a background" do
            let out = rolePrompt True "λ"
            out `shouldSatisfy` Text.isInfixOf "\ESC[1;36m"
            out `shouldSatisfy` (not . Text.isInfixOf "48;")

    describe "chrome glyphs" do
        it "exposes the shared Unicode markers" do
            glyphTool `shouldSatisfy` (`elem` ["◆ ", "* "])
            glyphInspect `shouldSatisfy` (`elem` ["◇ ", "o "])
            glyphToolAccent `shouldSatisfy` (`elem` ["❙ ", "| "])
            glyphOk `shouldSatisfy` (`elem` ["✓ ", "+ "])
            glyphErr `shouldSatisfy` (`elem` ["✗ ", "x "])
            glyphWarn `shouldSatisfy` (`elem` ["⚠ ", "! "])
            glyphCancel `shouldSatisfy` (`elem` ["⊘ ", "o "])
            glyphSession `shouldSatisfy` (`elem` ["⧉ ", "# "])
            glyphThink `shouldSatisfy` (`elem` ["◆ ", "* "])
            glyphToolOut `shouldSatisfy` (`elem` ["┊ ", "| "])
            spinnerFrames `shouldSatisfy` (not . null)

    describe "cliWindowTitle" do
        it "uses a stable placeholder when no session title is set" do
            cliWindowTitle (fromFilePath "/tmp/haskell-agent") Nothing
                `shouldBe` "New session"

        it "prefers a real session title over cwd" do
            cliWindowTitle (fromFilePath "/tmp/haskell-agent") (Just "fix the title")
                `shouldBe` "fix the title"

        it "ignores untitled placeholders" do
            cliWindowTitle (fromFilePath "/tmp/haskell-agent") (Just "untitled")
                `shouldBe` "New session"

    describe "WindowTitleController" do
        it "prefixes busy titles with a spinner frame" do
            busyWindowTitle "⠋" "fix the title"
                `shouldBe` "⠋ fix the title"

        it "UTF-8 encodes braille spinner frames in OSC window titles" do
            let bytes = oscWindowTitleBytes (busyWindowTitle "⠋" "fix the title")
            bytes
                `shouldBe`
                    TextEncoding.encodeUtf8 "\ESC]2;⠋ fix the title\a"
            ByteString.isInfixOf (ByteString.pack [0xE2, 0xA0, 0x8B]) bytes
                `shouldBe` True
            ByteString.elem 0x0B bytes `shouldBe` False

        it "strips OSC terminators from window titles" do
            oscWindowTitleBytes "hi\a there\ESC"
                `shouldBe`
                    TextEncoding.encodeUtf8 "\ESC]2;hi there\a"

        it "coordinates busy, renamed, and restored titles" do
            written <- newIORef []
            let firstFrame = case spinnerFrames of
                    frame : _ -> frame
                    [] -> "*"
            controller <- newWindowTitleController
                MotionOff
                "initial"
                id
                (\title -> modifyIORef' written (<> [title]))
            controller.windowTitleBeginBusy
            controller.windowTitleSet "renamed"
            controller.windowTitleEndBusy
            actual <- readIORef written
            actual
                `shouldBe`
                [ firstFrame <> " initial"
                , firstFrame <> " renamed"
                , "renamed"
                ]

        it "keeps reduced-motion busy titles static" do
            written <- newIORef []
            let firstFrame = case spinnerFrames of
                    frame : _ -> frame
                    [] -> "*"
            controller <- newWindowTitleController
                MotionReduced
                "initial"
                id
                (\title -> modifyIORef' written (<> [title]))
            controller.windowTitleBeginBusy
            controller.windowTitleWorker
            actual <- readIORef written
            actual `shouldBe` [firstFrame <> " initial"]
