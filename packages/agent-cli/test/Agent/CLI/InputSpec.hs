module Agent.CLI.InputSpec (spec) where

import Agent.CLI.Command (defaultSlashCatalog)
import Agent.CLI.Input
    ( ChoiceKey(..)
    , approvalKeyText
    , choiceMoveIndex
    , classifyPastedText
    , decodeBracketedPastePayload
    , displayEditorText
    , formatPasteChip
    , isClipboardPasteCsiBody
    , isClipboardPasteKey
    , isShiftEnterCsiBody
    , parseChoiceKey
    , replHistoryPath
    , submissionPromptText
    , terminalTextWidth
    , truncateDisplayText
    , visibleEditorText
    )
import Agent.CLI.Input.Editor
    ( EditorEffect(..)
    , EditorStep(..)
    , initialEditorState
    , reduceEditorKey
    )
import Agent.CLI.Input.KeyDecoder (decodeKittyEditorKey)
import Agent.CLI.Input.Types
    ( EditorKey(..)
    , EditorState(..)
    , ReplLine(..)
    )
import Data.Char (isControl)
import Data.Either (isLeft)
import Data.List (mapAccumL)
import qualified Data.Text as Text
import qualified Graphics.Vty as V
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
    ( Arbitrary(..)
    , Gen
    , Property
    , chooseInt
    , conjoin
    , counterexample
    , elements
    , vectorOf
    , (===)
    )

data EditorViewportCase = EditorViewportCase !Int !Text.Text !Int
    deriving (Show)

newtype EditorKeySequence = EditorKeySequence [EditorKey]
    deriving (Show)

spec :: Spec
spec = do
    describe "replHistoryPath" do
        it "is ~/.haskell-agent/history" do
            replHistoryPath "/home/marc"
                `shouldBe` "/home/marc" </> ".haskell-agent" </> "history"

    describe "approvalKeyText" do
        it "keeps a printable key as a one-character answer" do
            approvalKeyText 'y' `shouldBe` "y"
            approvalKeyText 'A' `shouldBe` "A"
            approvalKeyText 'n' `shouldBe` "n"

        it "maps Enter / Return to the empty default deny" do
            approvalKeyText '\n' `shouldBe` ""
            approvalKeyText '\r' `shouldBe` ""

    describe "parseChoiceKey" do
        it "maps arrows, vim keys, enter, cancel, and digits" do
            parseChoiceKey "\ESC[A" `shouldBe` Just ChoiceUp
            parseChoiceKey "\ESC[B" `shouldBe` Just ChoiceDown
            parseChoiceKey "\ESCOA" `shouldBe` Just ChoiceUp
            parseChoiceKey "\ESCOB" `shouldBe` Just ChoiceDown
            parseChoiceKey "k" `shouldBe` Just ChoiceUp
            parseChoiceKey "j" `shouldBe` Just ChoiceDown
            parseChoiceKey "\n" `shouldBe` Just ChoiceEnter
            parseChoiceKey "\r" `shouldBe` Just ChoiceEnter
            parseChoiceKey "\ESC" `shouldBe` Just ChoiceCancel
            parseChoiceKey "q" `shouldBe` Just ChoiceCancel
            parseChoiceKey "3" `shouldBe` Just (ChoiceDigit 3)
            parseChoiceKey "x" `shouldBe` Nothing

    describe "choiceMoveIndex" do
        it "wraps at both ends" do
            choiceMoveIndex 3 0 ChoiceUp `shouldBe` 2
            choiceMoveIndex 3 2 ChoiceDown `shouldBe` 0
            choiceMoveIndex 3 1 ChoiceUp `shouldBe` 0
            choiceMoveIndex 3 1 ChoiceDown `shouldBe` 2
            choiceMoveIndex 3 1 ChoiceEnter `shouldBe` 1

    describe "classifyPastedText" do
        it "detects bracketed-paste CSI wrappers" do
            let payload = "hello from clipboard"
                wrapped = "\ESC[200~" <> payload <> "\ESC[201~"
            classifyPastedText wrapped `shouldBe` (payload, True)
            classifyPastedText payload `shouldBe` (payload, False)

        it "detects printable sentinels from older input versions" do
            let payload = "hello from clipboard"
                wrapped = Text.pack [toEnum 0x27E6] <> payload
                    <> Text.pack [toEnum 0x27E7]
            classifyPastedText wrapped `shouldBe` (payload, True)

        it "treats a 4-line burst as a paste" do
            let burst = Text.unlines ["one", "two", "three", "four"]
            classifyPastedText burst `shouldBe` (burst, True)

    describe "submissionPromptText" do
        it "keeps an empty composer inert without attachments" do
            submissionPromptText 0 "" `shouldBe` Nothing
            submissionPromptText 0 " \n " `shouldBe` Nothing

        it "supplies fallback text for an image-only submission" do
            submissionPromptText 1 ""
                `shouldBe` Just "The user attached an image."
            submissionPromptText 2 " \n "
                `shouldBe` Just "The user attached an image."

        it "preserves user text exactly when attachments are present" do
            submissionPromptText 1 "  describe this  "
                `shouldBe` Just "  describe this  "

    describe "formatPasteChip" do
        it "keeps short pastes inline and chips long ones" do
            formatPasteChip "one line" `shouldBe` "one line"
            formatPasteChip (Text.unlines ["a", "b", "c", "d"])
                `shouldBe` "[Pasted: 4 lines]"

    describe "decodeBracketedPastePayload" do
        it "extracts a payload through the first end marker" do
            decodeBracketedPastePayload 20 "hello\ESC[201~ignored"
                `shouldBe` Right "hello"

        it "rejects incomplete and oversized pastes" do
            decodeBracketedPastePayload 20 "no end marker"
                `shouldSatisfy` isLeft
            decodeBracketedPastePayload 4 "hello\ESC[201~"
                `shouldSatisfy` isLeft

        it "accepts a payload exactly at the configured limit" do
            decodeBracketedPastePayload 5 "hello\ESC[201~"
                `shouldBe` Right "hello"

    describe "safe editor rendering" do
        it "renders pasted terminal controls as visible characters" do
            displayEditorText "\ESC]0;owned\BEL"
                `shouldBe` "␛]0;owned␇"

        it "does not leak a newline joined to a variation selector" do
            let input = Text.pack ['\n', '\xfe0f']
                displayed = displayEditorText input
            displayed `shouldBe` Text.singleton '\x21b5'
            displayed `shouldSatisfy` not . Text.any (== '\n')
            V.safeWctwidth displayed `shouldBe` terminalTextWidth input

        it "measures wide and combining Unicode in terminal columns" do
            terminalTextWidth "a界🙂e\x0301" `shouldBe` 6
            visibleEditorText 3 "a界b" 2 `shouldBe` ("界b", 2)

        it "keeps a wide cursor glyph visible in a narrow viewport" do
            visibleEditorText 2 "a界" 2 `shouldBe` ("界", 2)
            visibleEditorText 1 "界" 1 `shouldBe` ("…", 1)

        it "does not detach combining marks from a hidden wide glyph" do
            visibleEditorText 1 "界\x0301" 2 `shouldBe` ("…", 1)
            visibleEditorText 2 "a界\x0301" 3 `shouldBe` ("界\x0301", 2)

        it "preserves supported emoji and safely substitutes width mismatches" do
            let womanTechnologist =
                    Text.pack ['\x1f469', '\x200d', '\x1f4bb']
                usFlag =
                    Text.pack ['\x1f1fa', '\x1f1f8']
                thumbsUpMedium =
                    Text.pack ['\x1f44d', '\x1f3fd']
                keycapOne =
                    Text.pack ['1', '\xfe0f', '\x20e3']
                text =
                    womanTechnologist
                        <> usFlag
                        <> thumbsUpMedium
                        <> keycapOne
                displayed =
                    womanTechnologist
                        <> usFlag
                        <> thumbsUpMedium
                        <> "１"
            displayEditorText text `shouldBe` displayed
            terminalTextWidth text `shouldBe` 8
            visibleEditorText 2 womanTechnologist 3
                `shouldBe` (womanTechnologist, 2)
            visibleEditorText 1 womanTechnologist 3
                `shouldBe` ("…", 1)

        it "truncates the complete row without exceeding its column budget" do
            let truncated = truncateDisplayText 5 "/always-approve"
            truncated `shouldBe` "/alw…"
            terminalTextWidth truncated `shouldBe` 5

        modifyMaxSuccess (const 1000) $
            prop "keeps generated editor viewports visible and in bounds" $
                editorViewportProperty

    describe "clipboard image paste key" do
        it "recognizes legacy Ctrl+V without treating ordinary v as paste" do
            isClipboardPasteKey '\SYN' `shouldBe` True
            isClipboardPasteKey 'v' `shouldBe` False

        it "recognizes Kitty keyboard Ctrl+V and Cmd+V press events" do
            isClipboardPasteCsiBody "118;5u" `shouldBe` True
            isClipboardPasteCsiBody "118;9u" `shouldBe` True
            isClipboardPasteCsiBody "118;9:1u" `shouldBe` True
            isClipboardPasteCsiBody "118:86:86;9u" `shouldBe` True

        it "rejects unmodified v, other modified keys, and key releases" do
            isClipboardPasteCsiBody "118u" `shouldBe` False
            isClipboardPasteCsiBody "99;9u" `shouldBe` False
            isClipboardPasteCsiBody "118;9:3u" `shouldBe` False
            isClipboardPasteCsiBody "not-a-key" `shouldBe` False

    describe "Kitty Ctrl+R dictation" do
        it "decodes press events as dictation and ignores key releases" do
            decodeKittyEditorKey "114;5u" `shouldBe` Just EditorDictate
            decodeKittyEditorKey "114;5:1u" `shouldBe` Just EditorDictate
            decodeKittyEditorKey "114:82:82;5u" `shouldBe` Just EditorDictate
            decodeKittyEditorKey "114;5:3u" `shouldBe` Just EditorIgnore
            decodeKittyEditorKey "117;5u" `shouldBe` Just EditorKillStart

    describe "Shift+Enter" do
        it "recognizes xterm modifyOtherKeys and Kitty CSI-u encodings" do
            isShiftEnterCsiBody "27;2;13~" `shouldBe` True
            isShiftEnterCsiBody "13;2u" `shouldBe` True
            isShiftEnterCsiBody "13u" `shouldBe` False

    describe "inline editor reducer" do
        it "reduces common editing keys without performing IO" do
            let initial = editorState "a界b" 2
                cases =
                    [ ( EditorLeft
                      , "a界b"
                      , 1
                      )
                    , ( EditorRight
                      , "a界b"
                      , 3
                      )
                    , ( EditorBackspace
                      , "ab"
                      , 1
                      )
                    , ( EditorDelete
                      , "a界"
                      , 2
                      )
                    , ( EditorChar '!'
                      , "a界!b"
                      , 3
                      )
                    ]
            mapM_
                (\(key, expectedText, expectedCursor) -> do
                    let step = reduceEditorKey [] initial key
                    step.editorStepEffect `shouldBe` RedrawEditor
                    step.editorStepState.editorText `shouldBe` expectedText
                    step.editorStepState.editorCursor
                        `shouldBe` expectedCursor)
                cases

        it "moves through history and restores the in-progress draft" do
            let initial = editorState "draft" 5
                newest = reduceEditorKey ["new", "old"] initial EditorUp
                older =
                    reduceEditorKey
                        ["new", "old"]
                        newest.editorStepState
                        EditorUp
                restored =
                    reduceEditorKey
                        ["new", "old"]
                        newest.editorStepState
                        EditorDown
            newest.editorStepState.editorText `shouldBe` "new"
            newest.editorStepState.editorHistoryIndex `shouldBe` Just 0
            older.editorStepState.editorText `shouldBe` "old"
            older.editorStepState.editorHistoryIndex `shouldBe` Just 1
            restored.editorStepState.editorText `shouldBe` "draft"
            restored.editorStepState.editorHistoryIndex `shouldBe` Nothing

        it "turns effectful keys into explicit requests" do
            let initial = editorState "draft" 5
                eofStep =
                    reduceEditorKey [] (editorState "" 0) EditorEof
                effects =
                    [ (EditorEnter, SubmitEditor)
                    , (EditorCycleMode, ReturnEditor (ReplCycleMode "draft"))
                    , ( EditorClipboardPaste Nothing
                      , ReturnEditor (ReplClipboardPaste "draft" Nothing)
                      )
                    , (EditorInterrupt, CheckEditorInterrupt)
                    , (EditorClearScreen, ClearEditorScreen)
                    , (EditorDictate, DictateIntoEditor)
                    , (EditorInputError "bad key", ReportEditorError "bad key")
                    , (EditorIgnore, IgnoreEditorInput)
                    ]
            mapM_
                (\(key, expected) ->
                    (reduceEditorKey [] initial key).editorStepEffect
                        `shouldBe` expected)
                effects
            eofStep.editorStepEffect
                `shouldBe` ReturnEditor ReplEof

        it "preserves kill and yank behavior at text boundaries" do
            let initial = editorState "one  two" 8
                killed = reduceEditorKey [] initial EditorKillWord
                yanked =
                    reduceEditorKey [] killed.editorStepState EditorYank
                start = editorState "draft" 0
                end = editorState "draft" 5
            killed.editorStepState.editorText `shouldBe` "one  "
            killed.editorStepState.editorCursor `shouldBe` 5
            killed.editorStepState.editorKillBuffer `shouldBe` "two"
            yanked.editorStepState.editorText `shouldBe` "one  two"
            yanked.editorStepState.editorCursor `shouldBe` 8
            (reduceEditorKey [] start EditorBackspace).editorStepState
                `shouldBe` start
            (reduceEditorKey [] end EditorDelete).editorStepState
                `shouldBe` end

        it "returns paste classification without mutating the live draft" do
            let initial = editorState "ac" 1
                step = reduceEditorKey [] initial (EditorPaste "b")
            step.editorStepState `shouldBe` initial
            step.editorStepEffect
                `shouldBe`
                    ReturnEditor (ReplClipboardPasteOrText "ac" "b" "abc")

        it "keeps slash completion decisions in the pure layer" do
            let helpDraft =
                    initialEditorState defaultSlashCatalog True "/he"
                accepted =
                    reduceEditorKey [] helpDraft EditorEnter
                quitDraft =
                    initialEditorState defaultSlashCatalog True "/qu"
                submitted =
                    reduceEditorKey [] quitDraft EditorEnter
                dismissed =
                    reduceEditorKey [] helpDraft EditorEscape
            accepted.editorStepEffect `shouldBe` RedrawEditor
            accepted.editorStepState.editorText `shouldBe` "/help "
            submitted.editorStepEffect `shouldBe` SubmitEditor
            submitted.editorStepState.editorText `shouldBe` "/quit"
            dismissed.editorStepEffect `shouldBe` RedrawEditor
            dismissed.editorStepState.editorSlashDismissed `shouldBe` True

        modifyMaxSuccess (const 1000) $
            prop "keeps the cursor in bounds across generated transitions" $
                editorReducerCursorProperty

editorViewportProperty :: EditorViewportCase -> Property
editorViewportProperty (EditorViewportCase available raw requestedCursor) =
    conjoin
        [ counterexample "visible text exceeds available columns"
            ((terminalTextWidth shown <= available) === True)
        , counterexample "visible cursor is outside rendered text"
            ((column >= 0 && column <= terminalTextWidth shown) === True)
        , counterexample "nonempty editor text disappeared"
            ((not (Text.null shown)) === True)
        , counterexample "visible text contains terminal controls"
            (Text.all (not . isControl) shown === True)
        , counterexample "Vty and editor width models disagree"
            (V.safeWctwidth shown === terminalTextWidth shown)
        ]
  where
    cursor = max 0 (min (Text.length raw) requestedCursor)
    (shown, column) = visibleEditorText available raw cursor

instance Arbitrary EditorViewportCase where
    arbitrary = do
        available <- chooseInt (1, 80)
        atomCount <- chooseInt (1, 90)
        raw <- Text.concat <$> vectorOf atomCount genEditorAtom
        cursor <- chooseInt (-20, Text.length raw + 20)
        pure $
            EditorViewportCase
                available
                raw
                cursor

genEditorAtom :: Gen Text.Text
genEditorAtom =
    elements
        [ "a", "z", "0", " ", "\t", "\r", "\n"
        , "界", "語", "🙂", "🚀", "é", "ø", "e\x0301"
        , Text.pack ['\x1f469', '\x200d', '\x1f4bb']
        , Text.pack ['\x1f1fa', '\x1f1f8']
        , Text.pack ['\x1f44d', '\x1f3fd']
        , Text.pack ['1', '\xfe0f', '\x20e3']
        ]

editorState :: Text.Text -> Int -> EditorState
editorState text cursor =
    (initialEditorState defaultSlashCatalog False text)
        { editorCursor = cursor
        }

editorReducerCursorProperty :: EditorKeySequence -> Property
editorReducerCursorProperty (EditorKeySequence keys) =
    conjoin $
        zipWith cursorProperty [0 :: Int ..] states
  where
    initial = editorState "界e\x0301🙂" 4
    (_, states) = mapAccumL reduce initial keys
    reduce state key =
        let next = (reduceEditorKey [] state key).editorStepState
        in (next, next)
    cursorProperty index state =
        counterexample
            ("transition " <> show index <> " produced " <> show state)
            ( (state.editorCursor >= 0
                && state.editorCursor <= Text.length state.editorText)
                === True
            )

instance Arbitrary EditorKeySequence where
    arbitrary = do
        count <- chooseInt (0, 200)
        EditorKeySequence <$> vectorOf count genPureEditorKey

genPureEditorKey :: Gen EditorKey
genPureEditorKey =
    elements
        [ EditorChar 'a'
        , EditorChar '界'
        , EditorChar '\x0301'
        , EditorChar '🙂'
        , EditorBackspace
        , EditorDelete
        , EditorLeft
        , EditorRight
        , EditorHome
        , EditorEnd
        , EditorKillStart
        , EditorKillEnd
        , EditorKillWord
        , EditorYank
        , EditorEof
        , EditorIgnore
        ]
