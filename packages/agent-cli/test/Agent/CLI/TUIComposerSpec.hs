module Agent.CLI.TUIComposerSpec (spec) where

import Agent.CLI.Auth (LoadedAuth(..), staticCredentialProvider)
import Agent.CLI.Dictation
    ( DictationAuthError(..)
    , DictationBackend(..)
    , DictationTarget(..)
    , dictationBackendsForProvider
    , dictationBackendUnavailable
    , dictationTargetForSession
    , insertDictation
    , selectDictationBackend
    )
import Agent.CLI.GatewayClient (newGatewayModelAccessWith)
import Agent.CLI.Command (CopyRequest(..), ReplAction(..))
import Agent.Provider
    ( BillingMode(..)
    , Credential(..)
    , Provider(..)
    )
import Agent.CLI.Input
    ( ReplLine(..)
    , displayEditorText
    , terminalTextWidth
    )
import Agent.CLI.TUI.Composer
import Agent.CLI.TUI.Types
import Agent.TUI.Model
    ( NoticeKind(..)
    , UiNotice(..)
    , UiState(..)
    , initialUiState
    )
import Agent.TUI.TextWidth
    ( clampGraphemeCursor
    , graphemeCellWidth
    , graphemeClusters
    , nextGraphemeBoundary
    , previousGraphemeBoundary
    )
import Control.Concurrent.STM (atomically)
import qualified Data.ByteString as ByteString
import Data.Char (isControl)
import Data.Foldable (toList)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Graphics.Vty as V
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import qualified Graphics.Vty as V
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

data DraftCase = DraftCase !Int !Text !Int
    deriving (Show)

newtype PasteBytes = PasteBytes ByteString.ByteString
    deriving (Show)

spec :: Spec
spec = describe "fullscreen composer" do
    it "cancels a running turn even when the slash menu is open" do
        composerEscapeAction False True
            `shouldBe` EscapeCancelTurn

    it "runs /btw immediately only while a turn is active" do
        immediateBtwQuestion
            initialUiState { uiRunning = True }
            (ReplText "/btw why?")
            `shouldBe` Just "why?"
        immediateBtwQuestion
            initialUiState
            (ReplText "/btw why?")
            `shouldBe` Nothing
        immediateBtwQuestion
            initialUiState { uiRunning = True }
            (ReplText "ordinary follow-up")
            `shouldBe` Nothing

    it "runs safe inspection and copy commands immediately during active turns" do
        let running = initialUiState { uiRunning = True }
        immediateReplCommand running (ReplText "/copy")
            `shouldBe` Just (ReplCopy (CopyRequest 1 Nothing))
        immediateReplCommand running (ReplText "/copy-code 2")
            `shouldBe` Just (ReplCopyCode 2)
        immediateReplCommand running (ReplText "/copy-diff")
            `shouldBe` Just ReplCopyDiff
        immediateReplCommand running (ReplText "/copy-path")
            `shouldBe` Just ReplCopyPath
        immediateReplCommand running (ReplText "/copy-session")
            `shouldBe` Just ReplCopySession
        immediateReplCommand running (ReplText "/queue")
            `shouldBe` Just ReplQueue
        immediateReplCommand running (ReplText "/context")
            `shouldBe` Just ReplContext
        immediateReplCommand initialUiState (ReplText "/copy-path")
            `shouldBe` Nothing
        immediateReplCommand running (ReplText "/copy 2")
            `shouldBe` Nothing
        immediateReplCommand running (ReplText "/copy TO answer.md")
            `shouldBe` Nothing
        immediateReplCommand running (ReplText "/agents")
            `shouldBe` Nothing

    it "preserves paste provenance for steering prompts" do
        steeringPrompt
            initialUiState { uiRunning = True }
            True
            "WHERE listings.agent_id = $1"
            `shouldBe` Just (True, "WHERE listings.agent_id = $1")
        steeringPrompt initialUiState True "not running"
            `shouldBe` Nothing

    it "dismisses slash completion or preserves the draft while idle" do
        composerEscapeAction True True
            `shouldBe` EscapeDismissSlashMenu
        composerEscapeAction True False
            `shouldBe` EscapePreserveDraft

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

    it "bounds composer layout to logical lines around the cursor" do
        let draft =
                Text.intercalate
                    "\n"
                    (map (Text.pack . show) [1 :: Int .. 100])
            cursor = Text.length draft
            (rows, location) = wrapDraftWindow 8 10 draft cursor
        rows `shouldBe` map (Text.pack . show) [93 :: Int .. 100]
        location `shouldBe` (7, 3)
        Text.drop (draftWindowStart 8 draft cursor) draft
            `shouldBe` Text.intercalate
                "\n"
                (map (Text.pack . show) [93 :: Int .. 100])

    it "keeps enough following rows when the cursor is near the start" do
        let draft =
                Text.intercalate
                    "\n"
                    (map (Text.pack . show) [1 :: Int .. 100])
            (rows, location) = wrapDraftWindow 8 10 draft 0
        take 8 rows `shouldBe` map (Text.pack . show) [1 :: Int .. 8]
        location `shouldBe` (0, 0)
        draftWindowStart 8 draft 0 `shouldBe` 0

    it "distinguishes the cursor positions before and after a full-row newline" do
        wrapDraft 2 "ab\nc" 2
            `shouldBe` (["ab", "c"], (0, 2))
        wrapDraft 2 "ab\nc" 3
            `shouldBe` (["ab", "c"], (1, 0))

    it "wraps using terminal columns for wide characters" do
        wrapDraft 3 "a界b" 2
            `shouldBe` (["a界", "b"], (1, 0))

    it "keeps combining marks attached at a visual boundary" do
        wrapDraft 1 "e\x0301x" 2
            `shouldBe` (["e\x0301", "x"], (1, 0))

    it "fits wide glyphs into a one-cell composer" do
        wrapDraft 1 "界" 1
            `shouldBe` (["…", ""], (1, 0))

    it "never splits emoji graphemes while wrapping or placing the cursor" do
        let womanTechnologist =
                Text.pack ['\x1f469', '\x200d', '\x1f4bb']
        wrapDraft 2 womanTechnologist 3
            `shouldBe` ([womanTechnologist, ""], (1, 0))
        wrapDraft 2 ("a" <> womanTechnologist) 4
            `shouldBe` (["a", womanTechnologist, ""], (2, 0))
        wrapDraft 1 womanTechnologist 3
            `shouldBe` (["…", ""], (1, 0))
        draftCursorLocation womanTechnologist 1 `shouldBe` (0, 0)
        draftCursorLocation womanTechnologist 3 `shouldBe` (0, 2)

    it "moves the cursor between logical lines preserving the column" do
        verticalCursorMove 1 "one\ntwo three" 2 `shouldBe` Just 6
        verticalCursorMove (-1) "one\ntwo" 6 `shouldBe` Just 2

    it "keeps vertical movement inside the draft edges" do
        verticalCursorMove (-1) "one\ntwo" 2 `shouldBe` Nothing
        verticalCursorMove 1 "one\ntwo" 6 `shouldBe` Nothing
        verticalCursorMove (-1) "one" 1 `shouldBe` Nothing
        verticalCursorMove 1 "one" 1 `shouldBe` Nothing

    it "clamps vertical movement to shorter target lines" do
        verticalCursorMove 1 "abcdef\nxy" 5 `shouldBe` Just 9

    it "does not split wide characters when moving vertically" do
        verticalCursorMove 1 "abc\n\20320\22909" 3 `shouldBe` Just 5
        verticalCursorMove (-1) "\20320\22909\nabcd" 6 `shouldBe` Just 1
        verticalCursorMove (-1) "\20320\22909\nabcd" 7 `shouldBe` Just 2

    it "combines consecutive kills in the kill direction" do
        combineKill KillBackward "foo " "bar" `shouldBe` "foo bar"
        combineKill KillForward " bar" "foo" `shouldBe` "foo bar"

    it "classifies kill keys for kill-buffer accumulation" do
        isKillKey (V.EvKey (V.KChar 'w') [V.MCtrl]) `shouldBe` True
        isKillKey (V.EvKey (V.KChar 'k') [V.MCtrl, V.MShift])
            `shouldBe` True
        isKillKey (V.EvKey (V.KChar 'd') [V.MMeta]) `shouldBe` True
        isKillKey (V.EvKey V.KBS [V.MMeta]) `shouldBe` True
        isKillKey (V.EvKey V.KBS []) `shouldBe` False
        isKillKey (V.EvKey (V.KChar 'd') [V.MCtrl]) `shouldBe` False
        isKillKey (V.EvKey (V.KChar 'x') [V.MCtrl]) `shouldBe` False

    it "keeps the slash selection visible while scrolling" do
        slashMenuWindowStart 6 4 3 `shouldBe` 0
        slashMenuWindowStart 6 10 0 `shouldBe` 0
        slashMenuWindowStart 6 10 2 `shouldBe` 0
        slashMenuWindowStart 6 10 5 `shouldBe` 3
        slashMenuWindowStart 6 10 9 `shouldBe` 4

    modifyMaxSuccess (const 500) $
        prop "wraps generated Unicode drafts without overflowing rows" $
            wrapDraftProperty

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

    it "inserts dictation at the cursor with word-safe spacing" do
        insertDictation "please now" 6 "fix this"
            `shouldBe` ("please fix this now", 15)
        insertDictation "hello" 5 ", world"
            `shouldBe` ("hello, world", 12)

    it "routes dictation through the active model provider" do
        dictationBackendsForProvider OpenAIProvider
            `shouldBe` Right (OpenAIDictation :| [])
        dictationBackendsForProvider XAIProvider
            `shouldBe` Right (XAIDictation :| [])
        dictationBackendsForProvider OpenRouterProvider
            `shouldBe` Left
                "Dictation is not supported for openrouter models"
        dictationBackendsForProvider GeminiProvider
            `shouldBe` Left
                "Dictation is not supported for gemini models"

    it "borrows an OpenAI account before an xAI account for Claude" do
        dictationBackendsForProvider ClaudeCodeProvider
            `shouldBe` Right (OpenAIDictation :| [XAIDictation])
        attempted <- newIORef []
        let loadBackend backend = do
                modifyIORef' attempted (<> [backend])
                pure $ Right (fakeLoadedAuth backend)
        selected <- selectDictationBackend ClaudeCodeProvider loadBackend
        fmap fst selected `shouldBe` Right OpenAIDictation
        readIORef attempted `shouldReturn` [OpenAIDictation]

    it "falls back to xAI when Claude has no OpenAI credential" do
        attempted <- newIORef []
        let loadBackend backend = do
                modifyIORef' attempted (<> [backend])
                pure case backend of
                    OpenAIDictation ->
                        Left (DictationCredentialMissing "no openai")
                    XAIDictation ->
                        Right (fakeLoadedAuth backend)
        selected <- selectDictationBackend ClaudeCodeProvider loadBackend
        fmap fst selected `shouldBe` Right XAIDictation
        readIORef attempted `shouldReturn` [OpenAIDictation, XAIDictation]

    it "never borrows another provider's account for native dictation" do
        attempted <- newIORef []
        let loadBackend backend = do
                modifyIORef' attempted (<> [backend])
                pure (Left (DictationCredentialMissing "no credential"))
        selected <- selectDictationBackend XAIProvider loadBackend
        fmap fst selected `shouldBe` Left "no credential"
        readIORef attempted `shouldReturn` [XAIDictation]
        rejected <-
            selectDictationBackend OpenRouterProvider loadBackend
        fmap fst rejected
            `shouldBe` Left "Dictation is not supported for openrouter models"

    it "summarizes missing borrowed accounts without provider sign-in hints" do
        dictationBackendUnavailable
            ClaudeCodeProvider
            [ (OpenAIDictation, DictationCredentialMissing "no openai")
            , (XAIDictation, DictationCredentialMissing "no credentials found.")
            ]
            `shouldBe`
                "Dictation for claude-code models requires an OpenAI or xAI \
                \account; connect one with /login"
        dictationBackendUnavailable
            ClaudeCodeProvider
            [ (OpenAIDictation, DictationCredentialMissing "no openai")
            , (XAIDictation, DictationCredentialInvalid "invalid auth JSON")
            ]
            `shouldBe`
                "Dictation for claude-code models requires an OpenAI or xAI \
                \account; connect one with /login (xai: invalid auth JSON)"
        dictationBackendUnavailable
            XAIProvider
            [(XAIDictation, DictationCredentialInvalid "invalid auth JSON")]
            `shouldBe` "invalid auth JSON"

    it "keeps organization-gateway dictation inside the gateway boundary" do
        case dictationTargetForSession XAIProvider Nothing of
            DirectDictation provider ->
                provider `shouldBe` XAIProvider
            GatewayDictation _ ->
                expectationFailure "expected direct dictation"
        gateway <- newGatewayModelAccessWith (pure (Right []))
        case dictationTargetForSession XAIProvider (Just gateway) of
            GatewayDictation _ -> pure ()
            DirectDictation _ ->
                expectationFailure "expected gateway dictation"

    it "keeps dictation stop keys inside the composer" do
        dictationKeyAction (V.EvKey V.KEnter [])
            `shouldBe` Just DictationCommit
        dictationKeyAction (V.EvKey (V.KChar 'r') [V.MCtrl])
            `shouldBe` Just DictationCommit
        dictationKeyAction (V.EvKey (V.KChar '\DC2') [])
            `shouldBe` Just DictationCommit
        dictationKeyAction (V.EvKey V.KEsc [])
            `shouldBe` Just DictationAbort
        dictationKeyAction (V.EvKey (V.KChar 'c') [V.MCtrl])
            `shouldBe` Just DictationAbort
        dictationKeyAction (V.EvKey (V.KChar 'x') [])
            `shouldBe` Nothing

    it "renders a non-transient listening notice for live transcripts" do
        let idle = dictationProgressNotice ""
            live = dictationProgressNotice "hello from the mic"
        idle.noticeKind `shouldBe` NoticeProgress
        idle.noticeTransient `shouldBe` False
        idle.noticeText
            `shouldBe` "Listening… Enter to stop · Esc to cancel"
        live.noticeText `shouldBe` "Listening… hello from the mic"

    modifyMaxSuccess (const 300) $
        prop "decodes arbitrary paste bytes into stable safe text" $
            decodePasteProperty

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

    it "bounds queued prompts and admits another after consumption" do
        buffer <- newFullscreenInputBuffer
        let prompt index = FullscreenInput
                { fullscreenInputLine =
                    ReplText (Text.pack (show index))
                , fullscreenInputQueued = True
                , fullscreenInputDisplay = Nothing
                }
        accepted <- atomically $
            mapM (appendFullscreenInput buffer . prompt)
                [1 .. fullscreenInputCountLimit]
        accepted `shouldSatisfy` all (== Right ())
        atomically (appendFullscreenInput buffer (prompt 129))
            `shouldReturn`
                Left
                    "Prompt queue is full; wait for a queued prompt to be consumed."
        _ <- atomically (takeFullscreenInput buffer)
        atomically (appendFullscreenInput buffer (prompt 129))
            `shouldReturn` Right ()
  where
    input replLine = FullscreenInput
        { fullscreenInputLine = replLine
        , fullscreenInputQueued = True
        , fullscreenInputDisplay = Nothing
        }

wrapDraftProperty :: DraftCase -> Property
wrapDraftProperty (DraftCase width draft requestedCursor) =
    conjoin
        [ counterexample "wrapped rows are nonempty"
            (not (null rows) === True)
        , counterexample "wrapped row width"
            (all ((<= width) . terminalTextWidth) rows === True)
        , counterexample "cursor row"
            ((cursorRow >= 0 && cursorRow < length rows) === True)
        , counterexample "cursor column"
            ((cursorColumn >= 0 && cursorColumn <= width) === True)
        , counterexample "wrapping preserves draft text when glyphs fit"
            (if all ((<= width) . graphemeCellWidth)
                    (filter (/= "\n") (graphemeClusters draft))
                then Text.concat rows === displayedDraft
                else True === True)
        , counterexample "cursor movement is monotonic"
            ((previousLocation <= location && location <= nextLocation)
                === True)
        , counterexample "explicit newline moves the cursor"
            (all explicitNewlineMoves newlineOffsets === True)
        ]
  where
    cursor = clampGraphemeCursor draft requestedCursor
    (rows, location@(cursorRow, cursorColumn)) =
        wrapDraft width draft cursor
    (_, previousLocation) =
        wrapDraft width draft
            (previousGraphemeBoundary draft cursor)
    (_, nextLocation) =
        wrapDraft width draft
            (nextGraphemeBoundary draft cursor)
    displayedDraft =
        Text.concat (map displayEditorText (Text.splitOn "\n" draft))
    newlineOffsets =
        [ index
        | (index, character) <- zip [0 :: Int ..] (Text.unpack draft)
        , character == '\n'
        ]
    explicitNewlineMoves index =
        snd (wrapDraft width draft index)
            /= snd (wrapDraft width draft (index + 1))

decodePasteProperty :: PasteBytes -> Property
decodePasteProperty (PasteBytes bytes) =
    conjoin
        [ counterexample "paste contains unsafe controls"
            (Text.all allowed decoded === True)
        , counterexample "paste decoding is idempotent"
            (decodePaste (Text.encodeUtf8 decoded) === decoded)
        ]
  where
    decoded = decodePaste bytes
    allowed character =
        character == '\n'
            || character == '\t'
            || not (isControl character)

instance Arbitrary DraftCase where
    arbitrary = do
        width <- chooseInt (1, 120)
        atomCount <- chooseInt (0, 110)
        draft <- Text.concat <$> vectorOf atomCount genDraftAtom
        cursor <- chooseInt (-20, Text.length draft + 20)
        pure (DraftCase width draft cursor)

instance Arbitrary PasteBytes where
    arbitrary = do
        size <- chooseInt (0, 400)
        PasteBytes . ByteString.pack
            <$> vectorOf size (fromIntegral <$> chooseInt (0, 255))

genDraftAtom :: Gen Text
genDraftAtom =
    elements
        [ "a", "z", "0", " ", " ", "\n"
        , "界", "語", "🙂", "🚀", "é", "ø", "e\x0301"
        , Text.pack ['\x1f469', '\x200d', '\x1f4bb']
        , Text.pack
            [ '\x1f468', '\x200d', '\x1f469', '\x200d'
            , '\x1f467', '\x200d', '\x1f466'
            ]
        , Text.pack ['\x1f1fa', '\x1f1f8']
        , Text.pack ['\x1f44d', '\x1f3fd']
        , Text.pack ['1', '\xfe0f', '\x20e3']
        ]

fakeLoadedAuth :: DictationBackend -> LoadedAuth
fakeLoadedAuth backend =
    LoadedAuth
        { loadedProvider = provider
        , loadedTokenProvider =
            staticCredentialProvider ApiBilled credential
        , loadedAccountLabel = const (pure "fake")
        , loadedSelectionId = Nothing
        , loadedOpenAiPool = Nothing
        }
  where
    provider = case backend of
        OpenAIDictation -> OpenAIProvider
        XAIDictation -> XAIProvider
    credential =
        Credential
            { accessToken = "token"
            , accountId = "account"
            , leaseId = Nothing
            , provider
            }
