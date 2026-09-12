{-# LANGUAGE OverloadedStrings #-}

module Agent.CLI.TUIMeasuredViewportSpec (spec) where

import Agent.CLI.TUI.MeasuredViewport (measuredViewport, measuredViewportWithFooter)
import Agent.CLI.TUI.Scroll (conversationMessagesBelow, conversationMessagesBelowLabel)
import Brick
    ( App(..)
    , AttrMap
    , BrickEvent(..)
    , CursorLocation
    , EventM
    , Extent(..)
    , Location(..)
    , VScrollbarRenderer(..)
    , ViewportType(Vertical)
    , Widget
    , attrMap
    , attrName
    , cached
    , clickable
    , emptyWidget
    , fill
    , hBox
    , joinBorders
    , withVScrollBars
    , withVScrollBarRenderer
    , withAttr
    , VScrollBarOrientation(OnRight)
    , vLimit
    , customMain
    , halt
    , makeVisible
    , neverShowCursor
    , lookupExtent
    , lookupViewport
    , reportExtent
    , showCursor
    , showFirstCursor
    , txtWrap
    , txt
    , vBox
    , viewport
    , viewportScroll
    , vScrollBy
    , vScrollToEnd
    )
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (hBorder, vBorder)
import Control.Concurrent.STM
    ( atomically
    , newTChanIO
    , readTChan
    , retry
    , writeTChan
    )
import Control.Monad.IO.Class (liftIO)
import Data.IORef
    ( IORef
    , modifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.Foldable (toList)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Output.Mock as VMock
import Graphics.Vty.PictureToSpans (displayOpsForPic)
import Graphics.Vty.Span (SpanOp(..))
import Test.Hspec

data Name
    = Transcript
    | Chunk !Int
    | ChunkCache !Int
    | NewerGap
    deriving (Eq, Ord, Show)

data Renderer = Ordinary | Measured | OrdinaryScrollbar | MeasuredScrollbar | MeasuredFooter

data ScriptEvent
    = ScrollBy !Int
    | ScrollEnd
    | Reveal !Name
    | RevealMany ![Name]
    | Resize !V.DisplayRegion
    | Snapshot
    | Stop

spec :: Spec
spec =
    describe "measuredViewport" do
        it "drops the lower-bound marker after scrolling past a newer-history gap" do
            let content =
                    [ reportExtent (Chunk 0) $ vBox (replicate 4 (txt "history"))
                    , reportExtent NewerGap (txt "unloaded history")
                    , reportExtent (Chunk 1) $ vBox (replicate 4 (txt "live message"))
                    ]
            (beforeGap, _) <- runScript MeasuredFooter (40, 5) content [Stop]
            snd (last beforeGap) `shouldContain` "1+ Messages"
            (afterGap, _) <- runScript MeasuredFooter (40, 5) content
                [ScrollBy 2, Stop]
            snd (last afterGap) `shouldContain` "1 Message"
            snd (last afterGap) `shouldNotContain` "+ Messages"
            (bottom, _) <- runScript MeasuredFooter (40, 5) content
                [ScrollEnd, Stop]
            snd (last bottom) `shouldNotContain` "Message"

        it "counts offscreen cached message extents after scrolling and resizing" do
            let content =
                    [ cached (ChunkCache index) $
                        reportExtent (Chunk index) $
                            vBox (replicate 3 (txt "message"))
                    | index <- [0 .. 3]
                    ]
            (initial, _) <- runScript MeasuredFooter (40, 5) content [Stop]
            snd (last initial) `shouldContain` "3 Messages"
            (scrolled, _) <- runScript MeasuredFooter (40, 5) content
                [ScrollBy 3, Stop]
            snd (last scrolled) `shouldContain` "2 Messages"
            (resized, _) <- runScript MeasuredFooter (40, 5) content
                [Resize (40, 10), Stop]
            snd (last resized) `shouldContain` "1 Message"
            (bottom, _) <- runScript MeasuredFooter (40, 5) content
                [ScrollEnd, Stop]
            snd (last bottom) `shouldNotContain` "Message"

        it "uses an oracle which detects rendered text differences" do
            first <- runScript Ordinary (20, 3)
                [txtWrap "expected text"] [Stop]
            second <- runScript Ordinary (20, 3)
                [txtWrap "different text"] [Stop]
            first `shouldNotBe` second

        it "matches an ordinary viewport across arbitrary scrolling" do
            let script =
                    [ ScrollBy 17
                    , ScrollBy 31
                    , ScrollBy (-9)
                    , ScrollEnd
                    , ScrollBy (-23)
                    , Stop
                    ]
            ordinary <- runScript Ordinary (42, 9) chunks script
            measured <- runScript Measured (42, 9) chunks script
            measured `shouldBe` ordinary

        it "preserves the right scrollbar through scroll and resize" do
            let script = [ScrollBy 17, ScrollEnd, Resize (25, 11), ScrollBy (-9), Stop]
            ordinary <- runScript OrdinaryScrollbar (42, 9) chunks script
            measured <- runScript MeasuredScrollbar (42, 9) chunks script
            measured `shouldBe` ordinary

        it "matches short and narrow scrollbar viewports" do
            mapM_
                (\(bounds, content) -> do
                    let script = [ScrollEnd, ScrollBy (-3), Stop]
                    ordinary <- runScript OrdinaryScrollbar bounds content script
                    measured <- runScript MeasuredScrollbar bounds content script
                    measured `shouldBe` ordinary)
                [(bounds, content) | bounds <- [(40, 12), (2, 3), (1, 1)]
                                   , content <- [[], take 1 chunks]]

        it "joins borders at offscreen chunk boundaries" do
            let content = concat $ replicate 20
                    [hBorder, vLimit 3 (hBox [vBorder, fill 'x', vBorder])]
                script = [ScrollBy 4, ScrollBy 3, ScrollEnd, Stop]
            ordinary <- runScript Ordinary (38, 8) content script
            measured <- runScript Measured (38, 8) content script
            measured `shouldBe` ordinary

        it "preserves visible cursor positions while scrolling" do
            let content = zipWith
                    (\index chunk ->
                        showCursor (Chunk index) (Location (2, 0)) chunk)
                    [0 ..] chunks
                script = [ScrollBy 17, ScrollEnd, ScrollBy (-9), Stop]
            ordinary <- runScriptChoosing showFirstCursor Ordinary
                (38, 8) content script
            measured <- runScriptChoosing showFirstCursor Measured
                (38, 8) content script
            measured `shouldBe` ordinary

        it "dispatches actual Vty mouse input to the same clickable chunk" do
            let content = zipWith
                    (\index chunk -> clickable (Chunk index) chunk)
                    [0 ..] chunks
            ordinary <- runMouseScript Ordinary (38, 8) content
            measured <- runMouseScript Measured (38, 8) content
            measured `shouldBe` ordinary
            measured `shouldSatisfy` (not . null)

        it "matches visibility requests for a distant extent" do
            let script =
                    [ Reveal (Chunk 23)
                    , RevealMany [Chunk 3, Chunk 28, Chunk 11]
                    , Reveal (Chunk 2)
                    , Reveal (Chunk 37)
                    , Stop
                    ]
            ordinary <- runScript Ordinary (38, 8) chunks script
            measured <- runScript Measured (38, 8) chunks script
            measured `shouldBe` ordinary

        it "remeasures wrapped chunks after a terminal resize" do
            let script =
                    [ ScrollBy 29
                    , Resize (25, 11)
                    , ScrollEnd
                    , Resize (51, 7)
                    , Reveal (Chunk 12)
                    , Stop
                    ]
            ordinary <- runScript Ordinary (43, 9) chunks script
            measured <- runScript Measured (43, 9) chunks script
            measured `shouldBe` ordinary

        it "matches empty and viewport-short transcripts" do
            mapM_
                (\content -> do
                    ordinary <- runScript Ordinary (40, 12) content [Stop]
                    measured <- runScript Measured (40, 12) content [Stop]
                    measured `shouldBe` ordinary)
                [[], take 2 chunks]

        it "preserves cached clickable extents and visibility requests" do
            let content = zipWith
                    (\index chunk -> cached (ChunkCache index) $
                        clickable (Chunk index) chunk)
                    [0 ..] chunks
                script = [ScrollEnd, Reveal (Chunk 3), Reveal (Chunk 28), Stop]
            ordinary <- runScript Ordinary (38, 8) content script
            measured <- runScript Measured (38, 8) content script
            measured `shouldBe` ordinary

        it "matches Brick's combined released-height limit" do
            let content = [vLimit 60000 (fill 'a'), vLimit 60000 (fill 'b')]
                script = [ScrollEnd, ScrollBy (-10), Stop]
            ordinary <- runScript Ordinary (38, 8) content script
            measured <- runScript Measured (38, 8) content script
            measured `shouldBe` ordinary

-- Compare every picture rather than only the final frame. This catches a
-- one-frame blank viewport when Brick applies a queued jump after rendering
-- the child.
runScript
    :: Renderer
    -> V.DisplayRegion
    -> [Widget Name]
    -> [ScriptEvent]
    -> IO ([(V.DisplayRegion, String)], [String])
runScript renderer initialBounds content script = do
    runScriptChoosing neverShowCursor renderer initialBounds content script

runScriptChoosing
    :: (() -> [CursorLocation Name] -> Maybe (CursorLocation Name))
    -> Renderer
    -> V.DisplayRegion
    -> [Widget Name]
    -> [ScriptEvent]
    -> IO ([(V.DisplayRegion, String)], [String])
runScriptChoosing chooseCursor renderer initialBounds content script = do
    let observedScript = concatMap withSnapshot script
        withSnapshot Stop = [Stop]
        withSnapshot event = [event, Snapshot]
    events <- newBChan (max 1 (length observedScript))
    mapM_ (writeBChan events) observedScript
    boundsRef <- newIORef initialBounds
    picturesRef <- newIORef []
    observationsRef <- newIORef []
    (_, mockOutput) <- VMock.mockTerminal initialBounds
    let output = mockOutput
            { V.displayBounds = readIORef boundsRef
            , V.setDisplayBounds = writeIORef boundsRef
            }
    internalEvents <- newTChanIO
    let vty = V.Vty
            { V.update = \picture -> do
                bounds <- readIORef boundsRef
                let normalized = show
                        ( map (concatMap spanCharacters . toList) $
                            toList (displayOpsForPic picture bounds)
                        , V.picCursor picture
                        , map (map snd . concatMap spanCharacters . toList) $
                            toList (displayOpsForPic picture bounds)
                        )
                modifyIORef' picturesRef ((bounds, normalized) :)
                displayContext <- V.mkDisplayContext output output bounds
                V.outputPicture displayContext picture
            , V.nextEvent = atomically retry
            , V.nextEventNonblocking = pure Nothing
            , V.inputIface = V.Input
                { V.eventChannel = internalEvents
                , V.shutdownInput = pure ()
                , V.restoreInputState = pure ()
                , V.inputLogMsg = const (pure ())
                }
            , V.outputIface = output
            , V.refresh = pure ()
            , V.shutdown = pure ()
            , V.isShutdown = pure False
            }
        app = App
            { appDraw = const [drawRenderer renderer content]
            , appChooseCursor = chooseCursor
            , appHandleEvent = handleScript boundsRef observationsRef
            , appStartEvent = pure ()
            , appAttrMap = const testAttrMap
            }
    _ <- customMain vty (pure vty) (Just events) app ()
    (,) <$> (reverse <$> readIORef picturesRef)
        <*> (reverse <$> readIORef observationsRef)

-- Feed mouse input through Vty so Brick performs its normal extent hit test
-- and translates the terminal coordinate to a named MouseDown event.
runMouseScript
    :: Renderer
    -> V.DisplayRegion
    -> [Widget Name]
    -> IO [String]
runMouseScript renderer bounds content = do
    (_, mockOutput) <- VMock.mockTerminal bounds
    inputEvents <- newTChanIO
    internalEvents <- newTChanIO
    mapM_ (atomically . writeTChan inputEvents)
        [ V.EvKey (V.KChar 's') []
        , V.EvMouseDown 2 1 V.BLeft []
        , V.EvKey V.KEsc []
        ]
    hitsRef <- newIORef []
    displayContext <- V.mkDisplayContext mockOutput mockOutput bounds
    let vty = V.Vty
            { V.update = V.outputPicture displayContext
            , V.nextEvent = atomically (readTChan inputEvents)
            , V.nextEventNonblocking = Nothing <$ pure ()
            , V.inputIface = V.Input
                { V.eventChannel = internalEvents
                , V.shutdownInput = pure ()
                , V.restoreInputState = pure ()
                , V.inputLogMsg = const (pure ())
                }
            , V.outputIface = mockOutput
            , V.refresh = pure ()
            , V.shutdown = pure ()
            , V.isShutdown = pure False
            }
        app = App
            { appDraw = const [drawRenderer renderer content]
            , appChooseCursor = neverShowCursor
            , appHandleEvent = \case
                VtyEvent (V.EvKey (V.KChar 's') []) ->
                    vScrollBy (viewportScroll Transcript) 17
                MouseDown name button modifiers location -> do
                    liftIO $ modifyIORef' hitsRef
                        ((show name <> " " <> show button <> " "
                            <> show modifiers <> " " <> show location) :)
                    halt
                VtyEvent (V.EvKey V.KEsc []) ->
                    halt
                _ ->
                    pure ()
            , appStartEvent = pure ()
            , appAttrMap = const testAttrMap
            }
    _ <- customMain vty (pure vty) Nothing app ()
    reverse <$> readIORef hitsRef

-- SpanOp's Show instance omits text, and equivalent pictures can split spans
-- differently. Compare attributed characters rather than debug descriptions.
spanCharacters :: SpanOp -> [(V.Attr, Char)]
spanCharacters (TextSpan attr _ _ text) =
    map (\char -> (attr, char)) (LazyText.unpack text)
spanCharacters (Skip width) = replicate width (V.defAttr, ' ')
spanCharacters (RowEnd width) = replicate width (V.defAttr, ' ')

handleScript
    :: IORef V.DisplayRegion
    -> IORef [String]
    -> BrickEvent Name ScriptEvent
    -> EventM Name () ()
handleScript boundsRef observationsRef = \case
    AppEvent (ScrollBy rows) ->
        vScrollBy (viewportScroll Transcript) rows
    AppEvent ScrollEnd ->
        vScrollToEnd (viewportScroll Transcript)
    AppEvent (Reveal name) ->
        makeVisible name
    AppEvent (RevealMany names) ->
        mapM_ makeVisible names
    AppEvent (Resize bounds) ->
        liftIO (writeIORef boundsRef bounds)
    AppEvent Snapshot -> do
        currentViewport <- lookupViewport Transcript
        currentExtents <- mapM lookupExtent [Chunk index | index <- [0 .. 39]]
        liftIO $
            modifyIORef' observationsRef
                ((show currentViewport <> " " <> show currentExtents) :)
    AppEvent Stop ->
        halt
    _ ->
        pure ()

drawRenderer :: Renderer -> [Widget Name] -> Widget Name
drawRenderer renderer content =
    joinBorders $ case renderer of
        Ordinary -> viewport Transcript Vertical (vBox content)
        Measured -> measuredViewport Transcript 0 content
        MeasuredFooter ->
            measuredViewportWithFooter Transcript 0 (Just drawFooter) content
        OrdinaryScrollbar ->
            withVScrollBarRenderer testScrollbarRenderer $
                withVScrollBars OnRight $
                    viewport Transcript Vertical (vBox content)
        MeasuredScrollbar ->
            withVScrollBarRenderer testScrollbarRenderer $
                withVScrollBars OnRight $
                    measuredViewport Transcript 1 content
  where
    drawFooter bottom measuredExtents =
        maybe (txt " ") txt $
            conversationMessagesBelowLabel
                (or
                    [ height > 0 && top + height > bottom
                    | extent <- measuredExtents
                    , extent.extentName == NewerGap
                    , let Location (_, top) = extent.extentUpperLeft
                    , let (_, height) = extent.extentSize
                    ]) $
                conversationMessagesBelow bottom
                    [ (top, height)
                    | extent <- measuredExtents
                    , Chunk _ <- [extent.extentName]
                    , let Location (_, top) = extent.extentUpperLeft
                    , let (_, height) = extent.extentSize
                    ]

testScrollbarRenderer :: VScrollbarRenderer Name
testScrollbarRenderer =
    VScrollbarRenderer
        { renderVScrollbar =
            withAttr (attrName "scrollbar-thumb") (fill '┃')
        , renderVScrollbarTrough =
            withAttr (attrName "scrollbar-trough") (fill '│')
        , renderVScrollbarHandleBefore = emptyWidget
        , renderVScrollbarHandleAfter = emptyWidget
        , scrollbarWidthAllocation = 1
        }

testAttrMap :: AttrMap
testAttrMap =
    attrMap V.defAttr
        [ (attrName "scrollbar-thumb", V.defAttr `V.withForeColor` V.red)
        , (attrName "scrollbar-trough", V.defAttr `V.withForeColor` V.blue)
        ]

chunks :: [Widget Name]
chunks =
    [ reportExtent (Chunk index) $
        vBox
            [ txtWrap
                ("chunk " <> tshow index <> " line " <> tshow line <> suffix)
            | line <- [1 .. 1 + index `mod` 5]
            ]
    | index <- [0 .. 39]
    ]
  where
    suffix =
        " — wrapping text which changes height at narrow terminal widths"

tshow :: Show a => a -> Text
tshow = Text.pack . show
