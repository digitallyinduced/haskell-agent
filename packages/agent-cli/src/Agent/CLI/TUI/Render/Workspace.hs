-- | Internal fullscreen rendering helpers.
module Agent.CLI.TUI.Render.Workspace
    ( drawWorkspace
    , agentPopoverLayers
    , agentEntryWindow
    , agentPaneVisible
    , agentPaneEntryLimit
    , conversationScrollbarRenderer
    , selectedAgentConversation
    , conversationUiForTarget
    , activeConversationUi
    , applyChildConversationUiEvent
    ) where


import Agent.CLI.AgentViewport
    ( AgentEntry(..),
      AgentStep(..),
      AgentStepState(..),
      AgentTarget(..),
      agentDisplayName,
      agentEntryTreeLabelWithGlyphModel,
      agentStatusGlyph,
      lookupAgentEntry )
import Agent.CLI.Artifact ()
import Agent.CLI.Clipboard ()
import Agent.CLI.Command ()
import Agent.CLI.Dictation ()
import Agent.CLI.ImagePreview ()
import Agent.CLI.Input ( truncateDisplayText )
import Agent.CLI.Interrupt ()
import Agent.CLI.Permission ()
import Agent.CLI.Recap ()
import Agent.CLI.Render ()
import Agent.CLI.Resume ()
import Agent.CLI.Secret ()
import Agent.CLI.Status ()
import Agent.CLI.Style ( motionGlyphSet )
import Agent.CLI.TUI.History ( HistoryWindow(historyWindowTurns) )
import Agent.CLI.TUI.ImagePreview ()
import Agent.CLI.TUI.LambdaArt ()
import Agent.CLI.TUI.Motion ( isBackgroundAgentActive )
import Agent.CLI.TUI.Render.Transcript
    ( drawTranscript,
      historyRangeWidgets,
      drawEmptyConversation,
      drawConversationBlocks )
import Agent.CLI.TUI.Types
    ( AgentHover(agentHoverTarget, agentHoverPaneUpperLeft,
                 agentHoverPaneWidth, agentHoverUpperLeft),
      AppState(appAgentHover, appRuntime, appMotionElapsedMillis, appUi,
               appAgentSelected, appHistoryWindow, appAgentEntries),
      FullscreenRuntime(runtimeMotionMode),
      Name(AgentPopover, ConversationViewportExtent,
           ConversationViewport, AgentRow, AgentPane) )
import Agent.CLI.Terminal ()
import Agent.CLI.Timestamp ()
import Agent.Loop ()
import Agent.Syntax ()
import Agent.TUI.Markdown ()
import Agent.TUI.Model
    ( reduceUi, conversationIsEmpty, UiEvent, UiState(uiBlocks) )
import Agent.TUI.Motion ( quietIndicator )
import Agent.TUI.Presentation ()
import Agent.TUI.TextWidth ( displayTerminalText )
import Agent.ToolDispatch ()
import Brick
    ( getContext,
      clickable,
      emptyWidget,
      fill,
      hBox,
      hLimit,
      hLimitPercent,
      overrideAttr,
      padAll,
      padLeft,
      padLeftRight,
      padTop,
      reportExtent,
      translateBy,
      txt,
      vBox,
      vLimit,
      viewport,
      withAttr,
      withBorderStyle,
      withVScrollBarRenderer,
      withVScrollBars,
      AttrName,
      Location(Location),
      Context(availHeight, availWidth),
      Size(Fixed, Greedy),
      VScrollBarOrientation(OnRight),
      VScrollbarRenderer(..),
      ViewportType(Vertical),
      Widget(render, Widget),
      Padding(Pad) )
import Brick.BChan ()
import Brick.Widgets.Border ( borderWithLabel )
import Brick.Widgets.Border.Style ( unicodeRounded )
import Brick.Widgets.Center ()
import Codec.Picture ()
import Control.Applicative ()
import Control.Concurrent ()
import Control.Concurrent.Async ()
import Control.Concurrent.STM ()
import Control.Exception ()
import Control.Exception.Safe ()
import Control.Monad ()
import Control.Monad.IO.Class ()
import Control.Monad.State.Strict ()
import Data.Char ()
import Data.Foldable ()
import Data.IORef ()
import Data.List ( findIndex, intersperse, sortOn )
import Data.List.NonEmpty ()
import Data.Maybe ( fromMaybe )
import Data.Sequence ()
import Data.Text ( Text )
import Data.Time.Clock ()
import Data.Time.Clock.POSIX ()
import Data.Time.Format ()
import Data.Word ()
import GHC.Clock ()
import System.Environment ()
import System.IO ()
import System.Info ()
import System.Posix.Process ()
import System.Process ()
import qualified Brick.Types as B ()
import qualified Brick.Widgets.Border as Border ( borderAttr )
import qualified Agent.CLI.TUI.Bridge as Bridge ()
import qualified Agent.CLI.TUI.Composer as Composer ()
import qualified Data.Map.Strict as Map ()
import qualified Agent.CLI.TUI.Scroll as Scroll ()
import qualified Data.Sequence as Seq ( null )
import qualified Data.Set as Set ()
import qualified Data.Text as Text
    ( pack )
import qualified Data.Text.Encoding as TextEncoding ()
import qualified Agent.TUI.Theme as Theme
    ( assistantAttr,
      baseAttr,
      borderActiveAttr,
      borderAttr,
      controlLinkHoverAttr,
      errorAttr,
      headingAttr,
      mutedAttr,
      successAttr,
      thinkingAttr,
      toolAttr )
import qualified Graphics.Vty as V ()
import qualified Graphics.Vty.CrossPlatform as Vty ()

terminalTxt :: Text -> Widget n
terminalTxt = txt . displayTerminalText

drawWorkspace :: AppState -> Widget Name
drawWorkspace state =
    Widget Greedy Greedy do
        context <- getContext
        render $
            padTop (Pad 1) $
                hBox $
                    [drawConversationPane state]
                        <> if not
                            (agentPaneVisible
                                context.availWidth
                                context.availHeight
                                state.appAgentEntries)
                            then []
                            else
                                [ hLimitPercent 40 $
                                    hLimit agentPaneWidth $
                                        padLeft (Pad 1) $
                                            drawAgentPane
                                                state
                                                (agentPaneEntryLimit
                                                    context.availHeight)
                                                state.appAgentSelected
                                                ((.agentHoverTarget)
                                                    <$> state.appAgentHover)
                                                state.appAgentEntries
                                ]

drawConversationPane :: AppState -> Widget Name
drawConversationPane state =
    case selectedChildEntry state of
        Just entry ->
            reportExtent ConversationViewportExtent $
                withVScrollBarRenderer conversationScrollbarRenderer $
                    withVScrollBars OnRight $
                        viewport ConversationViewport Vertical $
                            padLeftRight 2 (drawAgentConversation state entry)
        Nothing
            | conversationIsEmpty state.appUi
                && Seq.null
                    state.appHistoryWindow.historyWindowTurns ->
                padLeftRight 2 $
                    vBox
                        [ drawTranscript state
                        , drawEmptyConversation state
                        ]
            | otherwise ->
                vBox $
                    historyRangeWidgets state
                        <> [ reportExtent ConversationViewportExtent $
                                withVScrollBarRenderer
                                    conversationScrollbarRenderer $
                                    withVScrollBars OnRight $
                                        viewport ConversationViewport Vertical $
                                            padLeftRight 2
                                                (drawTranscript state)
                           ]

selectedChildEntry :: AppState -> Maybe AgentEntry
selectedChildEntry state =
    selectedAgentConversation
        state.appAgentSelected
        state.appAgentEntries

selectedAgentConversation
    :: AgentTarget
    -> [AgentEntry]
    -> Maybe AgentEntry
selectedAgentConversation selected entries = case selected of
    AgentRoot -> Nothing
    target -> lookupAgentEntry target entries

conversationUiForTarget :: AgentTarget -> AppState -> Maybe UiState
conversationUiForTarget target state = case target of
    AgentRoot -> Just state.appUi
    AgentChild _ ->
        (.agentConversation)
            <$> lookupAgentEntry target state.appAgentEntries
    AgentNative _ ->
        (.agentConversation)
            <$> lookupAgentEntry target state.appAgentEntries

activeConversationUi :: AppState -> UiState
activeConversationUi state =
    fromMaybe state.appUi $
        conversationUiForTarget state.appAgentSelected state

applyChildConversationUiEvent
    :: AgentTarget
    -> UiEvent
    -> AppState
    -> AppState
applyChildConversationUiEvent target uiEvent state =
    state
        { appAgentEntries =
            map updateEntry state.appAgentEntries
        }
  where
    updateEntry entry
        | entry.agentTarget == target =
            entry
                { agentConversation =
                    reduceUi uiEvent entry.agentConversation
                }
        | otherwise = entry

drawAgentConversation :: AppState -> AgentEntry -> Widget Name
drawAgentConversation state entry =
    vBox
        [ withAttr Theme.headingAttr $
            terminalTxt ("Viewing " <> entry.agentPath)
        , withAttr Theme.mutedAttr $
            terminalTxt
                (entry.agentStatus
                    <> " · input is sent to /root")
        , padTop (Pad 1) $
            if Seq.null entry.agentConversation.uiBlocks
                then
                    withAttr Theme.mutedAttr $
                        terminalTxt "(no transcript yet)"
                else
                    drawConversationBlocks
                        state
                        entry.agentTarget
                        entry.agentConversation
        ]

-- Brick's default scrollbar uses a full block for the thumb and a blank
-- space for the trough. During rapid viewport reflow, some terminals can
-- leave the old full-block cells behind because the replacement trough is
-- visually identical to the surrounding background. A visible one-column
-- trough makes every old thumb position get explicitly repainted.
conversationScrollbarRenderer :: VScrollbarRenderer n
conversationScrollbarRenderer =
    VScrollbarRenderer
        { renderVScrollbar =
            withAttr Theme.mutedAttr (fill '┃')
        , renderVScrollbarTrough =
            withAttr Theme.borderAttr (fill '│')
        , renderVScrollbarHandleBefore = emptyWidget
        , renderVScrollbarHandleAfter = emptyWidget
        , scrollbarWidthAllocation = 1
        }

agentPaneWidth :: Int
agentPaneWidth = 42

agentPaneMinScreenWidth :: Int
agentPaneMinScreenWidth = 72

agentPaneMinAvailableHeight :: Int
agentPaneMinAvailableHeight = 10

agentPaneVisible :: Int -> Int -> [AgentEntry] -> Bool
agentPaneVisible width height entries =
    width >= agentPaneMinScreenWidth
        && height >= agentPaneMinAvailableHeight
        && length entries > 1

agentPaneEntryLimit :: Int -> Int
agentPaneEntryLimit availableHeight =
    -- Reserve the outer top pad, four pane chrome rows (border and padding),
    -- and two possible truncation indicators above and below.
    max 1 (min 12 (availableHeight - 7))

drawAgentPane
    :: AppState
    -> Int
    -> AgentTarget
    -> Maybe AgentTarget
    -> [AgentEntry]
    -> Widget Name
drawAgentPane state entryLimit selected hovered entries =
    clickable AgentPane $
        withAttr Theme.borderAttr $
            withBorderStyle unicodeRounded $
                borderWithLabel
                    (txt
                        (" Agents · "
                            <> Text.pack (show childCount)
                            <> " ")) $
                    padAll 1 $
                        vBox agentRows
  where
    ordered = sortOn (.agentPath) entries
    childCount =
        length
            [ ()
            | entry <- ordered
            , entry.agentTarget /= AgentRoot
            ]
    (hiddenBefore, shownEntries, hiddenAfter) =
        agentEntryWindow entryLimit selected ordered
    agentRows =
        [ drawHidden hiddenBefore "above"
        | hiddenBefore > 0
        ]
            <> map drawEntry shownEntries
            <> [ drawHidden hiddenAfter "below"
               | hiddenAfter > 0
               ]
    drawHidden count direction =
        withAttr Theme.mutedAttr $
            txt
                ("  … "
                    <> Text.pack (show count)
                    <> " "
                    <> direction)
    drawEntry entry =
        let
            index =
                maybe 0 id $
                    findIndex ((== entry.agentTarget) . (.agentTarget)) ordered
            marker =
                if entry.agentTarget == selected then "› " else "  "
            statusGlyph
                | isBackgroundAgentActive entry =
                    quietIndicator
                        motionGlyphSet
                        state.appRuntime.runtimeMotionMode
                        state.appMotionElapsedMillis
                | otherwise =
                    agentStatusGlyph entry.agentStatus
            row = hBox
                [ terminalTxt
                    (marker
                        <> agentEntryTreeLabelWithGlyphModel
                            statusGlyph
                            ordered
                            index
                            entry)
                , fill ' '
                ]
            styled =
                if hovered == Just entry.agentTarget
                    then withAttr Theme.controlLinkHoverAttr row
                    else if entry.agentTarget == selected
                        then withAttr Theme.successAttr row
                        else row
        in clickable (AgentRow entry.agentTarget) (vLimit 1 styled)

agentPopoverLayers :: AppState -> [Widget Name]
agentPopoverLayers state =
    case (length state.appAgentEntries > 1, state.appAgentHover) of
        (False, _) -> []
        (_, Nothing) -> []
        (True, Just hover) ->
            case lookupAgentEntry hover.agentHoverTarget state.appAgentEntries of
                Nothing -> []
                Just entry -> [positionAgentPopover state hover entry]

agentPopoverPreferredWidth :: Int
agentPopoverPreferredWidth = 40

agentPopoverMinWidth :: Int
agentPopoverMinWidth = 24

agentPopoverHeight :: Int
agentPopoverHeight = 9

agentPopoverGap :: Int
agentPopoverGap = 1

positionAgentPopover :: AppState -> AgentHover -> AgentEntry -> Widget Name
positionAgentPopover state hover entry =
    Widget Fixed Fixed do
        context <- getContext
        let screenWidth = max 0 context.availWidth
            screenHeight = max 0 context.availHeight
            Location (_, anchorY) = hover.agentHoverUpperLeft
            Location (paneX, _) = hover.agentHoverPaneUpperLeft
            paneRight = paneX + max 1 hover.agentHoverPaneWidth
            leftAvailable = max 0 (paneX - agentPopoverGap)
            rightAvailable =
                max 0 (screenWidth - paneRight - agentPopoverGap)
            placeLeft =
                leftAvailable >= agentPopoverMinWidth
                    || leftAvailable >= rightAvailable
            available =
                if placeLeft then leftAvailable else rightAvailable
            width = min agentPopoverPreferredWidth available
            height = min agentPopoverHeight screenHeight
            x
                | placeLeft =
                    max 0 (paneX - agentPopoverGap - width)
                | otherwise =
                    min
                        (max 0 (screenWidth - width - agentPopoverGap))
                        paneRight
            y = max 0 (min anchorY (screenHeight - height))
        if screenWidth < agentPaneMinScreenWidth
            || width < agentPopoverMinWidth
            || height < 5
            then render emptyWidget
            else
                render $
                    translateBy (Location (x, y)) $
                        drawAgentPopover state placeLeft width height entry

drawAgentPopover :: AppState -> Bool -> Int -> Int -> AgentEntry -> Widget Name
drawAgentPopover state placeLeft width height entry =
    clickable (AgentPopover entry.agentTarget) surface
  where
    surface
        | placeLeft = hBox [popover, bridge]
        | otherwise = hBox [bridge, popover]
    bridge =
        hLimit agentPopoverGap $
            vLimit height (fill ' ')
    popover =
        hLimit width $
            vLimit height $
                withAttr Theme.baseAttr $
                    overrideAttr Border.borderAttr Theme.borderActiveAttr $
                        withBorderStyle unicodeRounded $
                            borderWithLabel
                                (withAttr Theme.headingAttr $
                                    terminalTxt
                                        (" "
                                            <> truncateDisplayText
                                                (max 1 (width - 6))
                                                (agentDisplayName entry.agentPath)
                                            <> " ")) $
                                padAll 1 $
                                    vBox
                                        ( intersperse
                                            (vLimit 1 (fill ' '))
                                            (map
                                                (drawAgentStep
                                                    state
                                                    (max 1 (width - 4)))
                                                steps)
                                            <> [fill ' ']
                                        )
    steps =
        case take 2 entry.agentSteps of
            [] ->
                [ AgentStep
                    { agentStepState = AgentStepInfo
                    , agentStepTitle = "No recent activity"
                    , agentStepDetail = Just entry.agentStatus
                    }
                ]
            recent -> recent

drawAgentStep :: AppState -> Int -> AgentStep -> Widget Name
drawAgentStep state width step =
    vLimit 2 $
        vBox
            [ hBox
                [ withAttr (agentStepAttr step.agentStepState) $
                    txt (agentStepGlyph state step.agentStepState)
                , txt " "
                , withAttr Theme.assistantAttr $
                    terminalTxt
                        (truncateDisplayText
                            (max 1 (width - 2))
                            step.agentStepTitle)
                ]
            , padLeft (Pad 2) $
                withAttr Theme.mutedAttr $
                    terminalTxt
                        (truncateDisplayText
                            (max 1 (width - 2))
                            (fromMaybe
                                (agentStepStateLabel step.agentStepState)
                                step.agentStepDetail))
            ]

agentStepGlyph :: AppState -> AgentStepState -> Text
agentStepGlyph state = \case
    AgentStepRunning ->
        quietIndicator
            motionGlyphSet
            state.appRuntime.runtimeMotionMode
            state.appMotionElapsedMillis
    AgentStepCompleted -> "✓"
    AgentStepFailed -> "✕"
    AgentStepInfo -> "◆"

agentStepStateLabel :: AgentStepState -> Text
agentStepStateLabel = \case
    AgentStepRunning -> "running"
    AgentStepCompleted -> "completed"
    AgentStepFailed -> "failed"
    AgentStepInfo -> "recent activity"

agentStepAttr :: AgentStepState -> AttrName
agentStepAttr = \case
    AgentStepRunning -> Theme.thinkingAttr
    AgentStepCompleted -> Theme.successAttr
    AgentStepFailed -> Theme.errorAttr
    AgentStepInfo -> Theme.toolAttr

agentEntryWindow
    :: Int
    -> AgentTarget
    -> [AgentEntry]
    -> (Int, [AgentEntry], Int)
agentEntryWindow count selected entries
    | count <= 0 = (0, [], length entries)
    | length entries <= count = (0, entries, 0)
    | otherwise =
        ( start
        , take count (drop start entries)
        , length entries - start - count
        )
  where
    selectedIndex =
        maybe 0 id $
            findIndex ((== selected) . (.agentTarget)) entries
    start =
        max 0 $
            min
                (selectedIndex - count `div` 2)
                (length entries - count)
