-- | Internal fullscreen rendering helpers.
module Agent.CLI.TUI.Render.Transcript
    ( drawTranscript
    , historyRangeWidgets
    , stickyPromptLayers
    , drawEmptyConversation
    , drawConversationBlocks
    , quickStartVisible
    , quickStartWideVisible
    , quickStartRows
    , startupCapabilityLines
    ) where


import Agent.CLI.AgentViewport ( AgentTarget(AgentRoot) )
import Agent.CLI.Artifact ()
import Agent.CLI.Clipboard ()
import Agent.CLI.Command
    ( SkillCommand(skillCommandName)
    , SlashCatalog(slashCatalogSkills, slashCatalogToolNames)
    )
import Agent.CLI.Dictation ()
import Agent.CLI.ImagePreview ()
import Agent.CLI.Input
    ( displayEditorText, terminalTextWidth, truncateDisplayText )
import Agent.CLI.Interrupt ()
import Agent.CLI.Permission ()
import Agent.CLI.Recap ()
import Agent.CLI.Render ()
import Agent.CLI.Resume ()
import Agent.CLI.Secret ()
import Agent.CLI.Status ()
import Agent.CLI.Startup.Format
    ( agentBuildInfo, formatBuildInfoCompact )
import Agent.CLI.Style ()
import Agent.CLI.TUI.History
    ( HistoryWindow(historyWindowTurns, historyWindowTotalTurns,
                    historyWindowGenerationStart, historyWindowHasNewer,
                    historyWindowHasOlder, historyWindowPending),
      HistoryTurn(historyTurnCursor, historyTurnBlocks),
      HistoryDirection(..),
      HistoryCursor(HistoryCursor) )
import Agent.CLI.TUI.ImagePreview ()
import Agent.CLI.TUI.LambdaArt ( lambdaArtWidget )
import Agent.CLI.TUI.Motion
    ( motionModeForTerminalFocus, userActionPending )
import Agent.TUI.Accent ()
import Agent.CLI.TUI.Types
    ( AppState(appConversationAnchor, appTerminalFocus,
               appMotionElapsedMillis, appRuntime, appHoveredControl,
               appAgentEntries, appSlashCatalog, appHistoryWindow, appUi,
               appAgentSelected),
      FullscreenRuntime(runtimeColor, runtimeMotionMode),
      Name(QuickStartModel, CodeCopy, ConversationChunkCache,
           ConversationReserve, QuickStartWorktree, QuickStartResume,
           QuickStartCommands) )
import Agent.CLI.Terminal ()
import Agent.CLI.Timestamp ()
import Agent.Loop ()
import Agent.Syntax ()
import Agent.TUI.Markdown ()
import Agent.TUI.Model
    ( Focus(FocusScrollback),
      UiBlock,
      UiState(uiBlocks, uiFocus, uiSelectedBlock) )
import Agent.TUI.Motion
    ( MotionMode(MotionOff, MotionFull, MotionReduced) )
import Agent.TUI.Presentation ()
import Agent.TUI.TextWidth ( displayTerminalText )
import Agent.ToolDispatch ()
import Brick
    ( getContext,
      cached,
      clickable,
      fill,
      forceAttr,
      hBox,
      hLimit,
      hLimitPercent,
      padAll,
      padLeft,
      padLeftRight,
      padTop,
      padTopBottom,
      reportExtent,
      translateBy,
      txt,
      txtWrap,
      vBox,
      vLimit,
      withAttr,
      withBorderStyle,
      Location(Location),
      Context(availHeight, availWidth),
      Result(image),
      Size(Greedy),
      Widget(render, Widget),
      Padding(Pad) )
import Brick.BChan ()
import Brick.Widgets.Border ( borderWithLabel, vBorder )
import Brick.Widgets.Border.Style ( unicodeRounded )
import Brick.Widgets.Center ( center, hCenter )
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
import Data.Foldable ( toList )
import Data.IORef ()
import Data.List ()
import Data.List.NonEmpty ()
import Data.Maybe ( maybeToList )
import Data.Sequence ( Seq )
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
import qualified Brick.Types as B
    ( getContext, Size(Greedy), Widget(render, Widget) )
import qualified Brick.Widgets.Border as Border
    ( borderAttr )
import qualified Agent.CLI.TUI.Bridge as Bridge ()
import qualified Agent.CLI.TUI.Composer as Composer
    ( controlInteractionAttr )
import qualified Data.Map.Strict as Map ()
import qualified Agent.CLI.TUI.Scroll as Scroll
    ( ConversationAnchor(anchorText, anchorReserveRows),
      conversationAnchorSticky )
import qualified Data.Sequence as Seq ( (!?), length )
import qualified Data.Set as Set ( Set, member, toAscList )
import qualified Data.Text as Text
    ( intercalate,
      justifyLeft,
      lines,
      null,
      strip,
      pack )
import qualified Data.Text.Encoding as TextEncoding ()
import qualified Agent.TUI.Theme as Theme
    ( controlLinkAttr,
      headingAttr,
      mutedAttr,
      strongAttr,
      toolAttr,
      userAttr )
import qualified Agent.CLI.TUI.Transcript as Transcript
    ( transcriptChunks, transcriptChunkCacheKey )
import qualified Graphics.Vty as V
    ( backgroundFill,
      imageHeight,
      imageWidth,
      horizCat )
import qualified Graphics.Vty.CrossPlatform as Vty ()


import Agent.CLI.TUI.Render.Blocks (drawBlock, cacheableBlock)

terminalTxtWrap :: Text -> Widget n
terminalTxtWrap = txtWrap . displayTerminalText

drawTranscript :: AppState -> Widget Name
drawTranscript state =
    vBox $
        [ vBox $
            olderGap
                <> map
                    (drawBlock state AgentRoot state.appUi)
                    historicalBlocks
                <> newerGap
                <> [drawConversationBlocks state AgentRoot state.appUi]
        ]
            <> conversationReserveWidgets anchor
  where
    historicalBlocks =
        concatMap
            (toList . (.historyTurnBlocks))
            (toList state.appHistoryWindow.historyWindowTurns)
    olderGap =
        historyGapWidget
            HistoryOlder
            state.appHistoryWindow.historyWindowHasOlder
            state.appHistoryWindow.historyWindowPending
    newerGap =
        historyGapWidget
            HistoryNewer
            state.appHistoryWindow.historyWindowHasNewer
            state.appHistoryWindow.historyWindowPending
    anchor = state.appConversationAnchor

-- | Cache completed transcript blocks in moderately sized groups.
--
-- A Brick viewport must still lay out its complete child to determine the
-- scroll range. Per-block caching avoids repeated Markdown parsing, but a
-- redraw still has to combine one image per retained block. Grouping stable
-- blocks means ordinary scrolling traverses roughly one cached image per 32
-- blocks instead. Only full chunks are cached: Brick retains cache entries
-- until explicit invalidation, so caching the growing final chunk under a new
-- last-block key for every append would retain all of those obsolete images.
-- The final partial/live/animated group remains uncached.
drawTranscriptChunk
    :: AppState
    -> AgentTarget
    -> UiState
    -> Seq UiBlock
    -> Widget Name
drawTranscriptChunk state target ui blocks =
    case
        Transcript.transcriptChunkCacheKey
            (cacheableBlock state target ui)
            dynamicBlockIds
            blocks of
        Just (firstBlockId, lastBlockId) ->
            cached
                (ConversationChunkCache
                    target
                    firstBlockId
                    lastBlockId)
                rendered
        Nothing -> rendered
  where
    rendered =
        vBox (map (drawBlock state target ui) (toList blocks))
    dynamicBlockIds =
        [ blockId
        | state.appUi.uiFocus == FocusScrollback
        , state.appAgentSelected == target
        , blockId <- maybeToList ui.uiSelectedBlock
        ]
            <> [ blockId
               | Just (CodeCopy hoveredTarget blockId _) <-
                    [state.appHoveredControl]
               , hoveredTarget == target
               ]

drawConversationBlocks
    :: AppState
    -> AgentTarget
    -> UiState
    -> Widget Name
drawConversationBlocks state target ui =
    vBox $
        map
            (drawTranscriptChunk state target ui)
            (Transcript.transcriptChunks ui.uiBlocks)

historyRangeWidgets :: AppState -> [Widget Name]
historyRangeWidgets state =
    case historyRangeText state.appHistoryWindow of
        Nothing -> []
        Just range ->
            [ padLeftRight 2 $
                withAttr Theme.mutedAttr $
                    hCenter (txt range)
            ]

historyRangeText :: HistoryWindow -> Maybe Text
historyRangeText window = do
    firstTurn <- window.historyWindowTurns Seq.!? 0
    lastTurn <-
        window.historyWindowTurns
            Seq.!? (Seq.length window.historyWindowTurns - 1)
    let HistoryCursor generationStart =
            window.historyWindowGenerationStart
        HistoryCursor firstCursor = firstTurn.historyTurnCursor
        HistoryCursor lastCursor = lastTurn.historyTurnCursor
        total = window.historyWindowTotalTurns
        firstPosition = max 1 (firstCursor - generationStart + 1)
        lastPosition = max firstPosition (lastCursor - generationStart + 1)
        virtualized =
            window.historyWindowHasOlder
                || window.historyWindowHasNewer
                || fromIntegral (Seq.length window.historyWindowTurns) < total
        available =
            [ label
            | (isAvailable, label) <-
                [ (window.historyWindowHasOlder, "older")
                , (window.historyWindowHasNewer, "newer")
                ]
            , isAvailable
            ]
        suffix =
            case available of
                [] -> ""
                labels ->
                    " · "
                        <> Text.intercalate "/" labels
                        <> " load on scroll"
    if total <= 0 || not virtualized
        then Nothing
        else
            Just $
                "Turns "
                    <> showText firstPosition
                    <> "–"
                    <> showText (min total lastPosition)
                    <> " of "
                    <> showText total
                    <> suffix

historyGapWidget
    :: HistoryDirection
    -> Bool
    -> Set.Set HistoryDirection
    -> [Widget Name]
historyGapWidget direction available pending
    | not available = []
    | otherwise =
        [ padTopBottom 1 $
            withAttr Theme.mutedAttr $
                hCenter $
                    txt $
                        if direction `Set.member` pending
                            then "… loading " <> directionLabel <> " turns …"
                            else
                                "… "
                                    <> directionLabel
                                    <> " persisted turns are unloaded …"
        ]
  where
    directionLabel = case direction of
        HistoryOlder -> "older"
        HistoryNewer -> "newer"

showText :: Show a => a -> Text
showText = Text.pack . show

stickyPromptLayers :: AppState -> [Widget Name]
stickyPromptLayers state =
    case (state.appAgentSelected, state.appConversationAnchor) of
        (AgentRoot, Just anchor)
            | Scroll.conversationAnchorSticky anchor ->
                [ translateBy (Location (0, 2)) $
                    hLimitPercent conversationWidth $
                        padLeftRight 2 $
                            withAttr Theme.userAttr $
                                vLimit 5 $
                                    padAll 1 $
                                        terminalTxtWrap
                                            (stickyPromptPreview
                                                anchor.anchorText)
                ]
        _ -> []
  where
    conversationWidth =
        if length state.appAgentEntries <= 1 then 100 else 68

stickyPromptPreview :: Text -> Text
stickyPromptPreview text =
    case splitAt 3 (Text.lines text) of
        (shown, []) -> Text.intercalate "\n" shown
        (shown, _ : _) ->
            Text.intercalate "\n" (take 2 shown <> ["…"])

conversationReserveWidgets
    :: Maybe Scroll.ConversationAnchor
    -> [Widget Name]
conversationReserveWidgets = \case
    Just anchor
        | anchor.anchorReserveRows > 0 ->
            [ reportExtent ConversationReserve $
                vLimit anchor.anchorReserveRows (fill ' ')
            ]
    _ -> []

drawEmptyConversation :: AppState -> Widget Name
drawEmptyConversation state =
    B.Widget B.Greedy B.Greedy do
        context <- B.getContext
        let width = context.availWidth
            height = context.availHeight
            capabilityRows = quickStartCapabilityRows height
            panelWidth = quickStartPanelWidth width
        B.render $
            if not (quickStartVisible width height)
                then center (lambdaArtWidget colorEnabled frame)
                else
                    if quickStartWideVisible width height
                        then
                            center $
                                drawQuickStartDashboard
                                    state
                                    panelWidth
                                    height
                                    colorEnabled
                                    frame
                        else
                            center $
                                vBox
                                    [ vLimit
                                        (max 8
                                            (height
                                                - quickStartReservedRows
                                                    capabilityRows))
                                        (hCenter
                                            (lambdaArtWidget
                                                colorEnabled
                                                frame))
                                    , hCenter
                                        (drawQuickStartPanel
                                            state
                                            panelWidth
                                            capabilityRows)
                                    ]
  where
    colorEnabled = state.appRuntime.runtimeColor
    frame
        | userActionPending state = 0
        | otherwise =
            case motionModeForTerminalFocus
                state.appTerminalFocus
                state.appRuntime.runtimeMotionMode of
                MotionFull -> state.appMotionElapsedMillis
                MotionReduced -> 0
                MotionOff -> 0

quickStartReservedRows :: Int -> Int
quickStartReservedRows capabilityRows =
    11 + 2 * capabilityRows

quickStartCapabilityRows :: Int -> Int
quickStartCapabilityRows height
    | height >= 34 = 2
    | otherwise = 1

quickStartPanelWidth :: Int -> Int
quickStartPanelWidth width =
    max 44 (width - 4)

quickStartVisible :: Int -> Int -> Bool
quickStartVisible width height =
    width >= 48 && height >= 22

quickStartWideVisible :: Int -> Int -> Bool
quickStartWideVisible width height =
    width >= 104 && height >= 29

quickStartRows :: [(Name, Text, Text)]
quickStartRows =
    [ (QuickStartWorktree, "New worktree", "/worktree")
    , (QuickStartResume, "Resume session", "/resume")
    , (QuickStartCommands, "Browse commands", "/")
    , (QuickStartModel, "Manage models", "/model")
    ]

drawQuickStartDashboard
    :: AppState
    -> Int
    -> Int
    -> Bool
    -> Int
    -> Widget Name
drawQuickStartDashboard
    state
    panelWidth
    availableHeight
    colorEnabled
    frame =
    vLimit dashboardHeight $
        hLimit panelWidth $
            withBorderStyle unicodeRounded $
                borderWithLabel
                    (withAttr Theme.headingAttr (txt " haskell-agent ")) $
                    padAll 1 $
                        hBox
                            [ hLimit leftWidth $
                                vBox
                                    [ vLimit artHeight $
                                        hCenter
                                            (lambdaArtWidget
                                                colorEnabled
                                                frame)
                                    , withAttr Theme.mutedAttr $
                                        hCenter
                                            (txt
                                                (formatBuildInfoCompact
                                                    agentBuildInfo))
                                    , padTop (Pad 1) $
                                        hCenter $
                                            hLimit leftWidth
                                                (drawQuickStartActions state)
                                    ]
                            , withAttr Border.borderAttr vBorder
                            , padLeft (Pad 2) $
                                hLimit capabilityWidth $
                                    padRightWithBackground $
                                        drawDashboardCapabilities
                                            capabilityWidth
                                            toolRows
                                            skillRows
                                            toolNames
                                            skillNames
                            ]
  where
    innerWidth = max 1 (panelWidth - 4)
    leftWidth =
        min 48 (max 38 (innerWidth `div` 3))
    capabilityWidth =
        max 1 (innerWidth - leftWidth - 3)
    artHeight
        | availableHeight >= 35 = 21
        | otherwise = 16
    dashboardHeight
        | availableHeight >= 35 = 32
        | otherwise = 27
    toolRows
        | availableHeight >= 38 = 7
        | otherwise = 5
    skillRows
        | availableHeight >= 38 = 5
        | otherwise = 3
    toolNames = startupToolNames state
    skillNames = startupSkillNames state

-- | Make a widget greedily occupy its available width without painting a
-- rectangle of explicit space glyphs. Some terminals expose those glyphs as
-- dotted whitespace; Vty background cells remain visually blank while still
-- giving the surrounding border the intended width.
padRightWithBackground :: Widget n -> Widget n
padRightWithBackground widget@(Widget _ verticalSize _) =
    Widget Greedy verticalSize do
        context <- getContext
        result <- render (hLimit context.availWidth widget)
        let missingWidth =
                max 0 (context.availWidth - V.imageWidth result.image)
            padding =
                V.backgroundFill
                    missingWidth
                    (V.imageHeight result.image)
        pure result
            { image = V.horizCat [result.image, padding]
            }

drawDashboardCapabilities
    :: Int
    -> Int
    -> Int
    -> [Text]
    -> [Text]
    -> Widget Name
drawDashboardCapabilities
    width
    toolRows
    skillRows
    toolNames
    skillNames =
    vBox
        [ drawSection
            Theme.toolAttr
            "Available Tools"
            toolRows
            toolNames
        , padTop (Pad 1) $
            drawSection
                Theme.controlLinkAttr
                "Available Skills"
                skillRows
                skillNames
        , padTop (Pad 1) $
            withAttr Theme.mutedAttr $
                txt $
                    truncateDisplayText width $
                        Text.pack (show (length toolNames))
                            <> " tools · "
                            <> Text.pack (show (length skillNames))
                            <> " skills · Type / to browse all."
        ]
  where
    drawSection valueAttr label maxRows names =
        vBox
            [ withAttr Theme.strongAttr $
                txt
                    (label
                        <> " ("
                        <> Text.pack (show (length names))
                        <> ")")
            , padTop (Pad 1) $
                padLeft (Pad 2) $
                    vBox
                        (map
                            (withAttr valueAttr . txt)
                            (startupCapabilityLines
                                (max 1 (width - 2))
                                maxRows
                                names))
            ]

drawQuickStartPanel :: AppState -> Int -> Int -> Widget Name
drawQuickStartPanel state panelWidth capabilityRows =
    hLimit panelWidth $
        vBox
            [ withAttr Theme.headingAttr $
                hCenter (txt "haskell-agent")
            , withAttr Theme.mutedAttr $
                hCenter (txt (formatBuildInfoCompact agentBuildInfo))
            , padTop (Pad 1) $
                vBox
                    [ drawCapability
                        Theme.toolAttr
                        "Tools"
                        toolNames
                    , drawCapability
                        Theme.controlLinkAttr
                        "Skills"
                        skillNames
                    ]
            , padTop (Pad 1) $
                hCenter $
                    hLimit 44 $
                        drawQuickStartActions state
            , padTop (Pad 1) $
                withAttr Theme.mutedAttr $
                    hCenter (txt "Tip: Type / to browse commands and skills.")
            ]
  where
    toolNames = startupToolNames state
    skillNames = startupSkillNames state
    capabilityLabelWidth = 14
    capabilityTextWidth = max 1 (panelWidth - capabilityLabelWidth)

    drawCapability valueAttr label names =
        vBox $
            zipWith
                (drawCapabilityLine valueAttr heading)
                [0 :: Int ..]
                (startupCapabilityLines
                    capabilityTextWidth
                    capabilityRows
                    names)
      where
        heading =
            label <> " (" <> Text.pack (show (length names)) <> ")"

    drawCapabilityLine valueAttr heading lineIndex line =
        hBox
            [ hLimit capabilityLabelWidth $
                if lineIndex == 0
                    then
                        withAttr Theme.strongAttr $
                            txt $
                                Text.justifyLeft capabilityLabelWidth ' ' $
                                    truncateDisplayText
                                        capabilityLabelWidth
                                        heading
                    else fill ' '
            , hLimit capabilityTextWidth $
                withAttr valueAttr (txt line)
            ]

drawQuickStartActions :: AppState -> Widget Name
drawQuickStartActions state =
    vBox
        [ withAttr Theme.headingAttr $
            hCenter (txt "What would you like to do?")
        , vBox (map drawQuickStartRow quickStartRows)
        ]
  where
    drawQuickStartRow (name, label, command) =
        clickable name $
            case Composer.controlInteractionAttr state name of
                Just attr -> forceAttr attr row
                Nothing -> row
      where
        row =
            hBox
                [ withAttr Theme.controlLinkAttr (txt ("  " <> label))
                , vLimit 1 (fill ' ')
                , withAttr Theme.mutedAttr (txt command)
                , txt "  "
                ]

startupToolNames :: AppState -> [Text]
startupToolNames state =
    sanitizeCapabilityNames
        (Set.toAscList state.appSlashCatalog.slashCatalogToolNames)

startupSkillNames :: AppState -> [Text]
startupSkillNames state =
    map ("/" <>)
        (sanitizeCapabilityNames
            (map
                (.skillCommandName)
                state.appSlashCatalog.slashCatalogSkills))

startupCapabilityLines :: Int -> Int -> [Text] -> [Text]
startupCapabilityLines width maxRows rawNames
    | width <= 0 || maxRows <= 0 = []
    | null names = [truncateDisplayText width "none"]
    | otherwise = layout maxRows names
  where
    names = sanitizeCapabilityNames rawNames

    layout _ [] = []
    layout 1 remaining = [finalCapabilityLine width remaining]
    layout rows remaining =
        let (shown, rest) = takeCapabilityLine width remaining
            line = Text.intercalate capabilitySeparator shown
        in line :
            if null rest
                then []
                else layout (rows - 1) rest

sanitizeCapabilityNames :: [Text] -> [Text]
sanitizeCapabilityNames =
    filter (not . Text.null)
        . map (displayEditorText . Text.strip)

capabilitySeparator :: Text
capabilitySeparator = " · "

takeCapabilityLine :: Int -> [Text] -> ([Text], [Text])
takeCapabilityLine _ [] = ([], [])
takeCapabilityLine width (first : rest)
    | terminalTextWidth first > width =
        ([truncateDisplayText width first], rest)
    | otherwise = go [first] rest
  where
    go shown [] = (shown, [])
    go shown remaining@(next : remainingTail)
        | terminalTextWidth candidate <= width =
            go (shown <> [next]) remainingTail
        | otherwise = (shown, remaining)
      where
        candidate =
            Text.intercalate capabilitySeparator (shown <> [next])

finalCapabilityLine :: Int -> [Text] -> Text
finalCapabilityLine width [name] =
    truncateDisplayText width name
finalCapabilityLine width names
    | terminalTextWidth complete <= width = complete
    | otherwise = choose (length names - 1)
  where
    complete = Text.intercalate capabilitySeparator names
    total = length names

    choose shownCount
        | shownCount <= 0 =
            truncateDisplayText width (omissionText total)
        | terminalTextWidth candidate <= width = candidate
        | otherwise = choose (shownCount - 1)
      where
        omitted = total - shownCount
        candidate =
            Text.intercalate capabilitySeparator (take shownCount names)
                <> capabilitySeparator
                <> omissionText omitted

omissionText :: Int -> Text
omissionText omitted =
    "… +" <> Text.pack (show omitted)
