-- | Internal fullscreen rendering helpers.
module Agent.CLI.TUI.Render.Transcript
    ( drawTranscript
    , historyRangeWidgets
    , stickyPromptLayers
    , drawEmptyConversation
    , drawConversationBlocks
    , quickStartCardHeight
    , quickStartVisible
    , quickStartWideVisible
    , quickStartCardWidth
    , quickStartRows
    , drawQuickStartCard
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
    ( BuildInfo(buildVersion), agentBuildInfo )
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
import Agent.TUI.Accent ()
import Agent.CLI.TUI.Types
    ( AppState(appConversationAnchor, appHoveredControl,
               appAgentEntries, appSlashCatalog, appHistoryWindow, appUi,
               appAgentSelected),
      Name(QuickStartModel, CodeCopy, ConversationChunkCache,
           ConversationReserve, QuickStartWorktree, QuickStartResume,
           QuickStartCommands, QuickStartChangelog) )
import Agent.CLI.Terminal ()
import Agent.CLI.Timestamp ()
import Agent.Loop ()
import Agent.Syntax ()
import Agent.TUI.Markdown ()
import Agent.TUI.Model
    ( Focus(FocusScrollback),
      UiBlock,
      UiState(uiBlocks, uiFocus, uiSelectedBlock) )
import Agent.TUI.Presentation ()
import Agent.TUI.TextWidth ( displayTerminalText )
import Agent.ToolDispatch ()
import Brick
    ( cached,
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
      Widget,
      Padding(Pad) )
import Brick.BChan ()
import Brick.Widgets.Border ( border )
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
    ( getContext, Size(Greedy), Widget(Widget), render )
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
    ( mutedAttr,
      strongAttr,
      userAttr )
import qualified Agent.CLI.TUI.Transcript as Transcript
    ( coalesceInspectionBlocks
    , transcriptChunks
    , transcriptChunkCacheKey
    )
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
            ( toList
                . Transcript.coalesceInspectionBlocks
                . (.historyTurnBlocks)
            )
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
            ( Transcript.transcriptChunks
                (Transcript.coalesceInspectionBlocks ui.uiBlocks)
            )

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
            cardWidth = quickStartCardWidth width
        B.render $
            if not (quickStartVisible width height)
                then center drawQuickStartLogo
                else
                    center $
                        drawQuickStartCard
                            state
                            cardWidth
                            (quickStartWideVisible width height)

quickStartCardWidth :: Int -> Int
quickStartCardWidth width =
    max 1 (min 112 (width - 4))

quickStartCardHeight :: Bool -> Int
quickStartCardHeight showLogo =
    cardChromeRows
        + contentFixedRows
        + length quickStartRows
        + toolRows
  where
    cardChromeRows = 4
    contentFixedRows = 4
    toolRows = if showLogo then 2 else 1

quickStartVisible :: Int -> Int -> Bool
quickStartVisible width height =
    width >= 48 && height >= quickStartCardHeight False

quickStartWideVisible :: Int -> Int -> Bool
quickStartWideVisible width height =
    width >= 88 && height >= quickStartCardHeight True

quickStartRows :: [(Name, Text, Text)]
quickStartRows =
    [ (QuickStartWorktree, "New worktree", "/worktree")
    , (QuickStartResume, "Resume session", "/resume")
    , (QuickStartCommands, "Browse commands", "/")
    , (QuickStartModel, "Manage models", "/model")
    , (QuickStartChangelog, "View changelog", "/changelog")
    ]

drawQuickStartCard :: AppState -> Int -> Bool -> Widget Name
drawQuickStartCard state cardWidth showLogo =
    hLimit cardWidth $
        withBorderStyle unicodeRounded $
            border $
                padAll 1 $
                    if showLogo
                        then
                            hBox
                                [ drawQuickStartLogo
                                , padLeft (Pad logoGap) $
                                    hLimit contentWidth $
                                        drawQuickStartContent
                                            state
                                            contentWidth
                                            2
                                ]
                        else
                            hLimit innerWidth $
                                drawQuickStartContent state innerWidth 1
  where
    innerWidth = max 1 (cardWidth - 4)
    logoWidth = 14
    logoGap = 3
    contentWidth =
        max 1 (innerWidth - logoWidth - logoGap)

drawQuickStartLogo :: Widget Name
drawQuickStartLogo =
    hLimit 14 $
        vLimit 9 $
            center $
                forceAttr Theme.mutedAttr $
                    hLimit 12 $
                        vLimit 9 $
                            lambdaArtWidget False 0

drawQuickStartContent :: AppState -> Int -> Int -> Widget Name
drawQuickStartContent state width toolRows =
    vBox
        [ drawQuickStartIdentity width
        , padTop (Pad 1) $
            drawQuickStartActions state
        , padTop (Pad 1) $
            drawQuickStartCapabilities state width toolRows
        ]

drawQuickStartIdentity :: Int -> Widget Name
drawQuickStartIdentity width =
    hLimit width $
        hBox
            [ withAttr Theme.strongAttr (txt productName)
            , txt "  "
            , withAttr Theme.mutedAttr $
                txt $
                    truncateDisplayText versionWidth $
                        "v" <> agentBuildInfo.buildVersion
            ]
  where
    productName = "haskell-agent"
    versionWidth =
        max 1 (width - terminalTextWidth productName - 2)

drawQuickStartActions :: AppState -> Widget Name
drawQuickStartActions state =
    vBox (map drawQuickStartRow quickStartRows)
  where
    drawQuickStartRow (name, label, command) =
        clickable name $
            case Composer.controlInteractionAttr state name of
                Just attr -> forceAttr attr row
                Nothing -> row
      where
        row =
            hBox
                [ withAttr Theme.strongAttr (txt label)
                , vLimit 1 (fill ' ')
                , withAttr Theme.mutedAttr (txt command)
                ]

drawQuickStartCapabilities :: AppState -> Int -> Int -> Widget Name
drawQuickStartCapabilities state width toolRows =
    vBox
        [ drawCapability "Tools" toolRows (startupToolNames state)
        , drawCapability "Skills" 1 (startupSkillNames state)
        ]
  where
    labelWidth = min 13 (max 1 (width `div` 3))
    valueWidth = max 1 (width - labelWidth)

    drawCapability label maxRows names =
        vBox $
            zipWith
                (drawCapabilityLine heading)
                [0 :: Int ..]
                (startupCapabilityLines valueWidth maxRows names)
      where
        heading =
            label <> " (" <> Text.pack (show (length names)) <> ")"

    drawCapabilityLine heading lineIndex line =
        hBox
            [ hLimit labelWidth $
                if lineIndex == 0
                    then
                        withAttr Theme.strongAttr $
                            txt $
                                Text.justifyLeft labelWidth ' ' $
                                    truncateDisplayText labelWidth heading
                    else vLimit 1 (fill ' ')
            , hLimit valueWidth $
                withAttr Theme.mutedAttr (txt line)
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
