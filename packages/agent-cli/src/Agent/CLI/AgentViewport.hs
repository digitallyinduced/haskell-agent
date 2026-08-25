-- | Finder-style agent tree and transcript preview.
module Agent.CLI.AgentViewport
    ( AgentEntry(..)
    , AgentStep(..)
    , AgentStepState(..)
    , AgentTarget(..)
    , AgentViewportEnv(..)
    , AgentViewportState(..)
    , agentDisplayName
    , agentEntryTreeLabel
    , agentEntryTreeLabelWithGlyph
    , agentEntryTreeLabelWithGlyphModel
    , agentStatusGlyph
    , agentStepsForStatus
    , applyAgentViewportKey
    , formatAgentStatus
    , initialAgentViewportState
    , pickAgentViewport
    , refreshAgentViewportState
    , renderAgentTree
    , renderAgentViewportPanelFor
    , renderAgentViewportFrame
    , renderAgentViewportFrameFor
    , responseItemLines
    , responseItemPreviewLines
    , responseItemStepPreviews
    , selectAgentTarget
    , selectedAgentEntry
    ) where

import Agent.CLI.Render (summarizeToolCall)
import Agent.CLI.Picker (PickerKey(..), runOverlay)
import Agent.CLI.Style (roleMuted, rolePrompt, roleSuccess)
import Agent.CLI.TextLayout
    ( SplitPaneFrame(..)
    , clampSelectionIndex
    , renderSplitPaneFrame
    )
import Agent.Responses.Types
import Agent.Subagents (SubagentId(..), SubagentStatus(..))
import Agent.ToolDispatch (customToolCall, functionToolCall)
import Data.IORef (IORef)
import Data.List (findIndex, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Console.ANSI (getTerminalSize)
import System.IO (hFlush, hIsTerminalDevice, stderr, stdin)

data AgentTarget
    = AgentRoot
    | AgentChild !SubagentId
    deriving (Eq, Ord, Show)

data AgentStepState
    = AgentStepRunning
    | AgentStepCompleted
    | AgentStepFailed
    | AgentStepInfo
    deriving (Eq, Show)

data AgentStep = AgentStep
    { agentStepState :: !AgentStepState
    , agentStepTitle :: !Text
    , agentStepDetail :: !(Maybe Text)
    }
    deriving (Eq, Show)

-- | A flattened tree row with presentation metadata derived from the whole
-- hierarchy. Renderers consume these rows without rescanning later entries.
data AgentTreeRow = AgentTreeRow
    { treeRowEntry :: !AgentEntry
    , treeRowPrefix :: !Text
    }

data AgentEntry = AgentEntry
    { agentTarget :: !AgentTarget
    , agentPath :: !Text
    , agentStatus :: !Text
    , agentModel :: !(Maybe Text)
    , agentSteps :: ![AgentStep]
    , agentTranscript :: ![Text]
    }
    deriving (Eq, Show)

formatAgentStatus :: SubagentStatus -> Text
formatAgentStatus status = case status of
    Pending -> "pending"
    Running -> "running"
    Completed _ -> "done"
    Errored _ -> "error"
    Interrupted -> "interrupted"
    Closed -> "closed"
    NotFound -> "missing"

agentStatusGlyph :: Text -> Text
agentStatusGlyph status = case Text.toLower status of
    "active" -> "●"
    "running" -> "●"
    "ready" -> "○"
    "pending" -> "○"
    "done" -> "✓"
    "error" -> "✕"
    "interrupted" -> "■"
    "closed" -> "×"
    "missing" -> "?"
    _ -> "·"

data AgentViewportEnv = AgentViewportEnv
    { viewportSelected :: !(IORef AgentTarget)
    , viewportSelect :: !(AgentTarget -> IO ())
    , viewportEntries :: !(IO [AgentEntry])
    }

data AgentViewportState = AgentViewportState
    { viewportAll :: ![AgentEntry]
    , viewportIndex :: !Int
    }
    deriving (Eq, Show)

initialAgentViewportState :: AgentTarget -> [AgentEntry] -> AgentViewportState
initialAgentViewportState selected entries =
    AgentViewportState
        { viewportAll = ordered
        , viewportIndex =
            maybe 0 id (findIndex ((== selected) . (.agentTarget)) ordered)
        }
  where
    ordered = sortOn (.agentPath) entries

selectedAgentEntry :: AgentViewportState -> Maybe AgentEntry
selectedAgentEntry state = case state.viewportAll of
    [] -> Nothing
    entries ->
        Just
            (entries
                !! clampSelectionIndex (length entries) state.viewportIndex)

refreshAgentViewportState
    :: [AgentEntry]
    -> AgentViewportState
    -> AgentViewportState
refreshAgentViewportState entries state =
    initialAgentViewportState selected entries
  where
    selected =
        maybe AgentRoot (.agentTarget) (selectedAgentEntry state)

selectAgentTarget :: AgentTarget -> AgentViewportState -> AgentViewportState
selectAgentTarget target state =
    state
        { viewportIndex =
            maybe state.viewportIndex id
                (findIndex ((== target) . (.agentTarget)) state.viewportAll)
        }

applyAgentViewportKey
    :: PickerKey
    -> AgentViewportState
    -> Either (Maybe AgentTarget) AgentViewportState
applyAgentViewportKey key state = case key of
    PickerKeyCancel -> Left Nothing
    PickerKeyConfirm -> Left ((.agentTarget) <$> selectedAgentEntry state)
    PickerKeyUp -> Right (move (-1) state)
    PickerKeyDown -> Right (move 1 state)
    _ -> Right state
  where
    move delta current =
        let n = length current.viewportAll
        in if n == 0
            then current { viewportIndex = 0 }
            else current
                { viewportIndex =
                    (clampSelectionIndex n current.viewportIndex + delta) `mod` n
                }

renderAgentTree :: Bool -> AgentTarget -> [AgentEntry] -> Text
renderAgentTree color selected entries
    | length rows <= 1 = ""
    | otherwise =
        Text.intercalate "\n" $
            rolePrompt color "agents"
                : map renderEntry rows
                <> [roleMuted color
                    ("  viewing " <> selectedPath
                        <> " · /agents to switch")]
  where
    ordered = sortOn (.agentPath) entries
    rows = agentTreeRows ordered
    effectiveSelected =
        case findByTarget selected ordered of
            Just _ -> selected
            Nothing -> AgentRoot
    selectedPath =
        maybe "/root" (.agentPath)
            (findByTarget effectiveSelected ordered)
    renderEntry row =
        let entry = row.treeRowEntry
            isSelected = entry.agentTarget == effectiveSelected
            marker = if isSelected then "› " else "  "
            line = marker <> agentTreeRowLabel row
        in if isSelected then roleSuccess color line else line

renderAgentViewportFrame :: Bool -> AgentViewportState -> Text
renderAgentViewportFrame color = renderAgentViewportFrameFor color 24 100

renderAgentViewportFrameFor
    :: Bool
    -> Int
    -> Int
    -> AgentViewportState
    -> Text
renderAgentViewportFrameFor color terminalRows terminalCols state =
    renderAgentViewportFor
        color
        (max 1 (terminalRows - 4))
        terminalCols
        "↑↓/jk or scroll · click/enter keep · esc/q cancel"
        state

renderAgentViewportPanelFor
    :: Bool
    -> Int
    -> AgentTarget
    -> [AgentEntry]
    -> Text
renderAgentViewportPanelFor color terminalCols selected entries
    | length entries <= 1 = ""
    | otherwise =
        renderAgentViewportFor
            color
            6
            terminalCols
            ("viewing " <> selectedPath
                <> " · input routes to /root · /agents switch")
            state
  where
    state = initialAgentViewportState selected entries
    selectedPath =
        maybe "/root" (.agentPath) (selectedAgentEntry state)

renderAgentViewportFor
    :: Bool
    -> Int
    -> Int
    -> Text
    -> AgentViewportState
    -> Text
renderAgentViewportFor color bodyRows terminalCols footerText state =
    renderSplitPaneFrame SplitPaneFrame
        { splitPaneMinColumns = 20
        , splitPaneColumns = terminalCols
        , splitPaneBodyRows = bodyRows
        , splitPaneLeftMinWidth = 12
        , splitPaneLeftMaxWidth = 38
        , splitPaneDivider = " │ "
        , splitPaneTitle = "agents"
        , splitPaneHeaderDetail =
            \count ->
                Text.pack (show count)
                    <> if count == 1 then " agent" else " agents"
        , splitPaneLeftHeading = "hierarchy"
        , splitPaneRightHeading =
            \selected ->
                "transcript"
                    <> maybe
                        ""
                        (\row -> " · " <> row.treeRowEntry.agentPath)
                        selected
        , splitPaneItems = rows
        , splitPaneSelectedIndex = state.viewportIndex
        , splitPaneLeftLabel = \_ -> agentTreeRowLabel
        , splitPaneTranscript = (.treeRowEntry.agentTranscript)
        , splitPaneEmptyTranscript = "(no agents)"
        , splitPaneFooter = footerText
        , splitPanePromptStyle = rolePrompt color
        , splitPaneMutedStyle = roleMuted color
        , splitPaneSelectedStyle = roleSuccess color
        }
  where
    rows = agentTreeRows state.viewportAll

pickAgentViewport
    :: Bool
    -> AgentTarget
    -> [AgentEntry]
    -> IO (Maybe AgentTarget)
pickAgentViewport color selected entries = do
    isTty <- hIsTerminalDevice stdin
    let state = initialAgentViewportState selected entries
    if not isTty
        then do
            Text.hPutStrLn stderr (renderAgentTree color selected entries)
            hFlush stderr
            pure Nothing
        else do
            size <- getTerminalSize
            let (rows, cols) = maybe (24, 100) id size
            result <-
                runOverlay
                    (renderAgentViewportFrameFor color rows cols)
                    applyAgentViewportKey
                    state
            pure $ case result of
                Just (Just target) -> Just target
                _ -> Nothing

responseItemLines :: [ResponseItem] -> [Text]
responseItemLines = concatMap responseItemLineList

-- | Keep a compact agent preview: the first line for picker context plus
-- only the most recent logical lines for the live pane. Earlier response
-- items are traversed only as list spine once the tail is full; their message
-- bodies are not split or copied.
responseItemPreviewLines :: Int -> [ResponseItem] -> [Text]
responseItemPreviewLines count items
    | count <= 0 = maybe [] pure (responseItemFirstLine items)
    | remaining > 0 = trailing
    | otherwise = case responseItemFirstLine items of
        Nothing -> trailing
        Just firstLine -> case trailing of
            trailingFirst : _
                | trailingFirst == firstLine -> trailing
            _ -> firstLine : trailing
  where
    (remaining, trailing) =
        foldr collectTail (count, []) items
    collectTail item result@(needed, kept)
        | needed <= 0 = result
        | otherwise =
            let rows = responseItemLineList item
                rowCount = length rows
                selected = drop (max 0 (rowCount - needed)) rows
            in (max 0 (needed - rowCount), selected <> kept)

-- | Return the latest meaningful agent actions, newest first. Tool outputs are
-- folded into their originating calls so the preview shows one semantic step
-- instead of adjacent @tool: name@ / @tool: completed@ rows.
responseItemStepPreviews :: Int -> [ResponseItem] -> [AgentStep]
responseItemStepPreviews count items
    | count <= 0 = []
    | otherwise = go count Map.empty (reverse items)
  where
    go _ _ [] = []
    go remaining completed (item : rest)
        | remaining <= 0 = []
        | otherwise = case item of
            FunctionCallOutputItem output ->
                go remaining
                    (rememberNewestOutput output.callId
                        (outputStepState output.status)
                        completed)
                    rest
            CustomToolCallOutputItem output ->
                go remaining
                    (rememberNewestOutput output.callId
                        (outputStepState output.status)
                        completed)
                    rest
            FunctionCallItem call ->
                let state =
                        Map.findWithDefault
                            (callStepState call.status)
                            call.callId
                            completed
                    step =
                        AgentStep
                            { agentStepState = state
                            , agentStepTitle =
                                summarizeToolCall
                                    (functionToolCall
                                        call.callId
                                        call.name
                                        call.arguments)
                            , agentStepDetail = stepStateDetail state
                            }
                in step
                    : go (remaining - 1)
                        (Map.delete call.callId completed)
                        rest
            CustomToolCallItem call ->
                let state =
                        Map.findWithDefault
                            (callStepState call.status)
                            call.callId
                            completed
                    step =
                        AgentStep
                            { agentStepState = state
                            , agentStepTitle =
                                summarizeToolCall
                                    (customToolCall
                                        call.callId
                                        call.name
                                        call.input)
                            , agentStepDetail = stepStateDetail state
                            }
                in step
                    : go (remaining - 1)
                        (Map.delete call.callId completed)
                        rest
            MessageItem message
                | message.role == RoleAssistant ->
                    case textStep
                        (messageStepState message.status)
                        (responseMessageText message.content) of
                        Nothing -> go remaining completed rest
                        Just step ->
                            step : go (remaining - 1) completed rest
            _ -> go remaining completed rest

    -- We traverse newest-to-oldest. Keep the first output state seen for a
    -- call so an older partial update cannot overwrite its final status.
    rememberNewestOutput callId state =
        Map.insertWith (\_ newest -> newest) callId state

callStepState :: Maybe ItemStatus -> AgentStepState
callStepState = \case
    Just ItemIncomplete -> AgentStepFailed
    Just (ItemStatusUnknown status)
        | Text.toLower status `elem` ["failed", "error", "cancelled"] ->
            AgentStepFailed
    -- A completed call item means the model finished emitting the call, not
    -- that the tool finished executing. The matching output item settles it.
    _ -> AgentStepRunning

outputStepState :: Maybe ItemStatus -> AgentStepState
outputStepState = \case
    Just ItemCompleted -> AgentStepCompleted
    Just ItemIncomplete -> AgentStepFailed
    Just (ItemStatusUnknown status)
        | Text.toLower status `elem` ["failed", "error", "cancelled"] ->
            AgentStepFailed
    _ -> AgentStepInfo

stepStateDetail :: AgentStepState -> Maybe Text
stepStateDetail = \case
    AgentStepInfo -> Just "finished"
    _ -> Nothing

messageStepState :: Maybe ItemStatus -> AgentStepState
messageStepState = \case
    Just ItemInProgress -> AgentStepRunning
    Just ItemIncomplete -> AgentStepFailed
    Just (ItemStatusUnknown status)
        | Text.toLower status `elem` ["failed", "error", "cancelled"] ->
            AgentStepFailed
    _ -> AgentStepCompleted

agentStepsForStatus
    :: Int
    -> SubagentStatus
    -> [ResponseItem]
    -> [AgentStep]
agentStepsForStatus count status items
    | count <= 0 = []
    | otherwise = take count $ case status of
        Pending ->
            AgentStep AgentStepRunning "Waiting to start" Nothing : settled
        Running
            | any ((== AgentStepRunning) . (.agentStepState)) recent ->
                recent
            | otherwise ->
                AgentStep AgentStepRunning "Working…" Nothing : recent
        Completed result ->
            maybe
                (fallback
                    (AgentStep AgentStepCompleted "Finished" Nothing)
                    settled)
                (\raw ->
                    maybe settled (`prependDistinct` settled)
                        (textStep AgentStepCompleted raw))
                (nonEmptyText =<< result)
        Errored message ->
            AgentStep
                { agentStepState = AgentStepFailed
                , agentStepTitle = "Agent failed"
                , agentStepDetail = nonEmptyText message
                }
                : settled
        Interrupted ->
            AgentStep AgentStepFailed "Agent interrupted" Nothing : settled
        Closed ->
            fallback
                (AgentStep AgentStepCompleted "Agent closed" Nothing)
                settled
        NotFound ->
            AgentStep AgentStepFailed "Agent unavailable" Nothing : settled
  where
    recent = responseItemStepPreviews count items
    settled = map settleStep recent

    settleStep step
        | step.agentStepState == AgentStepRunning =
            step { agentStepState = AgentStepCompleted }
        | otherwise = step

    fallback step [] = [step]
    fallback _ steps = steps

prependDistinct :: AgentStep -> [AgentStep] -> [AgentStep]
prependDistinct step steps =
    case steps of
        current : _
            | normalizedStepTitle current == normalizedStepTitle step -> steps
        _ -> step : steps

normalizedStepTitle :: AgentStep -> Text
normalizedStepTitle =
    Text.toCaseFold . Text.unwords . Text.words . (.agentStepTitle)

textStep :: AgentStepState -> Text -> Maybe AgentStep
textStep state raw = do
    title <- listToMaybe lines_
    pure AgentStep
        { agentStepState = state
        , agentStepTitle = title
        , agentStepDetail = listToMaybe (drop 1 lines_)
        }
  where
    lines_ =
        filter (not . Text.null) $
            map (Text.unwords . Text.words) (Text.lines (Text.strip raw))

nonEmptyText :: Text -> Maybe Text
nonEmptyText raw =
    let compact = Text.unwords (Text.words raw)
    in if Text.null compact then Nothing else Just compact

responseItemFirstLine :: [ResponseItem] -> Maybe Text
responseItemFirstLine = go
  where
    go [] = Nothing
    go (item : rest) = case responseItemFirstItemLine item of
        Just line -> Just line
        Nothing -> go rest

responseItemFirstItemLine :: ResponseItem -> Maybe Text
responseItemFirstItemLine = \case
    MessageItem message ->
        labelledFirst
            (responseRoleLabel message.role)
            (responseMessageText message.content)
    FunctionCallItem call ->
        Just ("tool: " <> call.name)
    CustomToolCallItem call ->
        Just ("tool: " <> call.name)
    FunctionCallOutputItem _ -> Just "tool: completed"
    CustomToolCallOutputItem _ -> Just "tool: completed"
    _ -> Nothing

responseItemLineList :: ResponseItem -> [Text]
responseItemLineList = \case
    MessageItem message ->
        labelled
            (responseRoleLabel message.role)
            (responseMessageText message.content)
    FunctionCallItem call ->
        ["tool: " <> call.name]
    CustomToolCallItem call ->
        ["tool: " <> call.name]
    FunctionCallOutputItem _ -> ["tool: completed"]
    CustomToolCallOutputItem _ -> ["tool: completed"]
    _ -> []

responseRoleLabel :: ResponseRole -> Text
responseRoleLabel = \case
    RoleUser -> "user: "
    RoleAssistant -> "assistant: "
    RoleSystem -> "system: "
    RoleDeveloper -> "developer: "
    RoleUnknown value -> value <> ": "

responseMessageText :: MessageContent -> Text
responseMessageText = \case
    MessageContentText text -> text
    MessageContentParts parts ->
        Text.intercalate "\n" (concatMap responseContentText parts)

responseContentText :: ResponseContentPart -> [Text]
responseContentText = \case
    InputTextPart{text} -> [text]
    OutputTextPart{text} -> [text]
    RefusalPart{refusal} -> [refusal]
    ReasoningTextPart{text} -> [text]
    SummaryTextPart{text} -> [text]
    InputImagePart{} -> ["[image]"]
    InputFilePart{filename} -> ["[file" <> maybe "" (" " <>) filename <> "]"]
    InputAudioPart{} -> ["[audio]"]
    UnknownContentPart{} -> []

labelled :: Text -> Text -> [Text]
labelled prefix raw =
    case Text.lines (Text.strip raw) of
        [] -> []
        firstLine : rest ->
            (prefix <> firstLine)
                : map (Text.replicate (Text.length prefix) " " <>) rest

labelledFirst :: Text -> Text -> Maybe Text
labelledFirst prefix raw =
    let stripped = Text.strip raw
    in if Text.null stripped
        then Nothing
        else Just (prefix <> Text.takeWhile (/= '\n') stripped)

findByTarget :: AgentTarget -> [AgentEntry] -> Maybe AgentEntry
findByTarget target = go
  where
    go [] = Nothing
    go (entry : rest)
        | entry.agentTarget == target = Just entry
        | otherwise = go rest

agentEntryTreeLabel :: [AgentEntry] -> Int -> AgentEntry -> Text
agentEntryTreeLabel entries index entry =
    agentEntryTreeLabelWithGlyph
        (agentStatusGlyph entry.agentStatus)
        entries
        index
        entry

agentEntryTreeLabelWithGlyph
    :: Text
    -> [AgentEntry]
    -> Int
    -> AgentEntry
    -> Text
agentEntryTreeLabelWithGlyph glyph entries index entry =
    agentEntryTreeLabelWithGlyphDetail
        glyph
        (entry.agentStatus <> maybe "" (" · " <>) entry.agentModel)
        entries
        index
        entry

-- | Compact label for the narrow fullscreen agent pane. The glyph already
-- carries status, so prefer the model text when it is available.
agentEntryTreeLabelWithGlyphModel
    :: Text
    -> [AgentEntry]
    -> Int
    -> AgentEntry
    -> Text
agentEntryTreeLabelWithGlyphModel glyph entries index entry =
    agentEntryTreeLabelWithGlyphDetail
        glyph
        (maybe entry.agentStatus id entry.agentModel)
        entries
        index
        entry

agentEntryTreeLabelWithGlyphDetail
    :: Text
    -> Text
    -> [AgentEntry]
    -> Int
    -> AgentEntry
    -> Text
agentEntryTreeLabelWithGlyphDetail glyph detail entries index entry =
    treePrefixFrom
        (laterSiblingIndex (drop (index + 1) entries))
        (pathSegments entry.agentPath)
        <> agentDisplayName entry.agentPath
        <> "  "
        <> glyph
        <> " "
        <> detail

agentDisplayName :: Text -> Text
agentDisplayName path =
    case reverse (filter (not . Text.null) (Text.splitOn "/" path)) of
        name : _ -> name
        [] -> "root"

agentTreeRows :: [AgentEntry] -> [AgentTreeRow]
agentTreeRows entries =
    zipWith makeRow entries prefixes
  where
    (_, prefixes) = foldr collect (Map.empty, []) entries

    collect entry (laterSiblings, accumulated) =
        let segments = pathSegments entry.agentPath
            prefix = treePrefixFrom laterSiblings segments
        in ( rememberSibling segments laterSiblings
           , prefix : accumulated
           )

    makeRow entry prefix = AgentTreeRow
        { treeRowEntry = entry
        , treeRowPrefix = prefix
        }

laterSiblingIndex :: [AgentEntry] -> Map.Map [Text] (Set.Set Text)
laterSiblingIndex =
    foldr
        (rememberSibling . pathSegments . (.agentPath))
        Map.empty

rememberSibling
    :: [Text]
    -> Map.Map [Text] (Set.Set Text)
    -> Map.Map [Text] (Set.Set Text)
rememberSibling [] siblings = siblings
rememberSibling segments siblings =
    Map.insertWith Set.union
        (init segments)
        (Set.singleton (last segments))
        siblings

treePrefixFrom :: Map.Map [Text] (Set.Set Text) -> [Text] -> Text
treePrefixFrom laterSiblings segments =
    case segments of
        [] -> "▾ "
        _ ->
            let ancestors =
                    [ if hasLaterSibling (take level segments)
                        then "│  "
                        else "   "
                    | level <- [1 .. length segments - 1]
                    ]
                branch =
                    if hasLaterSibling segments
                        then "├─ "
                        else "└─ "
            in Text.concat ancestors <> branch
  where
    hasLaterSibling [] = False
    hasLaterSibling node =
        maybe
            False
            (not . Set.null . Set.delete (last node))
            (Map.lookup (init node) laterSiblings)

agentTreeRowLabel :: AgentTreeRow -> Text
agentTreeRowLabel row =
    let entry = row.treeRowEntry
    in row.treeRowPrefix
        <> agentDisplayName entry.agentPath
        <> "  "
        <> agentStatusGlyph entry.agentStatus
        <> " "
        <> entry.agentStatus
        <> maybe "" (" · " <>) entry.agentModel

pathSegments :: Text -> [Text]
pathSegments path =
    case filter (not . Text.null) (Text.splitOn "/" path) of
        "root" : rest -> rest
        parts -> parts
