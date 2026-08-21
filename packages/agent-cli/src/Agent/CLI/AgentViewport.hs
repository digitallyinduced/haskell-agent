-- | Finder-style agent tree and transcript preview.
module Agent.CLI.AgentViewport
    ( AgentEntry(..)
    , AgentTarget(..)
    , AgentViewportEnv(..)
    , AgentViewportState(..)
    , applyAgentViewportKey
    , formatAgentStatus
    , initialAgentViewportState
    , pickAgentViewport
    , renderAgentTree
    , renderAgentViewportFrame
    , renderAgentViewportFrameFor
    , responseItemLines
    ) where

import Agent.CLI.Picker (PickerKey(..), runOverlay)
import Agent.CLI.Style (roleMuted, rolePrompt, roleSuccess)
import Agent.OpenAI.Responses.Types
import Agent.Subagents (SubagentId(..), SubagentStatus(..))
import Data.IORef (IORef)
import Data.List (findIndex, sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Console.ANSI (getTerminalSize)
import System.IO (hFlush, hIsTerminalDevice, stderr, stdin)

data AgentTarget
    = AgentRoot
    | AgentChild !SubagentId
    deriving (Eq, Ord, Show)

data AgentEntry = AgentEntry
    { agentTarget :: !AgentTarget
    , agentPath :: !Text
    , agentStatus :: !Text
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

data AgentViewportEnv = AgentViewportEnv
    { viewportSelected :: !(IORef AgentTarget)
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

selectedEntry :: AgentViewportState -> Maybe AgentEntry
selectedEntry state = case state.viewportAll of
    [] -> Nothing
    entries -> Just (entries !! clamp (length entries) state.viewportIndex)

applyAgentViewportKey
    :: PickerKey
    -> AgentViewportState
    -> Either (Maybe AgentTarget) AgentViewportState
applyAgentViewportKey key state = case key of
    PickerKeyCancel -> Left Nothing
    PickerKeyConfirm -> Left ((.agentTarget) <$> selectedEntry state)
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
                    (clamp n current.viewportIndex + delta) `mod` n
                }

renderAgentTree :: Bool -> AgentTarget -> [AgentEntry] -> Text
renderAgentTree color selected entries
    | length ordered <= 1 = ""
    | otherwise =
        Text.intercalate "\n" $
            rolePrompt color "agents"
                : map renderEntry (zip [0 ..] ordered)
                <> [roleMuted color
                    ("  viewing " <> selectedPath
                        <> " · /agents to switch")]
  where
    ordered = sortOn (.agentPath) entries
    effectiveSelected =
        case findByTarget selected ordered of
            Just _ -> selected
            Nothing -> AgentRoot
    selectedPath =
        maybe "/root" (.agentPath)
            (findByTarget effectiveSelected ordered)
    renderEntry (index, entry) =
        let isSelected = entry.agentTarget == effectiveSelected
            marker = if isSelected then "› " else "  "
            branch = treePrefix ordered index entry.agentPath
            line =
                marker
                    <> branch
                    <> pathName entry.agentPath
                    <> "  "
                    <> entry.agentStatus
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
    Text.intercalate "\n" (header : headings : body <> [footer])
  where
    cols = max 20 terminalCols
    bodyRows = max 1 (terminalRows - 4)
    divider = roleMuted color " │ "
    leftWidth = max 12 (min 38 ((cols - 3) * 2 `div` 5))
    rightWidth = max 1 (cols - leftWidth - 3)
    entries = state.viewportAll
    n = length entries
    idx = clamp n state.viewportIndex
    shown = entryWindow bodyRows idx entries
    selected = selectedEntry state
    header =
        rolePrompt color "agents"
            <> roleMuted color
                (fitCell (max 0 (cols - 6))
                    (" · " <> Text.pack (show n)
                        <> if n == 1 then " agent" else " agents"))
    headings =
        rolePrompt color (fitCell leftWidth "hierarchy")
            <> divider
            <> rolePrompt color
                (fitCell rightWidth
                    ("transcript"
                        <> maybe "" (\entry -> " · " <> entry.agentPath) selected))
    leftRows =
        map
            (\(absoluteIndex, entry) ->
                let prefix = if absoluteIndex == idx then "› " else "  "
                    branch = treePrefix entries absoluteIndex entry.agentPath
                    text = fitCell leftWidth
                        (prefix <> branch <> pathName entry.agentPath
                            <> "  " <> entry.agentStatus)
                in if absoluteIndex == idx
                    then roleSuccess color text
                    else text)
            shown
            <> repeat (Text.replicate leftWidth " ")
    rightRows = case selected of
        Nothing -> roleMuted color (fitCell rightWidth "(no agents)") : repeat ""
        Just entry ->
            let preview = previewRows rightWidth bodyRows entry.agentTranscript
            in map (fitCell rightWidth) preview <> repeat ""
    body =
        take bodyRows $
            zipWith (\left right -> left <> divider <> right) leftRows rightRows
    footer =
        roleMuted color $
            fitCell cols "↑↓/jk switch viewport · enter keep · esc/q cancel"

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
responseItemLines = concatMap itemLines
  where
    itemLines = \case
        MessageItem message ->
            labelled (roleLabel message.role) (messageText message.content)
        FunctionCallItem call ->
            ["tool: " <> call.name]
        CustomToolCallItem call ->
            ["tool: " <> call.name]
        FunctionCallOutputItem _ -> ["tool: completed"]
        CustomToolCallOutputItem _ -> ["tool: completed"]
        _ -> []

    roleLabel = \case
        RoleUser -> "user: "
        RoleAssistant -> "assistant: "
        RoleSystem -> "system: "
        RoleDeveloper -> "developer: "
        RoleUnknown value -> value <> ": "

    messageText = \case
        MessageContentText text -> text
        MessageContentParts parts ->
            Text.intercalate "\n" (concatMap contentText parts)

    contentText = \case
        InputTextPart{text} -> [text]
        OutputTextPart{text} -> [text]
        RefusalPart{refusal} -> [refusal]
        ReasoningTextPart{text} -> [text]
        SummaryTextPart{text} -> [text]
        InputImagePart{} -> ["[image]"]
        InputFilePart{filename} -> ["[file" <> maybe "" (" " <>) filename <> "]"]
        InputAudioPart{} -> ["[audio]"]
        UnknownContentPart{} -> []

    labelled prefix raw =
        case Text.lines (Text.strip raw) of
            [] -> []
            firstLine : rest ->
                (prefix <> firstLine)
                    : map (Text.replicate (Text.length prefix) " " <>) rest

findByTarget :: AgentTarget -> [AgentEntry] -> Maybe AgentEntry
findByTarget target = go
  where
    go [] = Nothing
    go (entry : rest)
        | entry.agentTarget == target = Just entry
        | otherwise = go rest

pathName :: Text -> Text
pathName path =
    case reverse (filter (not . Text.null) (Text.splitOn "/" path)) of
        name : _ -> name
        [] -> "root"

treePrefix :: [AgentEntry] -> Int -> Text -> Text
treePrefix entries index path =
    case pathSegments path of
        [] -> "▾ "
        segments ->
            let ancestors =
                    [ if hasLaterSibling entries index (take level segments)
                        then "│  "
                        else "   "
                    | level <- [1 .. length segments - 1]
                    ]
                branch =
                    if hasLaterSibling entries index segments
                        then "├─ "
                        else "└─ "
            in Text.concat ancestors <> branch

hasLaterSibling :: [AgentEntry] -> Int -> [Text] -> Bool
hasLaterSibling entries index node =
    any isSibling (drop (index + 1) entries)
  where
    parent = init node
    isSibling entry =
        let candidate = pathSegments entry.agentPath
        in length candidate == length node
            && take (length parent) candidate == parent
            && candidate /= node

pathSegments :: Text -> [Text]
pathSegments path =
    case filter (not . Text.null) (Text.splitOn "/" path) of
        "root" : rest -> rest
        parts -> parts

entryWindow :: Int -> Int -> [a] -> [(Int, a)]
entryWindow count selected xs =
    let total = length xs
        start = max 0 (min selected (total - count))
    in zip [start ..] (take count (drop start xs))

previewRows :: Int -> Int -> [Text] -> [Text]
previewRows width count logicalLines =
    let wrapped = concatMap (hardWrap width) logicalLines
        rows
            | null wrapped = ["(empty transcript)"]
            | otherwise = wrapped
    in drop (max 0 (length rows - count)) rows

hardWrap :: Int -> Text -> [Text]
hardWrap width raw
    | Text.null raw = [""]
    | otherwise = go raw
  where
    width' = max 1 width
    go text
        | Text.null text = []
        | otherwise =
            let (line, rest) = Text.splitAt width' text
            in line : go rest

fitCell :: Int -> Text -> Text
fitCell width raw
    | width <= 0 = ""
    | Text.length clean <= width =
        clean <> Text.replicate (width - Text.length clean) " "
    | width == 1 = "…"
    | otherwise = Text.take (width - 1) clean <> "…"
  where
    clean =
        Text.map
            (\c -> if c == '\t' || c == '\r' || c == '\n' then ' ' else c)
            raw

clamp :: Int -> Int -> Int
clamp n i
    | n <= 0 = 0
    | i < 0 = 0
    | i >= n = n - 1
    | otherwise = i
