-- | Interactive permission card for mutating tools.
module Agent.CLI.Permission
    ( PermissionChoice(..)
    , PermissionState(..)
    , applyPermissionKey
    , initialPermissionState
    , promptPermission
    , promptRootAccess
    , renderPermissionFrame
    ) where

import Agent.CLI.Input (readApprovalLine)
import Agent.CLI.Notification
    ( AttentionRequest(PermissionRequested)
    , notifyAttention
    )
import Agent.CLI.Options (ApprovalAnswer(..), parseApprovalAnswer)
import Agent.CLI.Picker (PickerKey(..), runOverlay)
import Agent.CLI.Style (glyphWarn, roleMuted, roleSuccess, roleWarn)
import Agent.TUI.Presentation (permissionToolCallPromptRelative)
import Agent.ToolDispatch (ToolCall)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO (hIsTerminalDevice, stderr, stdin)
import System.OsPath (OsPath)
import Agent.OsPath (toText)

data PermissionChoice
    = PermissionAllowOnce
    | PermissionAllowAll
    | PermissionAllowTool
    | PermissionDeny
    deriving (Eq, Show)

data PermissionState = PermissionState
    { permSummary :: !Text
    , permIndex :: !Int
    }
    deriving (Eq, Show)

permissionLabels :: [Text]
permissionLabels =
    [ "Allow once"
    , "Always approve all tools for this project"
    , "Always allow this tool this session"
    , "Deny"
    ]

initialPermissionState :: Text -> PermissionState
initialPermissionState summary =
    PermissionState { permSummary = summary, permIndex = 0 }

applyPermissionKey
    :: PickerKey
    -> PermissionState
    -> Either PermissionChoice PermissionState
applyPermissionKey key state = case key of
    PickerKeyCancel -> Left PermissionDeny
    PickerKeyConfirm -> Left (choiceFromIndex state.permIndex)
    PickerKeyUp -> Right state { permIndex = move (-1) state.permIndex }
    PickerKeyDown -> Right state { permIndex = move 1 state.permIndex }
    PickerKeyChar 'A' -> Left PermissionAllowAll
    PickerKeyChar c ->
        case Text.toLower (Text.singleton c) of
            "y" -> Left PermissionAllowOnce
            "a" -> Left PermissionAllowTool
            "n" -> Left PermissionDeny
            _ -> Right state
    PickerKeyBackspace -> Right state
  where
    n = length permissionLabels
    move delta i = (i + delta) `mod` n

choiceFromIndex :: Int -> PermissionChoice
choiceFromIndex = \case
    0 -> PermissionAllowOnce
    1 -> PermissionAllowAll
    2 -> PermissionAllowTool
    _ -> PermissionDeny

renderPermissionFrame :: Bool -> PermissionState -> Text
renderPermissionFrame color state =
    let header =
            roleWarn color (glyphWarn <> state.permSummary)
        rows =
            zipWith
                (\i label -> renderRow color (i == state.permIndex) label)
                [0 ..]
                permissionLabels
        footer =
            roleMuted color
                "↑↓/jk or scroll · click/enter · y once · A all · a this tool · n/esc deny"
    in Text.intercalate "\n" (header : rows <> [footer])

renderRow :: Bool -> Bool -> Text -> Text
renderRow color selected label =
    let cursor = if selected then roleWarn color "› " else "  "
        body = if selected then roleSuccess color label else roleMuted color label
    in cursor <> body

-- | TTY card; non-TTY keeps cooked @y/n/a/A@. Uppercase @A@, @all@, or
-- @yolo@ enables project-wide auto-approval; lowercase @a@ remembers only the
-- current tool for this session.
promptPermission :: Bool -> Text -> ToolCall -> IO (Maybe PermissionChoice)
promptPermission color workspace call = do
    isTty <- hIsTerminalDevice stdin
    let summary = permissionToolCallPromptRelative workspace call
    if not isTty
        then cooked color summary
        else do
            notifyAttention stderr PermissionRequested
            result <-
                runOverlay
                    (renderPermissionFrame color)
                    applyPermissionKey
                    (initialPermissionState summary)
            pure (Just (fromMaybe PermissionDeny result))

-- | Ask whether an additional directory may be used for this session.
-- This is intentionally separate from tool permission: granting a directory
-- never enables a tool or persists beyond the current session.
promptRootAccess :: Bool -> OsPath -> IO Bool
promptRootAccess color root = do
    isTty <- hIsTerminalDevice stdin
    let summary = "Allow filesystem access to " <> toText root <> " for this session?"
        labels = ["Allow directory for this session", "Deny"]
        render state =
            let rows = zipWith
                    (\i label -> renderRow color (i == state) label)
                    [0 ..] labels
            in Text.intercalate "\n"
                (roleWarn color (glyphWarn <> summary) : rows
                    <> [roleMuted color "↑↓/jk or scroll · enter/click · y allow · n/esc deny"])
        step key state = case key of
            PickerKeyCancel -> Left False
            PickerKeyConfirm -> Left (state == 0)
            PickerKeyUp -> Right ((state - 1) `mod` length labels)
            PickerKeyDown -> Right ((state + 1) `mod` length labels)
            PickerKeyChar c
                | Text.toLower (Text.singleton c) == "y" -> Left True
                | Text.toLower (Text.singleton c) == "n" -> Left False
            _ -> Right state
    if not isTty
        then readApprovalLine (roleWarn color (glyphWarn <> summary <> " [y/N] ")) >>= \case
            Just raw -> pure (Text.toLower (Text.strip raw) `elem` ["y", "yes"])
            Nothing -> pure False
        else do
            notifyAttention stderr PermissionRequested
            fromMaybe False <$> runOverlay render step 0

cooked :: Bool -> Text -> IO (Maybe PermissionChoice)
cooked color summary = do
    let question =
            roleWarn color (glyphWarn <> summary <> " [y/N/a/A] ")
    readApprovalLine question >>= \case
        Nothing -> pure Nothing
        Just raw -> pure $ Just $ case parseApprovalAnswer raw of
            AllowOnce -> PermissionAllowOnce
            AllowAlways -> PermissionAllowTool
            AllowAll -> PermissionAllowAll
            Deny -> PermissionDeny
