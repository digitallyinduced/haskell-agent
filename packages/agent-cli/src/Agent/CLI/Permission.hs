-- | Interactive permission card for mutating tools.
module Agent.CLI.Permission
    ( PermissionChoice(..)
    , PermissionState(..)
    , applyPermissionKey
    , initialPermissionState
    , promptPermission
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
import Agent.TUI.Presentation (permissionToolCallPrompt)
import Agent.ToolDispatch (ToolCall)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO (hIsTerminalDevice, stderr, stdin)

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
promptPermission :: Bool -> ToolCall -> IO (Maybe PermissionChoice)
promptPermission color call = do
    isTty <- hIsTerminalDevice stdin
    let summary = permissionToolCallPrompt call
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
