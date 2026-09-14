-- | Interactive permission card for mutating tools.
module Agent.CLI.Permission
    ( PermissionChoice(..)
    , PermissionState(..)
    , approvalToolCallPrompt
    , approvalToolCallPromptRelative
    , approvalToolCallPromptOnceRelative
    , applyPermissionKey
    , applyPermissionOnceKey
    , initialPermissionState
    , promptPermission
    , promptPermissionOnce
    , promptRootAccess
    , renderPermissionFrame
    , renderPermissionOnceFrame
    , ApprovalPolicyChoice(..)
    , ApprovalPolicyDecision(..)
    , ApprovalPolicyState(..)
    , initialApprovalPolicyState
    , applyApprovalPolicyKey
    , approvalPolicyOptions
    , renderApprovalPolicyFrame
    ) where

import Agent.CLI.Input (readApprovalLine)
import Agent.ComputerUse (computerApprovalPrompt)
import Agent.CLI.Notification
    ( AttentionRequest(PermissionRequested)
    , notifyAttention
    )
import Agent.CLI.Options (ApprovalAnswer(..), parseApprovalAnswer)
import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.Runtime.Permission.Types (PermissionChoice(..))
import Agent.CLI.Picker (PickerKey(..), runOverlay)
import Agent.CLI.Style (glyphWarn, roleMuted, roleSuccess, roleWarn)
import Agent.TUI.Presentation (permissionToolCallPromptRelative)
import Agent.ToolDispatch (ToolCall(..), canonicalToolName)
import Control.Applicative ((<|>))
import Control.Monad (guard, when)
import Data.Aeson (Value(..), decodeStrict')
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified System.FilePath as FilePath
import System.IO (hIsTerminalDevice, stderr, stdin)
import System.OsPath (OsPath)
import Agent.OsPath (toText)

data ApprovalPolicyDecision
    = ApprovalPolicySelected !ApprovalPolicy
    | ApprovalPolicyCancelled
    deriving (Eq, Show)

data ApprovalPolicyChoice
    = ChoosePromptMutating
    | ChooseDenyMutating
    | ChooseApproveAll
    deriving (Eq, Show)

data ApprovalPolicyState = ApprovalPolicyState
    { approvalPolicyCurrent :: !ApprovalPolicy
    , approvalPolicyIndex :: !Int
    }
    deriving (Eq, Show)

approvalPolicyChoices :: [ApprovalPolicyChoice]
approvalPolicyChoices =
    [ ChoosePromptMutating
    , ChooseDenyMutating
    , ChooseApproveAll
    ]

approvalPolicyOptions :: [(ApprovalPolicy, Text, Text)]
approvalPolicyOptions =
    [ ( PromptMutating
      , "Ask before changes"
      , "Prompt before mutating tools"
      )
    , ( DenyMutating
      , "Read-only for this session"
      , "Block all mutating tools"
      )
    , ( ApproveAll
      , "Full access for this project"
      , "Automatically approve mutating tools"
      )
    ]

choicePolicy :: ApprovalPolicyChoice -> ApprovalPolicy
choicePolicy = \case
    ChoosePromptMutating -> PromptMutating
    ChooseDenyMutating -> DenyMutating
    ChooseApproveAll -> ApproveAll

initialApprovalPolicyState :: ApprovalPolicy -> ApprovalPolicyState
initialApprovalPolicyState policy =
    ApprovalPolicyState
        { approvalPolicyCurrent = policy
        , approvalPolicyIndex =
            case policy of
                PromptMutating -> 0
                DenyMutating -> 1
                ApproveAll -> 2
        }

applyApprovalPolicyKey
    :: PickerKey
    -> ApprovalPolicyState
    -> Either ApprovalPolicyDecision ApprovalPolicyState
applyApprovalPolicyKey key state = case key of
    PickerKeyCancel -> Left ApprovalPolicyCancelled
    PickerKeyConfirm ->
        Left
            (ApprovalPolicySelected
                (choicePolicy
                    (approvalPolicyChoices !! state.approvalPolicyIndex)))
    PickerKeyUp -> Right (move (-1) state)
    PickerKeyDown -> Right (move 1 state)
    PickerKeyLeft -> Right state
    PickerKeyRight -> Right state
    PickerKeyChar c -> case Text.toLower (Text.singleton c) of
        "p" -> Left (ApprovalPolicySelected PromptMutating)
        "r" -> Left (ApprovalPolicySelected DenyMutating)
        "f" -> Left (ApprovalPolicySelected ApproveAll)
        _ -> Right state
    PickerKeyBackspace -> Right state
    PickerKeyTab -> Right state
    PickerKeyBackTab -> Right state
  where
    move delta current =
        current
            { approvalPolicyIndex =
                (current.approvalPolicyIndex + delta)
                    `mod` length approvalPolicyChoices
            }

renderApprovalPolicyFrame :: Bool -> ApprovalPolicyState -> Text
renderApprovalPolicyFrame color state =
    let labels =
            [ (label, detail)
            | (_, label, detail) <- approvalPolicyOptions
            ]
        rows = zipWith
            (\i (label, detail) ->
                renderRow color (i == state.approvalPolicyIndex)
                    (label <> " — " <> roleMuted color detail))
            [0 ..] labels
    in Text.intercalate "\n"
        (roleWarn color (glyphWarn <> "Permissions") : rows
            <> [roleMuted color "↑↓/jk · enter/click · p ask · r read-only · f full access · esc cancel"])

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
    PickerKeyLeft -> Right state
    PickerKeyRight -> Right state
    PickerKeyChar 'A' -> Left PermissionAllowAll
    PickerKeyChar c ->
        case Text.toLower (Text.singleton c) of
            "y" -> Left PermissionAllowOnce
            "a" -> Left PermissionAllowTool
            "n" -> Left PermissionDeny
            _ -> Right state
    PickerKeyBackspace -> Right state
    PickerKeyTab -> Right state
    PickerKeyBackTab -> Right state
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
    let summary = approvalToolCallPromptRelative workspace call
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

-- | Approval UI for effects which can never be approved beyond the current
-- invocation. It intentionally offers no project-wide or per-tool choice.
promptPermissionOnce :: Bool -> Text -> ToolCall -> IO (Maybe PermissionChoice)
promptPermissionOnce color workspace call = do
    isTty <- hIsTerminalDevice stdin
    let summary = approvalToolCallPromptOnceRelative workspace call
        shellEscalation =
            canonicalToolName call.name == "write_stdin"
                || isJust (shellEscalationApprovalPrompt workspace call)
        cardSummary
            | shellEscalation = "Approve this invocation outside session filesystem isolation?"
            | otherwise = summary
    if not isTty
        then pure (Just PermissionDeny)
        else do
            notifyAttention stderr PermissionRequested
            -- Keep complete, potentially wrapped details in scrollback.
            -- Overlay redraw counts logical lines, so placing an arbitrarily
            -- long command inside it would corrupt its cursor accounting.
            when shellEscalation $
                Text.hPutStrLn stderr (roleWarn color summary)
            fmap (Just . fromMaybe PermissionDeny)
                (runOverlay (renderPermissionOnceFrame color cardSummary)
                    applyPermissionOnceKey 0)

renderPermissionOnceFrame :: Bool -> Text -> Int -> Text
renderPermissionOnceFrame color summary selected =
    Text.intercalate "\n"
        (roleWarn color (glyphWarn <> summary)
            : zipWith
                (\index label -> renderRow color (index == selected) label)
                [0 ..] ["Allow once", "Deny"]
            <> [roleMuted color
                "Once only · ↑↓/jk · enter/click · y allow · n/esc deny"])

applyPermissionOnceKey :: PickerKey -> Int -> Either PermissionChoice Int
applyPermissionOnceKey key selected = case key of
    PickerKeyCancel -> Left PermissionDeny
    PickerKeyConfirm ->
        Left (if selected == 0 then PermissionAllowOnce else PermissionDeny)
    PickerKeyUp -> Right ((selected - 1) `mod` 2)
    PickerKeyDown -> Right ((selected + 1) `mod` 2)
    PickerKeyChar character
        | Text.toLower (Text.singleton character) == "y" ->
            Left PermissionAllowOnce
        | Text.toLower (Text.singleton character) == "n" ->
            Left PermissionDeny
    _ -> Right selected

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

approvalToolCallPrompt :: ToolCall -> Text
approvalToolCallPrompt = approvalToolCallPromptRelative ""

approvalToolCallPromptRelative :: Text -> ToolCall -> Text
approvalToolCallPromptRelative workspace call =
    fromMaybe
        (permissionToolCallPromptRelative workspace call)
        (shellEscalationApprovalPrompt workspace call <|> computerApprovalPrompt call)

-- | Retained escalated processes require a new confirmation before receiving
-- additional input. The session identifier and complete input must remain
-- visible: stdin can submit further commands to an interactive shell.
approvalToolCallPromptOnceRelative :: Text -> ToolCall -> Text
approvalToolCallPromptOnceRelative workspace call
    | canonicalToolName call.name == "write_stdin" =
        Text.intercalate "\n"
            [ "Send this input to a process outside session filesystem isolation?"
            , "WARNING: this input may execute additional commands outside the session sandbox."
            , "Complete arguments (quoted JSON): " <> Text.pack (show call.arguments)
            , "Approval applies to this input only."
            ]
    | otherwise = approvalToolCallPromptRelative workspace call

-- | This presentation does not authorize execution. The tool's approval rule
-- and host-issued execution authorization enforce the isolation boundary.
-- Show complete quoted fields rather than the ordinary truncated summary;
-- escaping also keeps terminal control characters from rewriting the warning.
shellEscalationApprovalPrompt :: Text -> ToolCall -> Maybe Text
shellEscalationApprovalPrompt workspace call = do
    guard (canonicalToolName call.name `elem`
        ["shell_command", "run_command", "run_terminal_cmd"])
    Object fields <- decodeStrict' (Text.encodeUtf8 call.arguments)
    guard (KeyMap.lookup "sandbox_permissions" fields == Just (String "require_escalated"))
    let field key = case KeyMap.lookup key fields of
            Just (String value) -> Just value
            _ -> Nothing
        quoted = Text.pack . show
        workingDirectory = case if canonicalToolName call.name == "run_terminal_cmd" then Nothing else field "workdir" of
            Just requested
                | FilePath.isRelative (Text.unpack requested) ->
                    Text.pack (Text.unpack workspace FilePath.</> Text.unpack requested)
                | otherwise -> requested
            Nothing -> workspace
    pure $ Text.intercalate "\n"
        [ "Run this command outside session filesystem isolation?"
        , "WARNING: this command and its descendants can access files normally hidden by the session sandbox."
        , "$TMPDIR remains session-specific, but does not enforce isolation."
        , "Command: " <> quoted (fromMaybe "(missing command)" (field "command"))
        , "Working directory: " <> quoted workingDirectory
        , "Justification: " <> quoted (fromMaybe "(missing justification)" (field "justification"))
        , "Approval applies to this invocation only."
        ]
