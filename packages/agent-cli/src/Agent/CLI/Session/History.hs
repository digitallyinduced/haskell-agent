-- | Restore and reset persisted conversation history.
module Agent.CLI.Session.History
    ( detectGitBranch
    , foldSessionItems
    , hydrateUiHistory
    , resetLiveConversation
    ) where

import Agent.CLI.Session (SessionTurn(..))
import Agent.Loop (ImageAttachment)
import Agent.OpenAI.Compaction
    ( hasCompactionCheckpoint
    , isTranscriptResetTurn
    )
import Agent.Responses.Types (ResponseItem)
import Agent.Tools.PlanMode
    ( PlanModeEnv
    , deactivatePlanMode
    )
import Agent.TUI.Model
    ( UiEvent(..)
    , UiState
    , initialUiState
    , reduceUi
    )
import Agent.OsPath (unsafeToFilePath)
import Control.Exception.Safe
    ( SomeException
    , try
    )
import Data.IORef
    ( IORef
    , writeIORef
    )
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode(..))
import System.OsPath (OsPath)
import System.Process (readProcessWithExitCode)

-- | Drop live conversation state without touching persisted session files.
resetLiveConversation
    :: IORef (Maybe Text)
    -> IORef [ResponseItem]
    -> IORef [ImageAttachment]
    -> PlanModeEnv
    -> IO ()
resetLiveConversation previous transcriptRef attachmentsRef planMode = do
    writeIORef previous Nothing
    writeIORef transcriptRef []
    writeIORef attachmentsRef []
    deactivatePlanMode planMode

detectGitBranch :: OsPath -> IO Text
detectGitBranch cwd = do
    result <-
        (try $
            readProcessWithExitCode
                "git"
                ["-C", unsafeToFilePath cwd, "rev-parse", "--abbrev-ref", "HEAD"]
                "")
            :: IO (Either SomeException (ExitCode, String, String))
    pure $ case result of
        Right (ExitSuccess, output, _) ->
            let branch = Text.strip (Text.pack output)
            in if Text.null branch
                then ""
                else if branch == "HEAD" then "detached" else branch
        _ -> ""

-- | Apply compact turns as full transcript replacements when resuming.
foldSessionItems :: [SessionTurn] -> [ResponseItem]
foldSessionItems =
    concat . reverse . foldl' addTurn []
  where
    addTurn chunks turn
        | isTranscriptResetTurn turn.turnUserText =
            -- /clear and /new store an empty snapshot; /compact stores the
            -- rebuilt history. Either way, turnItems replaces prior history.
            [turn.turnItems]
        | hasCompactionCheckpoint turn.turnItems =
            [turn.turnItems]
        | otherwise =
            turn.turnItems : chunks

hydrateUiHistory :: [SessionTurn] -> UiState
hydrateUiHistory = foldl' addTurn initialUiState
  where
    addTurn state turn
        | isTranscriptResetTurn turn.turnUserText =
            addResetTurn state turn
        | otherwise =
            addRegularTurn state turn

    addResetTurn state turn =
        let cleared = reduceUi UiConversationCleared state
        in case turn.turnAssistantText of
            Nothing -> cleared
            Just text -> reduceUi (UiHistory text) cleared

    addRegularTurn state turn =
        let withUser =
                if Text.null (Text.strip turn.turnUserText)
                    then state
                    else reduceUi
                        (UiUserSubmitted turn.turnUserText)
                        state
            withAssistant = case turn.turnAssistantText of
                Nothing -> withUser
                Just text ->
                    reduceUi (UiAssistantHistory text) withUser
        in case turn.turnError of
            Nothing -> withAssistant
            Just err -> reduceUi (UiErrorMessage err) withAssistant
