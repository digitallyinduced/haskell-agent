-- | Restore and reset persisted conversation history.
module Agent.CLI.Session.History
    ( detectGitBranch
    , foldSessionItems
    , hydrateUiHistory
    , LiveConversation(..)
    , readLiveAttachments
    , readLivePreviousResponseId
    , readLiveTranscript
    , modifyLiveAttachments
    , resetLiveConversationState
    , resetLiveConversation
    , resetLiveConversationWith
    , writeLivePreviousResponseId
    , writeLiveTranscript
    ) where

import Agent.CLI.Session
    ( SessionTurn(..)
    , TranscriptEffect(..)
    )
import Agent.Loop (ImageAttachment)
import Agent.OpenAI.Compaction
    ( isCompactSessionTurn
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
    , atomicModifyIORef'
    , readIORef
    )
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode(..))
import System.OsPath (OsPath)
import System.Process (readProcessWithExitCode)

-- | The pieces of conversation state that must change together when a live
-- session is reset.
--
-- Keeping them in one value makes resets and multi-field updates atomic and
-- prevents callers from observing mismatched response IDs, transcripts, and
-- attachments.
data LiveConversation = LiveConversation
    { livePreviousResponseId :: !(Maybe Text)
    , liveTranscript :: ![ResponseItem]
    , liveAttachments :: ![ImageAttachment]
    } deriving (Eq, Show)

-- | Pure reset transition for live conversation state.
resetLiveConversationState :: LiveConversation -> LiveConversation
resetLiveConversationState state =
    state
        { livePreviousResponseId = Nothing
        , liveTranscript = []
        , liveAttachments = []
        }

readLivePreviousResponseId :: IORef LiveConversation -> IO (Maybe Text)
readLivePreviousResponseId = fmap (\state -> state.livePreviousResponseId) . readIORef

readLiveTranscript :: IORef LiveConversation -> IO [ResponseItem]
readLiveTranscript = fmap (\state -> state.liveTranscript) . readIORef

readLiveAttachments :: IORef LiveConversation -> IO [ImageAttachment]
readLiveAttachments = fmap (\state -> state.liveAttachments) . readIORef

modifyLiveAttachments
    :: IORef LiveConversation
    -> ([ImageAttachment] -> ([ImageAttachment], a))
    -> IO a
modifyLiveAttachments ref update =
    atomicModifyIORef' ref \state ->
        let (attachments, result) = update state.liveAttachments
        in (state { liveAttachments = attachments }, result)

writeLivePreviousResponseId
    :: IORef LiveConversation
    -> Maybe Text
    -> IO ()
writeLivePreviousResponseId ref value =
    atomicModifyIORef' ref \state ->
        (state { livePreviousResponseId = value }, ())

writeLiveTranscript
    :: IORef LiveConversation
    -> [ResponseItem]
    -> IO ()
writeLiveTranscript ref value =
    atomicModifyIORef' ref \state ->
        (state { liveTranscript = value }, ())

-- | Drop live conversation state without touching persisted session files.
resetLiveConversation
    :: IORef LiveConversation
    -> PlanModeEnv
    -> IO ()
resetLiveConversation =
    resetLiveConversationWith (pure ())

resetLiveConversationWith
    :: IO ()
    -> IORef LiveConversation
    -> PlanModeEnv
    -> IO ()
resetLiveConversationWith resetBackend conversationRef planMode = do
    resetBackend
    atomicModifyIORef' conversationRef \state ->
        (resetLiveConversationState state, ())
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
    addTurn chunks turn = case turn.turnEffect of
        TranscriptAppend -> turn.turnItems : chunks
        TranscriptReplace -> [turn.turnItems]
        TranscriptReset -> [turn.turnItems]

hydrateUiHistory :: [SessionTurn] -> UiState
hydrateUiHistory = foldl' addTurn initialUiState
  where
    addTurn state turn
        | isCompactSessionTurn turn.turnUserText =
            addCompactTurn state turn
        | isTranscriptResetTurn turn.turnUserText =
            addResetTurn state turn
        | otherwise =
            addRegularTurn state turn

    -- Compaction replaces the model's inference context, not the transcript
    -- presented to the user. Keep earlier blocks scrollable and append the
    -- compaction summary as the live UI does.
    addCompactTurn state turn =
        case turn.turnAssistantText of
            Nothing -> state
            Just text -> reduceUi (UiSystemMessage text) state

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
