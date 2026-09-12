-- | Provider-independent mutable state that survives backend replacement.
module Agent.CLI.SessionState
    ( SessionState(..)
    , newSessionState
    , removeImageAttachmentAt
    , removeImageAttachment
    ) where

import Agent.CLI.Session.ConversationStore (newConversationStore)
import Agent.Runtime.Session.History (LiveConversation)
import Agent.Loop (ImageAttachment)
import Data.IORef (IORef, newIORef)
import Data.Text (Text)

-- | State owned by the user session rather than by a provider connection.
--
-- Provider and dialect switches may close and recreate transport resources,
-- but they must keep this state object alive.
data SessionState = SessionState
    { sessionDraft :: !(IORef Text)
    -- | A prompt submitted as the first interactive turn after rebuilding a
    -- session (for example a @/fork@ directive). Unlike 'CliOptions.optPrompt',
    -- this does not turn the invocation into one-shot mode.
    , sessionInitialPrompt :: !(IORef (Maybe Text))
    , sessionConversation :: !(IORef LiveConversation)
    , sessionPreviewId :: !(IORef Int)
    }

newSessionState :: IO SessionState
newSessionState = do
    draft <- newIORef ""
    initialPrompt <- newIORef Nothing
    conversation <- newIORef =<< newConversationStore Nothing [] []
    previewId <- newIORef 1
    pure SessionState
        { sessionDraft = draft
        , sessionInitialPrompt = initialPrompt
        , sessionConversation = conversation
        , sessionPreviewId = previewId
        }

-- | Remove the captured image, independent of any difference between the
-- composer index and session attachments retained by earlier slash commands.
removeImageAttachment
    :: ImageAttachment
    -> [ImageAttachment]
    -> ([ImageAttachment], Bool)
removeImageAttachment image pending =
    case break (== image) pending of
        (before, _ : after) -> (before <> after, True)
        _ -> (pending, False)

-- | Remove one pending attachment by its zero-based composer index.
removeImageAttachmentAt
    :: Int
    -> [ImageAttachment]
    -> ([ImageAttachment], Bool)
removeImageAttachmentAt index pending
    | index < 0 = (pending, False)
    | otherwise =
        case splitAt index pending of
            (before, _ : after) -> (before <> after, True)
            _ -> (pending, False)
