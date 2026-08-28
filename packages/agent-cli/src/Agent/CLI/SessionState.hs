-- | Provider-independent mutable state that survives backend replacement.
module Agent.CLI.SessionState
    ( SessionState(..)
    , newSessionState
    , removeImageAttachmentAt
    ) where

import Agent.CLI.Session.ConversationStore (newConversationStore)
import Agent.CLI.Session.History (LiveConversation)
import Agent.Loop (ImageAttachment)
import Data.IORef (IORef, newIORef)
import Data.Text (Text)

-- | State owned by the user session rather than by a provider connection.
--
-- Provider and dialect switches may close and recreate transport resources,
-- but they must keep this state object alive.
data SessionState = SessionState
    { sessionDraft :: !(IORef Text)
    , sessionConversation :: !(IORef LiveConversation)
    , sessionPreviewId :: !(IORef Int)
    }

newSessionState :: IO SessionState
newSessionState = do
    draft <- newIORef ""
    conversation <- newIORef =<< newConversationStore Nothing [] []
    previewId <- newIORef 1
    pure SessionState
        { sessionDraft = draft
        , sessionConversation = conversation
        , sessionPreviewId = previewId
        }

-- | Remove one pending attachment by its stable zero-based composer index.
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
