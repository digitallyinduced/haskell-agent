-- | Provider-independent mutable state that survives backend replacement.
module Agent.CLI.SessionState
    ( SessionState(..)
    , newSessionState
    ) where

import Agent.Loop (ImageAttachment)
import Data.IORef (IORef, newIORef)
import Data.Text (Text)

-- | State owned by the user session rather than by a provider connection.
--
-- Provider and dialect switches may close and recreate transport resources,
-- but they must keep this state object alive.
data SessionState = SessionState
    { sessionDraft :: !(IORef Text)
    , sessionAttachments :: !(IORef [ImageAttachment])
    , sessionPreviewId :: !(IORef Int)
    }

newSessionState :: IO SessionState
newSessionState = do
    draft <- newIORef ""
    attachments <- newIORef []
    previewId <- newIORef 1
    pure SessionState
        { sessionDraft = draft
        , sessionAttachments = attachments
        , sessionPreviewId = previewId
        }
