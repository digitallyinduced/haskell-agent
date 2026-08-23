-- | Coherent mutable state for the live CLI session.
--
-- The CLI owns one mutable cell containing this record. Transitions stay pure
-- so turns, compaction, skills, auth, and UI readers cannot observe partially
-- committed session state.
module Agent.CLI.SessionState
    ( SessionState(..)
    , SessionAccountState(..)
    , SessionAccountPatch(..)
    , addSessionTokenUsage
    , applySessionAccountPatch
    , applySessionConversationPatch
    , appendSessionStartupContext
    , beginSessionTurn
    , clearSessionPreviousResponseId
    , replaceSessionTranscript
    , resetSessionConversation
    , setSessionTranscript
    ) where

import Agent.CLI.TurnState
    ( ConversationPatch
    , ConversationState(..)
    , FieldUpdate(..)
    , applyConversationPatch
    )
import Agent.Loop (TokenUsage, addTokenUsage, emptyTokenUsage)
import Agent.Responses.Types (ResponseItem)
import Agent.Skills (SkillCatalog, SkillInvocation)
import Data.Maybe (isJust)
import Data.Text (Text)

data SessionState = SessionState
    { sessionConversation :: !ConversationState
    , sessionSkillCatalog :: !SkillCatalog
    , sessionSkillInvocations :: ![SkillInvocation]
    , sessionAccount :: !SessionAccountState
    } deriving (Eq, Show)

data SessionAccountState = SessionAccountState
    { accountLabel :: !Text
    , accountId :: !Text
    , accountSelectionId :: !Text
    } deriving (Eq, Show)

data SessionAccountPatch = SessionAccountPatch
    { patchAccountLabel :: !(FieldUpdate Text)
    , patchAccountId :: !(FieldUpdate Text)
    , patchAccountSelectionId :: !(FieldUpdate Text)
    } deriving (Eq, Show)

applySessionAccountPatch
    :: SessionAccountPatch
    -> SessionState
    -> SessionState
applySessionAccountPatch patch state =
    state
        { sessionAccount = account
            { accountLabel =
                applyField patch.patchAccountLabel account.accountLabel
            , accountId =
                applyField patch.patchAccountId account.accountId
            , accountSelectionId =
                applyField
                    patch.patchAccountSelectionId
                    account.accountSelectionId
            }
        }
  where
    account = state.sessionAccount
    applyField update current = case update of
        KeepField -> current
        SetField value -> value

-- | Snapshot a turn's starting conversation while consuming startup context.
beginSessionTurn :: SessionState -> (SessionState, ConversationState)
beginSessionTurn state =
    ( state
        { sessionConversation =
            conversation { conversationStartupContext = Nothing }
        }
    , conversation
    )
  where
    conversation = state.sessionConversation

clearSessionPreviousResponseId :: SessionState -> (SessionState, Bool)
clearSessionPreviousResponseId state =
    ( state
        { sessionConversation =
            conversation { conversationPreviousResponseId = Nothing }
        }
    , isJust conversation.conversationPreviousResponseId
    )
  where
    conversation = state.sessionConversation

applySessionConversationPatch
    :: ConversationPatch
    -> SessionState
    -> SessionState
applySessionConversationPatch patch state =
    state
        { sessionConversation =
            applyConversationPatch patch state.sessionConversation
        }

addSessionTokenUsage :: TokenUsage -> SessionState -> SessionState
addSessionTokenUsage usage state =
    state
        { sessionConversation =
            conversation
                { conversationUsage =
                    addTokenUsage conversation.conversationUsage usage
                }
        }
  where
    conversation = state.sessionConversation

appendSessionStartupContext :: Text -> SessionState -> SessionState
appendSessionStartupContext context state =
    state
        { sessionConversation =
            conversation
                { conversationStartupContext =
                    Just $ case conversation.conversationStartupContext of
                        Nothing -> context
                        Just existing -> existing <> "\n\n" <> context
                }
        }
  where
    conversation = state.sessionConversation

setSessionTranscript :: [ResponseItem] -> SessionState -> SessionState
setSessionTranscript items state =
    state
        { sessionConversation =
            state.sessionConversation
                { conversationTranscript = items
                }
        }

-- | Install compacted local history and detach it from any server-side chain.
replaceSessionTranscript :: [ResponseItem] -> SessionState -> SessionState
replaceSessionTranscript items state =
    state
        { sessionConversation =
            state.sessionConversation
                { conversationPreviousResponseId = Nothing
                , conversationTranscript = items
                }
        }

resetSessionConversation :: Maybe Text -> SessionState -> SessionState
resetSessionConversation startup state =
    state
        { sessionConversation = ConversationState
            { conversationPreviousResponseId = Nothing
            , conversationTranscript = []
            , conversationStartupContext = startup
            , conversationUsage = emptyTokenUsage
            , conversationLastAssistant = Nothing
            }
        }
