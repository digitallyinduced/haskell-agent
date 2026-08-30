-- | Data types shared by slash-command parsing and presentation.
module Agent.CLI.Command.Types
    ( CopyRequest(..)
    , ForkRequest(..)
    , ReplAction(..)
    , ShellMode(..)
    , SkillCommand(..)
    , SlashCatalog(..)
    , SlashCommand(..)
    , SlashMenu(..)
    , SlashSuggestion(..)
    ) where

import Agent.Dialect (DialectId)
import Agent.ReasoningEffort (ReasoningEffort)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)

-- | Parsed @/fork@ options. A missing worktree choice asks interactively.
data ForkRequest = ForkRequest
    { forkWorktree :: !(Maybe Bool)
    , forkDirective :: !(Maybe Text)
    }
    deriving (Eq, Show)

-- | Parsed @/copy@ selection. Responses are numbered newest-first.
data CopyRequest = CopyRequest
    { copyResponseIndex :: !Int
    , copyDestination :: !(Maybe Text)
    }
    deriving (Eq, Show)

data ReplAction
    = ReplQuit
    | ReplReload
    | ReplPrompt Text
    | ReplExpandedPrompt !Text !Text
      -- ^ Original user-visible text and the model-visible expansion.
    | ReplInit
    | ReplReview (Maybe Text)
    | ReplDiff
    | ReplFork !ForkRequest
    | ReplExport (Maybe Text)
    | ReplPermissions
    | ReplShowEffort
    | ReplSetEffort ReasoningEffort
    | ReplToggleFast
    | ReplShowModel
    | ReplSetModel Text
    | ReplEnableCodeMode
    | ReplToggleAlwaysApprove
    | ReplPlan (Maybe Text)
    -- ^ Enter plan mode. @Just@ starts a turn with that description.
    | ReplViewPlan
    | ReplQueue
    | ReplTranscript
    | ReplEditPrompt
    | ReplContext
    | ReplHistory
    | ReplFind !(Maybe Text)
    | ReplBtw Text
    -- ^ Ask an isolated one-shot question over the current context.
    | ReplMetaConsole Text
    -- ^ Interpret a natural-language harness configuration request without
    -- adding it to the coding conversation.
    | ReplRecap
    -- ^ Generate a display-only "where was I" recap of the current session.
    | ReplRetry
    -- ^ Retry the last failed turn with its original attachments.
    | ReplShowSession
    | ReplShowSessionInfo
    | ReplDesktop
    -- ^ Open the current persisted conversation in the native macOS app.
    | ReplAfk (Maybe Text)
    -- ^ Hand the active session to tmux, optionally on @host:path@.
    | ReplWorktree
    | ReplRename Text
    | ReplRenameAuto
    | ReplLogin
    | ReplReloadAuth
    | ReplPaste !Bool !Text
    | ReplClearAttachments
    | ReplShowAttachments
    | ReplRemoveAttachment !Int
    | ReplCopy !CopyRequest
    | ReplCopyCode Int
    | ReplCopyDiff
    | ReplCopyPath
    | ReplCopySession
    | ReplShowTerminal
    | ReplAgents
    | ReplShowAgentLimit
    | ReplSetAgentLimit Int
    | ReplMcp
    | ReplMcpPrompt Text Text [(Text, Text)]
    | ReplGoalStatus
    | ReplGoalPause
    | ReplGoalResume
    | ReplGoalClear
    | ReplGoalSet !Text !Text !(Maybe Int) !Text
      -- ^ Original text, objective, optional token budget, and expansion.
    | ReplWorkflowRuns
    | ReplWorkflowManage !Text !(Maybe Text)
      -- ^ Operation and optional run id/display name.
    | ReplSkills !Bool
    | ReplShowShell
    | ReplSetShell !ShellMode
    | ReplInvokeSkill !Text !Text
    | ReplHelp (Maybe Text)
    -- ^ @Nothing@ lists every command; @Just@ is a canonical name without @/@.
    | ReplResume (Maybe Text)
    -- ^ @Nothing@ opens the session picker; @Just@ is a session id.
    | ReplHome
    -- ^ Return to the session picker.
    | ReplSearch !Text
    -- ^ Search persisted conversation turns and open matching sessions.
    | ReplCompact (Maybe Text)
    -- ^ Optional focus note for what to keep while compacting history.
    | ReplRewind
      -- ^ Restore conversation state before a selected prompt.
    | ReplClear
      -- ^ Soft-reset live transcript; keep the same session id.
    | ReplNew
      -- ^ Start a fresh persisted session id with empty history.
    | ReplDelete
      -- ^ Confirm deletion, then remove the current session after shutdown.
    | ReplUsage
    | ReplCommandError Text
    deriving (Eq, Show)

data ShellMode
    = ShellGhci
    | ShellBash
    | ShellBoth
    | ShellNone
    deriving (Eq, Show)

data SkillCommand = SkillCommand
    { skillCommandName :: !Text
    , skillCommandSummary :: !Text
    , skillCommandArgumentHint :: !(Maybe Text)
    , skillCommandSource :: !Text
    }
    deriving (Eq, Show)

-- | One REPL slash command. @slashName@ is the canonical name without a
-- leading @/@; aliases are also stored without @/@.
data SlashCommand = SlashCommand
    { slashName :: !Text
    , slashAliases :: ![Text]
    , slashUsage :: !Text
    , slashSummary :: !Text
    , slashTakesArguments :: !Bool
    , slashDialects :: !(Maybe [DialectId])
    , slashRequiredTools :: ![Text]
    }
    deriving (Eq, Show)

-- | Session-scoped slash-command capabilities and presentation data.
--
-- Tool-backed commands are filtered when the catalog is built, so the same
-- catalog must be used for parsing, help, and both completion UIs.
data SlashCatalog = SlashCatalog
    { slashCatalogDialect :: !DialectId
    , slashCatalogToolNames :: !(Set Text)
    , slashCatalogCommands :: ![SlashCommand]
    , slashCatalogCommandByName :: !(Map Text SlashCommand)
    , slashCatalogSkills :: ![SkillCommand]
    , slashCatalogSkillByName :: !(Map Text SkillCommand)
    , slashCatalogModelIds :: ![Text]
    }
    deriving (Eq, Show)

-- | One row in the live slash-command dropdown.
data SlashSuggestion = SlashSuggestion
    { slashSuggestionDisplay :: !Text
    , slashSuggestionReplacement :: !Text
    , slashSuggestionSummary :: !Text
    , slashSuggestionTakesArguments :: !Bool
    , slashSuggestionMatchPositions :: ![Int]
    }
    deriving (Eq, Show)

-- | Current live slash menu and the character range replaced on acceptance.
data SlashMenu = SlashMenu
    { slashMenuReplaceStart :: !Int
    , slashMenuReplaceEnd :: !Int
    , slashMenuSuggestions :: ![SlashSuggestion]
    }
    deriving (Eq, Show)
