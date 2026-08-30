-- | Dependency-light types shared by the inline editor and fullscreen TUI.
module Agent.CLI.Input.Types
    ( ReplLine(..)
    , ChoiceKey(..)
    , EditorState(..)
    , DisplayCell(..)
    , EditorKey(..)
    , KittyKey(..)
    , TimedRead(..)
    ) where

import Agent.CLI.Command (SlashCatalog)
import Agent.Loop (ImageAttachment)
import Data.Text (Text)

-- | Outcome of an interactive REPL read.
data ReplLine
    = ReplEof
    | ReplText Text
    -- | A private configuration request submitted by the fullscreen Meta
    -- Console. It is interpreted separately from the coding conversation.
    | ReplMeta Text
    | ReplPasted Text
    | ReplClipboardPaste !Text !(Maybe [ImageAttachment])
    -- | Classify a bracketed paste off the UI thread. The fields are the draft
    -- before the paste, the raw pasted payload, and the draft with that payload
    -- inserted. Image paths or clipboard images become attachments; otherwise
    -- the inserted draft is restored.
    | ReplClipboardPasteOrText !Text !Text !Text
    | ReplCycleMode Text
    | ReplChooseModel Text
    | ReplChooseEffort Text
    | ReplChooseAccount Text
    | ReplRemovePendingImage !Text !Int
    | ReplQuitInterrupt
    deriving (Eq, Show)

data ChoiceKey
    = ChoiceUp
    | ChoiceDown
    | ChoiceEnter
    | ChoiceCancel
    | ChoiceDigit Int
    deriving (Eq, Show)

data EditorState = EditorState
    { editorText :: !Text
    , editorCursor :: !Int
    , editorSelected :: !Int
    , editorHistoryIndex :: !(Maybe Int)
    , editorHistoryDraft :: !Text
    , editorKillBuffer :: !Text
    , editorPasted :: !Bool
    , editorSlashEnabled :: !Bool
    , editorSlashDismissed :: !Bool
    , editorSlashCatalog :: !SlashCatalog
    }
    deriving (Eq, Show)

data DisplayCell = DisplayCell
    { displayCellText :: !Text
    , displayCellWidth :: !Int
    , displayCellSourceLength :: !Int
    }

data EditorKey
    = EditorChar !Char
    | EditorEnter
    | EditorBackspace
    | EditorDelete
    | EditorLeft
    | EditorRight
    | EditorHome
    | EditorEnd
    | EditorUp
    | EditorDown
    | EditorTab
    | EditorEscape
    | EditorInterrupt
    | EditorEof
    | EditorKillStart
    | EditorKillEnd
    | EditorKillWord
    | EditorYank
    | EditorClearScreen
    | EditorDictate
    | EditorCycleMode
    | EditorClipboardPaste !(Maybe [ImageAttachment])
    | EditorPaste !Text
    | EditorInputError !Text
    | EditorIgnore
    deriving (Eq, Show)

data KittyKey = KittyKey
    { kittyCodepoint :: !Int
    , kittyModifiers :: !Int
    , kittyEvent :: !Int
    }

data TimedRead
    = TimedChar !Char
    | TimedOut
    | TimedEof
