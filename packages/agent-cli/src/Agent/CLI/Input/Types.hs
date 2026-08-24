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

import Agent.CLI.Command (SkillCommand)
import Agent.Loop (ImageAttachment)
import Data.Text (Text)

-- | Outcome of an interactive REPL read.
data ReplLine
    = ReplEof
    | ReplText Text
    | ReplPasted Text
    | ReplClipboardPaste !Text !(Maybe [ImageAttachment])
    | ReplClipboardPasteOrText !Text !Text
    | ReplCycleMode Text
    | ReplChooseModel Text
    | ReplChooseEffort Text
    | ReplChooseAccount Text
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
    , editorSkillCommands :: ![SkillCommand]
    , editorModelIds :: ![Text]
    }

data DisplayCell = DisplayCell
    { displayCellText :: !Text
    , displayCellWidth :: !Int
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
