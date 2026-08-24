-- | Buffered input submitted through the fullscreen composer.
module Agent.CLI.TUI.Composer.Buffer
    ( appendFullscreenInput
    , newFullscreenInputBuffer
    , promoteFullscreenInput
    , queuedFullscreenInputDisplays
    , readFullscreenInputs
    , takeFullscreenInput
    , takeFullscreenInputOr
    ) where

import Agent.CLI.Input (ReplLine(..))
import Agent.CLI.TUI.Types
    ( FullscreenInput(..)
    , FullscreenInputBuffer(..)
    )
import Control.Concurrent.STM
    ( STM
    , atomically
    , newTVarIO
    , orElse
    , readTVar
    , retry
    , writeTVar
    )
import Data.Sequence (Seq, ViewL(..), ViewR(..))
import qualified Data.Sequence as Seq
import Data.Text (Text)

newFullscreenInputBuffer :: IO FullscreenInputBuffer
newFullscreenInputBuffer =
    FullscreenInputBuffer <$> newTVarIO Seq.empty

queuedFullscreenInputDisplays
    :: FullscreenInputBuffer
    -> IO (Seq Text)
queuedFullscreenInputDisplays inputBuffer =
    atomically do
        queued <- readFullscreenInputs inputBuffer
        pure $ foldMap
            (\input ->
                if input.fullscreenInputQueued
                    then maybe Seq.empty Seq.singleton input.fullscreenInputDisplay
                    else Seq.empty)
            queued

readFullscreenInputs
    :: FullscreenInputBuffer
    -> STM (Seq FullscreenInput)
readFullscreenInputs (FullscreenInputBuffer inputs) =
    readTVar inputs

appendFullscreenInput
    :: FullscreenInputBuffer
    -> FullscreenInput
    -> STM ()
appendFullscreenInput (FullscreenInputBuffer inputs) input = do
    queued <- readTVar inputs
    writeTVar inputs (queued Seq.|> input)

-- | Put an interruptive prompt ahead of already queued prompts. Clipboard
-- actions entered after the last submitted prompt belong to the current draft,
-- so keep that trailing prelude immediately before the promoted prompt.
promoteFullscreenInput
    :: FullscreenInputBuffer
    -> FullscreenInput
    -> STM ()
promoteFullscreenInput (FullscreenInputBuffer inputs) input = do
    queued <- readTVar inputs
    let (remaining, prelude) = splitTrailingPromptPrelude queued
    writeTVar inputs $
        prelude Seq.>< Seq.singleton input Seq.>< remaining

splitTrailingPromptPrelude
    :: Seq FullscreenInput
    -> (Seq FullscreenInput, Seq FullscreenInput)
splitTrailingPromptPrelude = go Seq.empty
  where
    go prelude queued =
        case Seq.viewr queued of
            remaining :> input
                | isPromptPrelude input ->
                    go (input Seq.<| prelude) remaining
            _ -> (queued, prelude)

isPromptPrelude :: FullscreenInput -> Bool
isPromptPrelude input =
    case input.fullscreenInputLine of
        ReplClipboardPaste _ _ -> True
        ReplClipboardPasteOrText _ _ _ -> True
        _ -> False

takeFullscreenInput
    :: FullscreenInputBuffer
    -> STM FullscreenInput
takeFullscreenInput (FullscreenInputBuffer inputs) = do
    queued <- readTVar inputs
    case Seq.viewl queued of
        EmptyL -> retry
        input :< rest -> do
            writeTVar inputs rest
            pure input

-- | Prefer a prompt that has already been queued over a simultaneous
-- session-level wakeup, so provider restarts cannot consume and lose Enter.
takeFullscreenInputOr
    :: FullscreenInputBuffer
    -> STM wake
    -> STM (Either wake FullscreenInput)
takeFullscreenInputOr inputBuffer wake =
    (Right <$> takeFullscreenInput inputBuffer)
        `orElse` (Left <$> wake)
