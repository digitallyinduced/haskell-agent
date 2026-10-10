-- | Logical byte accounting for retained REPL input.
module Agent.CLI.InputBudget
    ( logicalReplLineBytes
    ) where

import Agent.CLI.Input.Types (ReplLine(..))
import Agent.Loop.InputBudget
    ( foldBytes
    , logicalImageBytes
    , logicalTextBytes
    , saturatingAdd
    )

logicalReplLineBytes :: ReplLine -> Int
logicalReplLineBytes = \case
    ReplEof -> 0
    ReplText text -> logicalTextBytes text
    ReplMeta text -> logicalTextBytes text
    ReplPasted text -> logicalTextBytes text
    ReplClipboardPaste draft images ->
        logicalTextBytes draft
            `saturatingAdd` maybe 0 (foldBytes logicalImageBytes) images
    ReplClipboardPasteCaptured images -> foldBytes logicalImageBytes images
    ReplRemoveCapturedImage image -> logicalImageBytes image
    ReplClipboardPasteOrText draft pasted inserted ->
        logicalTextBytes draft
            `saturatingAdd` logicalTextBytes pasted
            `saturatingAdd` logicalTextBytes inserted
    ReplCycleMode text -> logicalTextBytes text
    ReplChooseModel text -> logicalTextBytes text
    ReplChooseEffort text -> logicalTextBytes text
    ReplChooseAccount text -> logicalTextBytes text
    ReplRemovePendingImage text _ -> logicalTextBytes text
    ReplQuitInterrupt -> 0
