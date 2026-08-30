-- | Allocation-free logical byte accounting for retained input queues.
module Agent.CLI.InputBudget
    ( logicalImageBytes
    , logicalReplLineBytes
    , logicalTextBytes
    , logicalTurnInputBytes
    , saturatingAdd
    ) where

import Agent.CLI.Input.Types (ReplLine(..))
import Agent.InterAgentMessage
    ( InterAgentMessage(..)
    , interAgentMessagePayload
    )
import Agent.Loop
    ( FileAttachment(..)
    , ImageAttachment(..)
    , TurnInput(..)
    )
import Agent.ToolDispatch (ToolCallResult(..))
import qualified Data.ByteString as ByteString
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as Text

-- | UTF-8 payload bytes without constructing a second encoded copy.
logicalTextBytes :: Text -> Int
logicalTextBytes = Text.foldl' (\total char -> saturatingAdd total (charBytes char)) 0
  where
    charBytes char
        | codepoint <= 0x7f = 1
        | codepoint <= 0x7ff = 2
        | codepoint <= 0xffff = 3
        | otherwise = 4
      where
        codepoint = ord char

logicalImageBytes :: ImageAttachment -> Int
logicalImageBytes image =
    logicalTextBytes image.imageMime
        `saturatingAdd` ByteString.length image.imageBytes

logicalFileBytes :: FileAttachment -> Int
logicalFileBytes file =
    maybe 0 logicalTextBytes file.fileName
        `saturatingAdd` logicalTextBytes file.fileMime
        `saturatingAdd` ByteString.length file.fileBytes

logicalTurnInputBytes :: TurnInput -> Int
logicalTurnInputBytes = \case
    UserMessage text -> logicalTextBytes text
    AgentMessage message ->
        logicalTextBytes message.messageAuthor
            `saturatingAdd` logicalTextBytes message.messageRecipient
            `saturatingAdd` logicalTextBytes (interAgentMessagePayload message)
    UserMultimodal text images ->
        logicalTextBytes text
            `saturatingAdd` foldBytes logicalImageBytes images
    UserMultimodalFiles text images files ->
        logicalTextBytes text
            `saturatingAdd` foldBytes logicalImageBytes images
            `saturatingAdd` foldBytes logicalFileBytes files
    CompletedTool result ->
        logicalTextBytes result.callId
            `saturatingAdd` logicalTextBytes result.output

logicalReplLineBytes :: ReplLine -> Int
logicalReplLineBytes = \case
    ReplEof -> 0
    ReplText text -> logicalTextBytes text
    ReplMeta text -> logicalTextBytes text
    ReplPasted text -> logicalTextBytes text
    ReplClipboardPaste draft images ->
        logicalTextBytes draft
            `saturatingAdd` maybe 0 (foldBytes logicalImageBytes) images
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

foldBytes :: (a -> Int) -> [a] -> Int
foldBytes measure =
    foldr (\value total -> measure value `saturatingAdd` total) 0

saturatingAdd :: Int -> Int -> Int
saturatingAdd left right
    | right <= 0 = left
    | left > maxBound - right = maxBound
    | otherwise = left + right
