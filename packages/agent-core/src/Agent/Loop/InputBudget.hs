-- | Allocation-free logical byte accounting for retained input queues.
module Agent.Loop.InputBudget
    ( foldBytes
    , logicalImageBytes
    , logicalTextBytes
    , logicalTurnInputBytes
    , saturatingAdd
    ) where

import Agent.InterAgentMessage
    ( InterAgentMessage(..)
    , interAgentMessagePayload
    )
import Agent.Loop.Input
    ( FileAttachment(..)
    , ImageAttachment(..)
    , TurnAttachment(..)
    , TurnInput(..)
    )
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolResultImage(..)
    , ToolResultFile(..)
    , toolCallResultImages
    , toolCallResultFiles
    )
import qualified Data.ByteString as ByteString
import Data.Char (ord)
import qualified Data.List.NonEmpty as NonEmpty
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

logicalToolResultImageBytes :: ToolResultImage -> Int
logicalToolResultImageBytes image =
    logicalTextBytes image.imageUrl
        `saturatingAdd` maybe 0 logicalTextBytes image.imageDetail

logicalTurnInputBytes :: TurnInput -> Int
logicalTurnInputBytes = \case
    UserMessage text -> logicalTextBytes text
    AgentMessage message ->
        logicalTextBytes message.messageAuthor
            `saturatingAdd` logicalTextBytes message.messageRecipient
            `saturatingAdd` logicalTextBytes (interAgentMessagePayload message)
    UserMessageWithAttachments text attachments ->
        logicalTextBytes text
            `saturatingAdd` foldBytes logicalAttachmentBytes
                (NonEmpty.toList attachments)
    CompletedTool result ->
        logicalTextBytes result.callId
            `saturatingAdd` logicalTextBytes result.output
            `saturatingAdd` foldBytes
                logicalToolResultImageBytes
                (toolCallResultImages result)
            `saturatingAdd` foldBytes
                (\file ->
                    logicalTextBytes file.fileName
                        `saturatingAdd` logicalTextBytes file.fileMimeType
                        `saturatingAdd` ByteString.length file.fileData)
                (toolCallResultFiles result)

foldBytes :: (a -> Int) -> [a] -> Int
foldBytes measure =
    foldr (\value total -> measure value `saturatingAdd` total) 0

logicalAttachmentBytes :: TurnAttachment -> Int
logicalAttachmentBytes = \case
    ImageAttachmentItem image -> logicalImageBytes image
    FileAttachmentItem file -> logicalFileBytes file

saturatingAdd :: Int -> Int -> Int
saturatingAdd left right
    | right <= 0 = left
    | left > maxBound - right = maxBound
    | otherwise = left + right
