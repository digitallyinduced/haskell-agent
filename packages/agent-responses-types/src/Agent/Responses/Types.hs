-- | Lossless wire types for the OpenAI Responses API.
module Agent.Responses.Types
    ( -- * Create request
      ResponseCreateParams(..)
    , defaultResponseCreateParams
    , responseCreateParamsDecoder
    , ResponseInput(..)
    , ResponseInclude(..)
    , ContextManagement(..)
    , Conversation(..)
    , Prompt(..)
    , PromptCacheOptions(..)
    , ReasoningConfig(..)
    , ResponseTextConfig(..)
    , ResponseFormat(..)
    , StreamOptions(..)
    , ToolChoice(..)
    , ToolChoiceMode(..)

      -- * Items and content
    , ResponseItem(..)
    , ResponseItemType(..)
    , parseResponseItemType
    , responseItemTypeText
    , ResponseMessage(..)
    , ResponseAgentMessage(..)
    , InternalChatMetadata(..)
    , AdditionalToolsItem(..)
    , LocalShellCall(..)
    , LocalShellAction(..)
    , ToolSearchCall(..)
    , ToolSearchOutput(..)
    , WebSearchCall(..)
    , WebSearchAction(..)
    , ImageGenerationCall(..)
    , CompactionItem(..)
    , CompactionTriggerItem(..)
    , ContextCompactionItem(..)
    , ResponseRole(..)
    , MessageContent(..)
    , ResponseContentPart(..)
    , ItemStatus(..)
    , FunctionCall(..)
    , FunctionCallOutput(..)
    , CustomToolCall(..)
    , CustomToolCallOutput(..)
    , ReasoningItem(..)
    , ReasoningSummaryPart(..)
    , ItemReference(..)
    , TaggedObject(..)
    , aesonValueDecoder

      -- * Tools
    , ResponseTool(..)
    , ResponseToolType(..)
    , responseToolTypeText
    , knownResponseTool
    , FunctionTool(..)

      -- * Response
    , Response(..)
    , responseDecoder
    , ResponseStatus(..)
    , ResponseError(..)
    , IncompleteDetails(..)
    , ResponseUsage(..)
    , TokenDetails(..)

      -- * Streaming
    , ResponseStreamEvent(..)
    , ResponseStreamError(..)
    , StreamEventType(..)
    , responseStreamEventType
    , responseStreamEventSequenceNumber
    , streamEventTypeText
    , parseStreamEventWithType
    , responseStreamEventDecoder
    , responseStreamEventDecoderWithType
    , unparsedStreamEventTypeText
    ) where

import Agent.Responses.Types.Common
import Agent.Responses.Types.Content
import Agent.Responses.Types.Items
import Agent.Responses.Types.Request
import Agent.Responses.Types.Response
import Agent.Responses.Types.Streaming
import Agent.Responses.Types.Tools
