-- | Lossless wire types for the OpenAI Responses API.
module Agent.Responses.Types
    ( -- * Create request
      ResponseCreateParams(..)
    , defaultResponseCreateParams
    , responseCreateParamsEncoder
    , responseCreateParamsDecoder
    , ResponseInput(..)
    , responseInputEncoder
    , responseInputDecoder
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
    , responseItemEncoder
    , responseItemDecoder
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
    , responseRoleEncoder
    , responseRoleDecoder
    , MessageContent(..)
    , messageContentEncoder
    , messageContentDecoder
    , ResponseContentPart(..)
    , responseContentPartEncoder
    , responseContentPartDecoder
    , ItemStatus(..)
    , FunctionCall(..)
    , FunctionCallOutput(..)
    , CustomToolCall(..)
    , CustomToolCallOutput(..)
    , ReasoningItem(..)
    , ReasoningSummaryPart(..)
    , ItemReference(..)
    , TaggedObject(..)

      -- * Tools
    , ResponseTool(..)
    , responseToolEncoder
    , responseToolDecoder
    , ResponseToolType(..)
    , responseToolTypeText
    , knownResponseTool
    , FunctionTool(..)

      -- * Response
    , Response(..)
    , responseEncoder
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
    , responseStreamEventEncoder
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
