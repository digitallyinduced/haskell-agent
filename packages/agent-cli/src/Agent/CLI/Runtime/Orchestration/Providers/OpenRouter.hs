module Agent.CLI.Runtime.Orchestration.Providers.OpenRouter
    ( runOpenRouterProvider
    ) where

import Agent.CLI.Runtime.Orchestration.Providers.Common
    ( HttpProviderSession(..)
    , runHttpProvider
    )
import Agent.CLI.Runtime.Orchestration.Providers.Types (AgentProviderRequest(..))
import Agent.CLI.Runtime.Types (RunResult)
import Agent.OpenRouter.LoopBackend (openRouterBackend)
import Agent.Provider (runWithTokenProvider)
import Agent.Responses.GenericBackend (genericResponsesBackendWith)
import Agent.Responses.Types (ResponseCreateParams(model))
import Data.Maybe (fromMaybe)
import qualified Agent.OpenRouter as OpenRouter
import qualified Agent.Responses.GenericClient as GenericResponses

runOpenRouterProvider :: AgentProviderRequest -> IO RunResult
runOpenRouterProvider request@AgentProviderRequest{..} =
    runHttpProvider request HttpProviderSession
        { httpMakeBackend = makeBackend
        , httpSendCompact = sendCompact
        , httpTransportModel = transportModel
        , httpOccupancy = contextTokensRef
        }
  where
    optionsForRequest
        :: GenericResponses.GenericClientOptions
        -> ResponseCreateParams
        -> GenericResponses.GenericClientOptions
    optionsForRequest genericOptions responseRequest =
        genericOptions
            { GenericResponses.model =
                transportModel (fromMaybe model responseRequest.model)
            }
    makeBackend params =
        case customGenericOptions of
            Just genericOptions ->
                genericResponsesBackendWith
                    (\responseRequest onEvent ->
                        GenericResponses.createResponseWithEvents
                            (optionsForRequest genericOptions responseRequest)
                            responseRequest
                            onEvent)
                    params
            Nothing -> openRouterBackend openRouterOptions tokenProvider params
    sendCompact responseRequest =
        case customGenericOptions of
            Just genericOptions ->
                GenericResponses.createResponseWith
                    (optionsForRequest genericOptions responseRequest)
                    responseRequest
            Nothing ->
                runWithTokenProvider tokenProvider \credential ->
                    OpenRouter.createResponseWith
                        openRouterOptions credential responseRequest
