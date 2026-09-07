module Agent.CLI.Runtime.Orchestration.Providers.OpenRouter
    ( withOpenRouterProvider
    ) where

import Agent.CLI.Runtime.Orchestration.Providers.Common
    ( HttpProviderTransport(..)
    , withHttpProvider
    )
import Agent.CLI.Runtime.Orchestration.Providers.Types
    ( ProviderHost(..), ProviderRuntime, OpenRouterConfig(..), ProviderCompaction(..) )
import Agent.OpenRouter.LoopBackend (openRouterBackend)
import Agent.Provider (runWithTokenProvider)
import Agent.Responses.GenericBackend (genericResponsesBackendWith)
import Agent.Responses.Types (ResponseCreateParams(model))
import Data.Maybe (fromMaybe)
import qualified Agent.OpenRouter as OpenRouter
import qualified Agent.Responses.GenericClient as GenericResponses

withOpenRouterProvider
    :: OpenRouterConfig
    -> ProviderHost
    -> (ProviderRuntime -> IO a)
    -> IO a
withOpenRouterProvider OpenRouterConfig{genericOptions = customGenericOptions, ..}
        host use =
    withHttpProvider host HttpProviderTransport
        { httpMakeBackend = makeBackend
        , httpSendCompact = sendCompact
        , httpTransportModel = transportModel
        , httpOccupancy = host.compaction.contextTokensRef
        } use
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
            Nothing -> openRouterBackend clientOptions tokenProvider params
    sendCompact responseRequest =
        case customGenericOptions of
            Just genericOptions ->
                GenericResponses.createResponseWith
                    (optionsForRequest genericOptions responseRequest)
                    responseRequest
            Nothing ->
                runWithTokenProvider tokenProvider \credential ->
                    OpenRouter.createResponseWith
                        clientOptions credential responseRequest
