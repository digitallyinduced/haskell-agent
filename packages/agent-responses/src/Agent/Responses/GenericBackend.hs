-- | Loop backend for generic stateless Responses-compatible HTTP endpoints.
module Agent.Responses.GenericBackend
    ( genericResponsesBackend
    , genericResponsesBackendWithParams
    , genericResponsesBackendWith
    , genericResponsesBackendWithFixedParams
    ) where

import Agent.Error (ApiError)
import Agent.Loop (Backend)
import Agent.Responses.GenericClient
    ( GenericClientOptions
    , createResponseWithEvents
    )
import Agent.Responses.LoopBackend
    ( statelessResponsesBackend
    , statelessResponsesBackendWithParams
    )
import Agent.Responses.Types

genericResponsesBackend
    :: GenericClientOptions
    -> IO ResponseCreateParams
    -> Backend
genericResponsesBackend options =
    statelessResponsesBackend (createResponseWithEvents options)

genericResponsesBackendWithParams
    :: GenericClientOptions
    -> ResponseCreateParams
    -> Backend
genericResponsesBackendWithParams options =
    statelessResponsesBackendWithParams
        (createResponseWithEvents options)

genericResponsesBackendWith
    :: (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
genericResponsesBackendWith = statelessResponsesBackend

genericResponsesBackendWithFixedParams
    :: (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> ResponseCreateParams
    -> Backend
genericResponsesBackendWithFixedParams =
    statelessResponsesBackendWithParams
