-- | Loop backend for generic stateless Responses-compatible HTTP endpoints.
module Agent.Responses.GenericBackend
    ( genericResponsesBackend
    , genericResponsesBackendWith
    ) where

import Agent.Error (ApiError)
import Agent.Loop (Backend)
import Agent.Responses.GenericClient
    ( GenericClientOptions
    , createResponseWithEvents
    )
import Agent.Responses.LoopBackend
    ( statelessResponsesBackendPreservingHistory
    )
import Agent.Responses.Types

genericResponsesBackend
    :: GenericClientOptions
    -> IO ResponseCreateParams
    -> Backend
genericResponsesBackend options =
    statelessResponsesBackendPreservingHistory
        (createResponseWithEvents options)

genericResponsesBackendWith
    :: (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
genericResponsesBackendWith =
    statelessResponsesBackendPreservingHistory
