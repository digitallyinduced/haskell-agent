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
import Agent.Responses.LoopBackend (statelessResponsesBackend)
import Agent.Responses.Types
import Data.IORef

genericResponsesBackend
    :: GenericClientOptions
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
genericResponsesBackend options =
    statelessResponsesBackend (createResponseWithEvents options)

genericResponsesBackendWith
    :: (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
genericResponsesBackendWith = statelessResponsesBackend
