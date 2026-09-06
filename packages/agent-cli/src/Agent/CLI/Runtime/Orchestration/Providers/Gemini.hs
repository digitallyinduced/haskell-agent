module Agent.CLI.Runtime.Orchestration.Providers.Gemini
    ( withGeminiProvider
    ) where

import Agent.CLI.Runtime.Orchestration.Providers.Common
    ( HttpProviderTransport(..)
    , withHttpProvider
    )
import Agent.CLI.Runtime.Orchestration.Providers.Types
    ( ProviderHost(..), ProviderRuntime )
import Agent.Gemini.LoopBackend (tokenProviderStatelessGeminiBackend)
import Agent.Provider (TokenProvider, runWithTokenProvider)
import Data.IORef (newIORef)
import qualified Agent.Gemini.Client as GeminiClient
import qualified Agent.Gemini.Options as Gemini

withGeminiProvider
    :: TokenProvider
    -> ProviderHost
    -> (ProviderRuntime -> IO a)
    -> IO a
withGeminiProvider tokenProvider host use = do
    geminiOptions <- Gemini.clientOptionsFromEnv
    geminiOccupancy <- newIORef Nothing
    withHttpProvider host HttpProviderTransport
        { httpMakeBackend =
            tokenProviderStatelessGeminiBackend
                tokenProvider
                (GeminiClient.createResponseWithEvents geminiOptions)
        , httpSendCompact = \compactRequest ->
            runWithTokenProvider tokenProvider \credential ->
                GeminiClient.createResponseWith geminiOptions credential compactRequest
        , httpTransportModel = id
        , httpOccupancy = geminiOccupancy
        } use
