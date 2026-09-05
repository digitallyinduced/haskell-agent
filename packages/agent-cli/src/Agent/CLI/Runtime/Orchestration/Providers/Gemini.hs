module Agent.CLI.Runtime.Orchestration.Providers.Gemini
    ( runGeminiProvider
    ) where

import Agent.CLI.Runtime.Orchestration.Providers.Common
    ( HttpProviderSession(..)
    , runHttpProvider
    )
import Agent.CLI.Runtime.Orchestration.Providers.Types (AgentProviderRequest(..))
import Agent.CLI.Runtime.Types (RunResult)
import Agent.Gemini.LoopBackend (tokenProviderStatelessGeminiBackend)
import Agent.Provider (runWithTokenProvider)
import Data.IORef (newIORef)
import qualified Agent.Gemini.Client as GeminiClient
import qualified Agent.Gemini.Options as Gemini

runGeminiProvider :: AgentProviderRequest -> IO RunResult
runGeminiProvider request@AgentProviderRequest{tokenProvider} = do
    geminiOptions <- Gemini.clientOptionsFromEnv
    geminiOccupancy <- newIORef Nothing
    runHttpProvider request HttpProviderSession
        { httpMakeBackend =
            tokenProviderStatelessGeminiBackend
                tokenProvider
                (GeminiClient.createResponseWithEvents geminiOptions)
        , httpSendCompact = \compactRequest ->
            runWithTokenProvider tokenProvider \credential ->
                GeminiClient.createResponseWith geminiOptions credential compactRequest
        , httpTransportModel = id
        , httpOccupancy = geminiOccupancy
        }
