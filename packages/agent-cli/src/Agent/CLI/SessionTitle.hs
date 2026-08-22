-- | Asynchronous, provider-neutral session title generation.
module Agent.CLI.SessionTitle
    ( SessionTitleManager
    , SessionTitleResult(..)
    , cleanGeneratedTitle
    , invalidateSessionTitles
    , requestSessionTitle
    , takeSessionTitleResults
    , titleRefreshIndex
    , waitForSessionTitleResults
    , withSessionTitleManager
    ) where

import Agent.CLI.Btw (BtwBackendFactory)
import Agent.Loop (Backend(..), TurnInput(..), TurnOutput(..))
import Agent.Responses.Types
    ( ResponseCreateParams(..)
    , ResponseItem
    , ToolChoice(..)
    , ToolChoiceMode(..)
    )
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
import Control.Exception.Safe (tryAny)
import Control.Monad (forever, void)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import System.Timeout (timeout)

data SessionTitleJob = SessionTitleJob
    { jobSessionId :: !Text
    , jobMilestone :: !Int
    , jobSource :: !Text
    , jobGeneration :: !Int
    }

data SessionTitleResult = SessionTitleResult
    { resultSessionId :: !Text
    , resultMilestone :: !Int
    , resultTitle :: !Text
    , resultGeneration :: !Int
    } deriving (Eq, Show)

data SessionTitleManager = SessionTitleManager
    { titleJobs :: !(TQueue SessionTitleJob)
    , titleResults :: !(TQueue SessionTitleResult)
    , titleRequested :: !(TVar (Set (Text, Int, Int)))
    , titleGenerations :: !(TVar (Map Text Int))
    , titleBackendFactory :: !BtwBackendFactory
    , titleParams :: !(IORef ResponseCreateParams)
    }

withSessionTitleManager
    :: BtwBackendFactory
    -> IORef ResponseCreateParams
    -> (SessionTitleResult -> IO ())
    -> (SessionTitleManager -> IO a)
    -> IO a
withSessionTitleManager backendFactory paramsRef onGenerated action = do
    jobs <- newTQueueIO
    results <- newTQueueIO
    requested <- newTVarIO Set.empty
    generations <- newTVarIO Map.empty
    let manager = SessionTitleManager
            { titleJobs = jobs
            , titleResults = results
            , titleRequested = requested
            , titleGenerations = generations
            , titleBackendFactory = backendFactory
            , titleParams = paramsRef
            }
    withAsync (titleWorker onGenerated manager) \_ -> action manager

requestSessionTitle
    :: SessionTitleManager
    -> Text
    -> Int
    -> Text
    -> IO ()
requestSessionTitle manager sessionId milestone source =
    atomically do
        requested <- readTVar manager.titleRequested
        generations <- readTVar manager.titleGenerations
        let generation = Map.findWithDefault 0 sessionId generations
            key = (sessionId, milestone, generation)
        if Set.member key requested || Text.null (Text.strip source)
            then pure ()
            else do
                writeTVar manager.titleRequested (Set.insert key requested)
                writeTQueue manager.titleJobs SessionTitleJob
                    { jobSessionId = sessionId
                    , jobMilestone = milestone
                    , jobSource = source
                    , jobGeneration = generation
                    }

takeSessionTitleResults :: SessionTitleManager -> IO [SessionTitleResult]
takeSessionTitleResults manager = atomically do
    generations <- readTVar manager.titleGenerations
    drain generations []
  where
    drain generations acc =
        tryReadTQueue manager.titleResults >>= \case
            Nothing -> pure (reverse acc)
            Just result ->
                let current =
                        Map.findWithDefault 0 result.resultSessionId generations
                    acc' =
                        if result.resultGeneration == current
                            then result : acc
                            else acc
                in drain generations acc'

waitForSessionTitleResults
    :: Int
    -> SessionTitleManager
    -> IO [SessionTitleResult]
waitForSessionTitleResults timeoutMicros manager = do
    _ <- timeout timeoutMicros $ atomically do
        requested <- readTVar manager.titleRequested
        check (Set.null requested)
    takeSessionTitleResults manager

invalidateSessionTitles :: SessionTitleManager -> Text -> IO ()
invalidateSessionTitles manager sessionId =
    atomically do
        modifyTVar' manager.titleGenerations
            (Map.insertWith (+) sessionId 1)
        modifyTVar' manager.titleRequested
            (Set.filter (\(sid, _, _) -> sid /= sessionId))

titleRefreshIndex :: Int -> Int
titleRefreshIndex milestone
    | milestone >= 6 = 2
    | milestone >= 3 = 1
    | otherwise = 0

titleWorker :: (SessionTitleResult -> IO ()) -> SessionTitleManager -> IO ()
titleWorker onGenerated manager = forever do
    job <- atomically (readTQueue manager.titleJobs)
    generated <- tryAny (generateTitle manager job)
    accepted <- atomically do
        modifyTVar' manager.titleRequested
            (Set.delete
                (job.jobSessionId, job.jobMilestone, job.jobGeneration))
        generations <- readTVar manager.titleGenerations
        let current =
                Map.findWithDefault 0 job.jobSessionId generations
        case generated of
            Right (Just title)
                | current == job.jobGeneration -> do
                    let result = SessionTitleResult
                            { resultSessionId = job.jobSessionId
                            , resultMilestone = job.jobMilestone
                            , resultTitle = title
                            , resultGeneration = job.jobGeneration
                            }
                    writeTQueue manager.titleResults result
                    pure (Just result)
            _ -> pure Nothing
    mapM_ (void . tryAny . onGenerated) accepted

generateTitle :: SessionTitleManager -> SessionTitleJob -> IO (Maybe Text)
generateTitle manager job = do
    baseParams <- readIORef manager.titleParams
    let params = titleRequestParams baseParams
    privateParams <- newIORef params
    privateTranscript <- newIORef ([] :: [ResponseItem])
    let Backend submit =
            manager.titleBackendFactory privateParams privateTranscript
    timeout 45000000
        (submit Nothing [UserMessage (titlePrompt job.jobSource)] (\_ -> pure ()))
        >>= \case
            Nothing -> pure Nothing
            Just response -> case response of
                Left _ -> pure Nothing
                Right turn
                    | not (null turn.toolCalls) -> pure Nothing
                    | otherwise ->
                        pure (turn.assistantText >>= cleanGeneratedTitle)

titleRequestParams :: ResponseCreateParams -> ResponseCreateParams
titleRequestParams ResponseCreateParams{..} =
    ResponseCreateParams
        { input = Nothing
        , previousResponseId = Nothing
        , instructions = Just
            "Generate a short, distinctive session title. Output only the title."
        , tools = Just []
        , toolChoice = Just (ToolChoiceMode ToolChoiceNone)
        , parallelToolCalls = Just False
        , maxOutputTokens = Just 100
        , reasoning = Nothing
        , ..
        }

titlePrompt :: Text -> Text
titlePrompt source =
    Text.unlines
        [ "Generate a short and distinctive 5-10 word title for this coding session."
        , "Capture the main task or topic. Be information-dense and use no filler."
        , "Output only plain title text: no quotes, label, explanation, or markdown."
        , ""
        , "Conversation:"
        , Text.take 24000 source
        ]

cleanGeneratedTitle :: Text -> Maybe Text
cleanGeneratedTitle raw =
    let firstLine =
            Text.strip
                (case filter (not . Text.null) (map Text.strip (Text.lines raw)) of
                    line : _ -> line
                    [] -> "")
        withoutLabel =
            fromMaybePrefix "session title:" $
                fromMaybePrefix "title:" firstLine
        unquoted = stripMatchingQuotes (Text.strip withoutLabel)
        oneLine = Text.unwords (Text.words unquoted)
        capped = Text.take 80 oneLine
    in if Text.null capped then Nothing else Just capped
  where
    fromMaybePrefix prefix text =
        case Text.stripPrefix prefix (Text.toLower text) of
            Just _ -> Text.drop (Text.length prefix) text
            Nothing -> text
    stripMatchingQuotes text =
        case (Text.uncons text, Text.unsnoc text) of
            (Just ('"', _), Just (_, '"')) -> Text.dropEnd 1 (Text.drop 1 text)
            (Just ('\'', _), Just (_, '\'')) -> Text.dropEnd 1 (Text.drop 1 text)
            _ -> text
