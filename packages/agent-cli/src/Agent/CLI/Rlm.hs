{-# LANGUAGE DeriveGeneric #-}

-- | Small mailbox bridge used by the experimental recursive-language-model
-- mode.  The GHCi process is only a coordinator: every request is dispatched
-- to the normal in-process 'SubagentRegistry' runner.
module Agent.CLI.Rlm
    ( RlmMode(..)
    , RlmConfig(..)
    , RlmRuntime
    , newRlmRuntime
    , closeRlmRuntime
    , rlmGhciHelpers
    , rlmRootGuidance
    ) where

import Agent.InterAgentMessage (plainInterAgentContent)
import Agent.Loop (LoopResult(..), TokenUsage(..))
import Agent.Subagents
    ( RootTurnId
    , SubagentId(..)
    , SubagentLease
    , SubagentStatus(..)
    , getLastResult
    , getStatus
    , interruptSubagent
    , spawnSubagentAtPreparedForTurn
    , waitSubagentsFrom
    )
import Agent.Subagents.TaskPath (taskPathRoot)
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Agent.OsPath (unsafeToFilePath)
import Control.Concurrent
    ( QSem
    , MVar
    , newMVar
    , newQSem
    , readMVar
    , threadDelay
    , waitQSem
    , signalQSem
    )
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Concurrent.MVar (modifyMVar, withMVar)
import Control.Exception.Safe (finally, tryAny)
import Control.Monad (forM_, forever, void, when)
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as LazyByteString
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , listDirectory
    , removeFile
    , renameFile
    )
import System.FilePath ((</>), dropExtension)
import System.OsPath (OsPath)

data RlmMode = RlmReadOnly | RlmCoding
    deriving (Eq, Show)

data RlmConfig = RlmConfig
    { rlmMailbox :: !OsPath
    , rlmContext :: !MultiAgentContext
    , rlmModel :: !(Maybe Text)
    , rlmEffort :: !(Maybe Text)
    , rlmMaxCalls :: !Int
    , rlmParallelism :: !Int
    , rlmWorkerTimeoutSeconds :: !Int
    , rlmPrepareWorker :: !(SubagentId -> RlmMode -> IO SubagentLease)
    }

data RlmRuntime = RlmRuntime
    { rlmConfig :: !RlmConfig
    , rlmWatcher :: Async ()
    , rlmWorkers :: !(IORef [Async ()])
    , rlmSemaphore :: !QSem
    , rlmCodingLock :: !(MVar ())
    , rlmBudget :: !(MVar (Maybe RootTurnId, Int))
    , rlmCounter :: !(IORef Int)
    , rlmClosed :: !(MVar Bool)
    }

newRlmRuntime :: RlmConfig -> IO RlmRuntime
newRlmRuntime config = do
    let mailbox = unsafeToFilePath config.rlmMailbox
    createDirectoryIfMissing True mailbox
    workers <- newIORef []
    semaphore <- newQSem (max 1 config.rlmParallelism)
    codingLock <- newMVar ()
    budget <- newMVar (Nothing, 0)
    counter <- newIORef 0
    closed <- newMVar False
    let runtime0 = RlmRuntime
            { rlmConfig = config
            , rlmWatcher = error "rlm watcher not initialized"
            , rlmWorkers = workers
            , rlmSemaphore = semaphore
            , rlmCodingLock = codingLock
            , rlmBudget = budget
            , rlmCounter = counter
            , rlmClosed = closed
            }
    watcher <- async (watchMailbox runtime0)
    pure runtime0 { rlmWatcher = watcher }

closeRlmRuntime :: RlmRuntime -> IO ()
closeRlmRuntime runtime = do
    shouldClose <- modifyMVar runtime.rlmClosed \closed ->
        pure (True, not closed)
    when shouldClose $ do
        cancel runtime.rlmWatcher
        _ <- waitCatch runtime.rlmWatcher
        workers <- readIORef runtime.rlmWorkers
        mapM_ cancel workers
        mapM_ waitCatch workers

-- | Startup statements installed in the persistent GHCi session.
rlmGhciHelpers :: RlmRuntime -> [Text]
rlmGhciHelpers runtime =
    let mailbox = Text.pack (show (unsafeToFilePath runtime.rlmConfig.rlmMailbox))
        poll =
            "let rlmAwait = \\(_,resp) -> do { e <- AgentRlmDir.doesFileExist resp; "
                <> "if e then do { x <- AgentRlmIO.readFile resp; AgentRlmDir.removeFile resp; "
                <> "pure x } else AgentRlmConcurrent.threadDelay 50000 >> rlmAwait (\"\",resp) }"
        start =
            "let rlmStart = \\mode prompt -> do { (p,h) <- AgentRlmIO.openTempFile "
                <> mailbox
                <> " \"rlm-\"; AgentRlmIO.hPutStr h (mode ++ \"\\n\" ++ prompt); "
                <> "AgentRlmIO.hClose h; AgentRlmDir.renameFile p (p ++ \".req\"); "
                <> "pure (p ++ \".req\", p ++ \".resp\") }"
        query =
            "let rlmQuery = \\prompt -> rlmStart \"readonly\" prompt >>= rlmAwait; "
                <> "rlmCode = \\prompt -> rlmStart \"coding\" prompt >>= rlmAwait; "
                <> "rlmQueryMany = \\prompts -> mapM (rlmStart \"readonly\") prompts >>= mapM rlmAwait"
    in
        [ "import qualified System.IO as AgentRlmIO"
        , "import qualified System.Directory as AgentRlmDir"
        , "import qualified Control.Concurrent as AgentRlmConcurrent"
        , "import qualified System.FilePath as AgentRlmPath"
        , start
        , poll
        , query
        ]

rlmRootGuidance :: Text
rlmRootGuidance =
    "RLM mode is active. You have only run_ghci. Use rlmQuery for independent "
        <> "read-only investigations and rlmCode for focused implementation tasks. "
        <> "Use rlmQueryMany to submit independent investigations together. Workers "
        <> "are in-process subagents; synthesize their structured results and do not "
        <> "assume that a request succeeded unless its status is completed."

watchMailbox :: RlmRuntime -> IO ()
watchMailbox runtime =
    (`finally` pure ()) $ forever do
        closed <- readMVar runtime.rlmClosed
        if closed
            then pure ()
            else do
                files <- tryAny (listDirectory (unsafeToFilePath runtime.rlmConfig.rlmMailbox))
                case files of
                    Right names ->
                        forM_ (filter (".req" `isSuffixOf`) names) \name -> do
                            let mailbox = unsafeToFilePath runtime.rlmConfig.rlmMailbox
                                source = mailbox </> name
                                claimed = dropExtension source <> ".work"
                            claimedOk <- tryAny (renameFile source claimed)
                            case claimedOk of
                                Right () -> do
                                    worker <- async (handleRequest runtime claimed)
                                    atomicModifyIORef' runtime.rlmWorkers
                                        (\xs -> (worker : xs, ()))
                                Left _ -> pure ()
                    Left _ -> pure ()
                threadDelay 50000

handleRequest :: RlmRuntime -> FilePath -> IO ()
handleRequest runtime requestPath =
    (`finally` removeIfExists requestPath) do
        contents <- readFile requestPath
        let (modeLine, promptLines) = case lines contents of
                [] -> ("readonly", [])
                first : rest -> (first, rest)
            mode = if modeLine == "coding" then RlmCoding else RlmReadOnly
            responsePath = dropExtension requestPath <> ".resp"
        response <- withBudget runtime mode \rootTurnId ->
            withPermit runtime mode $
                dispatchRequest runtime rootTurnId mode (Text.pack (unlines promptLines))
        writeFile responsePath (Text.unpack response.responsePayload)

withBudget
    :: RlmRuntime
    -> RlmMode
    -> (RootTurnId -> IO RlmResponse)
    -> IO RlmResponse
withBudget runtime _mode action =
    case runtime.rlmConfig.rlmContext of
        ctx -> do
            rootTurn <- ctx.multiRootTurnId
            case rootTurn of
                Nothing -> pure (errorResponse "no active root turn")
                Just turnId -> do
                    allowed <- modifyBudget runtime turnId
                    if not allowed
                        then pure (errorResponse "RLM call budget exhausted")
                        else action turnId

modifyBudget :: RlmRuntime -> RootTurnId -> IO Bool
modifyBudget runtime turnId =
    modifyMVar runtime.rlmBudget \state@(current, used) ->
        if current == Just turnId
            then
                let limit = max 1 runtime.rlmConfig.rlmMaxCalls
                in if used >= limit
                    then pure (state, False)
                    else pure ((current, used + 1), True)
            else pure ((Just turnId, 1), True)

withPermit :: RlmRuntime -> RlmMode -> IO a -> IO a
withPermit runtime mode action =
    bracketQSem runtime.rlmSemaphore $
        if mode == RlmCoding
            then withMVar runtime.rlmCodingLock (const action)
            else action

dispatchRequest :: RlmRuntime -> RootTurnId -> RlmMode -> Text -> IO RlmResponse
dispatchRequest runtime rootTurn mode prompt = do
    n <- atomicModifyIORef' runtime.rlmCounter (\x -> (x + 1, x + 1))
    let taskName = Text.pack ("rlm_" <> show n)
    spawned <-
        spawnSubagentAtPreparedForTurn
            (let MultiAgentContext { multiRegistry = registry } =
                    runtime.rlmConfig.rlmContext
             in registry)
            (Just rootTurn)
            (\agentId -> runtime.rlmConfig.rlmPrepareWorker agentId mode)
            Nothing
            taskPathRoot
            0
            taskName
            (plainInterAgentContent prompt)
            Nothing
    case spawned of
        Left err -> pure (errorResponse err)
        Right (agentId, _) -> do
            let MultiAgentContext { multiRegistry = registry } =
                    runtime.rlmConfig.rlmContext
            (_, timedOut) <-
                waitSubagentsFrom
                    registry
                    Nothing
                    [agentId]
                    (max 1 runtime.rlmConfig.rlmWorkerTimeoutSeconds * 1000)
            status <- getStatus registry agentId
            result <- getLastResult registry agentId
            when timedOut (void (interruptSubagent registry agentId))
            pure (responseFor agentId status timedOut result)

data RlmResponse = RlmResponse
    { responsePayload :: !Text
    }

responseFor :: SubagentId -> SubagentStatus -> Bool -> Maybe LoopResult -> RlmResponse
responseFor agentId status timedOut result =
    let SubagentId agentName = agentId
        statusText :: Text
        statusText
            | timedOut = "timeout"
            | otherwise = case status of
                Pending -> "pending"
                Running -> "running"
                Completed _ -> "completed"
                Errored _ -> "error"
                Interrupted -> "interrupted"
                Closed -> "closed"
                NotFound -> "not_found"
        value = object
            [ "status" .= statusText
            , "agent_id" .= agentName
            , "result" .= maybe (Nothing :: Maybe Text) (.finalText) result
            , "turns_used" .= maybe 0 (.turnsUsed) result
            , "usage" .= maybe
                (object ["input" .= (0 :: Int), "output" .= (0 :: Int), "cached" .= (0 :: Int)])
                (usageValue . (.tokenUsage))
                result
            ]
    in RlmResponse (decodeUtf8Lazy (encode value))

usageValue :: TokenUsage -> Value
usageValue usage = object
    [ "input" .= usage.inputTokens
    , "output" .= usage.outputTokens
    , "cached" .= usage.cachedTokens
    ]

errorResponse :: Text -> RlmResponse
errorResponse message =
    RlmResponse (decodeUtf8Lazy (encode (object
        [ "status" .= ("error" :: Text)
        , "error" .= message
        ])))

decodeUtf8Lazy :: LazyByteString.ByteString -> Text
decodeUtf8Lazy = TextEncoding.decodeUtf8 . LazyByteString.toStrict

bracketQSem :: QSem -> IO a -> IO a
bracketQSem sem action = do
    waitQSem sem
    action `finally` signalQSem sem

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
    exists <- doesFileExist path
    when exists (void (tryAny (removeFile path)))

