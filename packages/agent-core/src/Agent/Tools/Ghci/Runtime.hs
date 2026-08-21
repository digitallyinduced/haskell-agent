-- | Persistent GHCi session shared by coding-tool providers.
--
-- Frames replies with a unique @putStrLn@ marker instead of relying on the
-- GHCi prompt (which is suppressed when stdin is not a TTY).
module Agent.Tools.Ghci.Runtime
    ( GhciSession(..)
    , GhciResult(..)
    , newGhciSession
    , closeGhciSession
    , evalGhci
    , classifyGhci
    , runGhciTool
    ) where

import Agent.ToolArgs
    ( objectArgs
    , optInt
    , optText
    , reqText
    )
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    )
import Agent.ToolDispatch (ToolCall(..), typedTool)
import Agent.Tools.Ghci.Classify
    ( GhciClass(..)
    , classifyGhciInput
    , defaultGhciExtensions
    , typeLooksEffectful
    )
import Agent.Tools.Types (AppTool(..), AppToolKind(..), ToolEnv(..))
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(..), Object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding as TextEncoding

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (concurrently, race)
import Control.Concurrent.MVar
import Control.Exception.Safe (SomeException, try)
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import System.IO (BufferMode(..), Handle, hClose, hFlush, hSetBinaryMode, hSetBuffering)
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(..)
    , createProcess
    , getProcessExitCode
    , interruptProcessGroupOf
    , proc
    , terminateProcess
    , waitForProcess
    )

data GhciResult = GhciResult
    { ghciOk :: !Bool
    , ghciTimedOut :: !Bool
    , ghciClass :: !GhciClass
    , ghciOutput :: !Text
    } deriving (Eq, Show)

data GhciProcess = GhciProcess
    { ghciStdin :: !Handle
    , ghciStdout :: !Handle
    , ghciStderr :: !Handle
    , ghciHandle :: !ProcessHandle
    , ghciBuffer :: !(IORef ByteString)
    , ghciDrainDone :: !(MVar ())
    }

data GhciSession = GhciSession
    { ghciEnv :: !ToolEnv
    , ghciLock :: !(MVar ())
    , ghciProcess :: !(MVar (Maybe GhciProcess))
    , ghciNextMarker :: !(IORef Int)
    }

newGhciSession :: ToolEnv -> IO GhciSession
newGhciSession env = do
    lock <- newMVar ()
    processVar <- newMVar Nothing
    nextMarker <- newIORef 0
    pure GhciSession
        { ghciEnv = env
        , ghciLock = lock
        , ghciProcess = processVar
        , ghciNextMarker = nextMarker
        }

closeGhciSession :: GhciSession -> IO ()
closeGhciSession session =
    modifyMVar_ session.ghciProcess \current -> do
        mapM_ shutdownProcess current
        pure Nothing

-- | Evaluate @expression@ in the persistent GHCi, classifying side effects first.
evalGhci :: GhciSession -> Text -> Int -> IO GhciResult
evalGhci session expression timeoutMs =
    withGhciLock session do
        classification <- classifyGhciLocked session expression
        result <- evalRawGhci session expression timeoutMs
        pure result { ghciClass = classification }

-- | Classify without evaluating. Fail closed on ambiguity.
classifyGhci :: GhciSession -> Text -> IO GhciClass
classifyGhci session expression =
    withGhciLock session (classifyGhciLocked session expression)

withGhciLock :: GhciSession -> IO a -> IO a
withGhciLock session action =
    withMVar session.ghciLock (const action)

classifyGhciLocked :: GhciSession -> Text -> IO GhciClass
classifyGhciLocked session expression =
    case classifyGhciInput expression of
        Just cls -> pure cls
        Nothing -> do
            typeResult <- evalRawGhci session (":type " <> expression) 15000
            if typeResult.ghciTimedOut || not typeResult.ghciOk
                then pure GhciEffectful
                else
                    if typeLooksEffectful typeResult.ghciOutput
                        then pure GhciEffectful
                        else pure GhciPure

evalRawGhci :: GhciSession -> Text -> Int -> IO GhciResult
evalRawGhci session expression timeoutMs = do
    process <- ensureProcess session
    marker <- nextMarker session
    let markerLine = "Prelude.putStrLn " <> Text.pack (show (Text.unpack marker))
    clearBuffer process
    sendLine process expression
    sendLine process markerLine
    awaited <- awaitMarker process marker timeoutMs
    case awaited of
        Left partial -> do
            void $ try @_ @SomeException (interruptProcessGroupOf process.ghciHandle)
            threadDelay 200000
            -- Drop any leftover output, then verify the process still responds.
            clearBuffer process
            recovered <- recoverAfterInterrupt session process
            when (not recovered) (restartProcess session)
            pure GhciResult
                { ghciOk = False
                , ghciTimedOut = True
                , ghciClass = GhciEffectful
                , ghciOutput = Text.strip partial
                }
        Right body ->
            pure GhciResult
                { ghciOk = not (isGhciError body)
                , ghciTimedOut = False
                , ghciClass = GhciPure
                , ghciOutput = Text.strip body
                }

recoverAfterInterrupt :: GhciSession -> GhciProcess -> IO Bool
recoverAfterInterrupt session process = do
    marker <- nextMarker session
    let markerLine = "Prelude.putStrLn " <> Text.pack (show (Text.unpack marker))
    sendLine process ":type ()"
    sendLine process markerLine
    awaited <- awaitMarker process marker 5000
    pure (either (const False) (const True) awaited)

isGhciError :: Text -> Bool
isGhciError body =
    any (`Text.isInfixOf` body)
        [ "error:"
        , "<interactive>:"
        , "Exception:"
        ]

nextMarker :: GhciSession -> IO Text
nextMarker session = do
    n <- atomicModifyIORef' session.ghciNextMarker \i -> (i + 1, i + 1)
    pure $ "{- AGENT_GHCI_DONE:" <> Text.pack (show n) <> " -}"

ensureProcess :: GhciSession -> IO GhciProcess
ensureProcess session =
    modifyMVar session.ghciProcess \current -> case current of
        Just process -> do
            exited <- getProcessExitCode process.ghciHandle
            case exited of
                Nothing -> pure (Just process, process)
                Just _ -> do
                    shutdownProcess process
                    process' <- spawnProcess session.ghciEnv
                    pure (Just process', process')
        Nothing -> do
            process <- spawnProcess session.ghciEnv
            pure (Just process, process)

restartProcess :: GhciSession -> IO ()
restartProcess session =
    modifyMVar_ session.ghciProcess \current -> do
        mapM_ shutdownProcess current
        Just <$> spawnProcess session.ghciEnv

ghciArgs :: [String]
ghciArgs = "-v0" : "-XGHC2021" : map ("-X" <>) defaultGhciExtensions

spawnProcess :: ToolEnv -> IO GhciProcess
spawnProcess env = do
    let spec = (proc "ghci" ghciArgs)
            { cwd = Just env.toolCwd
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            , create_group = True
            }
    createProcess spec >>= \case
        (Just hin, Just hout, Just herr, handle) -> do
            mapM_ prepareHandle [hin, hout, herr]
            buffer <- newIORef BS.empty
            drainDone <- newEmptyMVar
            _ <- forkIO do
                _ <- concurrently (drainHandle hout buffer) (drainHandle herr buffer)
                putMVar drainDone ()
            -- Give GHCi a moment to boot before the first eval.
            threadDelay 100000
            pure GhciProcess
                { ghciStdin = hin
                , ghciStdout = hout
                , ghciStderr = herr
                , ghciHandle = handle
                , ghciBuffer = buffer
                , ghciDrainDone = drainDone
                }
        _ -> error "Agent.Tools.Ghci: failed to create ghci pipes"

prepareHandle :: Handle -> IO ()
prepareHandle handle = do
    hSetBinaryMode handle True
    hSetBuffering handle NoBuffering

shutdownProcess :: GhciProcess -> IO ()
shutdownProcess process = do
    void $ try @_ @SomeException do
        sendLine process ":quit"
        threadDelay 200000
    void $ try @_ @SomeException (terminateProcess process.ghciHandle)
    void $ try @_ @SomeException (waitForProcess process.ghciHandle)
    void $ try @_ @SomeException (hClose process.ghciStdin)
    void $ try @_ @SomeException (hClose process.ghciStdout)
    void $ try @_ @SomeException (hClose process.ghciStderr)

sendLine :: GhciProcess -> Text -> IO ()
sendLine process text = do
    BS.hPut process.ghciStdin (encodeUtf8 (text <> "\n"))
    hFlush process.ghciStdin

clearBuffer :: GhciProcess -> IO ()
clearBuffer process = writeIORef process.ghciBuffer BS.empty

awaitMarker :: GhciProcess -> Text -> Int -> IO (Either Text Text)
awaitMarker process marker timeoutMs = do
    let needle = encodeUtf8 marker
        budget = max 1 timeoutMs * 1000
    raced <- race (threadDelay budget) (waitFor needle)
    case raced of
        Left () -> Left . decodeUtf8With lenientDecode <$> readIORef process.ghciBuffer
        Right body -> pure (Right body)
  where
    waitFor needle = do
        bytes <- readIORef process.ghciBuffer
        case BS.breakSubstring needle bytes of
            (before, after)
                | BS.null after -> threadDelay 5000 >> waitFor needle
                | otherwise -> do
                    writeIORef process.ghciBuffer (BS.drop (BS.length needle) after)
                    pure (decodeUtf8With lenientDecode before)

drainHandle :: Handle -> IORef ByteString -> IO ()
drainHandle handle ref = go
  where
    go = do
        chunk <- BS.hGetSome handle 4096
        if BS.null chunk
            then pure ()
            else do
                atomicModifyIORef' ref \soFar -> (soFar <> chunk, ())
                go


--------------------------------------------------------------------------------
-- run_ghci AppTool
--------------------------------------------------------------------------------

data GhciArgs = GhciArgs
    { expression :: Text
    , timeout :: Maybe Int
    , description :: Text
    }

instance FromJSON GhciArgs where
    parseJSON = objectArgs \object -> GhciArgs
        <$> reqText object "expression"
        <*> optionalTimeout object
        <*> reqText object "description"

optionalTimeout :: Object -> Parser (Maybe Int)
optionalTimeout object = do
    fromInt <- optInt object "timeout"
    fromText <- optText object "timeout"
    pure (fromInt <|> (fromText >>= readTimeout))

readTimeout :: Text -> Maybe Int
readTimeout text =
    case reads (Text.unpack text) of
        [(n, "")] -> Just n
        _ -> Nothing

-- | Provider-neutral persistent GHCi tool with per-call purity approval.
runGhciTool :: GhciSession -> AppTool
runGhciTool session = AppTool
    { appToolName = "run_ghci"
    , appToolDescription = ghciDescription
    , appToolParameters =
        [ PropertySchema "expression" PropertyString True $ Just
            "Haskell expression, statement, or GHCi :command to evaluate."
        , PropertySchema "timeout" PropertyInteger False $ Just
            "Optional timeout in milliseconds (max 300000). Default: 30000."
        , PropertySchema "description" PropertyString True $ Just
            "One sentence explanation as to why this evaluation is needed."
        ]
    , appToolHandler = typedTool "run_ghci" (runGhci session)
    , appToolKind = JsonFunction
    , appToolReadOnly = False
    , appToolIsReadOnlyCall = Just (isGhciReadOnlyCall session)
    }

ghciDescription :: Text
ghciDescription =
    "Evaluate Haskell in a persistent GHCi session for this agent.\n\
    \Bindings and loaded modules persist across calls.\n\
    \Pure expressions auto-approve; IO and side-effecting GHCi commands need approval.\n\
    \Prefer this over shell tools for calculations, type exploration, and small Haskell scripts.\n\
    \The session starts with GHC2021 plus BlockArguments, OverloadedStrings, \
    \OverloadedRecordDot, DuplicateRecordFields, NoFieldSelectors, LambdaCase, \
    \and RecordWildCards — LANGUAGE pragmas are not required for those."

isGhciReadOnlyCall :: GhciSession -> ToolCall -> IO Bool
isGhciReadOnlyCall session call =
    case decodeExpression call.arguments of
        Nothing -> pure False
        Just expression -> do
            classification <- classifyGhci session expression
            pure (classification == GhciPure)

decodeExpression :: Text -> Maybe Text
decodeExpression arguments =
    case Aeson.decodeStrict (TextEncoding.encodeUtf8 arguments) of
        Just (Aeson.Object object) ->
            case KeyMap.lookup (Key.fromText "expression") object of
                Just (Aeson.String value) -> Just value
                _ -> Nothing
        _ -> Nothing

runGhci :: GhciSession -> GhciArgs -> IO (Either Text Text)
runGhci session args
    | Text.null args.description =
        pure (Left "Missing parameter: description")
    | Text.null (Text.strip args.expression) =
        pure (Left "Missing parameter: expression")
    | otherwise = do
        let timeoutMs = min 300000 (max 1 (fromMaybe 30000 args.timeout))
        result <- evalGhci session args.expression timeoutMs
        let classLabel = case result.ghciClass of
                GhciPure -> "pure"
                GhciEffectful -> "io"
            body = result.ghciOutput
        if result.ghciTimedOut
            then pure $ Right $
                "class: " <> classLabel <> "\nexit: killed (timeout)\n" <> body
            else
                let status = if result.ghciOk then "ok" else "error"
                in pure $ Right $
                    "class: " <> classLabel <> "\n" <> status <> "\n" <> body
