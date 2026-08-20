-- | Persistent GHCi session for Grok/OpenRouter tools.
--
-- Frames replies with a unique @putStrLn@ marker instead of relying on the
-- GHCi prompt (which is suppressed when stdin is not a TTY).
module Agent.Tools.Grok.Ghci
    ( GhciSession(..)
    , GhciClass(..)
    , GhciResult(..)
    , newGhciSession
    , closeGhciSession
    , evalGhci
    , classifyGhci
    , classifyGhciInput
    , typeLooksEffectful
    ) where

import Agent.Tools.Types (ToolEnv(..))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (concurrently, race)
import Control.Concurrent.MVar
import Control.Exception.Safe (SomeException, try)
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum, isSpace)
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

data GhciClass
    = GhciPure
    | GhciEffectful
    deriving (Eq, Show)

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

-- | Static gate. @Nothing@ means "needs a :type probe".
classifyGhciInput :: Text -> Maybe GhciClass
classifyGhciInput raw =
    let text = Text.strip raw
    in if Text.null text
        then Just GhciEffectful
        else if mentionsUnsafe text
            then Just GhciEffectful
            else case ghciCommandName text of
                Just cmd
                    | cmd `elem` safeInfoCommands -> Just GhciPure
                    | cmd `elem` effectfulCommands -> Just GhciEffectful
                    | cmd == "set" && isPromptSet text -> Just GhciEffectful
                    | otherwise -> Just GhciEffectful
                Nothing
                    | looksLikeDoBlock text -> Just GhciEffectful
                    | isBinding text -> Just GhciPure
                    | otherwise -> Nothing

safeInfoCommands :: [Text]
safeInfoCommands =
    [ "type", "t", "kind", "k", "info", "i", "browse", "show", "doc"
    , "hoogle", "instances", "module"
    ]

effectfulCommands :: [Text]
effectfulCommands =
    [ "!", "cd", "def", "script", "load", "l", "reload", "r"
    , "add", "unadd", "main", "run", "edit", "e", "sprint"
    , "force", "print", "quit", "q", "issafe", "ctags", "etags"
    ]

mentionsUnsafe :: Text -> Bool
mentionsUnsafe text =
    any (`Text.isInfixOf` text)
        [ "unsafePerformIO"
        , "unsafeInterleaveIO"
        , "accursedUnutterablePerformIO"
        , "inlinePerformIO"
        , "unsafeDupablePerformIO"
        ]

ghciCommandName :: Text -> Maybe Text
ghciCommandName text =
    case Text.uncons (Text.dropWhile isSpace text) of
        Just (':', rest) ->
            let name = Text.takeWhile (\c -> isAlphaNum c || c == '!' || c == '-') rest
            in if Text.null name then Just "!" else Just (Text.toLower name)
        _ -> Nothing

isPromptSet :: Text -> Bool
isPromptSet text =
    let lowered = Text.toLower text
    in "prompt" `Text.isInfixOf` lowered

looksLikeDoBlock :: Text -> Bool
looksLikeDoBlock text =
    let stripped = Text.strip text
    in "do" == stripped
        || "do\n" `Text.isPrefixOf` stripped
        || "do " `Text.isPrefixOf` stripped
        || "\ndo " `Text.isInfixOf` stripped
        || "\ndo\n" `Text.isInfixOf` stripped

isBinding :: Text -> Bool
isBinding text =
    let stripped = Text.strip text
        lowered = Text.toLower stripped
    in "let " `Text.isPrefixOf` lowered
        || "let\n" `Text.isPrefixOf` lowered
        || (hasEqualsBinding stripped && not (Text.isPrefixOf "data " lowered)
            && not (Text.isPrefixOf "type " lowered)
            && not (Text.isPrefixOf "newtype " lowered)
            && not (Text.isPrefixOf "class " lowered)
            && not (Text.isPrefixOf "instance " lowered))

hasEqualsBinding :: Text -> Bool
hasEqualsBinding text =
    case Text.breakOn "=" text of
        (before, after)
            | Text.null after -> False
            | ":" `Text.isInfixOf` before -> False
            | otherwise ->
                let name = Text.strip before
                in not (Text.null name)
                    && Text.all (\c -> isAlphaNum c || c `elem` ("_' " :: String)) name

-- | True when a @:type@ reply denotes an IO (or clearly effectful) result.
typeLooksEffectful :: Text -> Bool
typeLooksEffectful output =
    let cleaned = Text.unwords (Text.words (stripTypeErrors output))
        typePart = case Text.breakOnEnd "::" cleaned of
            (prefix, rest)
                | Text.null prefix -> cleaned
                | otherwise -> Text.strip rest
        afterConstraints = case Text.breakOnEnd "=>" typePart of
            (prefix, rest)
                | Text.null prefix -> typePart
                | otherwise -> Text.strip rest
        resultSide = case Text.breakOnEnd "->" afterConstraints of
            (prefix, rest)
                | Text.null prefix -> afterConstraints
                | otherwise -> Text.strip rest
        tokens = tokenizeType resultSide
        allTokens = tokenizeType afterConstraints
    in case tokens of
        (headTok : _) -> headTok == "IO" || "MonadIO" `elem` allTokens
        [] -> "MonadIO" `elem` allTokens

stripTypeErrors :: Text -> Text
stripTypeErrors =
    Text.unlines
        . filter (\line -> not ("error:" `Text.isInfixOf` line)
            && not ("<interactive>" `Text.isPrefixOf` Text.strip line))
        . Text.lines

tokenizeType :: Text -> [Text]
tokenizeType = filter (not . Text.null) . Text.split (\c -> isSpace c || c == '(' || c == ')' || c == ',')

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

spawnProcess :: ToolEnv -> IO GhciProcess
spawnProcess env = do
    let spec = (proc "ghci" ["-v0"])
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
        _ -> error "Agent.Tools.Grok.Ghci: failed to create ghci pipes"

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
