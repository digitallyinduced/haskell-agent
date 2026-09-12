{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Experimental, trusted-input-only code-mode evaluator. Not a sandbox.
-- All JavaScriptCore activity stays on the bound thread owning the runtime.
-- No public execution timeout API is used: never run untrusted/CPU-bound input.
module JavaScriptCorePrototype
    ( Runtime, withRuntime, executeCell, executeCellWith ) where

import Control.Concurrent (runInBoundThread)
import Control.Exception.Safe (SomeException, bracket, catch, finally, try, displayException)
import Control.Monad (void, when)
import Data.IORef
import qualified Data.Sequence as Sequence
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Foreign hiding (void)
import Foreign.C

type Reference = Ptr ()
type Callback = Reference -> Reference -> Reference -> CSize -> Ptr Reference
    -> Ptr Reference -> IO Reference

foreign import ccall safe "JSContextGroupCreate" createGroup :: IO Reference
foreign import ccall safe "JSContextGroupRelease" releaseGroup :: Reference -> IO ()
foreign import ccall safe "JSGlobalContextCreateInGroup" createContext :: Reference -> Reference -> IO Reference
foreign import ccall safe "JSGlobalContextRelease" releaseContext :: Reference -> IO ()
foreign import ccall unsafe "JSContextGetGlobalObject" globalObject :: Reference -> IO Reference
foreign import ccall unsafe "JSStringCreateWithCharacters" createString :: Ptr Word16 -> CSize -> IO Reference
foreign import ccall unsafe "JSStringRelease" releaseString :: Reference -> IO ()
foreign import ccall unsafe "JSStringGetMaximumUTF8CStringSize" stringCapacity :: Reference -> IO CSize
foreign import ccall unsafe "JSStringGetUTF8CString" copyString :: Reference -> CString -> CSize -> IO CSize
foreign import ccall safe "JSValueToStringCopy" valueString :: Reference -> Reference -> Ptr Reference -> IO Reference
foreign import ccall unsafe "JSValueMakeString" makeString :: Reference -> Reference -> IO Reference
foreign import ccall unsafe "JSValueMakeUndefined" makeUndefined :: Reference -> IO Reference
foreign import ccall unsafe "JSValueProtect" protectValue :: Reference -> Reference -> IO ()
foreign import ccall unsafe "JSValueUnprotect" unprotectValue :: Reference -> Reference -> IO ()
foreign import ccall unsafe "JSObjectMakeFunctionWithCallback" makeFunction :: Reference -> Reference -> FunPtr Callback -> IO Reference
foreign import ccall safe "JSObjectSetProperty" setProperty :: Reference -> Reference -> Reference -> Reference -> CUInt -> Ptr Reference -> IO ()
foreign import ccall safe "JSEvaluateScript" evaluateScript :: Reference -> Reference -> Reference -> Reference -> CInt -> Ptr Reference -> IO Reference
foreign import ccall safe "JSObjectCallAsFunction" callFunction :: Reference -> Reference -> Reference -> CSize -> Ptr Reference -> Ptr Reference -> IO Reference
foreign import ccall "wrapper" wrapCallback :: Callback -> IO (FunPtr Callback)

data Runtime = Runtime Reference (FunPtr Callback) (IORef Callback)

-- | Caller must not use the runtime from another thread or retain it afterward.
withRuntime :: (Runtime -> IO a) -> IO a
withRuntime action = runInBoundThread $ do
    dispatch <- newIORef (\context _ _ _ _ _ -> makeUndefined context)
    let callback context function object count arguments exception = do
            current <- readIORef dispatch
            current context function object count arguments exception
    bracket (wrapCallback callback) freeHaskellFunPtr $ \function ->
        bracket createGroup releaseGroup $ \group ->
            action (Runtime group function dispatch)

withString :: String -> (Reference -> IO a) -> IO a
withString value action =
    BS.useAsCStringLen (Text.encodeUtf16LE (Text.pack value)) $ \(bytes, size) ->
        bracket (createString (castPtr bytes) (fromIntegral (size `div` 2))) releaseString action

readString :: Reference -> IO String
readString value = do
    capacity <- stringCapacity value
    allocaBytes (fromIntegral capacity) $ \buffer -> do
        count <- copyString value buffer capacity
        bytes <- BS.packCStringLen (buffer, max 0 (fromIntegral count - 1))
        pure (Text.unpack (Text.decodeUtf8 bytes))

readValue :: Reference -> Reference -> IO String
readValue context value =
    bracket (valueString context value nullPtr)
        (\string -> when (string /= nullPtr) (releaseString string)) $ \string ->
        if string == nullPtr then pure "<unprintable JavaScript value>" else readString string

checked :: Reference -> (Ptr Reference -> IO a) -> IO a
checked context action = alloca $ \exception -> do
    poke exception nullPtr
    result <- action exception
    failure <- peek exception
    when (failure /= nullPtr) $ readValue context failure >>= ioError . userError
    pure result

evaluate :: Reference -> String -> IO ()
evaluate context source = withString source $ \script ->
    void (checked context (evaluateScript context script nullPtr nullPtr 1))

data Pending = Pending Reference Reference String

executeCell :: Runtime -> String -> IO (Either String [String])
executeCell runtime = executeCellWith runtime (pure . Right)

-- | Tool handler consumes/returns JSON payloads, not request envelopes.
-- Pending Promise functions are protected until handled or cell cleanup.
-- Handlers run after evaluation returns, not inside a JavaScript callback.
executeCellWith :: Runtime -> (String -> IO (Either String String)) -> String
    -> IO (Either String [String])
executeCellWith (Runtime group function currentCallback) handler source = do
    outcome <- try $ bracket (createContext group nullPtr) releaseContext $ \context -> do
        output <- newIORef []
        completed <- newIORef Nothing
        pending <- newIORef Sequence.empty
        callbackFailure <- newIORef Nothing
        let releasePending (Pending resolve reject _) =
                unprotectValue context resolve >> unprotectValue context reject
            cleanup = readIORef pending >>= mapM_ releasePending
            dispatch arguments = case arguments of
                [operation, value, resolve, reject] -> do
                    name <- readValue context operation
                    case name of
                        "tool" -> do
                            payload <- readValue context value
                            protectValue context resolve
                            protectValue context reject
                            modifyIORef' pending (Sequence.|> Pending resolve reject payload)
                        _ -> ioError (userError "invalid native operation")
                [operation, value] -> do
                    name <- readValue context operation
                    case name of
                        "text" -> readValue context value >>= \text -> modifyIORef' output (text :)
                        "done" -> writeIORef completed (Just (Right ()))
                        "error" -> readValue context value >>= writeIORef completed . Just . Left
                        _ -> ioError (userError "invalid native operation")
                _ -> ioError (userError "invalid native argument count")
            callback _ _ _ count arguments _ = do
                (peekArray (fromIntegral count) arguments >>= dispatch)
                    `catch` \(failure :: SomeException) ->
                        writeIORef callbackFailure (Just (displayException failure))
                makeUndefined context
            resolvePending entry@(Pending resolve reject payload) =
                (do
                    response <- handler payload
                    let (continuation, text) = either (\err -> (reject, err)) (\value -> (resolve, value)) response
                    withString text $ \string -> do
                        value <- makeString context string
                        withArray [value] $ \arguments ->
                            void (checked context (callFunction context continuation nullPtr 1 arguments)))
                `finally` releasePending entry
            drain = do
                failure <- readIORef callbackFailure
                case failure of
                    Just message -> pure (Left message)
                    Nothing -> do
                        state <- readIORef completed
                        case state of
                            Just (Left message) -> pure (Left message)
                            Just (Right ()) -> Right . reverse <$> readIORef output
                            Nothing -> do
                                -- Remove one request at a time so exceptions leave
                                -- every remaining protected value owned by cleanup.
                                next <- atomicModifyIORef' pending $ \entries -> case Sequence.viewl entries of
                                    Sequence.EmptyL -> (Sequence.empty, Nothing)
                                    first Sequence.:< rest -> (rest, Just first)
                                case next of
                                    Nothing -> pure (Left "execution suspended without a pending tool request")
                                    Just entry -> resolvePending entry >> drain
        -- The runtime owns the trampoline until its context group is released.
        -- Only the active cell's closure is retained between callback entries.
        bracket
            (writeIORef currentCallback callback)
            (\() -> writeIORef currentCallback (\ctx _ _ _ _ _ -> makeUndefined ctx))
            $ \() ->
            (do
                global <- globalObject context
                withString "__native" $ \name -> do
                    native <- makeFunction context name function
                    checked context (setProperty context global name native 0)
                evaluate context (bootstrap <> "\n(async () => {\n\"use strict\";\n" <> source
                    <> "\n})().then(() => __native('done', ''), error => __native('error', String(error)));")
                drain)
            `finally` cleanup
    pure $ either (Left . displayException) id (outcome :: Either SomeException (Either String [String]))

bootstrap :: String
bootstrap = unlines
    [ "globalThis.text = value => __native('text', typeof value === 'string' ? value : JSON.stringify(value));"
    , "globalThis.tools = Object.freeze({"
    , "  echo: value => new Promise((resolve, reject) =>"
    , "    __native('tool', JSON.stringify(value), result => resolve(JSON.parse(result)), reject)),"
    , "  reject: value => Promise.reject(new Error(String(value)))"
    , "});"
    ]
