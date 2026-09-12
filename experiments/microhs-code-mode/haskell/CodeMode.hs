-- Standalone, sequential prototype. This deliberately does not
-- expose the production host or grant access to real tools.
module CodeMode
    ( module CodeMode.Json
    , callTool
    , callToolWithDecoder
    , emit
    , runCell
    ) where

import CodeMode.Json
-- MicroHs ships Control.Exception but not the safe-exceptions package.
-- All worker exceptions become protocol errors; process cancellation belongs
-- to the external host. This module does not start concurrent computations.
import Control.Exception (SomeException, catch, evaluate)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)

-- One invocation counter per worker process, reset for every runCell.
-- Persistent workers serialize cells; concurrent cells are unsupported.
{-# NOINLINE invocationCounter #-}
invocationCounter :: IORef Int
invocationCounter = unsafePerformIO (newIORef 0)

send :: Json -> IO ()
send value = do
    let encoded = encodeJson value
    -- Force the entire frame before writing, so a pure exception cannot leave
    -- a truncated JSON frame in front of the subsequent failure response.
    _ <- evaluate (length encoded)
    putStrLn encoded
    hFlush stdout

frame :: [(String, Json)] -> Json
frame fields = JsonObject (("jsonrpc", JsonString "2.0") : fields)

callTool :: String -> Json -> IO Json
callTool = callToolWithDecoder (pure . decodeJson)

callToolWithDecoder :: (String -> IO (Either String Json)) -> String -> Json -> IO Json
callToolWithDecoder decoder name arguments = do
    previous <- readIORef invocationCounter
    let sequenceNumber = previous + 1
    writeIORef invocationCounter sequenceNumber
    let identifier = JsonString ("tool-" ++ show sequenceNumber)
    send (frame
        [ ("id", identifier)
        , ("method", JsonString "tool/call")
        , ("params", JsonObject
            [ ("name", JsonString name)
            , ("arguments", arguments)
            ])
        ])
    line <- getLine
    decoded <- decoder line
    case decoded of
        Left message -> ioError (userError message)
        Right response
            | lookupField "jsonrpc" response /= Just (JsonString "2.0") ->
                ioError (userError "unsupported host JSON-RPC version")
            | lookupField "id" response /= Just identifier ->
                ioError (userError "host response identifier mismatch")
            | otherwise ->
                case (lookupField "result" response, lookupField "error" response) of
                    (Just result, Nothing) -> pure result
                    (Nothing, Just failure) ->
                        case lookupField "message" failure of
                            Just (JsonString message) -> ioError (userError message)
                            _ -> ioError (userError "invalid host error response")
                    _ -> ioError (userError "host response requires result or error")

emit :: Json -> IO ()
emit value = send (frame
    [ ("method", JsonString "content")
    , ("params", JsonObject [("value", value)])
    ])

runCell :: IO () -> IO ()
runCell action = catch execute reportFailure
  where
    execute = do
        writeIORef invocationCounter 0
        send (frame [("method", JsonString "ready")])
        action
        send (frame
            [ ("id", JsonString "cell-1")
            , ("result", JsonObject [("content", JsonArray [])])
            ])
    reportFailure :: SomeException -> IO ()
    reportFailure exception = send (frame
        [ ("id", JsonString "cell-1")
        , ("error", JsonObject
            [ ("code", JsonNumber "-32000")
            , ("message", JsonString (show exception))
            ])
        ])
