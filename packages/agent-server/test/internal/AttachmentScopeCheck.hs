-- Tests the internal module without widening the library's public API.
-- nix develop -c cabal repl agent-server:test:agent-server-attachment-test
-- :main
module Main (main) where

import Agent.Loop qualified as Loop
import Agent.Server.Identifier (newUUIDv7Text)
import Agent.Server.Runtime.Attachments
    ( TurnModelInput(..)
    , inlineTurnFiles
    , withMaterializedTurnFiles
    )
import Agent.Server.Types
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (cancel, waitCatch, withAsync)
import Control.Exception.Safe (bracket, tryAny)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Text qualified as Text
import System.Directory
import System.FilePath ((</>))
import System.Timeout (timeout)

main :: IO ()
main = withWorkspace \cwd -> do
    let attachments = cwd </> ".haskell-agent" </> "attachments"
        spec = TurnSpec
            { turnSpecSessionId = "session"
            , turnSpecClientRequestId = ClientRequestId "request"
            , turnSpecPrompt = "Inspect this"
            , turnSpecImages = []
            , turnSpecFiles = [FileAttachment "hello.txt" "text/plain" "hello"]
            , turnSpecBoundary = accessBoundary localPrincipal (GatewayBoundary Nothing)
            , turnSpecMessageClock = Nothing
            }
        empty = do
            remaining <- listDirectory attachments
            check (null remaining) "upload directory leaked"
        inspect = do
            entries <- listDirectory attachments
            case entries of
                [entry] -> do
                    contents <- BS.readFile (attachments </> entry </> "1-hello.txt")
                    check (contents == "hello") "attachment content changed"
                _ -> fail "expected one upload directory during callback"
    result <- withMaterializedTurnFiles cwd spec \prompt -> do
        inspect
        check ("Inspect this" `Text.isPrefixOf` prompt) "prompt changed"
        pure (Right ())
    check (result == Right ()) "successful callback failed"
    empty
    failed <- withMaterializedTurnFiles cwd spec (const (pure (Left "callback failed" :: Either Text.Text ())))
    check (failed == Left "callback failed") "callback error changed"
    empty
    thrown <- tryAny (withMaterializedTurnFiles cwd spec (const (fail "callback exception" :: IO (Either Text.Text ()))))
    check (isLeft thrown) "callback exception swallowed"
    empty
    entered <- newEmptyMVar
    blocked <- newEmptyMVar
    withAsync (withMaterializedTurnFiles cwd spec \_ -> do
        inspect
        putMVar entered ()
        _ <- takeMVar blocked
        pure (Right ())) \worker -> do
            ready <- timeout 5_000_000 (takeMVar entered)
            check (ready == Just ()) "callback did not start"
            cancel worker
            outcome <- waitCatch worker
            check (isLeft outcome) "cancellation swallowed"
    empty
    let invalid = spec
            { turnSpecFiles = spec.turnSpecFiles <>
                [FileAttachment (Text.replicate 300 "x") "text/plain" "bad"]
            }
    writeFailure <- withMaterializedTurnFiles cwd invalid (const (fail "callback ran after write failure" :: IO (Either Text.Text ())))
    check (writeFailure == Left "could not materialize the uploaded files") "write error changed"
    empty
    untouched <- withMaterializedTurnFiles (cwd </> "absent") (spec {turnSpecFiles = []}) (pure . Right)
    check (untouched == Right "Inspect this") "empty attachment path changed"
    let inline = inlineTurnFiles spec
            { turnSpecPrompt = "Book this"
            , turnSpecFiles =
                [ FileAttachment "receipt.pdf" "application/pdf" "%PDF-1.7"
                , FileAttachment "photo.jpg" "image/JPEG" "\xff\xd8\xff"
                , FileAttachment "notes.txt" "text/plain" "hello"
                ]
            }
    case inline.modelImages of
        [Loop.ImageAttachment mime bytes] ->
            check (mime == "image/jpeg" && bytes == "\xff\xd8\xff") "inline image changed"
        _ -> fail "expected one inline image"
    case inline.modelFiles of
        [Loop.FileAttachment name mime bytes] ->
            check
                (name == Just "receipt.pdf"
                    && mime == "application/pdf"
                    && bytes == "%PDF-1.7")
                "inline PDF changed"
        _ -> fail "expected one inline PDF"
    check ("Book this" `Text.isPrefixOf` inline.modelPrompt) "inline prompt changed"
    check ("- notes.txt (text/plain)" `Text.isInfixOf` inline.modelPrompt) "unreadable upload not named"
    check (not ("receipt.pdf" `Text.isInfixOf` inline.modelPrompt)) "readable upload named as unreadable"
    listDirectory attachments >>= \entries ->
        check (null entries) "inline uploads touched the workspace"
    putStrLn "Attachment scope: 12 checks passed"

check :: Bool -> String -> IO ()
check condition message = unless condition (fail message)

withWorkspace :: (FilePath -> IO a) -> IO a
withWorkspace action = do
    temporary <- getTemporaryDirectory
    name <- Text.unpack <$> newUUIDv7Text
    let path = temporary </> ("attachment-scope-" <> name)
    bracket (createDirectory path >> pure path) removePathForcibly action
