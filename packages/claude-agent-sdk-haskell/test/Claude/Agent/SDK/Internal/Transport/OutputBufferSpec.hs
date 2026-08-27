module Claude.Agent.SDK.Internal.Transport.OutputBufferSpec
    ( spec
    ) where

import Claude.Agent.SDK.Internal.Transport.OutputBuffer
    ( OutputBuffer
    , OutputLine(..)
    , OutputReadResult(..)
    , appendOutputChunk
    , emptyOutputBuffer
    , finishOutputBuffer
    , outputPartialLength
    , readBufferedLine
    , takeOutputLine
    )
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString8
import Data.IORef
    ( atomicModifyIORef'
    , newIORef
    )
import System.IO.Error (eofErrorType, mkIOError)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )

spec :: Spec
spec =
    describe "subprocess output buffering" do
        it "assembles a line split across 8 KiB reads" do
            let prefix = ByteString.replicate 8_192 97
                suffix = ByteString8.pack "tail\n"
                buffered =
                    appendOutputChunk
                        (appendOutputChunk emptyOutputBuffer prefix)
                        suffix
            case takeOutputLine 8_196 buffered of
                Just (OutputLine line remaining) -> do
                    line `shouldBe` prefix <> "tail"
                    outputPartialLength remaining `shouldBe` 0
                _ ->
                    fail "expected a complete line"

        it "retains and reuses every line and suffix from one chunk" do
            let buffered =
                    appendOutputChunk
                        emptyOutputBuffer
                        "first\nsecond\npartial"
            (first, afterFirst) <- expectLine 100 buffered
            first `shouldBe` "first"
            (second, afterSecond) <- expectLine 100 afterFirst
            second `shouldBe` "second"
            takeOutputLine 100 afterSecond `shouldBeNoLine` ()
            let (trailing, empty) = finishOutputBuffer afterSecond
            trailing `shouldBe` Just "partial"
            outputPartialLength empty `shouldBe` 0

        it "returns an empty record for a leading newline" do
            let buffered =
                    appendOutputChunk emptyOutputBuffer "\nnext"
            (first, remaining) <- expectLine 100 buffered
            first `shouldBe` ""
            finishOutputBuffer remaining `shouldBeFinal` Just "next"

        it "does not invent an empty record after a trailing newline" do
            let buffered =
                    appendOutputChunk emptyOutputBuffer "last\n"
            (line, remaining) <- expectLine 100 buffered
            line `shouldBe` "last"
            takeOutputLine 100 remaining `shouldBeNoLine` ()
            finishOutputBuffer remaining `shouldBeFinal` Nothing

        it "drains several queued lines in their original order" do
            let buffered =
                    appendOutputChunk
                        emptyOutputBuffer
                        "one\ntwo\nthree\nfour\n"
            (one, afterOne) <- expectLine 100 buffered
            (two, afterTwo) <- expectLine 100 afterOne
            (three, afterThree) <- expectLine 100 afterTwo
            (four, afterFour) <- expectLine 100 afterThree
            [one, two, three, four]
                `shouldBe` ["one", "two", "three", "four"]
            takeOutputLine 100 afterFour `shouldBeNoLine` ()

        it "accepts a record exactly at the configured limit" do
            let buffered =
                    appendOutputChunk
                        emptyOutputBuffer
                        (ByteString.replicate 64 120 <> "\n")
            case takeOutputLine 64 buffered of
                Just (OutputLine line _) ->
                    ByteString.length line `shouldBe` 64
                _ ->
                    fail "expected the boundary-sized line"

        it "rejects a newline-terminated record one byte over the limit" do
            let buffered =
                    appendOutputChunk
                        emptyOutputBuffer
                        (ByteString.replicate 65 120 <> "\n")
            case takeOutputLine 64 buffered of
                Just (OutputLineTooLarge _) ->
                    pure () :: IO ()
                _ ->
                    fail "expected an oversized line"

        it "retains records queued after an oversized line" do
            let buffered =
                    appendOutputChunk
                        emptyOutputBuffer
                        ( "ok\n"
                            <> ByteString.replicate 65 120
                            <> "\nafter\n"
                        )
            (first, afterFirst) <- expectLine 64 buffered
            first `shouldBe` "ok"
            afterOversized <-
                case takeOutputLine 64 afterFirst of
                    Just (OutputLineTooLarge remaining) ->
                        pure remaining
                    _ ->
                        fail "expected the queued oversized line"
            (lastLine, empty) <- expectLine 64 afterOversized
            lastLine `shouldBe` "after"
            takeOutputLine 64 empty `shouldBeNoLine` ()

        it "returns a final record when EOF has no newline" do
            let buffered =
                    appendOutputChunk
                        (appendOutputChunk emptyOutputBuffer "final")
                        "-record"
                (trailing, empty) = finishOutputBuffer buffered
            trailing `shouldBe` Just "final-record"
            finishOutputBuffer empty `shouldBeFinal` Nothing

        it "returns a buffered partial record for an IOException EOF" do
            bufferRef <-
                newIORef $
                    appendOutputChunk emptyOutputBuffer "partial"
            result <-
                readBufferedLine bufferRef 100 \_ ->
                    ioError $
                        mkIOError eofErrorType "test EOF" Nothing Nothing
            case result of
                OutputReadLine line ->
                    line `shouldBe` "partial"
                _ ->
                    fail "expected the partial line at EOF"

        it "drains queued lines before finishing the EOF partial record" do
            bufferRef <- newIORef emptyOutputBuffer
            readsRef <-
                newIORef
                    [ Just "one\ntwo\npartial"
                    , Nothing
                    ]
            let readChunk _ =
                    atomicModifyIORef' readsRef \case
                        [] ->
                            ([], "")
                        Nothing : remaining ->
                            (remaining, "")
                        Just chunk : remaining ->
                            (remaining, chunk)
                expectReadLine expected = do
                    result <-
                        readBufferedLine bufferRef 100 readChunk
                    case result of
                        OutputReadLine line ->
                            line `shouldBe` expected
                        _ ->
                            fail "expected a buffered line"
            expectReadLine "one"
            expectReadLine "two"
            expectReadLine "partial"
            readBufferedLine bufferRef 100 readChunk
                `shouldReturnReadEnd` ()

        it "rejects an oversized partial before an IOException EOF read" do
            bufferRef <-
                newIORef $
                    appendOutputChunk
                        emptyOutputBuffer
                        (ByteString.replicate 65 120)
            result <-
                readBufferedLine bufferRef 64 \_ ->
                    ioError $
                        mkIOError eofErrorType "test EOF" Nothing Nothing
            case result of
                OutputReadTooLarge ->
                    pure () :: IO ()
                _ ->
                    fail "expected the oversized partial to be rejected"

        it "keeps a partial record available after a retryable IOException" do
            bufferRef <-
                newIORef $
                    appendOutputChunk emptyOutputBuffer "partial"
            failed <-
                readBufferedLine bufferRef 100 \_ ->
                    ioError (userError "temporary read failure")
            case failed of
                OutputReadFailure _ ->
                    pure () :: IO ()
                _ ->
                    fail "expected a read failure"
            retried <-
                readBufferedLine bufferRef 100 \_ ->
                    pure "\n"
            case retried of
                OutputReadLine line ->
                    line `shouldBe` "partial"
                _ ->
                    fail "expected retry to reuse the partial line"

expectLine
    :: Int
    -> OutputBuffer
    -> IO
        ( ByteString.ByteString
        , OutputBuffer
        )
expectLine maximumBytes buffered =
    case takeOutputLine maximumBytes buffered of
        Just (OutputLine line remaining) ->
            pure (line, remaining)
        _ ->
            fail "expected a complete line"

shouldBeNoLine :: Maybe OutputLine -> () -> IO ()
shouldBeNoLine value _ =
    case value of
        Nothing -> pure ()
        Just _ -> fail "expected no complete line"

shouldBeFinal
    :: (Maybe ByteString.ByteString, buffer)
    -> Maybe ByteString.ByteString
    -> IO ()
shouldBeFinal (actual, _) expected =
    actual `shouldBe` expected

shouldReturnReadEnd :: IO OutputReadResult -> () -> IO ()
shouldReturnReadEnd action _ =
    action >>= \case
        OutputReadEnd -> pure ()
        _ -> fail "expected end of output"
