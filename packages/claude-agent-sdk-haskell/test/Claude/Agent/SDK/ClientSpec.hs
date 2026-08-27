module Claude.Agent.SDK.ClientSpec (spec) where

import Claude.Agent.SDK
import Claude.Agent.SDK.TestSupport
import Control.Concurrent
    ( forkFinally
    , forkIO
    , killThread
    , newEmptyMVar
    , putMVar
    , takeMVar
    , threadDelay
    , tryPutMVar
    )
import Control.Monad (void, when)
import Control.Exception.Safe (finally, tryAny)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import Data.Foldable (toList)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , modifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Directory (canonicalizePath, doesFileExist)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Posix.Signals (sigKILL, signalProcess)
import System.Posix.Types (ProcessID)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "ClaudeSDKClient subprocess transport" do
    it "passes typed options and SDK environment metadata to Claude Code" do
        withFakeClaude transportProbeScript \directory executable -> do
            let argsPath = directory </> "args"
                environmentPath = directory </> "environment"
                inputPath = directory </> "input"
                options =
                    (testOptions executable directory)
                        { systemPrompt =
                            SystemPromptText "system prompt"
                        , tools = Just []
                        , allowedTools = ["Read", "Grep"]
                        , disallowedTools = ["Bash"]
                        , permissionMode =
                            Just PermissionBypassPermissions
                        , settingSources = Just []
                        , strictMcpConfig = True
                        , includePartialMessages = True
                        , safeMode = True
                        , disableSlashCommands = True
                        , noChrome = True
                        , model = Just "claude-test"
                        , effort = Just "high"
                        , environment =
                            Just
                                [ ("FAKE_ARGS", argsPath)
                                , ("FAKE_ENVIRONMENT", environmentPath)
                                , ("FAKE_INPUT", inputPath)
                                , ("CLAUDECODE", "must-not-leak")
                                , ("CLAUDE_CODE_ENTRYPOINT", "wrong")
                                ]
                        , clientApplication = Just "sdk-test-client"
                        }

            queryResult <- query options "hello from Haskell" (const (pure ()))
            _ <- expectRight queryResult

            lines <$> readFile argsPath
                `shouldReturn` expectedArguments

            canonicalDirectory <- canonicalizePath directory
            lines <$> readFile environmentPath
                `shouldReturn`
                    [ "CLAUDE_CODE_ENTRYPOINT=sdk-cli"
                    , "CLAUDE_AGENT_SDK_CLIENT_APP=sdk-test-client"
                    , "CLAUDE_AGENT_SDK_VERSION=0.1.0.0"
                    , "CLAUDECODE=unset"
                    , "PWD=" <> canonicalDirectory
                    ]

            input <- ByteString.readFile inputPath
            Aeson.eitherDecodeStrict' input
                `shouldBe` Right (expectedQueryValue "hello from Haskell")

    it "serializes image content in streaming user messages" do
        withFakeClaude transportProbeScript \directory executable -> do
            let argsPath = directory </> "args"
                environmentPath = directory </> "environment"
                inputPath = directory </> "input"
                options =
                    (testOptions executable directory)
                        { environment =
                            Just
                                [ ("FAKE_ARGS", argsPath)
                                , ("FAKE_ENVIRONMENT", environmentPath)
                                , ("FAKE_INPUT", inputPath)
                                ]
                        }

            queryResult <-
                queryContent
                    options
                    [ UserImageBlock
                        { mediaType = "image/png"
                        , imageBytes = "png-bytes"
                        }
                    , UserTextBlock "describe this image"
                    ]
                    (const (pure ()))
            _ <- expectRight queryResult

            input <- ByteString.readFile inputPath
            Aeson.eitherDecodeStrict' input
                `shouldBe`
                    Right
                        (expectedContentQueryValue
                            [ Aeson.object
                                [ "type" Aeson..= ("image" :: Text)
                                , "source" Aeson..= Aeson.object
                                    [ "type" Aeson..= ("base64" :: Text)
                                    , "media_type" Aeson..=
                                        ("image/png" :: Text)
                                    , "data" Aeson..=
                                        ("cG5nLWJ5dGVz" :: Text)
                                    ]
                                ]
                            , Aeson.object
                                [ "type" Aeson..= ("text" :: Text)
                                , "text" Aeson..=
                                    ("describe this image" :: Text)
                                ]
                            ])

    it "supports minimal and full Claude Code system prompts" do
        mapM_
            (\(promptMode, expectMinimalPrompt) ->
                withFakeClaude transportProbeScript \directory executable -> do
                    let argsPath = directory </> "args"
                        environmentPath = directory </> "environment"
                        inputPath = directory </> "input"
                        options =
                            (testOptions executable directory)
                                { systemPrompt = promptMode
                                , environment =
                                    Just
                                        [ ("FAKE_ARGS", argsPath)
                                        , ( "FAKE_ENVIRONMENT"
                                          , environmentPath
                                          )
                                        , ("FAKE_INPUT", inputPath)
                                        ]
                                }
                    result <-
                        query options "system prompt mode" (const (pure ()))
                    _ <- expectRight result
                    arguments <- lines <$> readFile argsPath
                    if expectMinimalPrompt
                        then
                            arguments
                                `shouldContain` ["--system-prompt", ""]
                        else
                            arguments
                                `shouldNotContain` ["--system-prompt"]
            )
            [ (SystemPromptNone, True)
            , (SystemPromptClaudeCode, False)
            ]

    it "rejects structured output records larger than the configured limit" do
        withFakeClaude
            (oneShotScript [successResult "too large"])
            \directory executable -> do
                result <-
                    query
                        ( (testOptions executable directory)
                            { maxBufferSizeBytes = 64 }
                        )
                        "bounded output"
                        (const (pure ()))
                result `shouldSatisfy` \case
                    Left (CLIProtocolError message) ->
                        "larger than 64 bytes"
                            `Text.isInfixOf` message
                    _ -> False

    it "assembles one structured record emitted across many subprocess writes" do
        let reply = Text.replicate 20_000 "x"
        withFakeClaude
            (multiWriteScript (successResult reply))
            \directory executable -> do
                result <-
                    query
                        (testOptions executable directory)
                        "split output"
                        (const (pure ()))
                fmap resultText result
                    `shouldBe` Right (Just reply)

    it "aborts a blocked subprocess output read promptly" do
        withFakeClaude blockedOutputScript \directory executable -> do
            turnFinished <- newEmptyMVar
            let readyPath = directory </> "query-read"
                options =
                    (testOptions executable directory)
                        { streamStartupTimeoutMicros =
                            30 * 1_000_000
                        , turnTimeoutMicros =
                            30 * 1_000_000
                        , environment =
                            Just [("FAKE_READY", readyPath)]
                        }
            withClaudeSDKClient options \client -> do
                _ <-
                    forkIO do
                        result <-
                            withClaudeSDKTurn
                                client
                                (pure True)
                                Nothing
                                Nothing
                                Nothing
                                \turn -> do
                                    completed <-
                                        queryTurn
                                            turn
                                            "blocked output"
                                            (const (pure ()))
                                    pure ((, pure ()) <$> completed)
                        putMVar turnFinished result

                timeout 4_000_000 (waitForPath readyPath)
                    `shouldReturn` Just ()
                timeout 4_000_000 (abort client)
                    `shouldReturn` Just ()
                timeout 2_000_000 (takeMVar turnFinished)
                    `shouldReturn` Just
                        (Left
                            (CLIConnectionError
                                "Claude SDK transport was closed while reading."))

    it "aborts a blocked subprocess input write promptly" do
        testExecutable <- getExecutablePath
        withFakeClaude blockedInputScript \directory executable -> do
            let readyPath = directory </> "detached-reader"
                options =
                    (testOptions executable directory)
                        { promptWriteTimeoutMicros =
                            30 * 1_000_000
                        , turnTimeoutMicros =
                            30 * 1_000_000
                        , environment =
                            Just
                                [ ("FAKE_HELPER", testExecutable)
                                , ("FAKE_READY", readyPath)
                                ]
                        }
                prompt =
                    Text.replicate
                        (20 * 1_024 * 1_024)
                        "x"
                cleanup =
                    killRecordedProcess readyPath

            (withClaudeSDKClient options \client -> do
                turnFinished <- newEmptyMVar
                _ <-
                    forkIO do
                        result <-
                            withClaudeSDKTurn
                                client
                                (pure True)
                                Nothing
                                Nothing
                                Nothing
                                \turn -> do
                                    completed <-
                                        queryTurn
                                            turn
                                            prompt
                                            (const (pure ()))
                                    pure ((, pure ()) <$> completed)
                        putMVar turnFinished result

                timeout 4_000_000 (waitForPath readyPath)
                    `shouldReturn` Just ()
                aborted <- timeout 4_000_000 (abort client)
                when (aborted == Nothing) cleanup
                aborted `shouldBe` Just ()
                timeout 2_000_000 (takeMVar turnFinished)
                    `shouldReturn` Just
                        (Left
                            (CLIConnectionError
                                "Claude SDK transport was closed while writing.")))
                `finally` cleanup

    it "supports continue and named resume without combining session flags" do
        mapM_
            (\(configure, expectedStart) ->
                withFakeClaude transportProbeScript \directory executable -> do
                    let argsPath = directory </> "args"
                        environmentPath = directory </> "environment"
                        inputPath = directory </> "input"
                        options =
                            configure
                                ( (testOptions executable directory)
                                    { sessionId = Nothing
                                    , environment =
                                        Just
                                            [ ("FAKE_ARGS", argsPath)
                                            , ( "FAKE_ENVIRONMENT"
                                              , environmentPath
                                              )
                                            , ("FAKE_INPUT", inputPath)
                                            ]
                                    }
                                )
                    result <-
                        query options "resume prompt" (const (pure ()))
                    _ <- expectRight result
                    arguments <- lines <$> readFile argsPath
                    arguments `shouldContain` [expectedStart]
                    arguments `shouldNotContain` ["--session-id"]
                    input <- ByteString.readFile inputPath
                    querySessionId input `shouldBe` Right ""
            )
            [ ( \options ->
                    options { continueConversation = True }
              , "--continue"
              )
            , ( \options ->
                    options { resume = Just "named conversation" }
              , "--resume=named conversation"
              )
            ]

    it "rejects conflicting continue options before starting Claude Code" do
        withFakeClaude transportProbeScript \directory executable -> do
            result <-
                query
                    ( (testOptions executable directory)
                        { continueConversation = True }
                    )
                    "invalid options"
                    (const (pure ()))
            result `shouldBe`
                Left
                    (CLIProtocolError
                        "continueConversation cannot be combined with resume or sessionId.")

    it "reuses one persistent process for consecutive turns in a session" do
        withFakeClaude persistentScript \directory executable -> do
            let startsPath = directory </> "starts"
                inputsPath = directory </> "inputs"
                options =
                    (testOptions executable directory)
                        { environment =
                            Just
                                [ ("FAKE_STARTS", startsPath)
                                , ("FAKE_INPUTS", inputsPath)
                                ]
                        }
            callbackMessages <- newIORef []

            (first, second) <-
                withClaudeSDKClient options \client -> do
                    first <-
                        runTurn
                            client
                            callbackMessages
                            Nothing
                            "first prompt"
                    second <-
                        runTurn
                            client
                            callbackMessages
                            Nothing
                            "second prompt"
                    pure (first, second)

            fmap fst first `shouldBe` Right True
            fmap fst second `shouldBe` Right False
            fmap (resultText . snd) first
                `shouldBe` Right (Just "reply-1")
            fmap (resultText . snd) second
                `shouldBe` Right (Just "reply-2")

            lines <$> readFile startsPath `shouldReturn` ["started"]
            inputLines <- lines <$> readFile inputsPath
            traverse decodePrompt inputLines
                `shouldBe` Right ["first prompt", "second prompt"]

            messages <- readIORef callbackMessages
            let assistantTexts =
                    [ text
                    | MessageAssistant AssistantMessage{content} <- messages
                    , TextBlock{text} <- content
                    ]
            assistantTexts `shouldBe` ["reply-1", "reply-2"]

    it "reports a missing Claude Code executable as CLINotFoundError" do
        withFakeClaude "#!/bin/sh\nexit 0\n" \directory _ -> do
            let missing = directory </> "does-not-exist"
            result <-
                query
                    (testOptions missing directory)
                    "hello"
                    (const (pure ()))
            result `shouldBe` Left (CLINotFoundError missing)

    it "distinguishes a missing working directory from a missing executable" do
        withFakeClaude "#!/bin/sh\nexit 0\n" \directory executable -> do
            let missingDirectory = directory </> "missing-directory"
            result <-
                query
                    (testOptions executable missingDirectory)
                    "hello"
                    (const (pure ()))
            result `shouldSatisfy` \case
                Left (CLIConnectionError message) ->
                    "working directory does not exist"
                        `Text.isInfixOf` message
                        && Text.pack missingDirectory
                            `Text.isInfixOf` message
                _ -> False

    it "closes a custom transport when connect throws" do
        closeCount <- newIORef (0 :: Int)
        let options = testOptions "unused-by-custom-transport" "."
            transportFactory _ =
                pure Transport
                    { transportConnect =
                        ioError (userError "connect failed")
                    , transportWrite = \_ -> pure (Right ())
                    , transportRead = pure (Right Nothing)
                    , transportClose =
                        modifyIORef' closeCount (+ 1)
                    , transportIsReady = pure False
                    , transportEndInput = pure ()
                    , transportProcessExit = pure Nothing
                    , transportDiagnostic = pure ""
                    }

        result <-
            withClaudeSDKClientWithTransport
                options
                transportFactory
                \client ->
                    withClaudeSDKTurn
                        client
                        (pure True)
                        Nothing
                        Nothing
                        Nothing
                        \_ -> pure (Right ((), pure ()))

        result `shouldSatisfy` \case
            Left (CLIConnectionError message) ->
                "connect failed" `Text.isInfixOf` message
            _ -> False
        readIORef closeCount `shouldReturn` 1

    it "closes a custom transport when graceful shutdown throws" do
        closeCount <- newIORef (0 :: Int)
        let options = testOptions "unused-by-custom-transport" "."
            transportFactory request = do
                recordsRef <-
                    newIORef
                        [ resultLine
                            (transportModeSessionId request.transportMode)
                            "done"
                        ]
                pure Transport
                    { transportConnect = pure (Right ())
                    , transportWrite = \_ -> pure (Right ())
                    , transportRead =
                        atomicModifyIORef' recordsRef \case
                            [] -> ([], Right Nothing)
                            record : remaining ->
                                ( remaining
                                , Right
                                    (Just
                                        (TextEncoding.encodeUtf8 record))
                                )
                    , transportClose =
                        modifyIORef' closeCount (+ 1)
                    , transportIsReady = pure True
                    , transportEndInput =
                        ioError (userError "shutdown failed")
                    , transportProcessExit = pure Nothing
                    , transportDiagnostic = pure ""
                    }

        outcome <-
            tryAny $
                withClaudeSDKClientWithTransport
                    options
                    transportFactory
                    \client ->
                        withClaudeSDKTurn
                            client
                            (pure True)
                            Nothing
                            Nothing
                            Nothing
                            \turn -> do
                                completed <-
                                    queryTurn
                                        turn
                                        "hello"
                                        (const (pure ()))
                                pure ((, pure ()) <$> completed)

        outcome `shouldSatisfy` \case
            Left exception ->
                "shutdown failed"
                    `Text.isInfixOf` Text.pack (show exception)
            Right _ -> False
        readIORef closeCount `shouldReturn` 1

    it "runs through a caller-supplied public TransportFactory" do
        requestsRef <- newIORef []
        writesRef <- newIORef []
        messagesRef <- newIORef []
        connectCount <- newIORef (0 :: Int)
        endInputCount <- newIORef (0 :: Int)
        closeCount <- newIORef (0 :: Int)
        let options =
                (testOptions "unused-by-custom-transport" ".")
                    { model = Just "custom-model"
                    , effort = Just "medium"
                    }
            transportFactory request = do
                modifyIORef' requestsRef (<> [request])
                let sessionId = transportModeSessionId request.transportMode
                recordsRef <-
                    newIORef
                        [ assistantLine
                            sessionId
                            "custom reply"
                        , resultLine
                            sessionId
                            "custom reply"
                        ]
                pure Transport
                    { transportConnect =
                        modifyIORef' connectCount (+ 1)
                            >> pure (Right ())
                    , transportWrite = \bytes -> do
                        modifyIORef' writesRef (<> [bytes])
                        pure (Right ())
                    , transportRead =
                        atomicModifyIORef' recordsRef \case
                            [] ->
                                ([], Right Nothing)
                            record : remaining ->
                                ( remaining
                                , Right
                                    (Just
                                        (TextEncoding.encodeUtf8 record))
                                )
                    , transportClose =
                        modifyIORef' closeCount (+ 1)
                    , transportIsReady =
                        pure True
                    , transportEndInput =
                        modifyIORef' endInputCount (+ 1)
                    , transportProcessExit =
                        pure (Just ExitSuccess)
                    , transportDiagnostic =
                        pure "custom transport diagnostic"
                    }

        result <-
            withClaudeSDKClientWithTransport
                options
                transportFactory
                \client ->
                    withClaudeSDKTurn
                        client
                        (pure True)
                        Nothing
                        Nothing
                        Nothing
                        \turn -> do
                            completed <-
                                queryTurn
                                    turn
                                    "custom prompt"
                                    (\message ->
                                        modifyIORef'
                                            messagesRef
                                            (<> [message]))
                            pure ((, pure ()) <$> completed)

        fmap resultText result `shouldBe` Right (Just "custom reply")
        readIORef requestsRef `shouldReturn`
            [ TransportRequest
                { transportMode = TransportNew testSessionId
                , transportModel = Just "custom-model"
                , transportEffort = Just "medium"
                }
            ]
        writes <- readIORef writesRef
        traverse
            (decodePrompt . Text.unpack . TextEncoding.decodeUtf8)
            writes
            `shouldBe` Right ["custom prompt"]
        messages <- readIORef messagesRef
        messages `shouldSatisfy` any \case
            MessageAssistant AssistantMessage{content} ->
                content == [TextBlock "custom reply"]
            _ -> False
        readIORef connectCount `shouldReturn` 1
        readIORef endInputCount `shouldReturn` 1
        readIORef closeCount `shouldReturn` 1

    it "restarts a transport that is no longer ready" do
        requestsRef <- newIORef []
        readinessRefs <- newIORef []
        let options = testOptions "unused-by-custom-transport" "."
            transportFactory request = do
                modifyIORef' requestsRef (<> [request])
                ready <- newIORef True
                exited <- newIORef False
                modifyIORef' readinessRefs (<> [ready])
                pure Transport
                    { transportConnect =
                        pure (Right ())
                    , transportWrite =
                        \_ -> pure (Right ())
                    , transportRead =
                        pure (Right Nothing)
                    , transportClose = do
                        writeIORef ready False
                        writeIORef exited True
                    , transportIsReady =
                        readIORef ready
                    , transportEndInput = do
                        writeIORef ready False
                        writeIORef exited True
                    , transportProcessExit = do
                        hasExited <- readIORef exited
                        pure $
                            if hasExited
                                then Just ExitSuccess
                                else Nothing
                    , transportDiagnostic =
                        pure ""
                    }

        (first, second) <-
            withClaudeSDKClientWithTransport
                options
                transportFactory
                \client -> do
                    first <-
                        withClaudeSDKTurn
                            client
                            (pure True)
                            Nothing
                            Nothing
                            Nothing
                            \turn ->
                                pure $
                                    Right
                                        ( turnIsNewSession turn
                                        , pure ()
                                        )
                    readiness <- readIORef readinessRefs
                    case readiness of
                        ready : _ ->
                            writeIORef ready False
                        [] ->
                            expectationFailure
                                "expected the first transport"
                    second <-
                        withClaudeSDKTurn
                            client
                            (pure True)
                            Nothing
                            Nothing
                            Nothing
                            \turn ->
                                pure $
                                    Right
                                        ( turnIsNewSession turn
                                        , pure ()
                                        )
                    pure (first, second)

        first `shouldBe` Right True
        second `shouldBe` Right False
        readIORef requestsRef `shouldReturn`
            [ TransportRequest
                { transportMode = TransportNew testSessionId
                , transportModel = Nothing
                , transportEffort = Nothing
                }
            , TransportRequest
                { transportMode = TransportResume testSessionId
                , transportModel = Nothing
                , transportEffort = Nothing
                }
            ]

    it "retries the initial resume after a failed connection" do
        requestsRef <- newIORef []
        connectCount <- newIORef (0 :: Int)
        let options =
                (testOptions "unused-by-custom-transport" ".")
                    { sessionId = Nothing
                    , resume = Just "named conversation"
                    }
            transportFactory request = do
                modifyIORef' requestsRef (<> [request])
                ready <- newIORef False
                exited <- newIORef False
                pure Transport
                    { transportConnect = do
                        attempt <-
                            atomicModifyIORef' connectCount \count ->
                                let next = count + 1
                                in (next, next)
                        if attempt == 1
                            then
                                pure $
                                    Left $
                                        CLIConnectionError
                                            "simulated connection failure"
                            else do
                                writeIORef ready True
                                pure (Right ())
                    , transportWrite =
                        \_ -> pure (Right ())
                    , transportRead =
                        pure (Right Nothing)
                    , transportClose = do
                        writeIORef ready False
                        writeIORef exited True
                    , transportIsReady =
                        readIORef ready
                    , transportEndInput = do
                        writeIORef ready False
                        writeIORef exited True
                    , transportProcessExit = do
                        hasExited <- readIORef exited
                        pure $
                            if hasExited
                                then Just ExitSuccess
                                else Nothing
                    , transportDiagnostic =
                        pure ""
                    }

        (first, second) <-
            withClaudeSDKClientWithTransport
                options
                transportFactory
                \client -> do
                    first <-
                        withClaudeSDKTurn
                            client
                            (pure True)
                            Nothing
                            Nothing
                            Nothing
                            \_ ->
                                pure (Right ((), pure ()))
                    second <-
                        withClaudeSDKTurn
                            client
                            (pure True)
                            Nothing
                            Nothing
                            Nothing
                            \turn ->
                                pure $
                                    Right
                                        ( turnIsNewSession turn
                                        , pure ()
                                        )
                    pure (first, second)

        first `shouldBe`
            Left
                (CLIConnectionError
                    "simulated connection failure")
        second `shouldBe` Right False
        readIORef requestsRef `shouldReturn`
            [ TransportRequest
                { transportMode =
                    TransportResume "named conversation"
                , transportModel = Nothing
                , transportEffort = Nothing
                }
            , TransportRequest
                { transportMode =
                    TransportResume "named conversation"
                , transportModel = Nothing
                , transportEffort = Nothing
                }
            ]

    it "retains initial resume after an explicit one and uses it for blank input" do
        requestsRef <- newIORef []
        let options =
                (testOptions "unused-by-custom-transport" ".")
                    { sessionId = Nothing
                    , resume = Just "initial conversation"
                    }
            transportFactory request = do
                modifyIORef' requestsRef (<> [request])
                ready <- newIORef True
                exited <- newIORef False
                pure Transport
                    { transportConnect =
                        pure (Right ())
                    , transportWrite =
                        \_ -> pure (Right ())
                    , transportRead =
                        pure (Right Nothing)
                    , transportClose = do
                        writeIORef ready False
                        writeIORef exited True
                    , transportIsReady =
                        readIORef ready
                    , transportEndInput = do
                        writeIORef ready False
                        writeIORef exited True
                    , transportProcessExit = do
                        hasExited <- readIORef exited
                        pure $
                            if hasExited
                                then Just ExitSuccess
                                else Nothing
                    , transportDiagnostic =
                        pure ""
                    }

        withClaudeSDKClientWithTransport
            options
            transportFactory
            \client -> do
                first <-
                    withClaudeSDKTurn
                        client
                        (pure True)
                        (Just "explicit conversation")
                        Nothing
                        Nothing
                        \_ -> pure (Right ((), pure ()))
                second <-
                    withClaudeSDKTurn
                        client
                        (pure True)
                        (Just "   ")
                        Nothing
                        Nothing
                        \_ -> pure (Right ((), pure ()))
                third <-
                    withClaudeSDKTurn
                        client
                        (pure True)
                        Nothing
                        Nothing
                        Nothing
                        \_ -> pure (Right ((), pure ()))
                first `shouldBe` Right ()
                second `shouldBe` Right ()
                third `shouldBe` Right ()

        readIORef requestsRef `shouldReturn`
            [ TransportRequest
                { transportMode =
                    TransportResume "explicit conversation"
                , transportModel = Nothing
                , transportEffort = Nothing
                }
            , TransportRequest
                { transportMode =
                    TransportResume "initial conversation"
                , transportModel = Nothing
                , transportEffort = Nothing
                }
            ]

    it "invalidates Claude context when a commit is asynchronously cancelled" do
        requestsRef <- newIORef []
        commitStarted <- newEmptyMVar
        releaseCommit <- newEmptyMVar
        turnFinished <- newEmptyMVar
        let options = testOptions "unused-by-custom-transport" "."
            transportFactory request = do
                modifyIORef' requestsRef (<> [request])
                ready <- newIORef True
                exited <- newIORef False
                pure Transport
                    { transportConnect =
                        pure (Right ())
                    , transportWrite =
                        \_ -> pure (Right ())
                    , transportRead =
                        pure (Right Nothing)
                    , transportClose = do
                        writeIORef ready False
                        writeIORef exited True
                    , transportIsReady =
                        readIORef ready
                    , transportEndInput = do
                        writeIORef ready False
                        writeIORef exited True
                    , transportProcessExit = do
                        hasExited <- readIORef exited
                        pure $
                            if hasExited
                                then Just ExitSuccess
                                else Nothing
                    , transportDiagnostic =
                        pure ""
                    }

        (cancelled, nextTurn) <-
            withClaudeSDKClientWithTransport
                options
                transportFactory
                \client -> do
                    worker <-
                        forkFinally
                            ( withClaudeSDKTurn
                                client
                                (pure True)
                                Nothing
                                Nothing
                                Nothing
                                \_ ->
                                    pure $
                                        Right
                                            ( ()
                                            , putMVar commitStarted ()
                                                >> takeMVar releaseCommit
                                            )
                            )
                            (putMVar turnFinished)
                    takeMVar commitStarted
                    killThread worker
                    cancelled <-
                        timeout
                            2_000_000
                            (takeMVar turnFinished)
                    nextTurn <-
                        withClaudeSDKTurn
                            client
                            (pure True)
                            Nothing
                            Nothing
                            Nothing
                            \turn ->
                                pure $
                                    Right
                                        ( turnIsNewSession turn
                                        , pure ()
                                        )
                    pure (cancelled, nextTurn)

        cancelled `shouldSatisfy` \case
            Just (Left _) -> True
            _ -> False
        nextTurn `shouldBe` Right True
        requests <- readIORef requestsRef
        requests `shouldSatisfy` \case
            [ TransportRequest
                    { transportMode = TransportNew firstSession
                    }
                , TransportRequest
                    { transportMode = TransportNew secondSession
                    }
                ] ->
                    firstSession == testSessionId
                        && secondSession /= firstSession
            _ ->
                False

    it "retries a transport close that is asynchronously cancelled" do
        closeStarted <- newEmptyMVar
        neverFinishFirstClose <- newEmptyMVar
        abortFinished <- newEmptyMVar
        closeAttempts <- newIORef (0 :: Int)
        let options = testOptions "unused-by-custom-transport" "."
            transportFactory _ =
                pure Transport
                    { transportConnect =
                        pure (Right ())
                    , transportWrite =
                        \_ -> pure (Right ())
                    , transportRead =
                        pure (Right Nothing)
                    , transportClose = do
                        attempt <-
                            atomicModifyIORef' closeAttempts \count ->
                                let next = count + 1
                                in (next, next)
                        when (attempt == 1) do
                            putMVar closeStarted ()
                            takeMVar neverFinishFirstClose
                    , transportIsReady =
                        pure True
                    , transportEndInput =
                        pure ()
                    , transportProcessExit =
                        pure Nothing
                    , transportDiagnostic =
                        pure ""
                    }

        withClaudeSDKClientWithTransport
            options
            transportFactory
            \client -> do
                started <-
                    withClaudeSDKTurn
                        client
                        (pure True)
                        Nothing
                        Nothing
                        Nothing
                        \_ -> pure (Right ((), pure ()))
                started `shouldBe` Right ()

                worker <-
                    forkFinally
                        (abort client)
                        (putMVar abortFinished)
                takeMVar closeStarted
                killThread worker
                firstAbort <-
                    timeout
                        2_000_000
                        (takeMVar abortFinished)
                firstAbort `shouldSatisfy` \case
                    Just (Left _) -> True
                    _ -> False

                abort client

        readIORef closeAttempts `shouldReturn` 2

    it "retries a transport close after a synchronous failure" do
        closeAttempts <- newIORef (0 :: Int)
        let options = testOptions "unused-by-custom-transport" "."
            transportFactory _ =
                pure Transport
                    { transportConnect =
                        pure (Right ())
                    , transportWrite =
                        \_ -> pure (Right ())
                    , transportRead =
                        pure (Right Nothing)
                    , transportClose = do
                        attempt <-
                            atomicModifyIORef' closeAttempts \count ->
                                let next = count + 1
                                in (next, next)
                        when (attempt == 1) $
                            ioError
                                (userError
                                    "simulated close failure")
                    , transportIsReady =
                        pure True
                    , transportEndInput =
                        pure ()
                    , transportProcessExit =
                        pure Nothing
                    , transportDiagnostic =
                        pure ""
                    }

        withClaudeSDKClientWithTransport
            options
            transportFactory
            \client -> do
                started <-
                    withClaudeSDKTurn
                        client
                        (pure True)
                        Nothing
                        Nothing
                        Nothing
                        \_ -> pure (Right ((), pure ()))
                started `shouldBe` Right ()

                firstAbort <- tryAny (abort client)
                firstAbort `shouldSatisfy` \case
                    Left exception ->
                        "simulated close failure"
                            `Text.isInfixOf` Text.pack (show exception)
                    Right () -> False
                abort client

        readIORef closeAttempts `shouldReturn` 2

    it "aborts an active turn without waiting for the turn lock" do
        promptReceived <- newEmptyMVar
        releaseRead <- newEmptyMVar
        turnFinished <- newEmptyMVar
        closeCount <- newIORef (0 :: Int)
        let options = testOptions "unused-by-custom-transport" "."
            transportFactory _ =
                pure Transport
                    { transportConnect =
                        pure (Right ())
                    , transportWrite = \_ -> do
                        void (tryPutMVar promptReceived ())
                        pure (Right ())
                    , transportRead = do
                        takeMVar releaseRead
                        pure (Right Nothing)
                    , transportClose = do
                        modifyIORef' closeCount (+ 1)
                        void (tryPutMVar releaseRead ())
                    , transportIsReady =
                        pure True
                    , transportEndInput =
                        pure ()
                    , transportProcessExit =
                        pure (Just ExitSuccess)
                    , transportDiagnostic =
                        pure ""
                    }

        withClaudeSDKClientWithTransport
            options
            transportFactory
            \client -> do
                _ <-
                    forkIO do
                        result <-
                            withClaudeSDKTurn
                                client
                                (pure True)
                                Nothing
                                Nothing
                                Nothing
                                \turn -> do
                                    completed <-
                                        queryTurn
                                            turn
                                            "blocked prompt"
                                            (const (pure ()))
                                    pure ((, pure ()) <$> completed)
                        putMVar turnFinished result

                takeMVar promptReceived
                interrupted <-
                    timeout 1_000_000 (abort client)
                when (interrupted == Nothing) $
                    void (tryPutMVar releaseRead ())

                interrupted `shouldBe` Just ()
                completed <- timeout 2_000_000 (takeMVar turnFinished)
                completed `shouldSatisfy` \case
                    Just (Left _) -> True
                    _ -> False

        readIORef closeCount `shouldReturn` 1

runTurn
    :: ClaudeSDKClient
    -> IORef [Message]
    -> Maybe Text
    -> Text
    -> IO (Either ClaudeSDKError (Bool, ResultMessage))
runTurn client messagesRef previous prompt =
    withClaudeSDKTurn
        client
        (pure True)
        previous
        Nothing
        Nothing
        \turn -> do
            result <-
                queryTurn
                    turn
                    prompt
                    (\message ->
                        modifyIORef' messagesRef (<> [message]))
            pure case result of
                Left err ->
                    Left err
                Right completed ->
                    Right
                        ( (turnIsNewSession turn, completed)
                        , pure ()
                        )

resultText :: ResultMessage -> Maybe Text
resultText ResultMessage{result} = result

transportModeSessionId :: TransportMode -> Text
transportModeSessionId = \case
    TransportNew sessionId -> sessionId
    TransportResume _ -> testSessionId
    TransportContinue -> testSessionId

expectedArguments :: [String]
expectedArguments =
    [ "-p"
    , "--input-format"
    , "stream-json"
    , "--output-format"
    , "stream-json"
    , "--verbose"
    , "--system-prompt"
    , "system prompt"
    , "--session-id"
    , Text.unpack testSessionId
    , "--tools"
    , ""
    , "--allowedTools"
    , "Read,Grep"
    , "--disallowedTools"
    , "Bash"
    , "--allow-dangerously-skip-permissions"
    , "--permission-mode"
    , "bypassPermissions"
    , "--setting-sources"
    , ""
    , "--strict-mcp-config"
    , "--include-partial-messages"
    , "--safe-mode"
    , "--disable-slash-commands"
    , "--no-chrome"
    , "--model"
    , "claude-test"
    , "--effort"
    , "high"
    ]

expectedQueryValue :: Text -> Aeson.Value
expectedQueryValue prompt =
    expectedContentQueryValue
        [ Aeson.object
            [ "type" Aeson..= ("text" :: Text)
            , "text" Aeson..= prompt
            ]
        ]

expectedContentQueryValue :: [Aeson.Value] -> Aeson.Value
expectedContentQueryValue content =
    Aeson.object
        [ "type" Aeson..= ("user" :: Text)
        , "message" Aeson..= Aeson.object
            [ "role" Aeson..= ("user" :: Text)
            , "content" Aeson..= content
            ]
        , "parent_tool_use_id" Aeson..= Aeson.Null
        , "session_id" Aeson..= testSessionId
        , "origin" Aeson..= Aeson.object
            [ "kind" Aeson..= ("human" :: Text)
            ]
        ]

decodePrompt :: String -> Either String Text
decodePrompt bytes = do
    value <-
        Aeson.eitherDecodeStrict'
            (TextEncoding.encodeUtf8 (Text.pack bytes))
    case value of
        Aeson.Object object -> do
            message <- case KeyMap.lookup "message" object of
                Just (Aeson.Object nested) -> Right nested
                _ -> Left "missing message object"
            case KeyMap.lookup "content" message of
                Just (Aeson.Array values) ->
                    case toList values of
                        [Aeson.Object content]
                            | Just (Aeson.String prompt) <-
                                KeyMap.lookup "text" content ->
                                Right prompt
                        _ ->
                            Left "missing text content"
                _ -> Left "missing text content"
        _ ->
            Left "query was not an object"

querySessionId :: ByteString.ByteString -> Either String Text
querySessionId bytes = do
    value <- Aeson.eitherDecodeStrict' bytes
    case value of
        Aeson.Object object ->
            case KeyMap.lookup "session_id" object of
                Just (Aeson.String sessionId) ->
                    Right sessionId
                _ ->
                    Left "missing session_id"
        _ ->
            Left "query was not an object"

transportProbeScript :: String
transportProbeScript =
    Text.unpack $
        Text.unlines
            [ "#!/bin/sh"
            , "printf '%s\\n' \"$@\" > \"$FAKE_ARGS\""
            , "{"
            , "  printf 'CLAUDE_CODE_ENTRYPOINT=%s\\n' \"${CLAUDE_CODE_ENTRYPOINT-unset}\""
            , "  printf 'CLAUDE_AGENT_SDK_CLIENT_APP=%s\\n' \"${CLAUDE_AGENT_SDK_CLIENT_APP-unset}\""
            , "  printf 'CLAUDE_AGENT_SDK_VERSION=%s\\n' \"${CLAUDE_AGENT_SDK_VERSION-unset}\""
            , "  printf 'CLAUDECODE=%s\\n' \"${CLAUDECODE-unset}\""
            , "  printf 'PWD=%s\\n' \"$PWD\""
            , "} > \"$FAKE_ENVIRONMENT\""
            , "IFS= read -r query"
            , "printf '%s\\n' \"$query\" > \"$FAKE_INPUT\""
            , "printf '%s\\n' "
                <> shellQuote (successResult "probe complete")
            ]

blockedOutputScript :: String
blockedOutputScript =
    unlines
        [ "#!/bin/sh"
        , "IFS= read -r _query"
        , "printf 'ready\\n' > \"$FAKE_READY\""
        , "trap '' INT TERM"
        , "while :; do sleep 1; done"
        ]

blockedInputScript :: String
blockedInputScript =
    unlines
        [ "#!/bin/sh"
        , "exec 3<&0"
        , "\"$FAKE_HELPER\" --claude-sdk-test-hold-stdin \"$FAKE_READY\" <&3 >/dev/null 2>&1 &"
        , "while :; do sleep 1; done"
        ]

persistentScript :: String
persistentScript =
    Text.unpack $
        Text.unlines
            [ "#!/bin/sh"
            , "printf 'started\\n' >> \"$FAKE_STARTS\""
            , "count=0"
            , "while IFS= read -r query; do"
            , "  count=$((count + 1))"
            , "  printf '%s\\n' \"$query\" >> \"$FAKE_INPUTS\""
            , "  printf '%s\\n' \"{\\\"type\\\":\\\"assistant\\\",\\\"uuid\\\":\\\"assistant-$count\\\",\\\"session_id\\\":\\\""
                <> testSessionId
                <> "\\\",\\\"message\\\":{\\\"content\\\":[{\\\"type\\\":\\\"text\\\",\\\"text\\\":\\\"reply-$count\\\"}]}}\""
            , "  printf '%s\\n' \"{\\\"type\\\":\\\"result\\\",\\\"subtype\\\":\\\"success\\\",\\\"is_error\\\":false,\\\"session_id\\\":\\\""
                <> testSessionId
                <> "\\\",\\\"uuid\\\":\\\"result-$count\\\",\\\"result\\\":\\\"reply-$count\\\"}\""
            , "done"
            ]

successResult :: Text -> Text
successResult result =
    "{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":false,\
    \\"session_id\":\""
        <> testSessionId
        <> "\",\"uuid\":\"result\",\"result\":\""
        <> result
        <> "\"}"

multiWriteScript :: Text -> String
multiWriteScript record =
    Text.unpack $
        Text.unlines $
            [ "#!/bin/sh"
            , "IFS= read -r _query"
            ]
                <> concatMap
                    (\chunk ->
                        [ "printf '%s' " <> shellQuote chunk
                        , "sleep 0.01"
                        ])
                    (textChunks 997 record)
                <> ["printf '\\n'"]

textChunks :: Int -> Text -> [Text]
textChunks chunkSize value
    | Text.null value = []
    | otherwise =
        let (chunk, remaining) =
                Text.splitAt chunkSize value
         in chunk : textChunks chunkSize remaining

assistantLine :: Text -> Text -> Text
assistantLine sessionId text =
    "{\"type\":\"assistant\",\"uuid\":\"custom-assistant\",\
    \\"session_id\":\""
        <> sessionId
        <> "\",\"message\":{\"content\":[{\"type\":\"text\",\
           \\"text\":\""
        <> text
        <> "\"}]}}"

resultLine :: Text -> Text -> Text
resultLine sessionId result =
    "{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":false,\
    \\"session_id\":\""
        <> sessionId
        <> "\",\"uuid\":\"custom-result\",\"result\":\""
        <> result
        <> "\"}"

expectRight
    :: Show error
    => Either error value
    -> IO value
expectRight = \case
    Left err -> do
        expectationFailure ("expected Right, got Left " <> show err)
        fail "unreachable"
    Right value ->
        pure value

waitForPath :: FilePath -> IO ()
waitForPath path = do
    exists <- doesFileExist path
    if exists
        then pure ()
        else do
            threadDelay 10_000
            waitForPath path

killRecordedProcess :: FilePath -> IO ()
killRecordedProcess path =
    void $
        tryAny do
            processId <-
                read @ProcessID <$> readFile path
            signalProcess sigKILL processId
