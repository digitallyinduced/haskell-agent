module Agent.CLI.AppleFollowUpSpec (spec) where

import Agent.CLI.AppleFollowUp
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.AppleFollowUp" do
    describe "parseFollowUpReadyJson" do
        it "accepts a ready object" do
            parseFollowUpReadyJson "{\"ready\":true}\n"
                `shouldBe` True

        it "rejects anything else" do
            parseFollowUpReadyJson "{\"ready\":false}\n" `shouldBe` False
            parseFollowUpReadyJson "{\"route\":\"steer\"}\n" `shouldBe` False

    describe "parseFollowUpRouteJson" do
        it "reads steer and queue" do
            parseFollowUpRouteJson "{\"route\":\"steer\"}"
                `shouldBe` Just FollowUpSteer
            parseFollowUpRouteJson "{\"route\":\"Queue\"}\n"
                `shouldBe` Just FollowUpQueue

        it "rejects an unknown route" do
            parseFollowUpRouteJson "{\"route\":\"later\"}"
                `shouldBe` Nothing

    describe "routeRequestLine" do
        it "emits one JSON object and caps the fields" do
            let line = routeRequestLine (Text.replicate 2000 "t") (Text.replicate 3000 "m")
            Text.length line `shouldSatisfy` (< 8000)
            Text.any (== '\n') line `shouldBe` False

    describe "maintainAppleFollowUpRouterTimed" do
        it "installs a router once the helper is ready" do
            withHelper serveScript \executable -> do
                let timing =
                        defaultAppleFollowUpTiming
                            { appleFollowUpReadyTimeoutMicros = 1_000_000
                            , appleFollowUpRequestTimeoutMicros = 1_000_000
                            }
                installed <- newEmptyMVar
                result <-
                    timeout 2_000_000 $
                        withAsync
                            (maintainAppleFollowUpRouterTimed
                                timing
                                executable
                                (\route -> putMVar installed route)
                                (\_ -> pure ()))
                            \_ -> do
                                route <- takeMVar installed
                                route "task" "queue-me"
                result `shouldBe` Just FollowUpQueue

        it "returns when the helper never becomes ready" do
            withHelper "#!/bin/sh\nexec sleep 30\n" \executable -> do
                let timing =
                        defaultAppleFollowUpTiming
                            { appleFollowUpReadyTimeoutMicros = 200_000
                            , appleFollowUpRequestTimeoutMicros = 200_000
                            }
                result <-
                    timeout 2_000_000 $
                        maintainAppleFollowUpRouterTimed
                            timing
                            executable
                            (\_ -> pure ())
                            (\_ -> pure ())
                result `shouldBe` Just ()

    describe "classifyFollowUps" do
        it "returns each decision and stops on an unusable response" do
            withHelper classifyScript \executable -> do
                let timing =
                        defaultAppleFollowUpTiming
                            { appleFollowUpReadyTimeoutMicros = 1_000_000
                            , appleFollowUpRequestTimeoutMicros = 1_000_000
                            }
                classifyFollowUps
                    timing
                    executable
                    [("task", "keep"), ("task", "queue-me")]
                    `shouldReturn` Right [FollowUpSteer, FollowUpQueue]
                classifyFollowUps timing executable [("task", "bad")]
                    `shouldReturn`
                        Left
                            "follow-up 1 was not classified: follow-up router returned no decision"

serveScript :: String
serveScript =
    unlines
        [ "#!/bin/sh"
        , "printf '%s\\n' '{\"ready\":true}'"
        , "while IFS= read -r line; do"
        , "  case \"$line\" in"
        , "    *queue-me*) printf '%s\\n' '{\"route\":\"queue\"}' ;;"
        , "    *) printf '%s\\n' '{\"route\":\"steer\"}' ;;"
        , "  esac"
        , "done"
        ]

classifyScript :: String
classifyScript =
    unlines
        [ "#!/bin/sh"
        , "printf '%s\\n' '{\"ready\":true}'"
        , "while IFS= read -r line; do"
        , "  case \"$line\" in"
        , "    *bad*) printf '%s\\n' '{\"route\":\"later\"}' ;;"
        , "    *queue-me*) printf '%s\\n' '{\"route\":\"queue\"}' ;;"
        , "    *) printf '%s\\n' '{\"route\":\"steer\"}' ;;"
        , "  esac"
        , "done"
        ]

withHelper :: String -> (FilePath -> IO a) -> IO a
withHelper script action =
    withSystemTempDirectory "apple-follow-up-spec-" \directory -> do
        let executable = directory </> "helper"
        Text.writeFile executable (Text.pack script)
        setFileMode executable 0o755
        action executable
