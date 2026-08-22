module Agent.CLI.BenchmarkSpec (spec) where

import Agent.CLI.Benchmark
import qualified Data.Aeson as Aeson
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.Benchmark" do
    it "extracts strict BENCH_RESULT JSON" do
        extractBenchResult
            "BENCH_RESULT {\"count\":3,\"ok\":true}"
            `shouldBe` Just (Aeson.object
                [ "count" Aeson..= (3 :: Int)
                , "ok" Aeson..= True
                ])
        extractBenchResult
            "prose\nBENCH_RESULT {\"count\":3,\"ok\":true}"
            `shouldBe` Nothing

    it "parses benchmark arm and selection options" do
        defaults <- defaultBenchmarkOptions
        parseBenchmarkArgs defaults
            [ "--agent", "/tmp/agent-cli"
            , "--repetitions", "2"
            , "--arms", "direct,forced-haskell"
            , "--tasks", "privacy-canary,simple-control"
            ]
            `shouldBe` Right (defaults
                { benchmarkAgent = "/tmp/agent-cli"
                , benchmarkRepetitions = 2
                , benchmarkArms = [DirectTools, ForcedHaskell]
                , benchmarkTaskNames =
                    ["privacy-canary", "simple-control"]
                })
