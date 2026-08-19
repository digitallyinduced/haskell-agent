module Main (main) where

import qualified Agent.ErrorSpec as ErrorSpec
import qualified Agent.LoopSpec as LoopSpec
import qualified Agent.ToolArgsSpec as ToolArgsSpec
import qualified Agent.ToolDispatchSpec as ToolDispatchSpec
import qualified Agent.Transport.WebSocketSpec as WebSocketSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
    ErrorSpec.spec
    LoopSpec.spec
    ToolArgsSpec.spec
    ToolDispatchSpec.spec
    WebSocketSpec.spec
