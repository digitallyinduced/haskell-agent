module Main (main) where

import qualified Claude.Agent.SDK.ClientSpec as ClientSpec
import qualified Claude.Agent.SDK.CapabilitiesSpec as CapabilitiesSpec
import qualified Claude.Agent.SDK.ControlSpec as ControlSpec
import qualified Claude.Agent.SDK.Internal.Transport.OutputBufferSpec as OutputBufferSpec
import qualified Claude.Agent.SDK.MessageParserSpec as MessageParserSpec
import qualified Claude.Agent.SDK.QuerySpec as QuerySpec
import Control.Concurrent (threadDelay)
import qualified System.Posix.IO.ByteString as PosixByteString
import System.Environment (getArgs)
import System.Posix.IO (stdInput)
import System.Posix.Process (createSession, getProcessID)
import Test.Hspec (hspec)

main :: IO ()
main =
    getArgs >>= \case
        ["--claude-sdk-test-hold-stdin", readyPath] ->
            holdStdinOpen readyPath
        _ ->
            hspec do
                ClientSpec.spec
                CapabilitiesSpec.spec
                ControlSpec.spec
                OutputBufferSpec.spec
                MessageParserSpec.spec
                QuerySpec.spec

holdStdinOpen :: FilePath -> IO ()
holdStdinOpen readyPath = do
    _ <- createSession
    _ <- PosixByteString.fdRead stdInput 1
    processId <- getProcessID
    writeFile readyPath (show processId)
    threadDelay (60 * 1_000_000)
