-- | Bounded response bodies shared by gateway service transports.
module Agent.CLI.Gateway.Http
    ( gatewayMaxResponseBytes
    , readBoundedBody
    ) where

import Data.ByteString qualified as BS
import Network.HTTP.Client qualified as HTTP

gatewayMaxResponseBytes :: Int
gatewayMaxResponseBytes = 64 * 1024

readBoundedBody
    :: Int
    -> HTTP.BodyReader
    -> IO (Maybe BS.ByteString)
readBoundedBody limit = go 0 []
  where
    go total chunks reader = do
        chunk <- HTTP.brRead reader
        if BS.null chunk
            then pure (Just (BS.concat (reverse chunks)))
            else
                let next = total + BS.length chunk
                in if next > limit
                    then pure Nothing
                    else go next (chunk : chunks) reader
