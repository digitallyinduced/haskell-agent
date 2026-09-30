{-# LANGUAGE OverloadedStrings #-}
-- Transport-only reproduction of Gateway.Catalog's newTlsManager/httpLbs path.
-- No credential store, database, terminal, or model request is involved.
import Control.Exception.Safe (bracket)
import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import Data.Aeson (Value, eitherDecodeStrict', encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as LBS
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [url, countText] | Just count <- readMaybe countText, count > (0 :: Int) -> do
            initial <- parseRequest url
            unless (host initial == "127.0.0.1" || host initial == "localhost") $
                fail "Only loopback fixtures are allowed"
            let request = initial
                    { requestHeaders = [("Authorization", "Bearer benchmark-only"), ("Accept", "application/json")]
                    , redirectCount = 0
                    , responseTimeout = responseTimeoutMicro 5000000
                    }
            forM_ [0 .. count - 1] $ \sample -> do
                start <- getMonotonicTimeNSec
                bracket newTlsManager closeManager $ \manager -> do
                    created <- getMonotonicTimeNSec
                    forM_ [0 :: Int, 1] $ \reuse -> do
                        sent <- getMonotonicTimeNSec
                        response <- httpLbs request manager
                        received <- getMonotonicTimeNSec
                        unless (statusCode (responseStatus response) == 200) $
                            fail "Fixture returned non-200"
                        value <- either fail pure
                            (eitherDecodeStrict' (LBS.toStrict (responseBody response)) :: Either String Value)
                        -- Force the complete JSON tree, not just the outer constructor.
                        checksum <- evaluate (LBS.length (encode value))
                        decoded <- getMonotonicTimeNSec
                        let ms a b = fromIntegral (b - a) / 1000000 :: Double
                        LBS.putStrLn $ encode $ object
                            [ "sample" .= sample, "reuse" .= reuse
                            , "manager_ms" .= ms start created
                            , "http_ms" .= ms sent received
                            , "decode_ms" .= ms received decoded
                            , "total_ms" .= ms (if reuse == 0 then start else sent) decoded
                            , "checksum" .= checksum
                            ]
        _ -> fail "Usage: local-gateway-probe LOOPBACK_URL COUNT"
