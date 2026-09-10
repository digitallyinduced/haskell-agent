{-# LANGUAGE OverloadedStrings #-}
-- Credential-free GET with explicit DNS/TCP/TLS boundaries and Nagle control.
import Control.Exception.Safe (bracket, bracketOnError)
import Control.Monad (forM_)
import Data.Aeson (encode, object, (.=))
import Data.Default.Class (def)
import qualified Data.ByteString.Lazy.Char8 as LBS
import GHC.Clock (getMonotonicTimeNSec)
import qualified Network.Connection as C
import qualified Network.Socket as S
import System.Timeout (timeout)

main :: IO ()
main = forM_ [False, True, True, False, False, True, True, False] $ \noDelay -> do
    outcome <- timeout 10000000 $ do
        start <- getMonotonicTimeNSec
        context <- C.initConnectionContext
        contextReady <- getMonotonicTimeNSec
        addresses <- S.getAddrInfo
            (Just S.defaultHints { S.addrFlags = [S.AI_ADDRCONFIG], S.addrSocketType = S.Stream })
            (Just "platform.digitallyinduced.com") (Just "443")
        dnsReady <- getMonotonicTimeNSec
        address <- case addresses of
            a : _ -> pure a
            [] -> fail "No address"
        let acquire = bracketOnError
                (S.socket (S.addrFamily address) S.Stream (S.addrProtocol address)) S.close $ \sock -> do
                    S.setSocketOption sock S.NoDelay (if noDelay then 1 else 0)
                    S.connect sock (S.addrAddress address)
                    tcpReady <- getMonotonicTimeNSec
                    conn <- C.connectFromSocket context sock C.ConnectionParams
                        { C.connectionHostname = "platform.digitallyinduced.com"
                        , C.connectionPort = 443
                        , C.connectionUseSecure = Just def
                        , C.connectionUseSocks = Nothing
                        }
                    tlsReady <- getMonotonicTimeNSec
                    pure (conn, tcpReady, tlsReady)
        bracket acquire (\(conn, _, _) -> C.connectionClose conn) $ \(conn, tcpReady, tlsReady) -> do
            C.connectionPut conn "GET /v1/models HTTP/1.1\r\nHost: platform.digitallyinduced.com\r\nConnection: close\r\n\r\n"
            _ <- C.connectionGetLine 4096 conn
            headersReady <- getMonotonicTimeNSec
            let ms a b = fromIntegral (b - a) / 1000000 :: Double
            LBS.putStrLn $ encode $ object
                [ "nodelay" .= noDelay
                , "context_ms" .= ms start contextReady
                , "dns_ms" .= ms contextReady dnsReady
                , "tcp_ms" .= ms dnsReady tcpReady
                , "tls_ms" .= ms tcpReady tlsReady
                , "first_line_ms" .= ms tlsReady headersReady
                , "total_ms" .= ms start headersReady
                ]
    case outcome of
        Nothing -> fail "Probe timed out"
        Just () -> pure ()
