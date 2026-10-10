{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Authenticated, read-only catalog benchmark. Never prints credentials/body.
import Control.Exception.Safe (bracket, try)
import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as Output
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (getHomeDirectory)

data Credential = Credential Text Text
instance FromJSON Credential where
    parseJSON = withObject "credential" $ \o ->
        Credential <$> o .: "base_url" <*> o .: "access_token"

main :: IO ()
main = do
    home <- getHomeDirectory
    bytes <- LBS.readFile (home <> "/.haskell-agent/credentials/gateway.json")
    Credential origin token <- case eitherDecode bytes of
        Left _ -> fail "Cannot decode gateway credential"
        Right c -> pure c
    unless (origin `elem`
        ["https://platform.digitallyinduced.com", "https://platform.digitallyinduced.com/",
         "https://platform.digitallyinduced.com/v1", "https://platform.digitallyinduced.com/v1/"]) $
        fail "Credential origin mismatch"
    initial <- parseRequest "https://platform.digitallyinduced.com/v1/models"
    let request = initial
            { requestHeaders = [("Authorization", "Bearer " <> Text.encodeUtf8 token),
                                ("Accept", "application/json")]
            , redirectCount = 0
            , responseTimeout = responseTimeoutMicro 5000000
            }
    start <- getMonotonicTimeNSec
    bracket newTlsManager closeManager $ \manager -> do
        result <- try (httpLbs request manager)
        response <- case result of
            Left (_ :: HttpException) -> fail "Catalog transport failed; details omitted"
            Right r -> pure r
        unless (statusCode (responseStatus response) == 200) $
            fail "Catalog request failed; response omitted"
        value <- case eitherDecode (responseBody response) :: Either String Value of
            Left _ -> fail "Invalid catalog JSON"
            Right v -> pure v
        checksum <- evaluate (LBS.length (encode value))
        end <- getMonotonicTimeNSec
        Output.putStrLn $ encode $ object
            [ "total_ms" .= (fromIntegral (end - start) / 1000000 :: Double)
            , "checksum" .= checksum
            ]
