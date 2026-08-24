-- | Redacted structured logging for the Telegram gateway.
module Agent.Telegram.Log
    ( logTelegramEvent
    ) where

import Data.Aeson (Value, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as Text
import Data.Time.Clock (getCurrentTime)
import System.IO (stderr)

logTelegramEvent :: Text -> [(Key.Key, Value)] -> IO ()
logTelegramEvent event fields = do
    now <- getCurrentTime
    Text.hPutStrLn stderr $
        TextEncoding.decodeUtf8 . LBS.toStrict . encode . object $
            [ "at" .= now
            , "gateway" .= ("telegram" :: Text)
            , "event" .= event
            ]
                <> fields
