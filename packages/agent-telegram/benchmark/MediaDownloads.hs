{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Agent.Telegram
    ( downloadTelegramMediaAttachmentsWith
    )
import Agent.Telegram.Types
    ( TelegramFileMedia(..)
    , TelegramMedia(..)
    , TelegramMediaKind(..)
    )
import Control.Concurrent (threadDelay)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Data.Text as Text
import System.Environment (getArgs)
import System.IO.Temp (withSystemTempDirectory)
import System.OsPath (OsPath, unsafeEncodeUtf)
import Text.Printf (printf)
import Text.Read (readMaybe)

main :: IO ()
main =
    getArgs >>= \case
        [countArg, delayArg] -> do
            count <- parsePositive "attachment count" countArg
            delayMillis <- parsePositive "download delay milliseconds" delayArg
            withSystemTempDirectory "agent-telegram-media-bench" \directory ->
                do
                    let tempDir = unsafeEncodeUtf directory
                    serial <- measure
                        (serialDownloads count delayMillis tempDir)
                    bounded <- measure
                        (boundedDownloads count delayMillis tempDir)
                    printf "serial,%.3f\nbounded-4,%.3f\n"
                        serial
                        bounded
        _ ->
            fail "usage: media-downloads-bench ATTACHMENT_COUNT DELAY_MS"

serialDownloads :: Int -> Int -> OsPath -> IO ()
serialDownloads count delayMillis tempDir =
    mapM_ (downloadOne delayMillis tempDir) [0 .. count - 1]

boundedDownloads :: Int -> Int -> OsPath -> IO ()
boundedDownloads count delayMillis tempDir = do
    _ <- downloadTelegramMediaAttachmentsWith
        (pure . Text.unpack)
        (\_ target -> threadDelay (delayMillis * 1_000) >> pure target)
        tempDir
        42
        (attachments count)
    pure ()

downloadOne :: Int -> OsPath -> Int -> IO ()
downloadOne delayMillis _tempDir _index =
    threadDelay (delayMillis * 1_000)

attachments :: Int -> [TelegramMedia]
attachments count =
    [ TelegramMedia
        { telegramMediaKind = TelegramMediaDocument
        , telegramMediaFile =
            Just TelegramFileMedia
                { fileMediaFileId = Text.pack (show index)
                , fileMediaName = Just "payload.bin"
                , fileMediaMimeType = Just "application/octet-stream"
                , fileMediaFileSize = Nothing
                , fileMediaDuration = Nothing
                }
        , telegramMediaDescription = ""
        }
    | index <- [0 .. count - 1]
    ]

measure :: IO a -> IO Double
measure action = do
    started <- getCurrentTime
    _ <- action
    finished <- getCurrentTime
    pure (realToFrac (diffUTCTime finished started) * 1_000)

parsePositive :: String -> String -> IO Int
parsePositive label raw =
    case readMaybe raw of
        Just value | value > 0 -> pure value
        _ -> fail ("invalid " <> label <> ": " <> raw)
