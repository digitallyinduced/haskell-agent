{-# LANGUAGE ScopedTypeVariables #-}

-- | Restricted conversion for untrusted OGG voice recordings.
module Agent.Audio.Conversion (AudioConversionFailure (..), convertOggToWav) where

import Control.Exception.Safe (IOException, catch)
import Data.ByteString qualified as BS
import System.Exit (ExitCode (..))
import System.Posix.Files (getSymbolicLinkStatus, isRegularFile, fileSize)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

data AudioConversionFailure = InvalidAudioRecording
    deriving (Eq, Show)

-- | Input and output must be in caller-owned private storage. Only regular
-- OGG files up to 20 MiB and ten minutes are admitted. No network protocols,
-- playlists, inherited stdin, or diagnostic output are accepted.
convertOggToWav :: FilePath -> FilePath -> IO (Either AudioConversionFailure BS.ByteString)
convertOggToWav input output =
    catch convert (\(_ :: IOException) -> pure (Left InvalidAudioRecording))
  where
    convert = do
        status <- getSymbolicLinkStatus input
        if not (isRegularFile status) || fileSize status <= 0 || fileSize status > 20 * 1024 * 1024
            then pure (Left InvalidAudioRecording)
            else do
                result <- timeout (60 * 1000000) $ readProcessWithExitCode "ffmpeg"
                    [ "-nostdin", "-v", "quiet", "-y"
                    , "-protocol_whitelist", "file", "-f", "ogg", "-i", input
                    , "-map", "0:a:0", "-vn", "-ac", "1", "-ar", "16000"
                    , "-t", "601", "-c:a", "pcm_s16le", "-f", "wav", output
                    ] ""
                case result of
                    Just (ExitSuccess, _, _) -> do
                        converted <- getSymbolicLinkStatus output
                        if not (isRegularFile converted)
                            || fileSize converted <= 44 || fileSize converted > 600 * 32000 + 4096
                            then pure (Left InvalidAudioRecording)
                            else Right <$> BS.readFile output
                    _ -> pure (Left InvalidAudioRecording)
