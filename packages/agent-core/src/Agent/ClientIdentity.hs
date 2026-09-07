-- | Non-secret client identity for requests to the organization gateway.
module Agent.ClientIdentity (gatewayUserAgent) where

import Data.Char (isAscii, isAlphaNum)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)
import qualified Paths_agent_core as Paths
import System.Environment (getProgName, lookupEnv)
import System.Info (arch, os)

-- | Native's @ha_runtime_init@ explicitly sets the RTS program name to
-- @haskell-agent-macos@ before any Haskell gateway calls. Do not infer the
-- frontend from the OS: the CLI also runs on macOS. The version describes
-- the shared agent runtime, not the separately versioned native UI bundle.
gatewayUserAgent :: IO ByteString
gatewayUserAgent = do
    program <- getProgName
    commit <- validatedCommit <$> lookupEnv "AGENT_BUILD_COMMIT"
    let product
            | program == "haskell-agent-macos" = "haskell-agent-macos"
            | otherwise = "agent-cli"
    pure $ BS.pack $
        product <> "/" <> showVersion Paths.version
            <> " (" <> os <> "; " <> arch
            <> "; commit " <> commit <> ")"

validatedCommit :: Maybe String -> String
validatedCommit value =
    fromMaybe "development" do
        commit <- value
        if not (null commit) && all validCommitCharacter commit
            then Just commit
            else Nothing
  where
    validCommitCharacter character =
        isAscii character
            && (isAlphaNum character || character `elem` ("-._" :: String))
