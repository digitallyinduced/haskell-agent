-- | Exact organization-gateway routing boundaries for shared frontends.
--
-- A boundary is captured when work is admitted and must travel with that work
-- until every callback has completed. 'Nothing' is intentionally represented
-- as a concrete boundary: direct-provider work must not become gateway work
-- (or vice versa) merely because credentials changed while it was queued.
module Agent.CLI.GatewayBoundary
    ( GatewayBoundary(..)
    , GatewayBoundarySnapshot(..)
    , GatewayBoundaryError(..)
    , gatewayBoundaryFromCredential
    , gatewayBoundariesMatch
    , renderGatewayBoundaryError
    , validateGatewayBoundary
    , validateGatewaySessionBoundary
    , loadGatewayBoundary
    , loadGatewayBoundaryAt
    , loadGatewayBoundarySnapshot
    , loadGatewayBoundarySnapshotAt
    , withCurrentGatewayBoundary
    , withCurrentGatewayBoundaryAt
    , withCurrentGatewayCredentialBoundary
    , withCurrentGatewayCredentialBoundaryAt
    , withExpectedGatewayBoundary
    , withExpectedGatewayBoundaryAt
    , withGatewayTurnBoundary
    , withGatewayTurnBoundaryAt
    ) where

import Agent.CLI.GatewayClient
    ( GatewayCredential
    , gatewayCredentialIdentity
    , loadGatewayCredentialAt
    , withGatewayCredentialLeaseAt
    , withGatewayCredentialTurnLeaseAt
    )
import Agent.CLI.Models (validateResumedGatewayBoundary)
import Data.Bifunctor (first)
import Data.Text (Text)
import System.Directory.OsPath qualified as Directory
import System.OsPath (OsPath)

-- | The exact credential identity under which an operation was admitted.
--
-- The identity is a stable, non-secret digest produced by
-- 'gatewayCredentialIdentity'. 'Nothing' denotes the direct-provider route.
newtype GatewayBoundary = GatewayBoundary
    { gatewayBoundaryIdentity :: Maybe Text
    }
    deriving (Eq, Ord, Show)

-- | A credential and its inseparable routing boundary.
--
-- This type deliberately has no 'Show' instance: a gateway credential contains
-- a bearer token and must never be rendered by generic logging.
data GatewayBoundarySnapshot = GatewayBoundarySnapshot
    { gatewayBoundaryCredential :: !(Maybe GatewayCredential)
    , gatewayBoundary :: !GatewayBoundary
    }
    deriving (Eq)

-- | Failures that prevent an operation from crossing a routing boundary.
data GatewayBoundaryError
    = GatewayBoundaryCredentialLoadFailed !Text
    | GatewayBoundaryChanged
    | GatewayBoundarySessionRejected !Text
    deriving (Eq, Show)

-- | Construct the exact boundary associated with a credential snapshot.
gatewayBoundaryFromCredential
    :: Maybe GatewayCredential
    -> GatewayBoundary
gatewayBoundaryFromCredential credential =
    GatewayBoundary
        { gatewayBoundaryIdentity =
            gatewayCredentialIdentity <$> credential
        }

-- | Exact boundary equality. Direct and gateway routes are distinct, and
-- replacing a credential creates a different gateway boundary.
gatewayBoundariesMatch :: GatewayBoundary -> GatewayBoundary -> Bool
gatewayBoundariesMatch = (==)

renderGatewayBoundaryError :: GatewayBoundaryError -> Text
renderGatewayBoundaryError = \case
    GatewayBoundaryCredentialLoadFailed err ->
        "Could not load gateway credentials: " <> err
    GatewayBoundaryChanged ->
        "Gateway credentials changed during the session operation."
    GatewayBoundarySessionRejected err -> err

-- | Validate that a freshly loaded boundary is still the admitted boundary.
validateGatewayBoundary
    :: GatewayBoundary
    -- ^ Boundary captured at admission.
    -> GatewayBoundary
    -- ^ Current boundary.
    -> Either GatewayBoundaryError ()
validateGatewayBoundary expected current
    | gatewayBoundariesMatch expected current = Right ()
    | otherwise = Left GatewayBoundaryChanged

-- | Validate persisted session routing metadata against the current boundary.
--
-- This preserves the stricter distinction between a direct connection,
-- a gateway connection without legacy identity binding, and an exact gateway
-- credential identity.
validateGatewaySessionBoundary
    :: GatewayBoundary
    -> Text
    -> Maybe Text
    -> Either GatewayBoundaryError ()
validateGatewaySessionBoundary boundary connection persistedIdentity =
    first GatewayBoundarySessionRejected $
        validateResumedGatewayBoundary
            boundary.gatewayBoundaryIdentity
            connection
            persistedIdentity

loadGatewayBoundary :: IO (Either GatewayBoundaryError GatewayBoundary)
loadGatewayBoundary = do
    home <- Directory.getHomeDirectory
    loadGatewayBoundaryAt home

loadGatewayBoundaryAt
    :: OsPath
    -> IO (Either GatewayBoundaryError GatewayBoundary)
loadGatewayBoundaryAt home =
    fmap (fmap (\snapshot -> snapshot.gatewayBoundary))
        (loadGatewayBoundarySnapshotAt home)

loadGatewayBoundarySnapshot
    :: IO (Either GatewayBoundaryError GatewayBoundarySnapshot)
loadGatewayBoundarySnapshot = do
    home <- Directory.getHomeDirectory
    loadGatewayBoundarySnapshotAt home

loadGatewayBoundarySnapshotAt
    :: OsPath
    -> IO (Either GatewayBoundaryError GatewayBoundarySnapshot)
loadGatewayBoundarySnapshotAt home =
    snapshotFromLoaded <$> loadGatewayCredentialAt home

-- | Run a short operation under one credential lease and supply its exact
-- boundary. Credential replacement cannot interleave with the action.
withCurrentGatewayBoundary
    :: (GatewayBoundary -> IO value)
    -> IO (Either GatewayBoundaryError value)
withCurrentGatewayBoundary action =
    Directory.getHomeDirectory >>= \home ->
        withCurrentGatewayBoundaryAt home action

withCurrentGatewayBoundaryAt
    :: OsPath
    -> (GatewayBoundary -> IO value)
    -> IO (Either GatewayBoundaryError value)
withCurrentGatewayBoundaryAt home action =
    withGatewayCredentialLeaseAt home $
        loadGatewayBoundaryAt home >>= traverse action

-- | Like 'withCurrentGatewayBoundary', but also supplies the credential
-- snapshot when a transport needs it. Keep the snapshot inside this callback;
-- it contains the bearer token.
withCurrentGatewayCredentialBoundary
    :: (GatewayBoundarySnapshot -> IO value)
    -> IO (Either GatewayBoundaryError value)
withCurrentGatewayCredentialBoundary action =
    Directory.getHomeDirectory >>= \home ->
        withCurrentGatewayCredentialBoundaryAt home action

withCurrentGatewayCredentialBoundaryAt
    :: OsPath
    -> (GatewayBoundarySnapshot -> IO value)
    -> IO (Either GatewayBoundaryError value)
withCurrentGatewayCredentialBoundaryAt home action =
    withGatewayCredentialLeaseAt home $
        loadGatewayBoundarySnapshotAt home >>= traverse action

-- | Revalidate an admitted boundary and run one callback atomically under a
-- short credential lease. Use this for approval, progress, and terminal
-- callbacks, including the terminal callback after a turn lease is released.
withExpectedGatewayBoundary
    :: GatewayBoundary
    -> IO value
    -> IO (Either GatewayBoundaryError value)
withExpectedGatewayBoundary expected action =
    Directory.getHomeDirectory >>= \home ->
        withExpectedGatewayBoundaryAt home expected action

withExpectedGatewayBoundaryAt
    :: OsPath
    -> GatewayBoundary
    -> IO value
    -> IO (Either GatewayBoundaryError value)
withExpectedGatewayBoundaryAt home expected action =
    withGatewayCredentialLeaseAt home $
        loadGatewayBoundaryAt home >>= runLoadedIfExpected expected action

-- | Hold the credential lease for the full lifetime of a turn.
--
-- Long-running turns use the admission-aware lease so a waiting credential
-- writer cannot be starved by newly admitted turns. Callbacks made during the
-- turn may take short leases; after this function returns, terminal delivery
-- must still use 'withExpectedGatewayBoundary'.
withGatewayTurnBoundary
    :: GatewayBoundary
    -> IO value
    -> IO (Either GatewayBoundaryError value)
withGatewayTurnBoundary expected action =
    Directory.getHomeDirectory >>= \home ->
        withGatewayTurnBoundaryAt home expected action

withGatewayTurnBoundaryAt
    :: OsPath
    -> GatewayBoundary
    -> IO value
    -> IO (Either GatewayBoundaryError value)
withGatewayTurnBoundaryAt home expected action =
    withGatewayCredentialTurnLeaseAt home $
        loadGatewayBoundaryAt home >>= runLoadedIfExpected expected action

snapshotFromLoaded
    :: Either Text (Maybe GatewayCredential)
    -> Either GatewayBoundaryError GatewayBoundarySnapshot
snapshotFromLoaded =
    first GatewayBoundaryCredentialLoadFailed
        . fmap
            (\credential ->
                GatewayBoundarySnapshot
                    { gatewayBoundaryCredential = credential
                    , gatewayBoundary =
                        gatewayBoundaryFromCredential credential
                    })

runIfExpected
    :: GatewayBoundary
    -> IO value
    -> GatewayBoundary
    -> IO (Either GatewayBoundaryError value)
runIfExpected expected action current =
    case validateGatewayBoundary expected current of
        Left err -> pure (Left err)
        Right () -> Right <$> action

runLoadedIfExpected
    :: GatewayBoundary
    -> IO value
    -> Either GatewayBoundaryError GatewayBoundary
    -> IO (Either GatewayBoundaryError value)
runLoadedIfExpected expected action = \case
    Left err -> pure (Left err)
    Right current -> runIfExpected expected action current
