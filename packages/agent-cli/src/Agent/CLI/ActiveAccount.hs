-- | One coherent snapshot of the credential identity presented by a session.
module Agent.CLI.ActiveAccount
    ( ActiveAccount(..)
    , ActiveAccountRef
    , newActiveAccount
    , readActiveAccount
    , writeActiveAccount
    , modifyActiveAccount
    , trackCredentialAccount
    ) where

import Agent.Provider
    ( Credential(..)
    , TokenProvider
    , getNextToken
    , tokenProviderWithNextToken
    )
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , atomicWriteIORef
    , newIORef
    , readIORef
    )
import Data.Text (Text)

data ActiveAccount = ActiveAccount
    { activeAccountId :: !Text
    , activeSelectionId :: !Text
    , activeAccountLabel :: !Text
    }
    deriving (Eq, Show)

newtype ActiveAccountRef = ActiveAccountRef (IORef ActiveAccount)

newActiveAccount :: ActiveAccount -> IO ActiveAccountRef
newActiveAccount = fmap ActiveAccountRef . newIORef

readActiveAccount :: ActiveAccountRef -> IO ActiveAccount
readActiveAccount (ActiveAccountRef ref) = readIORef ref

writeActiveAccount :: ActiveAccountRef -> ActiveAccount -> IO ()
writeActiveAccount (ActiveAccountRef ref) = atomicWriteIORef ref

modifyActiveAccount :: ActiveAccountRef -> (ActiveAccount -> ActiveAccount) -> IO ()
modifyActiveAccount (ActiveAccountRef ref) update =
    atomicModifyIORef' ref (\current -> (update current, ()))

-- | Resolve the label before publishing any part of a newly acquired
-- credential. A failed or cancelled resolver leaves the previous snapshot
-- intact. Refreshing the same account retains its stable credential-source id.
trackCredentialAccount
    :: ActiveAccountRef
    -> (Credential -> IO Text)
    -> TokenProvider
    -> TokenProvider
trackCredentialAccount ref resolveLabel provider =
    tokenProviderWithNextToken provider \failed ->
        getNextToken provider failed >>= \case
            Left err -> pure (Left err)
            Right credential -> do
                label <- resolveLabel credential
                modifyActiveAccount ref \current -> ActiveAccount
                    { activeAccountId = credential.accountId
                    , activeSelectionId =
                        if current.activeAccountId == credential.accountId
                            then current.activeSelectionId
                            else credential.accountId
                    , activeAccountLabel = label
                    }
                pure (Right credential)
