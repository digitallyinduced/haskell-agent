-- | Standalone provider transport using the local credential store.
module Agent.CLI.Mail.Transport
    ( productionMailTransport
    , module Agent.Mail.Transport
    ) where

import Agent.CLI.Mail.OAuth (refreshMailOAuthCredential)
import qualified Agent.CLI.Mail.Store as Store
import Agent.Mail.Transport
import Agent.Mail.Types
    ( MailTransport
    , MailTransportHooks(..)
    )

productionMailTransport :: MailTransport
productionMailTransport =
    mailTransportWithHooks MailTransportHooks
        { mailTransportRefreshCredential =
            refreshMailOAuthCredential
        , mailTransportRecordAccountState =
            Store.setMailAccountStateIfUnchanged
        }
