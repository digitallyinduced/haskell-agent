-- | Standalone provider transport using the local credential store.
module Agent.Integrations.Email.Transport
    ( productionMailTransport
    , module Agent.Mail.Transport
    ) where

import Agent.Integrations.Email.OAuth (refreshMailOAuthCredential)
import qualified Agent.Integrations.Email.Store as Store
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
