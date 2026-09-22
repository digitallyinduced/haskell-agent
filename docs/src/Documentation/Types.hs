module Documentation.Types where

import Data.Text (Text)
import Text.Blaze.Html (Html)

data Page = Page
    { pagePath :: !Text
    , pageTitle :: !Text
    , pageDescription :: !Text
    , pageGroup :: !Text
    , pageBody :: Html
    }
