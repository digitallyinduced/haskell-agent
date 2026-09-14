-- | Small URL helpers shared by HTTP clients.
module Agent.Http.Url
    ( trimTrailingSlash
    ) where

import Data.List (dropWhileEnd)

-- | Drop trailing @/@ characters from a URL prefix.
trimTrailingSlash :: String -> String
trimTrailingSlash = dropWhileEnd (== '/')
