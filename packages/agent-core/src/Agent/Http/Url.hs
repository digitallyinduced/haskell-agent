-- | Small URL helpers shared by HTTP clients.
module Agent.Http.Url
    ( trimTrailingSlash
    ) where

-- | Drop trailing @/@ characters from a URL prefix.
trimTrailingSlash :: String -> String
trimTrailingSlash = reverse . dropWhile (== '/') . reverse
