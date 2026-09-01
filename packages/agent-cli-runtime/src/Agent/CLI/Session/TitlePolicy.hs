-- | Stable persisted-session title refresh policy.
module Agent.CLI.Session.TitlePolicy
    ( titleRefreshIndex
    ) where

titleRefreshIndex :: Int -> Int
titleRefreshIndex milestone
    | milestone >= 6 = 2
    | milestone >= 3 = 1
    | otherwise = 0
