-- | Extract useful artifacts from the most recent assistant response.
module Agent.CLI.Artifact
    ( fencedBlocks
    , fencedCodeBlock
    , lastDiffBlock
    ) where

import qualified Agent.TUI.FencedCode as FencedCode
import Data.Text (Text)
import qualified Data.Text as Text

-- | Markdown fenced blocks as @(language, body)@ pairs.
fencedBlocks :: Text -> [(Text, Text)]
fencedBlocks input =
    map
        (\block ->
            ( Text.toLower (Text.strip block.fencedInfo)
            , block.fencedBody
            ))
        (FencedCode.fencedBlocks input)

fencedCodeBlock :: Int -> Text -> Maybe Text
fencedCodeBlock index text
    | index <= 0 = Nothing
    | otherwise = snd <$> atMay (index - 1) (fencedBlocks text)

lastDiffBlock :: Text -> Maybe Text
lastDiffBlock text =
    case filter (isDiffLanguage . fst) (fencedBlocks text) of
        [] -> Nothing
        blocks -> Just (snd (last blocks))
  where
    isDiffLanguage language =
        language `elem` ["diff", "patch", "udiff"]

atMay :: Int -> [a] -> Maybe a
atMay n _ | n < 0 = Nothing
atMay _ [] = Nothing
atMay 0 (x:_) = Just x
atMay n (_:xs) = atMay (n - 1) xs
