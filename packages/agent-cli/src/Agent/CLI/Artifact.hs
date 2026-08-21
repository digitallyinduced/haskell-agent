-- | Extract useful artifacts from the most recent assistant response.
module Agent.CLI.Artifact
    ( fencedBlocks
    , fencedCodeBlock
    , lastDiffBlock
    ) where

import Data.Text (Text)
import qualified Data.Text as Text

-- | Markdown fenced blocks as @(language, body)@ pairs.
fencedBlocks :: Text -> [(Text, Text)]
fencedBlocks = go . Text.lines
  where
    go [] = []
    go (line:rest) =
        case fenceStart line of
            Nothing -> go rest
            Just (marker, language) ->
                let (body, remaining) = break (isFenceEnd marker) rest
                    after = case remaining of
                        [] -> []
                        (_:xs) -> xs
                in (language, Text.unlines body) : go after

    fenceStart line =
        let stripped = Text.stripStart line
            opening marker =
                Just
                    ( marker
                    , Text.toLower (Text.strip (Text.drop 3 stripped))
                    )
        in if "```" `Text.isPrefixOf` stripped
            then opening '`'
            else if "~~~" `Text.isPrefixOf` stripped
                then opening '~'
                else Nothing

    isFenceEnd marker line =
        Text.replicate 3 (Text.singleton marker)
            `Text.isPrefixOf` Text.stripStart line

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
