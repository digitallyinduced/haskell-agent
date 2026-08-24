-- | Bracketed-paste classification and decoding.
module Agent.CLI.Input.Paste
    ( submissionPromptText
    , classifyPastedText
    , formatPasteChip
    , stripBracketedPaste
    , decodeBracketedPastePayload
    ) where

import Data.Text (Text)
import qualified Data.Text as Text

submissionPromptText :: Int -> Text -> Maybe Text
submissionPromptText attachmentCount text
    | not (Text.null (Text.strip text)) = Just text
    | attachmentCount > 0 = Just "The user attached an image."
    | otherwise = Nothing

classifyPastedText :: Text -> (Text, Bool)
classifyPastedText raw =
    let stripped = stripBracketedPaste raw
        looksPasted = raw /= stripped || Text.count "\n" stripped >= 3
    in (stripped, looksPasted)

formatPasteChip :: Text -> Text
formatPasteChip text =
    let n = max 1 (length (Text.lines text))
    in if n < 4
        then text
        else "[Pasted: " <> Text.pack (show n) <> " lines]"

stripBracketedPaste :: Text -> Text
stripBracketedPaste text =
    Text.filter (not . isPasteSentinel)
        (Text.replace pasteEnd "" (Text.replace pasteStart "" text))
  where
    pasteStart = "\ESC[200~"
    pasteEnd = "\ESC[201~"

isPasteSentinel :: Char -> Bool
isPasteSentinel char =
    char == '\x27E6' || char == '\x27E7'

decodeBracketedPastePayload :: Int -> Text -> Either Text Text
decodeBracketedPastePayload limit input =
    let (payload, rest) = Text.breakOn (Text.pack "\ESC[201~") input
    in if Text.null rest
        then Left "bracketed paste is missing its end marker"
        else if Text.length payload > max 0 limit
            then Left "bracketed paste exceeds the size limit"
            else Right payload
