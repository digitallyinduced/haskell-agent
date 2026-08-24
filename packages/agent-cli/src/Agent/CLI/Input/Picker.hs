-- | Pure picker and approval-key helpers.
module Agent.CLI.Input.Picker
    ( parseChoiceKey
    , choiceMoveIndex
    , approvalKeyText
    ) where

import Agent.CLI.Input.Types (ChoiceKey(..))
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as Text

parseChoiceKey :: String -> Maybe ChoiceKey
parseChoiceKey = \case
    "\n" -> Just ChoiceEnter
    "\r" -> Just ChoiceEnter
    "\ESC" -> Just ChoiceCancel
    "\ESC[A" -> Just ChoiceUp
    "\ESC[B" -> Just ChoiceDown
    "\ESCOA" -> Just ChoiceUp
    "\ESCOB" -> Just ChoiceDown
    "k" -> Just ChoiceUp
    "j" -> Just ChoiceDown
    "q" -> Just ChoiceCancel
    "Q" -> Just ChoiceCancel
    [c]
        | c >= '1' && c <= '9' -> Just (ChoiceDigit (ord c - ord '0'))
    _ -> Nothing

choiceMoveIndex :: Int -> Int -> ChoiceKey -> Int
choiceMoveIndex len idx key
    | len <= 0 = 0
    | otherwise = case key of
        ChoiceUp -> if idx <= 0 then len - 1 else idx - 1
        ChoiceDown -> if idx >= len - 1 then 0 else idx + 1
        _ -> idx

approvalKeyText :: Char -> Text
approvalKeyText c
    | c == '\n' || c == '\r' = ""
    | otherwise = Text.singleton c
