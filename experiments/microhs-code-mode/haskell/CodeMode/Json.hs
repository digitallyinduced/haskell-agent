-- A dependency-free JSON codec for the MicroHs experiment. Numeric lexemes are
-- preserved rather than rounded through Double. This is not a production codec.
module CodeMode.Json
    ( Json(..)
    , encodeJson
    , decodeJson
    , lookupField
    ) where

import Control.Monad (replicateM)
import Data.Char (chr, ord, digitToInt, isHexDigit)
import Data.List (intercalate, nub)
import Numeric (showHex)
import Text.ParserCombinators.ReadP

data Json
    = JsonNull
    | JsonBool Bool
    | JsonNumber String
    | JsonString String
    | JsonArray [Json]
    | JsonObject [(String, Json)]
    deriving (Eq, Show)

lookupField :: String -> Json -> Maybe Json
lookupField key (JsonObject fields) = lookup key fields
lookupField _ _ = Nothing

encodeJson :: Json -> String
encodeJson JsonNull = "null"
encodeJson (JsonBool True) = "true"
encodeJson (JsonBool False) = "false"
encodeJson (JsonNumber value)
    | validNumber value = value
    | otherwise = error "invalid JSON number"
encodeJson (JsonString value) = encodeString value
encodeJson (JsonArray values) =
    "[" ++ intercalate "," (map encodeJson values) ++ "]"
encodeJson (JsonObject fields)
    | length names /= length (nub names) = error "duplicate JSON object key"
    | otherwise =
        "{" ++ intercalate "," (map encodeField fields) ++ "}"
  where
    names = map fst fields
    encodeField (key, value) = encodeString key ++ ":" ++ encodeJson value

encodeString :: String -> String
encodeString value = "\"" ++ concatMap encodeCharacter value ++ "\""

encodeCharacter :: Char -> String
encodeCharacter '"' = "\\\""
encodeCharacter '\\' = "\\\\"
encodeCharacter character
    | code >= 0xd800 && code <= 0xdfff = error "unpaired JSON surrogate"
    | code < 0x20 || code >= 0x7f =
        if code <= 0xffff
            then unicodeEscape code
            else unicodeEscape (0xd800 + (code - 0x10000) `div` 1024)
                ++ unicodeEscape (0xdc00 + (code - 0x10000) `mod` 1024)
    | otherwise = [character]
  where
    code = ord character

unicodeEscape :: Int -> String
unicodeEscape value =
    let digits = showHex value ""
    in "\\u" ++ replicate (4 - length digits) '0' ++ digits

decodeJson :: String -> Either String Json
decodeJson source =
    case readP_to_S (whitespace *> jsonValue 128 <* eof) source of
        [(value, "")] -> Right value
        _ -> Left "invalid JSON (maximum nesting depth is 128)"

whitespace :: ReadP ()
whitespace = skipMany (satisfy (`elem` " \t\r\n"))

token :: ReadP a -> ReadP a
token parser = parser <* whitespace

jsonValue :: Int -> ReadP Json
jsonValue depth
    | depth <= 0 = pfail
    | otherwise = token $
        (string "null" >> pure JsonNull)
        <++ (string "true" >> pure (JsonBool True))
        <++ (string "false" >> pure (JsonBool False))
        <++ (JsonString <$> jsonString)
        <++ (JsonNumber <$> jsonNumber)
        <++ (JsonArray <$> between (token (char '[')) (char ']')
            (sepBy (jsonValue (depth - 1)) (token (char ','))))
        <++ jsonObject depth

jsonObject :: Int -> ReadP Json
jsonObject depth = do
    fields <- between (token (char '{')) (char '}')
        (sepBy field (token (char ',')))
    let names = map fst fields
    if length names == length (nub names)
        then pure (JsonObject fields)
        else pfail
  where
    field = do
        key <- token jsonString
        _ <- token (char ':')
        value <- jsonValue (depth - 1)
        pure (key, value)

jsonString :: ReadP String
jsonString = between (char '"') (char '"') (many jsonCharacter)

jsonCharacter :: ReadP Char
jsonCharacter =
    satisfy (\character ->
        character /= '"' && character /= '\\' && ord character >= 0x20
            && not (ord character >= 0xd800 && ord character <= 0xdfff))
    <++ do
        _ <- char '\\'
        escaped <- get
        case escaped of
            '"' -> pure '"'
            '\\' -> pure '\\'
            '/' -> pure '/'
            'b' -> pure '\b'
            'f' -> pure '\f'
            'n' -> pure '\n'
            'r' -> pure '\r'
            't' -> pure '\t'
            'u' -> unicodeCharacter
            _ -> pfail

unicodeCharacter :: ReadP Char
unicodeCharacter = do
    first <- hexadecimalQuad
    if first >= 0xd800 && first <= 0xdbff
        then do
            _ <- string "\\u"
            second <- hexadecimalQuad
            if second >= 0xdc00 && second <= 0xdfff
                then pure (chr (0x10000 + (first - 0xd800) * 1024
                    + second - 0xdc00))
                else pfail
        else if first >= 0xdc00 && first <= 0xdfff
            then pfail
            else pure (chr first)

hexadecimalQuad :: ReadP Int
hexadecimalQuad = do
    digits <- replicateM 4 (satisfy isHexDigit)
    pure (foldl (\value digit -> value * 16 + digitToInt digit) 0 digits)

jsonNumber :: ReadP String
jsonNumber = do
    sign <- option "" (string "-")
    integral <- string "0" <++ do
        initial <- satisfy (\character -> character >= '1' && character <= '9')
        remainder <- munch asciiDigit
        pure (initial : remainder)
    fractional <- option "" $ do
        _ <- char '.'
        digits <- munch1 asciiDigit
        pure ('.' : digits)
    exponent <- option "" $ do
        marker <- satisfy (`elem` "eE")
        exponentSign <- option "" ((string "+") <++ (string "-"))
        digits <- munch1 asciiDigit
        pure ([marker] ++ exponentSign ++ digits)
    pure (sign ++ integral ++ fractional ++ exponent)

asciiDigit :: Char -> Bool
asciiDigit character = character >= '0' && character <= '9'

validNumber :: String -> Bool
validNumber value =
    case readP_to_S (jsonNumber <* eof) value of
        [(_, "")] -> True
        _ -> False
