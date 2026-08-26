-- | Direct, typed JSON decoders.
--
-- A decoder consumes bytes and constructs the requested Haskell value.  No
-- generic JSON value representation is exposed.  Object fields are visited
-- once; an unknown-field callback can use 'rawJson' to retain its validated
-- bytes.
module Agent.Json.Decoder
    ( Decoder
    , JsonType(..)
    , NamedField
    , UnknownField
    , DecodeError(..)
    , PathElement(..)
    , nullValue
    , bool
    , text
    , string
    , scientific
    , int
    , integer
    , word
    , double
    , array
    , list
    , vector
    , nullable
    , byType
    , maybe
    , rawJson
    , skip
    , object
    , field
    , unknownField
    , mapEither
    , mapDecoder
    , decode
    , validateRawJson
    , renderDecodeError
    ) where

import Agent.Json (RawJson)
import Agent.Json.Decoder.Backend
import Agent.Json.Decoder.Portable
    ( DecodeError(..)
    , PathElement(..)
    , decode
    , renderDecodeError
    )
import qualified Data.ByteString as BS
import Data.Scientific
    ( Scientific
    , base10Exponent
    , coefficient
    , toBoundedInteger
    , toRealFloat
    )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Prelude
import Prelude hiding (maybe)

nullValue :: a -> Decoder a
nullValue = NullDecoder

bool :: Decoder Bool
bool = BoolDecoder

text :: Decoder Text
text = TextDecoder

string :: Decoder String
string = mapDecoder Text.unpack text

scientific :: Decoder Scientific
scientific = ScientificDecoder

int :: Decoder Int
int = mapEither bounded scientific
  where
    bounded value =
        Prelude.maybe
            (Left "expected an integer in the Int range")
            Right
            (toBoundedInteger value)

integer :: Decoder Integer
integer = mapEither bounded scientific
  where
    bounded value
        | abs decimalExponent > maximumIntegerExponent =
            Left "integer exponent exceeds the configured safety limit"
        | decimalExponent >= 0 =
            Right (coefficient value * 10 ^ decimalExponent)
        | otherwise =
            let divisor = 10 ^ negate decimalExponent
                (quotient, remainder) = coefficient value `quotRem` divisor
            in if remainder == 0
                then Right quotient
                else Left "expected an integer"
      where
        decimalExponent = base10Exponent value

    maximumIntegerExponent = 100_000

word :: Decoder Word
word = mapEither bounded scientific
  where
    bounded value =
        Prelude.maybe
            (Left "expected a non-negative integer in the Word range")
            Right
            (toBoundedInteger value)

double :: Decoder Double
double = mapDecoder toDouble scientific
  where
    toDouble value = toRealFloat value :: Double

array :: Decoder a -> Decoder [a]
array = ArrayDecoder

list :: Decoder a -> Decoder [a]
list = array

vector :: Decoder a -> Decoder (Vector.Vector a)
vector decoder = mapDecoder Vector.fromList (array decoder)

nullable :: Decoder a -> Decoder (Maybe a)
nullable = NullableDecoder

byType :: (JsonType -> Decoder a) -> Decoder a
byType = ByTypeDecoder

maybe :: Decoder a -> Decoder (Maybe a)
maybe = nullable

rawJson :: Decoder RawJson
rawJson = RawJsonDecoder

skip :: Decoder ()
skip = SkipDecoder

object
    :: state
    -> [NamedField state]
    -> UnknownField state
    -> (state -> Either Text a)
    -> Decoder a
object = ObjectDecoder

field
    :: Text
    -> Decoder value
    -> (value -> state -> Either Text state)
    -> NamedField state
field = NamedField

unknownField
    :: Decoder value
    -> (Text -> value -> state -> Either Text state)
    -> UnknownField state
unknownField = UnknownField

mapEither :: (a -> Either Text b) -> Decoder a -> Decoder b
mapEither = MapDecoder

mapDecoder :: (a -> b) -> Decoder a -> Decoder b
mapDecoder transform = mapEither (Right . transform)

validateRawJson :: BS.ByteString -> Either DecodeError RawJson
validateRawJson = decode rawJson
