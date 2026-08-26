-- | Direct JSON encoding.
--
-- An 'Encoder' compiles a domain value to a private exact-size write program.
-- Jsonifier's 'Json' type is a @(size, poke)@ program rather than a semantic
-- JSON value tree; it is never exposed by this module.
module Agent.Json.Encoder
    ( Encoder
    , Field
    , encode
    , contramap
    , nullValue
    , bool
    , text
    , string
    , scientific
    , int
    , integer
    , word
    , double
    , list
    , vector
    , maybe
    , rawJson
    , object
    , objectWithExtensions
    , field
    , optionalField
    , nullableField
    , extensionsField
    ) where

import Agent.Json
    ( Extensions
    , RawJson
    , extensionsToList
    , rawJsonBytes
    )
import qualified Data.ByteString as BS
import Data.Scientific (Scientific)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Jsonifier
import qualified Prelude
import Prelude hiding (maybe)

newtype Encoder a =
    Encoder (a -> Jsonifier.Json)

data Field a =
    Field
        { fieldName :: !(Maybe Text)
        , renderField :: !(a -> [(Text, Jsonifier.Json)])
        }

encode :: Encoder a -> a -> BS.ByteString
encode (Encoder renderValue) =
    Jsonifier.toByteString . renderValue

contramap :: (b -> a) -> Encoder a -> Encoder b
contramap transform (Encoder renderValue) =
    Encoder (renderValue . transform)

nullValue :: Encoder a
nullValue = Encoder (const Jsonifier.null)

bool :: Encoder Bool
bool = Encoder Jsonifier.bool

text :: Encoder Text
text = Encoder Jsonifier.textString

string :: Encoder String
string = contramap Text.pack text

scientific :: Encoder Scientific
scientific = Encoder Jsonifier.scientificNumber

int :: Encoder Int
int = Encoder Jsonifier.intNumber

integer :: Encoder Integer
integer = Encoder (Jsonifier.scientificNumber . fromInteger)

word :: Encoder Word
word = Encoder Jsonifier.wordNumber

double :: Encoder Double
double = Encoder \value ->
    if isNaN value || isInfinite value
        then Jsonifier.null
        else Jsonifier.doubleNumber value

list :: Encoder a -> Encoder [a]
list (Encoder renderValue) =
    Encoder (Jsonifier.array . map renderValue)

vector :: Foldable collection => Encoder a -> Encoder (collection a)
vector (Encoder renderValue) =
    Encoder (Jsonifier.array . foldr (\value rest -> renderValue value : rest) [])

maybe :: Encoder a -> Encoder (Maybe a)
maybe (Encoder renderValue) =
    Encoder (Prelude.maybe Jsonifier.null renderValue)

rawJson :: Encoder RawJson
rawJson =
    Encoder (Jsonifier.fromByteString . rawJsonBytes)

object :: [Field a] -> Encoder a
object fields =
    let reserved =
            Set.fromList
                [ key
                | Field { fieldName = Just key } <- fields
                ]
        withoutReserved (key, _) = key `Set.notMember` reserved
    in Encoder \value ->
        Jsonifier.object $
            concatMap
                (\Field { fieldName, renderField } ->
                    case fieldName of
                        Just _ -> renderField value
                        Nothing ->
                            filter withoutReserved (renderField value))
                fields

objectWithExtensions
    :: (a -> Extensions)
    -> [Field a]
    -> Encoder a
objectWithExtensions getExtensions fields =
    object (fields <> [extensionsField getExtensions])

field
    :: Text
    -> Encoder value
    -> (object -> value)
    -> Field object
field key (Encoder renderValue) getValue =
    Field
        { fieldName = Just key
        , renderField = \objectValue ->
            [(key, renderValue (getValue objectValue))]
        }

optionalField
    :: Text
    -> Encoder value
    -> (object -> Maybe value)
    -> Field object
optionalField key (Encoder renderValue) getValue =
    Field
        { fieldName = Just key
        , renderField = \objectValue -> case getValue objectValue of
            Nothing -> []
            Just value -> [(key, renderValue value)]
        }

nullableField
    :: Text
    -> Encoder value
    -> (object -> Maybe value)
    -> Field object
nullableField key (Encoder renderValue) getValue =
    Field
        { fieldName = Just key
        , renderField = \objectValue ->
            [(key, Prelude.maybe Jsonifier.null renderValue (getValue objectValue))]
        }

extensionsField :: (object -> Extensions) -> Field object
extensionsField getExtensions =
    Field
        { fieldName = Nothing
        , renderField =
            map
                (\(key, value) ->
                    (key, Jsonifier.fromByteString (rawJsonBytes value)))
                . extensionsToList
                . getExtensions
        }
