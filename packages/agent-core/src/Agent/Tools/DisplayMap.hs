-- | Versioned, persistent map presentation results. The result is a public
-- cross-client document, not an in-process request protocol.
module Agent.Tools.DisplayMap
    ( MapLocation(..)
    , MapResult(..)
    , displayMapTool
    , mapResultDecoder
    , decodeMapResult
    , renderMapResult
    , mapResultText
    , maximumMapResultBytes
    ) where

import Agent.Json.Decode (Decoder)
import qualified Agent.Json.Decode as Json
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch (typedTool)
import Agent.Tools.Types (AppTool, ToolExecutionPolicy(..), jsonTool)
import Control.Monad (unless)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

data MapLocation = MapLocation
    { identifier :: !Text
    , name :: !Text
    , latitude :: !Double
    , longitude :: !Double
    , address :: !(Maybe Text)
    , description :: !(Maybe Text)
    } deriving (Eq, Show)

data MapResult = MapResult
    { title :: !Text
    , locations :: ![MapLocation]
    } deriving (Eq, Show)

maximumMapResultBytes :: Int
maximumMapResultBytes = 256 * 1024

boundedText :: Text -> Int -> Decoder Text
boundedText field limit = do
    value <- Json.text
    unless (not (Text.null (Text.strip value))
        && BS.length (Text.encodeUtf8 value) <= limit
        && not (Text.any (\character -> character < ' ' && character /= '\n' && character /= '\t') value)) $
        fail (Text.unpack field <> " must be nonblank and at most " <> show limit <> " UTF-8 bytes, without control characters")
    pure value

coordinate :: Text -> Double -> Decoder Double
coordinate field limit = do
    value <- Json.double
    unless (not (isNaN value || isInfinite value) && abs value <= limit) $
        fail (Text.unpack field <> " is outside the valid coordinate range")
    pure value

locationDecoder :: Decoder MapLocation
locationDecoder = Json.object $
    MapLocation
        <$> Json.atKey "id" (boundedText "id" 64)
        <*> Json.atKey "name" (boundedText "name" 200)
        <*> Json.atKey "latitude" (coordinate "latitude" 90)
        <*> Json.atKey "longitude" (coordinate "longitude" 180)
        <*> Json.optionalKey "address" (boundedText "address" 500)
        <*> Json.optionalKey "description" (boundedText "description" 1000)

locationsDecoder :: Decoder [MapLocation]
locationsDecoder = do
    locations <- Json.list locationDecoder
    unless (not (null locations) && length locations <= 100) $
        fail "locations must contain between 1 and 100 places"
    unless (Set.size (Set.fromList (map (.identifier) locations)) == length locations) $
        fail "location IDs must be unique within a map"
    pure locations

mapArgumentsDecoder :: Decoder MapResult
mapArgumentsDecoder = Json.object $
    MapResult
        <$> Json.atKey "title" (boundedText "title" 200)
        <*> Json.atKey "locations" locationsDecoder

mapResultDecoder :: Decoder MapResult
mapResultDecoder = Json.object do
    kind <- Json.atKey "type" Json.text
    version <- Json.atKey "version" Json.int
    unless (kind == "map" && version == 1) $
        fail "Unsupported map result type or version"
    MapResult
        <$> Json.atKey "title" (boundedText "title" 200)
        <*> Json.atKey "locations" locationsDecoder

decodeMapResult :: Text -> Maybe MapResult
decodeMapResult value
    | Text.compareLength value maximumMapResultBytes == GT = Nothing
    | not ("{" `Text.isPrefixOf` Text.stripStart value) = Nothing
    | BS.length bytes > maximumMapResultBytes = Nothing
    | otherwise = either (const Nothing) Just (Json.decodeEither mapResultDecoder bytes)
  where
    bytes = Text.encodeUtf8 value

renderMapResult :: MapResult -> Either Text Text
renderMapResult result =
    let bytes = LBS.toStrict (Aeson.encode (Aeson.object
            [ "type" .= ("map" :: Text)
            , "version" .= (1 :: Int)
            , "title" .= result.title
            , "locations" .= map locationJSON result.locations
            , "text" .= mapResultText result
            ]))
    in if BS.length bytes > maximumMapResultBytes
        then Left "Map result exceeds 256 KiB; use fewer places or shorter descriptions."
        else Right (Text.decodeUtf8 bytes)
  where
    locationJSON :: MapLocation -> Aeson.Value
    locationJSON location = Aeson.object
        [ "id" .= location.identifier
        , "name" .= location.name
        , "latitude" .= location.latitude
        , "longitude" .= location.longitude
        , "address" .= location.address
        , "description" .= location.description
        ]

mapResultText :: MapResult -> Text
mapResultText result = Text.intercalate "\n" $
    [result.title, "Locations use supplied coordinates; they have not been verified by a map service."]
        <> concat
            [ [Text.pack (show index) <> ". " <> location.name
                <> " (" <> Text.pack (show location.latitude)
                <> ", " <> Text.pack (show location.longitude) <> ")"]
                <> maybe [] (pure . ("   " <>)) location.address
                <> maybe [] (pure . ("   " <>)) location.description
            | (index, location) <- zip [1 :: Int ..] result.locations
            ]

displayMapTool :: AppTool
displayMapTool =
    jsonTool "display_map"
        "Present named locations as a persistent map result. Native clients render an interactive map; other clients show a place list. Supply coordinates from the user or retrieved sources; never invent precise coordinates. This tool does not search, geocode, calculate routes, or access current location. For a follow-up, create a new map. Use 1–100 locations with unique IDs; strings have UTF-8 byte limits."
        [ PropertySchema "title" PropertyString True (Just "Map title; 1–200 UTF-8 bytes.")
        , PropertySchema "locations" (PropertyArray (PropertyObject
            [ PropertySchema "id" PropertyString True (Just "Unique identifier within this map; 1–64 UTF-8 bytes.")
            , PropertySchema "name" PropertyString True (Just "Place name; 1–200 UTF-8 bytes.")
            , PropertySchema "latitude" PropertyNumber True (Just "Finite latitude in degrees, from -90 to 90.")
            , PropertySchema "longitude" PropertyNumber True (Just "Finite longitude in degrees, from -180 to 180.")
            , PropertySchema "address" PropertyString False (Just "Optional supplied address; 1–500 UTF-8 bytes.")
            , PropertySchema "description" PropertyString False (Just "Optional supplied details; 1–1000 UTF-8 bytes.")
            ])) True (Just "Between 1 and 100 locations.")
        ]
        True ParallelSafe
        (typedTool "display_map" mapArgumentsDecoder (pure . renderMapResult))
