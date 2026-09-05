-- | Revisioned accessibility observations for computer use.
module Agent.CLI.ComputerUse.Accessibility
    ( AccessibilitySnapshot(..)
    , AccessibilityPatchOperation(..)
    , AccessibilityObservation(..)
    , AccessibilityDeltaState
    , initialAccessibilityDeltaState
    , decodeAccessibilitySnapshot
    , advanceAccessibilityObservation
    , unavailableAccessibilityObservation
    , resetAccessibilityDeltaState
    , applyAccessibilityPatch
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- | A versioned native snapshot. @scope@ identifies the foreground
-- application/window and @contents@ contains the bounded AX tree.
data AccessibilitySnapshot = AccessibilitySnapshot
    { accessibilitySnapshotSchemaVersion :: !Int
    , accessibilitySnapshotScope :: !Aeson.Value
    , accessibilitySnapshotContents :: !Aeson.Value
    } deriving (Eq, Show)

instance Aeson.ToJSON AccessibilitySnapshot where
    toJSON snapshot = Aeson.object
        [ "schema_version" Aeson..=
            snapshot.accessibilitySnapshotSchemaVersion
        , "scope" Aeson..= snapshot.accessibilitySnapshotScope
        , "contents" Aeson..= snapshot.accessibilitySnapshotContents
        ]

instance Aeson.FromJSON AccessibilitySnapshot where
    parseJSON = Aeson.withObject "AccessibilitySnapshot" \object -> do
        schemaVersion <- object Aeson..: "schema_version"
        if schemaVersion /= (1 :: Int)
            then fail "unsupported accessibility snapshot schema_version"
            else AccessibilitySnapshot schemaVersion
                <$> object Aeson..: "scope"
                <*> object Aeson..: "contents"

-- | The subset of RFC 6902 operations emitted by the snapshot differ.
data AccessibilityPatchOperation
    = AccessibilityAdd !Text !Aeson.Value
    | AccessibilityRemove !Text
    | AccessibilityReplace !Text !Aeson.Value
    deriving (Eq, Show)

instance Aeson.ToJSON AccessibilityPatchOperation where
    toJSON = \case
        AccessibilityAdd path value -> Aeson.object
            [ "op" Aeson..= ("add" :: Text)
            , "path" Aeson..= path
            , "value" Aeson..= value
            ]
        AccessibilityRemove path -> Aeson.object
            [ "op" Aeson..= ("remove" :: Text)
            , "path" Aeson..= path
            ]
        AccessibilityReplace path value -> Aeson.object
            [ "op" Aeson..= ("replace" :: Text)
            , "path" Aeson..= path
            , "value" Aeson..= value
            ]

data AccessibilityObservation
    = AccessibilityFull !Int !AccessibilitySnapshot
    | AccessibilityDelta
        !Int
        !Int
        ![AccessibilityPatchOperation]
    | AccessibilityUnavailable !Int !Text
    deriving (Eq, Show)

instance Aeson.ToJSON AccessibilityObservation where
    toJSON = \case
        AccessibilityFull revision snapshot -> Aeson.object
            [ "kind" Aeson..= ("full" :: Text)
            , "revision" Aeson..= revision
            , "snapshot" Aeson..= snapshot
            ]
        AccessibilityDelta baseRevision revision patch -> Aeson.object
            [ "kind" Aeson..= ("delta" :: Text)
            , "base_revision" Aeson..= baseRevision
            , "revision" Aeson..= revision
            , "patch" Aeson..= patch
            ]
        AccessibilityUnavailable revision reason -> Aeson.object
            [ "kind" Aeson..= ("unavailable" :: Text)
            , "revision" Aeson..= revision
            , "reason" Aeson..= reason
            ]

data AccessibilityDeltaState = AccessibilityDeltaState
    { deltaRevision :: !Int
    , deltaBaseline :: !(Maybe AccessibilitySnapshot)
    , deltaCountSinceFull :: !Int
    } deriving (Eq, Show)

initialAccessibilityDeltaState :: AccessibilityDeltaState
initialAccessibilityDeltaState = AccessibilityDeltaState 0 Nothing 0

resetAccessibilityDeltaState
    :: AccessibilityDeltaState
    -> AccessibilityDeltaState
resetAccessibilityDeltaState state =
    state { deltaBaseline = Nothing, deltaCountSinceFull = 0 }

decodeAccessibilitySnapshot
    :: BS.ByteString
    -> Either Text AccessibilitySnapshot
decodeAccessibilitySnapshot bytes =
    case Aeson.eitherDecodeStrict' bytes of
        Left err -> Left (Text.pack err)
        Right snapshot -> Right snapshot

-- | Advance the model-visible accessibility stream. Large or periodically
-- checkpointed changes are sent as a full snapshot.
advanceAccessibilityObservation
    :: AccessibilityDeltaState
    -> AccessibilitySnapshot
    -> (AccessibilityObservation, AccessibilityDeltaState)
advanceAccessibilityObservation state snapshot =
    case state.deltaBaseline of
        Nothing -> full
        Just baseline
            | baseline.accessibilitySnapshotSchemaVersion
                /= snapshot.accessibilitySnapshotSchemaVersion -> full
            | baseline.accessibilitySnapshotScope
                /= snapshot.accessibilitySnapshotScope -> full
            | state.deltaCountSinceFull >= 8 -> full
            | patchSize * 5 >= fullSize * 3 -> full
            | otherwise ->
                ( AccessibilityDelta state.deltaRevision revision patch
                , AccessibilityDeltaState
                    revision
                    (Just snapshot)
                    (state.deltaCountSinceFull + 1)
                )
          where
            patch = diffValue "" (Aeson.toJSON baseline) (Aeson.toJSON snapshot)
            patchSize = encodedSize patch
            fullSize = encodedSize snapshot
  where
    revision = state.deltaRevision + 1
    full =
        ( AccessibilityFull revision snapshot
        , AccessibilityDeltaState revision (Just snapshot) 0
        )

unavailableAccessibilityObservation
    :: Text
    -> AccessibilityDeltaState
    -> (AccessibilityObservation, AccessibilityDeltaState)
unavailableAccessibilityObservation reason state =
    ( AccessibilityUnavailable revision reason
    , AccessibilityDeltaState revision Nothing 0
    )
  where
    revision = state.deltaRevision + 1

encodedSize :: Aeson.ToJSON value => value -> Int
encodedSize = fromIntegral . LBS.length . Aeson.encode

diffValue
    :: Text
    -> Aeson.Value
    -> Aeson.Value
    -> [AccessibilityPatchOperation]
diffValue _ old new
    | old == new = []
diffValue path (Aeson.Object old) (Aeson.Object new) =
    removals <> changes
  where
    removals =
        [ AccessibilityRemove (childPath path key)
        | (key, _) <- KeyMap.toAscList old
        , not (KeyMap.member key new)
        ]
    changes = concat
        [ case KeyMap.lookup key old of
            Nothing -> [AccessibilityAdd (childPath path key) value]
            Just oldValue -> diffValue (childPath path key) oldValue value
        | (key, value) <- KeyMap.toAscList new
        ]
-- Equal-length arrays can be compared by index, which keeps ordinary AX
-- property changes small. Structural array changes use one exact replacement
-- and let the checkpoint-size policy decide whether a full snapshot is better.
diffValue path (Aeson.Array old) new@(Aeson.Array values)
    | Vector.length old == Vector.length values =
        concat
            [ diffValue
                (path <> "/" <> Text.pack (show index))
                oldValue
                newValue
            | (index, (oldValue, newValue)) <-
                zip [0 :: Int ..]
                    (zip (Vector.toList old) (Vector.toList values))
            ]
    | otherwise = [AccessibilityReplace path new]
diffValue path _ new = [AccessibilityReplace path new]

childPath :: Text -> Key.Key -> Text
childPath parent key =
    parent <> "/" <> escapePointerToken (Key.toText key)

escapePointerToken :: Text -> Text
escapePointerToken = Text.replace "/" "~1" . Text.replace "~" "~0"

-- | Apply emitted operations. Exported primarily for protocol consumers and
-- reconstruction-property tests.
applyAccessibilityPatch
    :: Aeson.Value
    -> [AccessibilityPatchOperation]
    -> Either Text Aeson.Value
applyAccessibilityPatch = foldl' step . Right
  where
    step result operation = result >>= \value -> applyOne value operation

applyOne
    :: Aeson.Value
    -> AccessibilityPatchOperation
    -> Either Text Aeson.Value
applyOne value operation =
    case operation of
        AccessibilityAdd path replacement ->
            updateAt True path (const (Right replacement)) value
        AccessibilityRemove path ->
            removeAt path value
        AccessibilityReplace path replacement ->
            updateAt False path (const (Right replacement)) value

updateAt
    :: Bool
    -> Text
    -> (Aeson.Value -> Either Text Aeson.Value)
    -> Aeson.Value
    -> Either Text Aeson.Value
updateAt allowMissing path update root
    | Text.null path = update root
    | otherwise = descend (pointerTokens path) root
  where
    descend [] value = update value
    descend (token : rest) (Aeson.Object object)
        | null rest
        , allowMissing =
            Right (Aeson.Object
                (KeyMap.insert (Key.fromText token)
                    (either (const Aeson.Null) id (update Aeson.Null))
                    object))
        | otherwise = do
            child <- maybe
                (Left ("JSON Pointer does not exist: " <> path))
                Right
                (KeyMap.lookup (Key.fromText token) object)
            replacement <- descend rest child
            Right (Aeson.Object
                (KeyMap.insert (Key.fromText token) replacement object))
    descend (token : rest) (Aeson.Array values) = do
        index <- parseIndex token
        child <- maybe
            (Left ("JSON Pointer does not exist: " <> path))
            Right
            (values Vector.!? index)
        replacement <- descend rest child
        Right (Aeson.Array (values Vector.// [(index, replacement)]))
    descend _ _ = Left ("JSON Pointer does not identify a container: " <> path)

removeAt :: Text -> Aeson.Value -> Either Text Aeson.Value
removeAt path root
    | Text.null path = Left "cannot remove the document root"
    | otherwise = descend (pointerTokens path) root
  where
    descend [] _ = Left "cannot remove the document root"
    descend [token] (Aeson.Object object)
        | KeyMap.member key object =
            Right (Aeson.Object (KeyMap.delete key object))
        | otherwise = Left ("JSON Pointer does not exist: " <> path)
      where
        key = Key.fromText token
    descend [token] (Aeson.Array values) = do
        index <- parseIndex token
        if index < Vector.length values
            then Right (Aeson.Array
                (Vector.take index values <> Vector.drop (index + 1) values))
            else Left ("JSON Pointer does not exist: " <> path)
    descend (token : rest) (Aeson.Object object) = do
        child <- maybe
            (Left ("JSON Pointer does not exist: " <> path))
            Right
            (KeyMap.lookup key object)
        replacement <- descend rest child
        Right (Aeson.Object (KeyMap.insert key replacement object))
      where
        key = Key.fromText token
    descend (token : rest) (Aeson.Array values) = do
        index <- parseIndex token
        child <- maybe
            (Left ("JSON Pointer does not exist: " <> path))
            Right
            (values Vector.!? index)
        replacement <- descend rest child
        Right (Aeson.Array (values Vector.// [(index, replacement)]))
    descend _ _ = Left ("JSON Pointer does not identify a container: " <> path)

pointerTokens :: Text -> [Text]
pointerTokens =
    map unescapePointerToken . Text.splitOn "/" . Text.drop 1

unescapePointerToken :: Text -> Text
unescapePointerToken = Text.replace "~1" "/" . Text.replace "~0" "~"

parseIndex :: Text -> Either Text Int
parseIndex token =
    case reads (Text.unpack token) of
        [(index, "")] | index >= 0 -> Right index
        _ -> Left ("invalid JSON Pointer array index: " <> token)
