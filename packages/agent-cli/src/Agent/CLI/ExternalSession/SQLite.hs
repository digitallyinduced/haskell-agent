module Agent.CLI.ExternalSession.SQLite
    ( decodeJsonish
    , execute
    , forRows
    , queryRows
    , sqlDataText
    , tableColumns
    , withReadOnlyDatabase
    , withTemporaryDatabase
    ) where

import Agent.CLI.ExternalSession.JSONL (decodeBoundedJsonValue)
import Control.Applicative ((<|>))
import Control.Exception.Safe
    ( IOException
    , bracket
    , finally
    , tryIO
    )
import Control.Monad (void)
import Data.Aeson (Value)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (digitToInt, isHexDigit)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import qualified Data.Text.Encoding
import Data.Text.Encoding.Error (lenientDecode)
import Database.SQLite3
    ( Database
    , SQLData(..)
    , SQLOpenFlag(..)
    , SQLVFS(SQLVFSDefault)
    , StepResult(..)
    )
import qualified Database.SQLite3 as SQLite
import System.Directory (removeFile)
import System.IO (hClose, openBinaryTempFile)
import System.Posix.Files
    ( ownerReadMode
    , ownerWriteMode
    , setFileMode
    , unionFileModes
    )

withReadOnlyDatabase :: FilePath -> (Database -> IO value) -> IO value
withReadOnlyDatabase path action =
    bracket
        (SQLite.open2
            (Text.pack path)
            [ SQLOpenReadOnly
            , SQLOpenNoFollow
            , SQLOpenPrivateCache
            ]
            SQLVFSDefault)
        SQLite.close
        \database -> do
            SQLite.exec database "PRAGMA query_only = ON"
            SQLite.exec database "PRAGMA busy_timeout = 5000"
            action database

withTemporaryDatabase
    :: FilePath
    -> String
    -> (Database -> IO value)
    -> IO value
withTemporaryDatabase scratch prefix action = do
    (path, handle) <- openBinaryTempFile scratch prefix
    hClose handle
    setFileMode path (ownerReadMode `unionFileModes` ownerWriteMode)
    let cleanup = mapM_ removeQuietly
            [path, path <> "-journal", path <> "-wal", path <> "-shm"]
    flip finally cleanup $
        bracket
            (SQLite.open2
                (Text.pack path)
                [ SQLOpenReadWrite
                , SQLOpenNoFollow
                , SQLOpenPrivateCache
                ]
                SQLVFSDefault)
            SQLite.close
            \database -> do
                SQLite.exec database "PRAGMA journal_mode = OFF"
                SQLite.exec database "PRAGMA synchronous = OFF"
                action database

removeQuietly :: FilePath -> IO ()
removeQuietly path =
    void (tryIO (removeFile path) :: IO (Either IOException ()))

forRows
    :: Database
    -> Text
    -> [SQLData]
    -> ([SQLData] -> IO ())
    -> IO ()
forRows database sql parameters consume =
    SQLite.withStatement database sql \statement -> do
        SQLite.bind statement parameters
        let loop =
                SQLite.step statement >>= \case
                    Row -> SQLite.columns statement >>= consume >> loop
                    Done -> pure ()
        loop

queryRows :: Database -> Text -> [SQLData] -> IO [[SQLData]]
queryRows database sql parameters = do
    rowsRef <- newIORef []
    forRows database sql parameters \row ->
        modifyIORef' rowsRef (row :)
    reverse <$> readIORef rowsRef

execute :: Database -> Text -> [SQLData] -> IO ()
execute database sql parameters =
    SQLite.withStatement database sql \statement -> do
        SQLite.bind statement parameters
        let loop =
                SQLite.step statement >>= \case
                    Row -> loop
                    Done -> pure ()
        loop

tableColumns :: Database -> Text -> IO (Set Text)
tableColumns database table
    | table `notElem` ["threads", "composerHeaders", "ItemTable", "blobs",
                       "cursorDiskKV"] =
        pure Set.empty
    | otherwise = do
        rows <- queryRows database
            ("PRAGMA table_info(\"" <> table <> "\")")
            []
        pure $ Set.fromList
            [ sqlDataText value
            | row <- rows
            , value <- take 1 (drop 1 row)
            ]

sqlDataText :: SQLData -> Text
sqlDataText = \case
    SQLText text -> text
    SQLBlob bytes -> decodeUtf8With lenientDecode bytes
    SQLInteger integer -> Text.pack (show integer)
    SQLFloat number -> Text.pack (show number)
    SQLNull -> ""

decodeJsonish :: SQLData -> Maybe Value
decodeJsonish raw =
    case raw of
        SQLText text -> decodeText text
        SQLBlob bytes ->
            decodeBoundedJsonValue bytes
                <|> (decodeUtf8Strict bytes >>= decodeText)
        _ -> Nothing
  where
    decodeText text =
        let stripped = Text.strip text
            bytes = encodeUtf8 stripped
        in (decodeHex bytes >>= decodeBoundedJsonValue)
            <|> decodeBoundedJsonValue bytes

decodeUtf8Strict :: BS.ByteString -> Maybe Text
decodeUtf8Strict bytes =
    case Data.Text.Encoding.decodeUtf8' bytes of
        Left _ -> Nothing
        Right text -> Just text

decodeHex :: BS.ByteString -> Maybe BS.ByteString
decodeHex bytes
    | BS.null bytes = Nothing
    | odd (BS.length bytes) = Nothing
    | not (BS8.all isHexDigit bytes) = Nothing
    | otherwise =
        BS.pack <$> traverse pair
            [ (BS.index bytes index, BS.index bytes (index + 1))
            | index <- [0, 2 .. BS.length bytes - 2]
            ]
  where
    pair (high, low) =
        Just $ fromIntegral
            (digitToInt (toEnum (fromIntegral high))
                * 16
                + digitToInt (toEnum (fromIntegral low)))
