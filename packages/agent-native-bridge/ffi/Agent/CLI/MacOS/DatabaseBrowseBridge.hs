{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Read-only scoped database browsing and callback-scoped row marshalling.
module Agent.CLI.MacOS.DatabaseBrowseBridge () where

import Agent.CLI.Database (DatabaseScope(..))
import Agent.CLI.Database.Store
    ( DatabaseBrowsePage(..), DatabaseScopes, deriveDatabaseScopes
    , listDatabaseObjects, loadDatabaseRows )
import Agent.CLI.MacOS.Marshalling (decodeInput, withOptionalText, withText)
import Agent.CLI.Project (resolveProjectRoot)
import Agent.CLI.Session (sessionsRoot)
import Agent.CLI.SessionAdmin (managedPostgresConfigForHome)
import Agent.Store.Postgres (Store, closeStore, openStore)
import Agent.Store.Postgres.Custom
    ( CatalogColumn(..), CatalogDefinition(..), CatalogObject(..) )
import Agent.Store.Types (renderStoreError)
import Control.Concurrent (forkIO)
import Control.Exception.Safe (bracket, tryAny)
import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Ptr (FunPtr, Ptr, nullFunPtr, nullPtr)
import System.Directory.OsPath (getHomeDirectory)
import System.OsPath (decodeFS, takeDirectory, unsafeEncodeUtf)

type DataCatalogCallback =
    Ptr () -> CInt -> CInt -> CInt
    -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize -> CInt
    -> CString -> CSize -> CString -> CSize -> IO ()

type DataRowsCallback =
    Ptr () -> CInt -> Int64 -> Int64 -> CInt -> CInt
    -> CString -> CSize -> Int64 -> CInt
    -> CString -> CSize -> IO ()

foreign import ccall "dynamic"
    invokeDataCatalogCallback
        :: FunPtr DataCatalogCallback -> DataCatalogCallback

foreign import ccall "dynamic"
    invokeDataRowsCallback
        :: FunPtr DataRowsCallback -> DataRowsCallback

foreign export ccall ha_data_catalog_list
    :: Ptr Word8 -> CSize -> FunPtr DataCatalogCallback -> Ptr () -> IO CInt

foreign export ccall ha_data_rows_load
    :: Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize
    -> Int64 -> CInt -> FunPtr DataRowsCallback -> Ptr () -> IO CInt

ha_data_catalog_list
    :: Ptr Word8 -> CSize -> FunPtr DataCatalogCallback -> Ptr () -> IO CInt
ha_data_catalog_list cwdBytes (CSize cwdLength) callback context
    | callback == nullFunPtr = pure 1
    | cwdBytes == nullPtr && cwdLength > 0 = pure 2
    | otherwise = do
        cwd <- decodeInput cwdBytes cwdLength
        _ <- forkIO do
            tryAny (loadDataCatalogFor (Text.unpack cwd)) >>= \case
                Left exception ->
                    withText (Text.pack (show exception)) \errorPtr errorLength ->
                        dataCatalogTerminal
                            callback context (-1) errorPtr errorLength
                Right (Left err) ->
                    withText err \errorPtr errorLength ->
                        dataCatalogTerminal
                            callback context (-1) errorPtr errorLength
                Right (Right scopedObjects) -> do
                    forM_ scopedObjects \(scope, objects) ->
                        forM_ objects (emitDataCatalogObject callback context scope)
                    dataCatalogTerminal callback context 2 nullPtr 0
        pure 0

ha_data_rows_load
    :: Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize
    -> Int64 -> CInt -> FunPtr DataRowsCallback -> Ptr () -> IO CInt
ha_data_rows_load
    cwdBytes
    (CSize cwdLength)
    rawScope
    objectBytes
    (CSize objectLength)
    offset
    rawLimit
    callback
    context
    | callback == nullFunPtr = pure 1
    | cwdBytes == nullPtr && cwdLength > 0 = pure 2
    | objectBytes == nullPtr && objectLength > 0 = pure 2
    | offset < 0 = pure 3
    | rawLimit < 1 || rawLimit > 500 = pure 3
    | Nothing <- dataScopeFromCode rawScope = pure 3
    | otherwise = do
        cwd <- decodeInput cwdBytes cwdLength
        objectName <- decodeInput objectBytes objectLength
        if Text.null objectName
            then pure 3
            else do
                let Just scope = dataScopeFromCode rawScope
                    limit = fromIntegral rawLimit
                _ <- forkIO do
                    tryAny
                        (loadDataPageFor
                            (Text.unpack cwd)
                            scope
                            objectName
                            offset
                            limit)
                        >>= \case
                            Left exception ->
                                withText
                                    (Text.pack (show exception))
                                    \errorPtr errorLength ->
                                        dataRowsTerminal
                                            callback context (-1) offset 0 False
                                            errorPtr errorLength
                            Right (Left err) ->
                                withText err \errorPtr errorLength ->
                                    dataRowsTerminal
                                        callback context (-1) offset 0 False
                                        errorPtr errorLength
                            Right (Right page) -> do
                                forM_
                                    (zip [0 :: Int64 ..] page.databaseBrowseRows)
                                    \(rowIndex, row) ->
                                        forM_
                                            (zip [0 :: Int ..] row)
                                            \(columnIndex, value) ->
                                                withDataValue value
                                                    \kind valuePtr valueLength ->
                                                        invokeDataRowsCallback
                                                            callback context 0
                                                            offset rowIndex
                                                            (fromIntegral columnIndex)
                                                            kind
                                                            valuePtr valueLength
                                                            0 0 nullPtr 0
                                dataRowsTerminal
                                    callback
                                    context
                                    1
                                    offset
                                    (fromIntegral
                                        (length page.databaseBrowseRows))
                                    page.databaseBrowseHasMore
                                    nullPtr
                                    0
                pure 0

loadDataCatalogFor
    :: FilePath
    -> IO (Either Text [(CInt, [CatalogObject])])
loadDataCatalogFor cwd =
    withDatabaseStoreFor cwd \store scopes -> do
        results <- mapM
            (\(scopeCode, scope) ->
                fmap (fmap ((,) scopeCode)) $
                    listDatabaseObjects store scopes scope)
            [ (0, DatabaseUserScope)
            , (1, DatabaseRepositoryScope)
            , (2, DatabaseCheckoutScope)
            ]
        pure (sequence results)

loadDataPageFor
    :: FilePath
    -> DatabaseScope
    -> Text
    -> Int64
    -> Int
    -> IO (Either Text DatabaseBrowsePage)
loadDataPageFor cwd selected objectName offset limit =
    withDatabaseStoreFor cwd \store scopes ->
        loadDatabaseRows store scopes selected objectName offset limit

withDatabaseStoreFor
    :: FilePath
    -> (Store -> DatabaseScopes -> IO (Either Text value))
    -> IO (Either Text value)
withDatabaseStoreFor cwd action = do
    home <- getHomeDirectory
    projectRoot <- resolveProjectRoot (unsafeEncodeUtf cwd)
    stateDirectory <- decodeFS (takeDirectory (sessionsRoot home))
    projectRootPath <- decodeFS projectRoot
    deriveDatabaseScopes stateDirectory projectRootPath >>= \case
        Left err -> pure (Left err)
        Right scopes -> do
            config <- managedPostgresConfigForHome home
            openStore config >>= \case
                Left err -> pure (Left (renderStoreError err))
                Right opened ->
                    bracket (pure opened) closeStore \store ->
                        action store scopes

emitDataCatalogObject
    :: FunPtr DataCatalogCallback
    -> Ptr ()
    -> CInt
    -> CatalogObject
    -> IO ()
emitDataCatalogObject callback context scope object =
    withText object.catalogObjectName \objectPtr objectLength ->
    withOptionalText definition.definitionComment \commentPtr commentLength -> do
        invokeDataCatalogCallback callback context
            0 scope kind
            objectPtr objectLength
            commentPtr commentLength
            nullPtr 0 nullPtr 0 0 nullPtr 0 nullPtr 0
        forM_ definition.definitionColumns \column ->
            withText column.columnName \columnPtr columnLength ->
            withText column.columnType \typePtr typeLength ->
            withOptionalText column.columnComment
                \columnCommentPtr columnCommentLength ->
                    invokeDataCatalogCallback callback context
                        1 scope kind
                        objectPtr objectLength
                        nullPtr 0
                        columnPtr columnLength
                        typePtr typeLength
                        (if column.columnNullable then 1 else 0)
                        columnCommentPtr columnCommentLength
                        nullPtr 0
  where
    definition = object.catalogObjectDefinition
    kind = dataObjectKind object.catalogObjectKind

dataObjectKind :: Text -> CInt
dataObjectKind = \case
    "view" -> 1
    "materialized_view" -> 1
    _ -> 0

dataScopeFromCode :: CInt -> Maybe DatabaseScope
dataScopeFromCode = \case
    0 -> Just DatabaseUserScope
    1 -> Just DatabaseRepositoryScope
    2 -> Just DatabaseCheckoutScope
    _ -> Nothing

dataCatalogTerminal
    :: FunPtr DataCatalogCallback
    -> Ptr ()
    -> CInt
    -> CString
    -> CSize
    -> IO ()
dataCatalogTerminal callback context status errorPtr errorLength =
    invokeDataCatalogCallback callback context
        status 0 0
        nullPtr 0 nullPtr 0
        nullPtr 0 nullPtr 0 0
        nullPtr 0 errorPtr errorLength

withDataValue
    :: Aeson.Value
    -> (CInt -> CString -> CSize -> IO value)
    -> IO value
withDataValue value action =
    case value of
        Aeson.Null -> action 0 nullPtr 0
        Aeson.String text -> withText text (action 1)
        Aeson.Number _ -> withEncoded 2
        Aeson.Bool True -> withText "true" (action 3)
        Aeson.Bool False -> withText "false" (action 3)
        Aeson.Array _ -> withEncoded 4
        Aeson.Object _ -> withEncoded 4
  where
    withEncoded kind =
        withText
            (TextEncoding.decodeUtf8 (LBS.toStrict (Aeson.encode value)))
            (action kind)

dataRowsTerminal
    :: FunPtr DataRowsCallback
    -> Ptr ()
    -> CInt
    -> Int64
    -> Int64
    -> Bool
    -> CString
    -> CSize
    -> IO ()
dataRowsTerminal
    callback context status offset rowCount hasMore errorPtr errorLength =
        invokeDataRowsCallback callback context
            status offset (-1) (-1) (-1)
            nullPtr 0 rowCount (if hasMore then 1 else 0)
            errorPtr errorLength
