{-# LANGUAGE ForeignFunctionInterface #-}
-- Native parsing with the same Json representation and validation as the
-- baseline. No native pointers escape decodeJsonNative.
module CodeMode.NativeJson (decodeJsonNative) where

import CodeMode.Json (Json(..))
-- MicroHs ships Control.Exception, not the safe-exceptions package.
import Control.Exception (bracket, evaluate)
import Data.Char (ord)
import Data.List (nub)
import Foreign.C.String (CString, newCString, peekCStringLen)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr, nullPtr)

foreign import ccall "native_json_parse" nativeParse :: CString -> IO (Ptr ())
foreign import ccall "native_json_free" nativeFree :: Ptr () -> IO ()
foreign import ccall "native_json_root" nativeRoot :: Ptr () -> IO (Ptr ())
foreign import ccall "native_json_kind" nativeKind :: Ptr () -> IO Int
foreign import ccall "native_json_length" nativeLength :: Ptr () -> IO Int
foreign import ccall "native_json_text" nativeText :: Ptr () -> IO CString
foreign import ccall "native_json_child" nativeChild :: Ptr () -> IO (Ptr ())
foreign import ccall "native_json_next" nativeNext :: Ptr () -> IO (Ptr ())

decodeJsonNative :: String -> IO (Either String Json)
decodeJsonNative source
    | any invalidCharacter source = pure (Left "invalid JSON character")
    | otherwise = bracket (newCString source) free $ \input ->
        bracket (nativeParse input) nativeFree $ \document ->
            if document == nullPtr
                then pure (Left "invalid JSON")
                else do
                    root <- nativeRoot document
                    result <- convertValue 128 root
                    -- Force strings while the document is live: pointer-backed
                    -- lazy conversion must not outlive nativeFree.
                    _ <- evaluate (forceResult result)
                    pure result
  where
    invalidCharacter character =
        character == '\0' || (ord character >= 0xd800 && ord character <= 0xdfff)

convertText :: Ptr () -> IO String
convertText value = do
    pointer <- nativeText value
    count <- nativeLength value
    peekCStringLen (pointer, count)

convertValue :: Int -> Ptr () -> IO (Either String Json)
convertValue depth value
    | depth <= 0 = pure (Left "maximum JSON nesting depth is 128")
    | otherwise = do
        kind <- nativeKind value
        case kind of
            0 -> pure (Right JsonNull)
            1 -> pure (Right (JsonBool False))
            2 -> pure (Right (JsonBool True))
            3 -> Right . JsonNumber <$> convertText value
            4 -> Right . JsonString <$> convertText value
            5 -> do
                count <- nativeLength value
                child <- nativeChild value
                values <- convertArray (depth - 1) count child
                pure (JsonArray <$> values)
            6 -> do
                count <- nativeLength value
                child <- nativeChild value
                fields <- convertObject (depth - 1) count child
                pure $ do
                    entries <- fields
                    let names = map fst entries
                    if length names /= length (nub names)
                        then Left "duplicate JSON object key"
                        else Right (JsonObject entries)
            _ -> pure (Left "unexpected native JSON type")

convertArray :: Int -> Int -> Ptr () -> IO (Either String [Json])
convertArray _ 0 _ = pure (Right [])
convertArray depth count value = do
    converted <- convertValue depth value
    case converted of
        Left message -> pure (Left message)
        Right entry -> do
            next <- nativeNext value
            remaining <- convertArray depth (count - 1) next
            pure ((entry :) <$> remaining)

convertObject :: Int -> Int -> Ptr () -> IO (Either String [(String, Json)])
convertObject _ 0 _ = pure (Right [])
convertObject depth count key = do
    name <- convertText key
    value <- nativeNext key
    converted <- convertValue depth value
    case converted of
        Left message -> pure (Left message)
        Right entry -> do
            next <- nativeNext value
            remaining <- convertObject depth (count - 1) next
            pure (((name, entry) :) <$> remaining)

forceResult :: Either String Json -> ()
forceResult (Left message) = forceString message
forceResult (Right value) = forceJson value

forceString :: String -> ()
forceString [] = ()
forceString (character : rest) = character `seq` forceString rest

forceJson :: Json -> ()
forceJson JsonNull = ()
forceJson (JsonBool value) = value `seq` ()
forceJson (JsonNumber value) = forceString value
forceJson (JsonString value) = forceString value
forceJson (JsonArray values) = foldr (\value rest -> forceJson value `seq` rest) () values
forceJson (JsonObject fields) =
    foldr (\(name, value) rest -> forceString name `seq` forceJson value `seq` rest) () fields
