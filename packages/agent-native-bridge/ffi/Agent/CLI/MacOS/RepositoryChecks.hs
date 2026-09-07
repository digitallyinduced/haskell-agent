{-# LANGUAGE ForeignFunctionInterface #-}

-- | Native repository-check handle acquisition, callbacks, and destruction.
-- Keep publication and cancellation ownership together across the C boundary.
module Agent.CLI.MacOS.RepositoryChecks
    ( RepositoryCheckHandle(..)
    , ha_repository_check_destroy
    ) where

import Agent.CLI.MacOS.Marshalling (withText)
import Agent.CLI.MacOS.RepositoryInput
import Agent.CLI.MacOS.RepositoryWorkers
    ( isRepositoryCallbackThread
    , tryRepositorySynchronous
    , withRepositoryCallbackThread
    )
import qualified Agent.CLI.RepositoryReview as RepositoryReview
import Control.Concurrent
    ( newEmptyMVar, readMVar, putMVar )
import Control.Concurrent.Async (Async, asyncWithUnmask, cancel, waitCatch)
import Control.Exception.Safe (mask, onException)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Foreign
    ( FunPtr, Ptr, StablePtr, castPtr, castPtrToStablePtr, castStablePtrToPtr
    , deRefStablePtr, freeStablePtr, newStablePtr, nullFunPtr, nullPtr
    , peekByteOff, plusPtr, poke, sizeOf
    )
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CSize(..))
import qualified System.Exit

type RepositoryCheckOutputCallback =
    Ptr () -> CInt -> Ptr Word8 -> CSize -> IO ()

type RepositoryCheckExitCallback =
    Ptr () -> CInt -> CInt -> CString -> CSize -> IO ()

foreign import ccall "dynamic"
    invokeRepositoryCheckOutputCallback
        :: FunPtr RepositoryCheckOutputCallback -> RepositoryCheckOutputCallback

foreign import ccall "dynamic"
    invokeRepositoryCheckExitCallback
        :: FunPtr RepositoryCheckExitCallback -> RepositoryCheckExitCallback

foreign export ccall ha_repository_check_start
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr () -> CSize
    -> FunPtr RepositoryCheckOutputCallback
    -> FunPtr RepositoryCheckExitCallback
    -> Ptr () -> Ptr (Ptr ()) -> IO CInt

foreign export ccall ha_repository_check_cancel :: Ptr () -> IO ()

foreign export ccall ha_repository_check_destroy :: Ptr () -> IO ()

data RepositoryCheckHandle = RepositoryCheckHandle
    { repositoryCheckValue :: !(IORef (Maybe RepositoryReview.RepositoryCheck))
    , repositoryCheckCancelRequested :: !(IORef Bool)
    , repositoryCheckOwner :: !(Async ())
    }

ha_repository_check_start
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr () -> CSize
    -> FunPtr RepositoryCheckOutputCallback
    -> FunPtr RepositoryCheckExitCallback
    -> Ptr () -> Ptr (Ptr ()) -> IO CInt
ha_repository_check_start pathBytes pathLength snapshotBytes snapshotLength
    executableBytes executableLength argumentsPointer argumentCount
    outputCallback exitCallback context outCheck
    | outputCallback == nullFunPtr
        || exitCallback == nullFunPtr
        || outCheck == nullPtr = pure 1
    | argumentsPointer == nullPtr && argumentCount > 0 = pure 2
    | argumentCount > fromIntegral maxRepositoryCheckArguments = pure 2
    | otherwise =
        copyRequiredTexts
            [ (pathBytes, pathLength)
            , (snapshotBytes, snapshotLength)
            , (executableBytes, executableLength)
            ] >>= \case
                Left _ -> pure 2
                Right [path, expected, executable] -> do
                    copiedArguments <- copyRepositoryCheckArguments
                        argumentsPointer argumentCount
                    case copiedArguments of
                        Left _ -> pure 2
                        Right arguments -> mask \_ -> do
                            poke outCheck nullPtr
                            checkRef <- newIORef Nothing
                            cancelRef <- newIORef False
                            gate <- newEmptyMVar
                            owner <- asyncWithUnmask \unmask ->
                                readMVar gate >> do
                                    -- asyncWithUnmask starts masked. Keep
                                    -- acquisition and publication in that
                                    -- masked region so ownership transfer is
                                    -- atomic with respect to cancellation. Its
                                    -- blocking I/O remains interruptible under
                                    -- masked-interruptible semantics and owns
                                    -- cleanup brackets.
                                    started <- tryRepositorySynchronous
                                        (RepositoryReview.startRepositoryCheck
                                            (Text.unpack path)
                                            expected
                                            (Text.unpack executable)
                                            (map Text.unpack arguments)
                                            (\stream bytes ->
                                                BS.useAsCStringLen bytes
                                                    \(pointer, length) ->
                                                        withRepositoryCallbackThread
                                                            (invokeRepositoryCheckOutputCallback
                                                                outputCallback
                                                                context
                                                                (case stream of
                                                                    RepositoryReview.RepositoryCheckStdout -> 1
                                                                    RepositoryReview.RepositoryCheckStderr -> 2)
                                                                (castPtr pointer)
                                                                (fromIntegral length)))
                                            (\cancelled exitCode ->
                                                withRepositoryCallbackThread
                                                    (invokeRepositoryCheckExitCallback
                                                        exitCallback
                                                        context
                                                        (if cancelled then 1 else 0)
                                                        (case exitCode of
                                                            System.Exit.ExitSuccess -> 0
                                                            System.Exit.ExitFailure code ->
                                                                fromIntegral code)
                                                        nullPtr 0)))
                                    case started of
                                        Left exception ->
                                            unmask
                                                (withText
                                                    (Text.pack (show exception))
                                                    \errorPtr errorLength ->
                                                        withRepositoryCallbackThread
                                                            (invokeRepositoryCheckExitCallback
                                                                exitCallback
                                                                context 0 (-1)
                                                                errorPtr errorLength))
                                        Right (Left err) ->
                                            unmask
                                                (withText
                                                    (RepositoryReview.repositoryErrorText
                                                        err)
                                                    \errorPtr errorLength ->
                                                        withRepositoryCallbackThread
                                                            (invokeRepositoryCheckExitCallback
                                                                exitCallback
                                                                context 0 (-1)
                                                                errorPtr errorLength))
                                        Right (Right check) -> do
                                            writeIORef checkRef (Just check)
                                            cancelRequested <- readIORef cancelRef
                                            when cancelRequested
                                                (RepositoryReview.cancelRepositoryCheck
                                                    check)
                                            unmask
                                                (RepositoryReview.waitRepositoryCheck
                                                    check)
                                                `onException`
                                                    RepositoryReview.cancelRepositoryCheck
                                                        check
                            stable <- newStablePtr RepositoryCheckHandle
                                { repositoryCheckValue = checkRef
                                , repositoryCheckCancelRequested = cancelRef
                                , repositoryCheckOwner = owner
                                }
                                `onException` cancel owner
                            (do
                                poke outCheck (castStablePtrToPtr stable)
                                -- No callback can begin before the owned
                                -- handle is visible through out_check.
                                putMVar gate ()
                                pure 0)
                                `onException` do
                                    cancel owner
                                    freeStablePtr stable
                                    poke outCheck nullPtr
                Right _ -> pure 3

ha_repository_check_cancel :: Ptr () -> IO ()
ha_repository_check_cancel pointer
    | pointer == nullPtr = pure ()
    | otherwise = do
        isRepositoryCallbackThread >>= \case
            True -> pure ()
            False -> do
                let stable =
                        castPtrToStablePtr pointer
                            :: StablePtr RepositoryCheckHandle
                handle <- deRefStablePtr stable
                writeIORef handle.repositoryCheckCancelRequested True
                readIORef handle.repositoryCheckValue >>= mapM_
                    RepositoryReview.cancelRepositoryCheck

ha_repository_check_destroy :: Ptr () -> IO ()
ha_repository_check_destroy pointer
    | pointer == nullPtr = pure ()
    | otherwise = do
        isRepositoryCallbackThread >>= \case
            True -> pure ()
            False -> do
                let stable =
                        castPtrToStablePtr pointer
                            :: StablePtr RepositoryCheckHandle
                handle <- deRefStablePtr stable
                _ <- waitCatch handle.repositoryCheckOwner
                freeStablePtr stable

copyRepositoryCheckArguments
    :: Ptr () -> CSize -> IO (Either () [Text])
copyRepositoryCheckArguments pointer count
    | count > fromIntegral maxRepositoryCheckArguments = pure (Left ())
    | itemSize <= 0 = pure (Left ())
    | otherwise = go 0 (0 :: Int) []
  where
    pointerSize = sizeOf (nullPtr :: Ptr ())
    sizeSize = sizeOf (undefined :: CSize)
    itemSize = pointerSize + sizeSize
    countInt = fromIntegral count
    go index total acc
        | index >= countInt = pure (Right (reverse acc))
        | index > maxBound `div` itemSize = pure (Left ())
        | otherwise = do
            let base = castPtr pointer `plusPtr` (index * itemSize)
            bytes <- peekByteOff base 0 :: IO (Ptr Word8)
            length <- peekByteOff base pointerSize :: IO CSize
            if length > fromIntegral maxRepositoryCheckArgumentBytes
                || toInteger total + toInteger length
                    > toInteger maxRepositoryCheckTotalArgumentBytes
                then pure (Left ())
                else copyRequiredText bytes length >>= \case
                    Left _ -> pure (Left ())
                    Right value ->
                        go
                            (index + 1)
                            (total + fromIntegral length)
                            (value : acc)

maxRepositoryCheckArguments :: Int
maxRepositoryCheckArguments = 4096

maxRepositoryCheckArgumentBytes :: Int
maxRepositoryCheckArgumentBytes = 1024 * 1024

maxRepositoryCheckTotalArgumentBytes :: Int
maxRepositoryCheckTotalArgumentBytes = 8 * 1024 * 1024
