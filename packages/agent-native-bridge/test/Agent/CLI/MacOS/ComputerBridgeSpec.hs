{-# LANGUAGE ForeignFunctionInterface #-}

module Agent.CLI.MacOS.ComputerBridgeSpec (spec) where

import Agent.CLI.ComputerUse
    ( ComputerObservation(..)
    , ScreenshotEncoding(..)
    , computerUseTool
    )
import Agent.CLI.MacOS.Bridge (composeNativeTools)
import Agent.CLI.MacOS.ComputerBridge
    ( CComputerAction(..)
    , CComputerPoint(..)
    , ComputerBatch(..)
    , ComputerCallback
    , ComputerHost(..)
    , ComputerRegistration(..)
    , computerActionStructSize
    , encodeComputerActions
    , invokeComputerSessionTransaction
    , invokeComputerTransaction
    , newComputerSession
    )
import Agent.Loop (ImageAttachment(..))
import Agent.Responses.Types
    ( ComputerAction(..)
    , ComputerPoint(..)
    )
import Agent.Tools.Types (AppTool(..), AppToolGroup(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (poll, wait, withAsync)
import Control.Concurrent.MVar
    ( modifyMVar_
    , newEmptyMVar
    , newMVar
    , putMVar
    , takeMVar
    )
import Control.Exception.Safe (bracket)
import qualified Data.ByteString as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Word (Word8, Word32, Word64)
import Foreign
    ( FunPtr
    , Ptr
    , castPtr
    , copyBytes
    , freeHaskellFunPtr
    , nullPtr
    , peekArray
    , poke
    )
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Storable (Storable, sizeOf)
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
    , it
    , shouldBe
    , shouldReturn
    )

foreign import ccall "ha_computer_callback_abi_smoke"
    computerCallbackABISmoke :: IO CInt

foreign import ccall "wrapper"
    wrapComputerCallback :: ComputerCallback -> IO (FunPtr ComputerCallback)

spec :: Spec
spec = describe "native computer bridge" do
    it "keeps the Haskell records aligned with the versioned C ABI" do
        sizeOf (undefined :: CComputerPoint) `shouldBe` 8
        sizeOf (undefined :: CComputerAction) `shouldBe` 64
        computerActionStructSize `shouldBe` 64
        computerCallbackABISmoke `shouldReturn` 0

    it "encodes UTF-8, drag ranges, buttons, and modifier masks" do
        let result = encodeComputerActions
                [ ClickAction 10 20 "back" ["shift", "fn"]
                , TypeAction "hé"
                , DragAction
                    [ComputerPoint 1 2, ComputerPoint 3 4]
                    ["cmd"]
                , KeypressAction ["ctrl", "A"]
                , WaitAction
                ]
        case result of
            Left err -> fail (show err)
            Right batch -> do
                batch.computerBatchText `shouldBe`
                    BS.pack [104, 195, 169, 65]
                batch.computerBatchPoints `shouldBe`
                    [CComputerPoint 1 2, CComputerPoint 3 4]
                case batch.computerBatchActions of
                    [click, typed, drag, keypress, waited] -> do
                        ( click.cComputerAction
                            , click.cComputerX
                            , click.cComputerY
                            , click.cComputerButton
                            , click.cComputerModifiers
                            ) `shouldBe` (1, 10, 20, 4, 17)
                        ( typed.cComputerAction
                            , typed.cComputerTextOffset
                            , typed.cComputerTextLength
                            ) `shouldBe` (6, 0, 3)
                        ( drag.cComputerAction
                            , drag.cComputerPointOffset
                            , drag.cComputerPointCount
                            , drag.cComputerModifiers
                            ) `shouldBe` (5, 0, 2, 8)
                        ( keypress.cComputerAction
                            , keypress.cComputerTextOffset
                            , keypress.cComputerTextLength
                            , keypress.cComputerModifiers
                            ) `shouldBe` (7, 3, 1, 2)
                        waited.cComputerAction `shouldBe` 8
                    actions -> fail ("unexpected actions: " <> show actions)

    it "never sends screenshot markers through the native action ABI" do
        encodeComputerActions [ScreenshotAction] `shouldBe`
            Left "Computer screenshot must not cross the native action ABI."

    it "round-trips the display lease and typed batch through the callback" do
        observed <- newIORef []
        withComputerHost (recordingComputerCallback observed) \host -> do
            result <- invokeComputerTransaction
                host
                ScreenshotPng
                [ ClickAction 10 20 "left" ["shift"]
                , TypeAction "λ"
                , DragAction
                    [ComputerPoint 1 2, ComputerPoint 3 4]
                    ["cmd"]
                ]
                (\dimensions ->
                    if dimensions == (100, 80)
                        then Right ()
                        else Left "unexpected dimensions")
            result `shouldBe` Right
                (ComputerObservation
                    (ImageAttachment "image/png" pngSignature)
                    Nothing)
            readIORef observed `shouldReturn`
                [ ObservedComputerInvocation
                    1 1 0 0 0 [] [] BS.empty 0 65536
                , ObservedComputerInvocation
                    1 2 41 100 80
                    [ (plainObservedAction 1)
                        { cComputerX = 10
                        , cComputerY = 20
                        , cComputerButton = 1
                        , cComputerModifiers = 1
                        }
                    , (plainObservedAction 6)
                        { cComputerTextLength = 2
                        }
                    , (plainObservedAction 5)
                        { cComputerModifiers = 8
                        , cComputerPointCount = 2
                        }
                    ]
                    [CComputerPoint 1 2, CComputerPoint 3 4]
                    (BS.pack [206, 187])
                    1
                    16777216
                ]

    it "executes later actions against the lease from the model's screenshot" do
        observed <- newIORef []
        withComputerHost (recordingComputerCallback observed) \host -> do
            session <- newComputerSession
            invokeComputerSessionTransaction
                host session ScreenshotPng
                [ClickAction 10 20 "left" []]
                (const (Right ()))
                `shouldReturn` Left
                    "Take a fresh computer screenshot before sending input actions."
            readIORef observed `shouldReturn` []
            invokeComputerSessionTransaction
                host session ScreenshotPng [] (const (Right ()))
                `shouldReturn` Right
                    (ComputerObservation
                        (ImageAttachment "image/png" pngSignature)
                        Nothing)
            invokeComputerSessionTransaction
                host session ScreenshotPng
                [ClickAction 10 20 "left" []]
                (const (Right ()))
                `shouldReturn` Right
                    (ComputerObservation
                        (ImageAttachment "image/png" pngSignature)
                        Nothing)
            invocations <- readIORef observed
            map observedOperationAndToken invocations `shouldBe`
                [(1, 0), (2, 41), (2, 42)]

    it "waits for an in-flight transaction before disabling its callback" do
        runEntered <- newEmptyMVar
        releaseRun <- newEmptyMVar
        observed <- newIORef []
        let blockingCallback context abi operation token width height
                actions actionCount points pointCount text textLength
                imageFormat output outputCapacity outputLength outputToken
                outputWidth outputHeight outputImageFormat = do
                    if operation == 2
                        then putMVar runEntered () >> takeMVar releaseRun
                        else pure ()
                    recordingComputerCallback observed
                        context abi operation token width height
                        actions actionCount points pointCount text textLength
                        imageFormat output outputCapacity outputLength
                        outputToken outputWidth outputHeight outputImageFormat
        withCallback blockingCallback \callback -> do
            registration <- newMVar
                (Just (ComputerRegistration callback nullPtr))
            let host = ComputerHost registration
            withAsync
                (invokeComputerTransaction
                    host
                    ScreenshotPng
                    []
                    (const (Right ())))
                \running -> do
                    takeMVar runEntered
                    withAsync
                        (modifyMVar_ registration (const (pure Nothing)))
                        \disabling -> do
                            threadDelay 20000
                            poll disabling >>= \case
                                Nothing -> pure ()
                                Just _ -> expectationFailure
                                    "callback disabled during a transaction"
                            putMVar releaseRun ()
                            wait running `shouldReturn` Right
                                (ComputerObservation
                                    (ImageAttachment "image/png" pngSignature)
                                    Nothing)
                            wait disabling
            invokeComputerTransaction host ScreenshotPng [] (const (Right ()))
                `shouldReturn` Left "Native computer control is not active."

    it "replaces the generic computer tool once without enabling it" do
        let genericComputer = computerUseTool
            nativeComputer = genericComputer
                { appToolDescription = "native computer"
                }
            otherTool = genericComputer
                { appToolName = "other"
                }
            names tools = map (\tool -> tool.appToolName) tools
            descriptions tools =
                map (\tool -> tool.appToolDescription) tools
        names (composeNativeTools
            (Just nativeComputer)
            [ ExecutionToolGroup [genericComputer, otherTool]
            , HostToolGroup [genericComputer]
            ]) `shouldBe` ["computer", "other"]
        descriptions (composeNativeTools
            (Just nativeComputer)
            [ExecutionToolGroup [genericComputer]]) `shouldBe`
                ["native computer"]
        names (composeNativeTools
            (Just nativeComputer)
            [ExecutionToolGroup [otherTool]]) `shouldBe` ["other"]
        names (composeNativeTools
            Nothing
            [ ExecutionToolGroup [genericComputer, otherTool]
            , HostToolGroup [genericComputer]
            ]) `shouldBe` ["computer", "other", "computer"]

data ObservedComputerInvocation = ObservedComputerInvocation
    !Word32
    !CInt
    !Word64
    !CInt
    !CInt
    ![CComputerAction]
    ![CComputerPoint]
    !BS.ByteString
    !CInt
    !CSize
    deriving (Eq, Show)

recordingComputerCallback
    :: IORef [ObservedComputerInvocation]
    -> ComputerCallback
recordingComputerCallback observed _ abi operation token width height
        actions actionCount points pointCount text textLength imageFormat
        output outputCapacity outputLength outputToken outputWidth outputHeight
        outputImageFormat = do
    decodedActions <- peekValues actions actionCount
    decodedPoints <- peekValues points pointCount
    decodedText <- peekBytes text textLength
    modifyIORef' observed (<> [ObservedComputerInvocation
        abi operation token width height decodedActions decodedPoints
        decodedText imageFormat outputCapacity])
    case operation of
        1 -> do
            poke outputLength 0
            poke outputToken 41
            poke outputWidth 100
            poke outputHeight 80
            poke outputImageFormat 0
            pure 0
        2 -> do
            status <- writeBytes pngSignature
                output outputCapacity outputLength
            poke outputToken (token + 1)
            poke outputWidth 100
            poke outputHeight 80
            poke outputImageFormat 1
            pure status
        _ -> pure 1

peekValues :: Storable value => Ptr value -> CSize -> IO [value]
peekValues _ (CSize 0) = pure []
peekValues pointer (CSize count) =
    peekArray (fromIntegral count) pointer

peekBytes :: Ptr value -> CSize -> IO BS.ByteString
peekBytes _ (CSize 0) = pure BS.empty
peekBytes pointer (CSize length) =
    BS.packCStringLen (castPtr pointer, fromIntegral length)

writeBytes :: BS.ByteString -> Ptr Word8 -> CSize -> Ptr CSize -> IO CInt
writeBytes bytes output (CSize capacity) outputLength
    | BS.length bytes > fromIntegral capacity = pure 7
    | otherwise = do
        BS.useAsCStringLen bytes \(source, length) ->
            copyBytes output (castPtr source) length
        poke outputLength (fromIntegral (BS.length bytes))
        pure 0

withComputerHost
    :: ComputerCallback
    -> (ComputerHost -> IO value)
    -> IO value
withComputerHost callback action =
    withCallback callback \funPtr -> do
        registration <- newMVar
            (Just (ComputerRegistration funPtr nullPtr))
        action (ComputerHost registration)

withCallback
    :: ComputerCallback
    -> (FunPtr ComputerCallback -> IO value)
    -> IO value
withCallback callback =
    bracket (wrapComputerCallback callback) freeHaskellFunPtr

plainObservedAction :: CInt -> CComputerAction
plainObservedAction action = CComputerAction
    { cComputerStructSize = 64
    , cComputerAction = action
    , cComputerX = 0
    , cComputerY = 0
    , cComputerDeltaX = 0
    , cComputerDeltaY = 0
    , cComputerButton = 0
    , cComputerModifiers = 0
    , cComputerTextOffset = 0
    , cComputerTextLength = 0
    , cComputerPointOffset = 0
    , cComputerPointCount = 0
    }

pngSignature :: BS.ByteString
pngSignature = BS.pack [137, 80, 78, 71, 13, 10, 26, 10]

observedOperationAndToken
    :: ObservedComputerInvocation
    -> (CInt, Word64)
observedOperationAndToken
        (ObservedComputerInvocation _ operation token _ _ _ _ _ _ _) =
    (operation, token)
