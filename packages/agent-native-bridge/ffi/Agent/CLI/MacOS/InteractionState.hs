{-# LANGUAGE ForeignFunctionInterface #-}

-- | Callback representation and STM ownership of pending native interactions.
module Agent.CLI.MacOS.InteractionState
    ( CInteractionOption(..)
    , InteractionCallback
    , invokeInteractionCallback
    , NativeInteractionResolution(..)
    , PendingInteraction(..)
    , resolvePendingInteraction
    , cancelPendingInteractions
    , cancelledInteractionResolution
    , InteractionCallbackTarget(..)
    , InteractionRuntime(..)
    ) where

import Control.Concurrent (MVar)
import Control.Concurrent.STM
import Control.Monad (forM_, void, when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Word (Word8)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.Storable (Storable(..), peekByteOff, pokeByteOff)

data CInteractionOption = CInteractionOption
    { cInteractionOptionLabel :: !(Ptr Word8)
    , cInteractionOptionLabelLength :: !CSize
    }

instance Storable CInteractionOption where
    sizeOf _ = sizeOf (nullPtr :: Ptr Word8) + sizeOf (undefined :: CSize)
    alignment _ =
        max
            (alignment (nullPtr :: Ptr Word8))
            (alignment (undefined :: CSize))
    peek pointer =
        CInteractionOption
            <$> peekByteOff pointer 0
            <*> peekByteOff pointer (sizeOf (nullPtr :: Ptr Word8))
    poke pointer option = do
        pokeByteOff pointer 0 option.cInteractionOptionLabel
        pokeByteOff
            pointer
            (sizeOf (nullPtr :: Ptr Word8))
            option.cInteractionOptionLabelLength

type InteractionCallback =
    Ptr ()
    -> Ptr Word8 -> CSize -- turn id
    -> Ptr Word8 -> CSize -- interaction id
    -> CInt -- kind
    -> Ptr Word8 -> CSize -- prompt/body
    -> Ptr CInteractionOption -> CSize
    -> IO ()

foreign import ccall "dynamic"
    invokeInteractionCallback
        :: FunPtr InteractionCallback -> InteractionCallback

data NativeInteractionResolution = NativeInteractionResolution
    { interactionSelectedIndex :: !Int
    , interactionCustomText :: !(Maybe Text)
    } deriving (Eq, Show)

data PendingInteraction = PendingInteraction
    { pendingInteractionOptionCount :: !Int
    , pendingInteractionWaiter :: !(TMVar NativeInteractionResolution)
    }

resolvePendingInteraction
    :: TVar (Map (Text, Text) PendingInteraction)
    -> (Text, Text)
    -> NativeInteractionResolution
    -> STM Bool
resolvePendingInteraction pendingRef key resolution = do
    pending <- readTVar pendingRef
    case Map.lookup key pending of
        Nothing -> pure False
        Just interaction@PendingInteraction
            { pendingInteractionOptionCount = optionCount
            }
            | resolution.interactionSelectedIndex < (-1)
                || resolution.interactionSelectedIndex >= optionCount ->
                pure False
            | otherwise -> do
                published <- tryPutTMVar
                    interaction.pendingInteractionWaiter
                    resolution
                when published $
                    writeTVar pendingRef (Map.delete key pending)
                pure published

cancelPendingInteractions
    :: TVar (Map (Text, Text) PendingInteraction)
    -> STM ()
cancelPendingInteractions pendingRef = do
    pending <- readTVar pendingRef
    writeTVar pendingRef Map.empty
    forM_ (Map.elems pending) \interaction ->
        void $ tryPutTMVar
            interaction.pendingInteractionWaiter
            cancelledInteractionResolution

cancelledInteractionResolution :: NativeInteractionResolution
cancelledInteractionResolution = NativeInteractionResolution
    { interactionSelectedIndex = -1
    , interactionCustomText = Nothing
    }

data InteractionCallbackTarget = InteractionCallbackTarget
    { interactionTargetCallback :: !(FunPtr InteractionCallback)
    , interactionTargetContext :: !(Ptr ())
    }

data InteractionRuntime = InteractionRuntime
    { interactionCallbackTarget :: !(TVar (Maybe InteractionCallbackTarget))
    , interactionCallbackLock :: !(MVar ())
    , interactionPending
        :: !(TVar (Map (Text, Text) PendingInteraction))
    }
