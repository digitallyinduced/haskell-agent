{-# LANGUAGE ForeignFunctionInterface #-}

-- | Typed, engine-independent access to the terminal's syntax tokenizer.
module Agent.CLI.MacOS.SyntaxBridge (ha_syntax_highlight) where

import Agent.Syntax
import Control.Concurrent.MVar
import Control.Exception.Safe (catchAny)
import Control.Monad (foldM)
import qualified Data.ByteString as ByteString
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)
import Foreign.C.Types (CSize(..))
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)

type SyntaxSpanCallback = Ptr () -> CSize -> CSize -> Int32 -> IO ()

foreign import ccall "dynamic"
    invokeSyntaxSpanCallback :: FunPtr SyntaxSpanCallback -> SyntaxSpanCallback

foreign export ccall ha_syntax_highlight
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr SyntaxSpanCallback -> Ptr () -> IO Int32

{-# NOINLINE syntaxHighlighterState #-}
syntaxHighlighterState :: MVar (Maybe SyntaxHighlighter)
syntaxHighlighterState = unsafePerformIO (newMVar Nothing)

-- Failed discovery is deliberately not cached: a host may configure its
-- bundled syntax directory after its first plain-text rendering.
loadedHighlighter :: Text -> IO (Either Text SyntaxHighlighter)
loadedHighlighter language =
    modifyMVar syntaxHighlighterState \previous -> do
        initial <- maybe newSyntaxHighlighter (pure . Right) previous
        result <- case initial of
            Left message -> pure (Left message)
            Right highlighter -> loadSyntaxLanguage highlighter language
        pure (either (const previous) Just result, result)

ha_syntax_highlight
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr SyntaxSpanCallback -> Ptr () -> IO Int32
ha_syntax_highlight languagePointer languageLength sourcePointer sourceLength callback context
    | callback == nullFunPtr
        || (languagePointer == nullPtr && languageLength /= 0)
        || (sourcePointer == nullPtr && sourceLength /= 0) = pure 2
    | sourceLength > 256 * 1024 || languageLength > 4096 = pure 1
    | otherwise = catchAny highlight (const (pure 3))
  where
    highlight = do
        languageBytes <- readBytes languagePointer languageLength
        sourceBytes <- readBytes sourcePointer sourceLength
        case (Text.decodeUtf8' languageBytes, Text.decodeUtf8' sourceBytes) of
            (Right language, Right source)
                | Text.count "\n" source >= 5000 -> pure 1
                | otherwise -> do
                    loadedHighlighter language >>= \case
                        Left _ -> pure 1
                        Right highlighter ->
                            case highlightCode highlighter language source of
                                Left _ -> pure 1
                                Right highlightedLines -> do
                                    -- Newlines are absent from Skylighting's
                                    -- line tokens; account for exactly one UTF-8
                                    -- byte between lines without coloring it.
                                    _ <- foldM emitLine 0 highlightedLines
                                    pure 0
            _ -> pure 2
    emitLine offset spans = do
        end <- foldM emitSpan offset spans
        pure (end + 1)
    emitSpan offset token = do
        let byteLength = fromIntegral
                (ByteString.length (Text.encodeUtf8 token.syntaxText))
        if byteLength == 0 then pure () else
            invokeSyntaxSpanCallback callback context offset byteLength
                (syntaxClassCode token.syntaxClass)
        pure (offset + byteLength)
    readBytes pointer byteLength
        | byteLength == 0 = pure ByteString.empty
        | otherwise = ByteString.packCStringLen
            (castPtr pointer, fromIntegral byteLength)

-- Do not derive ABI values from constructor order.
syntaxClassCode :: SyntaxClass -> Int32
syntaxClassCode = \case
    SyntaxNormal -> 0
    SyntaxKeyword -> 1
    SyntaxType -> 2
    SyntaxFunction -> 3
    SyntaxVariable -> 4
    SyntaxString -> 5
    SyntaxNumber -> 6
    SyntaxComment -> 7
    SyntaxOperator -> 8
    SyntaxAnnotation -> 9
    SyntaxPreprocessor -> 10
    SyntaxWarning -> 11
    SyntaxError -> 12
