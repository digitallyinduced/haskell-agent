{-# LANGUAGE ForeignFunctionInterface #-}
-- | Terminal dimensions must not compete with the editor for stdin.
module Agent.CLI.TerminalSize (getTerminalSize) where

import Foreign (Ptr, alloca, peek)
import Foreign.C.Types (CInt(..))

foreign import ccall unsafe "agent_cli_terminal_size"
    terminalSize :: Ptr CInt -> Ptr CInt -> IO CInt

getTerminalSize :: IO (Maybe (Int, Int))
getTerminalSize = alloca \rows -> alloca \columns ->
    terminalSize rows columns >>= \case
        0 -> pure Nothing
        _ -> Just <$> ((,) <$> (fromIntegral <$> peek rows) <*> (fromIntegral <$> peek columns))
