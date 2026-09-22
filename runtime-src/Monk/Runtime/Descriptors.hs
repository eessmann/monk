{-# LANGUAGE ForeignFunctionInterface #-}

module Monk.Runtime.Descriptors (descriptorMask, initialDescriptorOpen, duplicatePrivate, duplicatePrivateAbove, privateCloseOnExec, nativeErrorMessage) where

import Control.Exception (IOException)
import Data.Bits (shiftL, (.&.))
import Data.ByteString qualified as B
import Foreign.C.Error (throwErrnoIfMinus1, throwErrnoIfMinus1_)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import GHC.IO.Exception (ioe_errno)
import System.Posix.Types (Fd (..))

foreign import ccall unsafe "monk_initial_descriptor_mask" initialMask :: IO CInt

descriptorMask :: IO Int
descriptorMask = fromIntegral <$> initialMask

initialDescriptorOpen :: Int -> IO Bool
initialDescriptorOpen fd = if fd < 0 || fd > 5 then pure False else (\mask -> mask .&. shiftL 1 fd /= 0) <$> descriptorMask

-- | Own a close-on-exec descriptor above semantic stdio and transport fd3.
foreign import ccall unsafe "monk_duplicate_private" duplicateNative :: CInt -> IO CInt

foreign import ccall unsafe "monk_private_cloexec" privateNative :: IO CInt

duplicatePrivate :: Fd -> IO Fd
duplicatePrivate (Fd fd) = Fd <$> throwErrnoIfMinus1 "duplicate private descriptor" (duplicateNative fd)

privateCloseOnExec :: IO ()
privateCloseOnExec = throwErrnoIfMinus1_ "protect private descriptors" privateNative

foreign import ccall unsafe "monk_duplicate_above" duplicateAboveNative :: CInt -> CInt -> IO CInt

duplicatePrivateAbove :: Fd -> Int -> IO Fd
duplicatePrivateAbove (Fd fd) minimumFd = Fd <$> throwErrnoIfMinus1 "duplicate descriptor above targets" (duplicateAboveNative fd (fromIntegral minimumFd))

foreign import ccall unsafe "monk_error_message" messageNative :: CInt -> IO CString

nativeErrorMessage :: IOException -> IO B.ByteString
nativeErrorMessage failure = case ioe_errno failure of
  Just code -> messageNative code >>= B.packCString
  Nothing -> pure (B.pack [66, 97, 100, 32, 102, 105, 108, 101, 32, 100, 101, 115, 99, 114, 105, 112, 116, 111, 114])
