{-# LANGUAGE ForeignFunctionInterface #-}

module Monk.Runtime.Descriptors (descriptorMask, initialDescriptorOpen) where

import Data.Bits (shiftL, (.&.))
import Foreign.C.Types (CInt (..))

foreign import ccall unsafe "monk_initial_descriptor_mask" initialMask :: IO CInt

descriptorMask :: IO Int
descriptorMask = fromIntegral <$> initialMask

initialDescriptorOpen :: Int -> IO Bool
initialDescriptorOpen fd = if fd < 0 || fd > 3 then pure False else (\mask -> mask .&. shiftL 1 fd /= 0) <$> descriptorMask
