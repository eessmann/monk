-- | Runtime wire ABI and native target are independent of the shell semantics.
module Monk.Runtime.NativeTarget
  ( runtimeABI,
    runtimeProfile,
    runtimeTarget,
    runtimeDescriptionHeader,
    runtimeCapabilities,
    supportedNativeTarget,
  )
where

import Data.Bits (finiteBitSize)
import Monk.Runtime.Abi2 (abiCapabilities, abiProfile, abiTargets, abiVersion)
import System.Info (arch, os)

runtimeABI :: Int
runtimeABI = abiVersion

runtimeProfile :: String
runtimeProfile = abiProfile

runtimeTarget :: String
runtimeTarget = architecture <> "-" <> os
  where
    architecture = if arch == "arm64" then "aarch64" else arch

supportedNativeTarget :: Bool
supportedNativeTarget = finiteBitSize (0 :: Int) == 64 && runtimeTarget `elem` abiTargets

runtimeDescriptionHeader :: String
runtimeDescriptionHeader = "monk-runtime " <> show runtimeABI <> " " <> runtimeProfile

runtimeCapabilities :: String
runtimeCapabilities = abiCapabilities
