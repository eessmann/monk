-- | Runtime wire ABI and native target are independent of the shell semantics.
module Monk.Runtime.NativeTarget
  ( runtimeABI,
    runtimeProfile,
    runtimeTarget,
    runtimeDescriptionHeader,
    supportedNativeTarget,
  )
where

import Data.Bits (finiteBitSize)
import System.Info (arch, os)

runtimeABI :: Int
runtimeABI = 2

runtimeProfile :: String
runtimeProfile = "bash53-i64"

runtimeTarget :: String
runtimeTarget = architecture <> "-" <> os
  where
    architecture = if arch == "arm64" then "aarch64" else arch

supportedNativeTarget :: Bool
supportedNativeTarget = finiteBitSize (0 :: Int) == 64 && runtimeTarget `elem` ["x86_64-linux", "aarch64-linux", "aarch64-darwin"]

runtimeDescriptionHeader :: String
runtimeDescriptionHeader = "monk-runtime " <> show runtimeABI <> " " <> runtimeProfile
