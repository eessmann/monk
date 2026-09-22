{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KM
import Data.List (isInfixOf)
import Data.Text qualified as T
import Monk.Tooling.Package (Linkage (..), Target (..), inspectLinkage, packageReport, readTarget, verifyDescription)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

main :: IO ()
main =
  defaultMain $
    testGroup
      "tooling"
      [ testCase "static AArch64 ELF is admitted without claiming execution" $
          inspectLinkage Aarch64Linux "Machine: AArch64\n" "" "" @?= Right (Linkage [] Nothing),
        testCase "ELF interpreter is rejected" $
          assertLeft "dynamic interpreter" (inspectLinkage Aarch64Linux "Machine: AArch64\n" "  INTERP 0x0000\n" ""),
        testCase "ELF needed library is rejected" $
          assertLeft "dynamic dependencies" (inspectLinkage Aarch64Linux "Machine: AArch64\n" "" "(NEEDED) libc.so.6"),
        testCase "wrong ELF architecture is rejected" $
          assertLeft "architecture" (inspectLinkage Aarch64Linux "Machine: Advanced Micro Devices X86-64\n" "" ""),
        testCase "Darwin permits only the named Apple libraries" $
          inspectLinkage Aarch64Darwin "arm64\n" "runtime:\n /usr/lib/libSystem.B.dylib (compatibility version 1.0.0)\n" "cmd LC_BUILD_VERSION\n platform 1\n minos 13.0\n"
            @?= Right (Linkage ["/usr/lib/libSystem.B.dylib"] (Just "13.0")),
        testCase "Darwin rejects a Nix dependency" $
          assertLeft "non-Apple" (inspectLinkage Aarch64Darwin "arm64\n" "runtime:\n /nix/store/libgmp.dylib (version 1)\n" "cmd LC_BUILD_VERSION\n platform 1\n minos 13.0\n"),
        testCase "Darwin requires a deployment target" $
          assertLeft "deployment target" (inspectLinkage Aarch64Darwin "arm64\n" "runtime:\n" ""),
        testCase "release report never claims unrun execution" $ do
          let report = packageReport Aarch64Linux (Linkage [] Nothing) "signed-final-artifact" "Darwin arm64"
          case report of
            Object fields -> do
              KM.lookup "execution_verified" fields @?= Just (Bool False)
              KM.lookup "linkage_verified" fields @?= Just (Bool True)
              KM.lookup "target" fields @?= Just (String "aarch64-linux")
            _ -> assertBool "expected object report" False,
        testCase "description must match ABI profile and target" $ do
          verifyDescription Aarch64Linux "monk-runtime 2 bash53-i64\necho integer\ntarget aarch64-linux\n" @?= Right ["echo", "integer"]
          assertLeft "ABI/profile/target" (verifyDescription Aarch64Linux "monk-runtime 2 bash53-i64\necho\ntarget aarch64-darwin\n"),
        testCase "target names are validated" $ do
          readTarget "aarch64-linux" @?= Right Aarch64Linux
          assertLeft "unknown target" (readTarget "x86-darwin"),
        testCase "runtime inspector is available from the Haskell CLI" $ do
          (status, output, _) <- readProcessWithExitCode "monk-tool" ["runtime", "inspect", "--help"] ""
          status @?= ExitSuccess
          assertBool "missing --target option" ("--target" `isInfixOf` output)
          assertBool "missing --binary option" ("--binary" `isInfixOf` output)
      ]

assertLeft :: T.Text -> Either T.Text a -> IO ()
assertLeft expected result = case result of
  Left reason -> assertBool ("expected " <> T.unpack expected <> " in " <> T.unpack reason) (expected `T.isInfixOf` reason)
  Right _ -> assertBool "expected rejection" False
