{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (Value (..), encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.List (isInfixOf)
import Data.Text qualified as T
import Evidence qualified
import Monk.Runtime.Abi2 (abiCapabilities)
import Monk.Tooling.Package (Linkage (..), Target (..), attestExecution, inspectLinkage, nativeCheckReceipt, packageReport, readTarget, requiredNativeSuites, verifyArtifactIdentity, verifyDescription)
import Monk.Tooling.Process (ProcessResult (..), ProcessSpec (..), runProcess)
import Monk.Tooling.Summary (compact, summaryReport)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, getTemporaryDirectory, removeFile, removePathForcibly)
import System.Environment qualified as Env
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Posix.Files (setFileMode)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode, readProcessWithExitCode)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

main :: IO ()
main =
  defaultMain $
    testGroup
      "tooling"
      [ Evidence.tests,
        testCase "static AArch64 ELF is admitted without claiming execution" $
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
        testCase "Darwin does not allow arbitrary libraries in the system directory" $
          assertLeft "non-Apple" (inspectLinkage Aarch64Darwin "arm64\n" "runtime:\n /usr/lib/unapproved.dylib (version 1)\n" "cmd LC_BUILD_VERSION\n platform 1\n minos 13.0\n"),
        testCase "Darwin requires a deployment target" $
          assertLeft "deployment target" (inspectLinkage Aarch64Darwin "arm64\n" "runtime:\n" ""),
        testCase "Darwin rejects an iOS deployment command" $
          assertLeft "deployment target" (inspectLinkage Aarch64Darwin "arm64\n" "runtime:\n /usr/lib/libSystem.B.dylib (version 1)\n" "Load command 1\n cmd LC_BUILD_VERSION\n platform 2\n minos 13.0\n"),
        testCase "release report never claims unrun execution" $ do
          let report = packageReport Aarch64Linux (Linkage [] Nothing) "signed-final-artifact" "Darwin arm64"
          case report of
            Object fields -> do
              KM.lookup "execution_verified" fields @?= Just (Bool False)
              KM.lookup "linkage_verified" fields @?= Just (Bool True)
              KM.lookup "target" fields @?= Just (String "aarch64-linux")
              KM.lookup "abi" fields @?= Just (Number 2)
              KM.lookup "profile" fields @?= Just (String "bash53-i64")
              KM.lookup "bytes" fields @?= Just (Number 21)
              KM.lookup "sha256" fields @?= Just (String "e05d1e4d956a15bf7e7d601a6cc52f9b658ce72f889c0d71e731a44b1ac34c9b")
              KM.lookup "runtime_description_verified" fields @?= Just (Bool False)
              case KM.lookup "platform_requirements" fields of
                Just (Object requirements) -> KM.lookup "minimum_execution_verified" requirements @?= Just (Bool False)
                _ -> assertBool "missing platform requirements" False
            _ -> assertBool "expected object report" False,
        testCase "description must match ABI profile and target" $ do
          let capabilities = T.pack abiCapabilities
          verifyDescription Aarch64Linux ("monk-runtime 2 bash53-i64\n" <> capabilities <> "\ntarget aarch64-linux\n") @?= Right (T.words capabilities)
          assertLeft "ABI/profile/target" (verifyDescription Aarch64Linux "monk-runtime 2 bash53-i64\necho integer\ntarget aarch64-linux\n")
          assertLeft "ABI/profile/target" (verifyDescription Aarch64Linux "monk-runtime 2 bash53-i64\necho\ntarget aarch64-darwin\n"),
        testCase "native attestation requires the exact packaged artifact" $ do
          let report = packageReport Aarch64Linux (Linkage [] Nothing) "signed-final-artifact" "test host"
          verifyArtifactIdentity report "signed-final-artifact" @?= Right ()
          assertLeft "SHA-256" (verifyArtifactIdentity report "different artifact"),
        testCase "target names are validated" $ do
          readTarget "aarch64-linux" @?= Right Aarch64Linux
          assertLeft "unknown target" (readTarget "x86-darwin"),
        testCase "execution attestation verifies the release report and description" $ do
          let report = packageReport Aarch64Linux (Linkage [] Nothing) "runtime" "test host"
              description = "monk-runtime 2 bash53-i64\n" <> T.pack abiCapabilities <> "\ntarget aarch64-linux\n"
              receipts = [nativeCheckReceipt suite "runtime" "checker" | suite <- requiredNativeSuites]
              checker = "d2d2328e3359f3de3515871090d1316cbcdc5383204c204f9390788c3ef8618f"
          case attestExecution report description checker receipts of
            Left failure -> assertBool (T.unpack failure) False
            Right (Object fields) -> do
              KM.lookup "execution_verified" fields @?= Just (Bool True)
              KM.lookup "runtime_description_verified" fields @?= Just (Bool True)
              assertBool "missing execution checks" (KM.member "execution_checks" fields)
            Right _ -> assertBool "expected report object" False
          assertLeft "ABI/profile/target" (attestExecution report "monk-runtime 2 bash53-i64\necho\ntarget x86_64-linux\n" checker receipts)
          assertLeft "linkage" (attestExecution (object ["linkage_verified" .= False]) description checker receipts)
          assertLeft "missing native check" (attestExecution report description checker (drop 1 receipts))
          assertLeft "checker" (attestExecution report description "other checker" receipts)
          assertLeft "runtime" (attestExecution report description checker [nativeCheckReceipt suite "other runtime" "checker" | suite <- requiredNativeSuites]),
        testCase "runtime inspector is available from the Haskell CLI" $ do
          (status, output, _) <- readProcessWithExitCode "monk-tool" ["runtime", "inspect", "--help"] ""
          status @?= ExitSuccess
          assertBool "missing --target option" ("--target" `isInfixOf` output)
          assertBool "missing --binary option" ("--binary" `isInfixOf` output),
        testCase "public boundary CLI finds the built library without Cabal exec" $ do
          inherited <- Env.getEnvironment
          let clean = filter (\(name, _) -> name `notElem` ["GHC_ENVIRONMENT", "GHC_PACKAGE_PATH"]) inherited
          (status, output, failure) <-
            readCreateProcessWithExitCode
              ((proc "monk-tool" ["boundaries", "check"]) {env = Just clean})
              ""
          assertBool (output <> failure) (status == ExitSuccess),
        testCase "native child transport receipt rejects an unreviewed script" $ do
          tool <- findExecutable "monk-tool" >>= maybe (fail "monk-tool unavailable") pure
          runtime <- findExecutable "monk-runtime" >>= maybe (fail "monk-runtime unavailable") pure
          temporary <- getTemporaryDirectory
          (directory, handle) <- openTempFile temporary "monk-child-check-"
          hClose handle
          removeFile directory
          let scriptDirectory = directory </> "test/native"
          createDirectoryIfMissing True scriptDirectory
          writeFile (scriptDirectory </> "child-transport.sh") "#!/bin/sh\nexit 0\n"
          (status, _, failure) <-
            readCreateProcessWithExitCode
              ((proc tool ["runtime", "check", "--suite", "child-transport", "--runtime", runtime]) {cwd = Just directory})
              ""
          assertBool "unreviewed script was accepted" (status /= ExitSuccess && "differs" `isInfixOf` failure)
          removePathForcibly directory,
        testCase "attestation executes the packaged binary for its description" $ do
          tool <- findExecutable "monk-tool" >>= maybe (fail "monk-tool unavailable") pure
          temporary <- getTemporaryDirectory
          (directory, handle) <- openTempFile temporary "monk-attest-"
          hClose handle
          removeFile directory
          createDirectoryIfMissing True directory
          let binary = directory </> "runtime"
              report = directory </> "package.json"
              description = directory </> "description.txt"
          B.writeFile binary "#!/bin/sh\nprintf 'monk-runtime 2 bash53-i64\\necho\\ntarget x86_64-linux\\n'\n"
          setFileMode binary 0o700
          bytes <- B.readFile binary
          BL.writeFile report (encode (packageReport Aarch64Linux (Linkage [] Nothing) bytes "test host"))
          B.writeFile description "monk-runtime 2 bash53-i64\necho\ntarget aarch64-linux\n"
          (status, _, failure) <-
            readCreateProcessWithExitCode
              (proc tool ["runtime", "attest", "--package-report", report, "--binary", binary, "--description-file", description, "--check-receipts", directory, "--output", directory </> "verified.json"])
              ""
          assertBool "description file was trusted without execution" (status /= ExitSuccess && "differs" `isInfixOf` failure)
          removePathForcibly directory,
        testCase "summary drops raw streams and observations recursively" $ do
          let input = object ["base64" .= ("raw" :: T.Text), "fixtures" .= [object ["observations" .= [True], "stdout_base64" .= ("abc" :: T.Text), "stderr_base64" .= ("def" :: T.Text), "stdout" .= object ["base64" .= ("abc" :: T.Text), "sha256" .= ("digest" :: T.Text)]]]]
          compact input @?= object ["fixtures" .= [object ["stdout" .= object ["sha256" .= ("digest" :: T.Text)]]]],
        testCase "summary records the complete source report hash" $ do
          let raw = BL.toStrict (encode (object ["status" .= ("match" :: T.Text)]))
          case summaryReport "/tmp/raw.json" raw of
            Left failure -> assertBool failure False
            Right (Object fields) -> do
              KM.lookup "status" fields @?= Just (String "match")
              case KM.lookup "raw_report" fields of
                Just (Object source) -> do
                  KM.lookup "path" source @?= Just (String "/tmp/raw.json")
                  KM.lookup "sha256" source @?= Just (String "d0928a42b4e34a3c937e0ff09dffb48e8bc98d996f956655651dc248501b9dd8")
                _ -> assertBool "missing raw report identity" False
            Right _ -> assertBool "expected summary object" False,
        testCase "summary CLI creates once and refuses to replace evidence" $ do
          temporary <- getTemporaryDirectory
          (input, handle) <- openTempFile temporary "monk-summary-"
          hClose handle
          let output = input <> ".summary.json"
          BL.writeFile input (encode (object ["base64" .= ("raw" :: T.Text), "status" .= ("match" :: T.Text)]))
          (firstStatus, _, _) <- readProcessWithExitCode "monk-tool" ["evidence", "summary", input, output] ""
          firstStatus @?= ExitSuccess
          saved <- B.readFile output
          assertBool "raw base64 leaked into summary" (not ("base64" `isInfixOf` show saved))
          (secondStatus, _, _) <- readProcessWithExitCode "monk-tool" ["evidence", "summary", input, output] ""
          assertBool "existing evidence was replaced" (secondStatus /= ExitSuccess)
          B.readFile output >>= (@?= saved)
          removeFile input
          exists <- doesFileExist output
          when exists (removeFile output),
        testCase "process runner preserves raw streams and nonzero status" $ do
          result <-
            runProcess
              ProcessSpec
                { executable = "/bin/sh",
                  arguments = ["-c", "cat; printf '\\377'; printf '\\376' >&2; exit 7"],
                  workingDirectory = Nothing,
                  environment = Nothing,
                  stdinBytes = B.pack [0, 255],
                  timeoutMicros = 5000000
                }
          processExit result @?= ExitFailure 7
          processStdout result @?= B.pack [0, 255, 255]
          processStderr result @?= B.pack [254]
          processTimedOut result @?= False,
        testCase "process runner bounds a blocked process group" $ do
          result <-
            runProcess
              ProcessSpec
                { executable = "/bin/sh",
                  arguments = ["-c", "sleep 60 & wait"],
                  workingDirectory = Nothing,
                  environment = Nothing,
                  stdinBytes = "",
                  timeoutMicros = 300000
                }
          processTimedOut result @?= True,
        testCase "process runner bounds descendants holding output pipes" $ do
          result <-
            runProcess
              ProcessSpec
                { executable = "/bin/sh",
                  arguments = ["-c", "sleep 60 & exit 0"],
                  workingDirectory = Nothing,
                  environment = Nothing,
                  stdinBytes = "",
                  timeoutMicros = 300000
                }
          processTimedOut result @?= True
      ]

assertLeft :: T.Text -> Either T.Text a -> IO ()
assertLeft expected result = case result of
  Left reason -> assertBool ("expected " <> T.unpack expected <> " in " <> T.unpack reason) (expected `T.isInfixOf` reason)
  Right _ -> assertBool "expected rejection" False
