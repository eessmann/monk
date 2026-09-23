module HostProcess (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Data.ByteString qualified as B
import Data.Text qualified as T
import Monk.Tooling.Process
import System.Directory (doesFileExist)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "host process ownership"
    [ testCase "early closed stdin preserves child output and status" $
        forM_ [1, 4, 64, 8191, 8192, 262144] $ \size -> replicateM_ 20 $ do
          result <- runProcess (ProcessSpec "/bin/sh" ["-c", "exec 0<&-; printf done"] Nothing Nothing (B.replicate size 255) 2000000)
          processExit result @?= ExitSuccess
          processStdout result @?= "done"
          processStderr result @?= ""
          processTimedOut result @?= False,
      testCase "fast exited leaders retain owned descendant group identities" $
        replicateM_ 40 $ do
          result <- runProcess (ProcessSpec "/bin/sh" ["-c", "sleep 60 & exit 0"] Nothing Nothing "" 100000)
          processExit result @?= ExitSuccess
          processTimedOut result @?= True,
      testCase "simultaneous large streams are drained without deadlock" $ do
        let input = B.replicate 262144 255
        result <- runProcess (ProcessSpec "/bin/sh" ["-c", "tee /dev/stderr"] Nothing Nothing input 5000000)
        processStdout result @?= input
        processStderr result @?= input
        processTimedOut result @?= False,
      testCase "cancellation terminates descendants and closes blocked input" $
        withSystemTempDirectory "monk-process-cancellation" $ \root -> do
          let ready = root </> "ready"
              leaked = root </> "leaked"
              spec = ProcessSpec "/bin/sh" ["-c", "touch \"$1\"; (sleep 1; touch \"$2\") & wait", "sh", ready, leaked] Nothing Nothing (encodeUtf8 (T.replicate 1048576 "x")) 5000000
          worker <- async (runProcess spec)
          started <- timeout 2000000 (awaitFile ready)
          cancel worker
          assertBool "child did not start" (isJust started)
          threadDelay 1500000
          exists <- doesFileExist leaked
          assertBool "cancelled process left a running descendant" (not exists)
    ]

awaitFile :: FilePath -> IO ()
awaitFile path = do
  exists <- doesFileExist path
  unless exists (threadDelay 10000 >> awaitFile path)
