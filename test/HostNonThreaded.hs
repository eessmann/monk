-- This test executable must be linked without -threaded. A blocking
-- waitForProcess would prevent both the stream tasks and deadline from running.
module Main (main) where

import Control.Concurrent (rtsSupportsBoundThreads)
import Data.ByteString qualified as B
import Monk.Host.Process
import System.Exit (ExitCode (ExitSuccess))

main :: IO ()
main = do
  when rtsSupportsBoundThreads (fail "host-nonthreaded-test must be built without -threaded")
  let bytes = B.replicate 262144 255
  copied <- runProcess (ProcessSpec "/bin/cat" [] Nothing Nothing bytes 2000000)
  unless (processExit copied == ExitSuccess && processStdout copied == bytes && not (processTimedOut copied)) $
    fail "nonthreaded runner failed to drain simultaneous input and output"
  blocked <- runProcess (ProcessSpec "/bin/sh" ["-c", "sleep 60 & wait"] Nothing Nothing "" 200000)
  unless (processTimedOut blocked) (fail "nonthreaded process deadline did not fire")
  descendant <- runProcess (ProcessSpec "/bin/sh" ["-c", "sleep 60 & exit 0"] Nothing Nothing "" 200000)
  unless (processExit descendant == ExitSuccess && processTimedOut descendant) $
    fail "nonthreaded runner did not terminate inherited descendant pipes"
