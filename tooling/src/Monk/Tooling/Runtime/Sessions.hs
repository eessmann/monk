{-# LANGUAGE OverloadedStrings #-}

module Monk.Tooling.Runtime.Sessions
  ( runDescriptors,
    runProcessSubstitution,
    runSession,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Monk.Tooling.Process (ProcessResult (..))
import Monk.Tooling.Runtime.Common
import System.Directory (Permissions (..), createDirectory, doesFileExist, getPermissions, setPermissions)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose)
import System.Posix.Files (createNamedPipe, createSymbolicLink, ownerReadMode, ownerWriteMode, unionFileModes)
import System.Posix.IO (OpenMode (ReadWrite), closeFd, createPipe, defaultFileFlags, fdToHandle, fdWrite, openFd)
import System.Process (CreateProcess (..), StdStream (..), createProcess, getPid, proc, waitForProcess)
import System.Timeout (timeout)

b :: String -> B.ByteString
b = C.pack

linesOf :: [String] -> String
linesOf = (<> "\n") . intercalate "\n"

runFish :: Context -> FilePath -> String -> B.ByteString -> IO ProcessResult
runFish ctx directory body input = do
  script <- writeScript (directory </> "evaluate.fish") (rpcHeader <> body <> "\n")
  env <- sessionEnvironment (runtime ctx)
  invokeWith (runtime ctx) ["--abi", "2", "session-run", script] input (Just directory) (Just env) 10000000

runDescriptors :: Context -> IO ()
runDescriptors ctx = do
  withWorkspace $ \directory -> do
    let output = directory </> "errors"
        missing = directory </> "missing"
        body =
          linesOf
            [ "rpc fd-push 7",
              "rpc fd-data 7 3 'first\nsecond\nthird\n'",
              "rpc fd-dup 7 source.sh 1 4 3",
              "rpc read 7 source.sh 1 3 1 '' 5 '' scalar 1",
              "printf 'first=%s\\n' $response[4]",
              "rpc read 7 source.sh 1 4 1 '" <> "\n" <> "' -1 '' scalar 1",
              "printf 'tail=%s\\n' $response[4]",
              "rpc read 7 source.sh 1 3 1 '" <> "\n" <> "' -1 '' scalar 1",
              "printf 'second=%s\\n' $response[4]",
              "rpc fd-push 7",
              "rpc fd-close 7 3",
              "rpc read 7 source.sh 5 3 1 '" <> "\n" <> "' -1 '' scalar 1",
              "printf 'closed=%s,%s\\n' $response[2] $response[3]",
              "rpc fd-pop 7",
              "rpc read 7 source.sh 1 3 1 '" <> "\n" <> "' -1 '' scalar 1",
              "printf 'third=%s\\n' $response[4]",
              "rpc fd-open 7 source.sh 9 2 write " <> quoteFish output,
              "rpc fd-open 7 source.sh 9 0 read " <> quoteFish missing,
              "printf 'failure=%s\\n' $response[2]",
              "rpc fd-pop 7"
            ]
    result <- runFish ctx directory body ""
    checkResult "descriptor aliases and scopes" (0, "first=first\ntail=\nsecond=second\nclosed=1,0\nthird=third\nfailure=1\n", "source.sh: line 5: read: 3: invalid file descriptor: Bad file descriptor\n") result
    captured <- B.readFile output
    check "descriptor error redirection" (captured == b ("source.sh: line 9: " <> missing <> ": No such file or directory\n"))
  withWorkspace $ \directory -> do
    nested <- writeScript (directory </> "nested.fish") (rpcHeader <> "rpc read 7 source.sh 1 3 1 '" <> "\n" <> "' -1 '' scalar 1\nprintf '%s\\n\\n' $response[4]\n")
    let packet = directory </> "packet"
        body =
          linesOf
            [ "rpc fd-push 7",
              "rpc fd-data 7 3 'captured\nremaining\n'",
              "set body (string collect < " <> quoteFish nested <> ")",
              "begin",
              " printf '%s\\0' warning 7 \"$body\" 4 | command \"$MONK_RUNTIME\" --abi 2 child-capture-session > " <> quoteFish packet,
              "end 3<&0",
              "set packet (string split0 < " <> quoteFish packet <> ")",
              "printf 'capture=%s,%s\\n' $packet[2] $packet[3]",
              "rpc read 7 source.sh 1 3 1 '" <> "\n" <> "' -1 '' scalar 1",
              "printf 'after=%s\\n' $response[4]",
              "rpc fd-pop 7"
            ]
    result <- runFish ctx directory body ""
    checkResult "nested descriptor capture" (0, "capture=0,captured\nafter=remaining\n", "") result
  withWorkspace $ \directory -> do
    let output = directory </> "output"
        errors = directory </> "errors"
        body =
          linesOf
            [ "rpc fd-push 7",
              "rpc fd-open 7 source.sh 1 1 write " <> quoteFish output,
              "rpc fd-open 7 source.sh 1 2 write " <> quoteFish errors,
              "rpc run 7 directory-output source.sh 12 pwd 1 'current-directory'",
              "rpc run 7 directory-output source.sh 13 cd 2 'directory-diagnostic'",
              "rpc fd-close 7 1",
              "rpc run 7 directory-output source.sh 14 pwd 1 bytes",
              "set closedout $response[2]",
              "rpc fd-close 7 2",
              "rpc run 7 directory-output source.sh 15 cd 2 ignored",
              "set closederr $response[2]",
              "rpc fd-pop 7",
              "printf 'closed=%s,%s\\n' $closedout $closederr"
            ]
    result <- runFish ctx directory body ""
    checkResult "directory virtual descriptors" (0, "closed=1,0\n", "ignored") result
    B.readFile output >>= check "virtual stdout" . (== "current-directory")
    B.readFile errors >>= check "virtual stderr" . (== "directory-diagnosticsource.sh: line 14: pwd: write error: Bad file descriptor\n")
  withWorkspace $ \directory -> do
    let body =
          linesOf
            [ "rpc fd-push 7",
              "rpc fd-open 7 source.sh 1 3 write " <> quoteFish (directory </> "out"),
              "rpc read 7 source.sh 9 3 1 '" <> "\n" <> "' -1 '' scalar 1",
              "printf 'write-only=%s,%s\\n' $response[2] $response[3]",
              "rpc fd-open 7 source.sh 1 3 read " <> quoteFish directory,
              "rpc read 7 source.sh 10 3 1 '" <> "\n" <> "' -1 '' scalar 1",
              "printf 'directory=%s,%s\\n' $response[2] $response[3]",
              "rpc fd-pop 7"
            ]
    result <- runFish ctx directory body ""
    checkResult "read errno and assignment" (0, "write-only=1,0\ndirectory=1,0\n", "source.sh: line 9: read: 3: read error: Bad file descriptor\nsource.sh: line 10: read: 3: read error: Is a directory\n") result
  withWorkspace $ \directory -> do
    let errors = directory </> "errors"
        missing = directory </> "missing"
    nested <- writeScript (directory </> "nested.fish") (replace " 5>&2" "" rpcHeader <> "rpc read 3 source.sh 18 9 1 '' -1 '' scalar 1\n")
    probe <- writeScript (directory </> "closed.sh") "#!/bin/sh\n[ ! -e /dev/fd/2 ]\n"
    let body =
          linesOf
            [ "rpc fd-push 7",
              "rpc fd-open 7 source.sh 1 2 write " <> quoteFish errors,
              "rpc fd-push 7",
              "rpc fd-close 7 2",
              "rpc read 7 source.sh 11 9 1 '' -1 '' scalar 1",
              "rpc wait 7 source.sh 12 0",
              "rpc fd-open 7 source.sh 13 0 read " <> quoteFish missing,
              "rpc fd-push 7",
              "rpc fd-close 7 1",
              "rpc run 7 builtin source.sh 14 printf value",
              "rpc run 7 builtin source.sh 15 echo value",
              "rpc run 7 external-site source.sh 16 " <> quoteFish missing,
              "rpc spawn 7 builtin source.sh 17 printf value",
              "rpc wait 7 source.sh 17 $response[3]",
              "rpc run 7 pipeline 0 1 builtin 4 source.sh 19 printf value",
              "rpc fd-pop 7",
              "rpc run 7 external /bin/sh " <> quoteFish probe,
              "printf 'external=%s\\n' $response[2]",
              "set body (string collect < " <> quoteFish nested <> ")",
              "rpc run 7 body \"$body\"",
              "printf 'region=%s\\n' $response[2]",
              "rpc fd-pop 7",
              "rpc fd-pop 7"
            ]
    result <- runFish ctx directory body ""
    let expected = b $ "source.sh: line 11: read: 9: invalid file descriptor: Bad file descriptor\nsource.sh: line 12: wait: pid 0 is not a child of this shell\nsource.sh: line 13: " <> missing <> ": No such file or directory\nsource.sh: line 14: printf: write error: Bad file descriptor\nsource.sh: line 15: echo: write error: Bad file descriptor\n"
    checkResult "diagnostic fallback" (0, "external=0\nregion=0\n", expected) result
    B.readFile errors >>= check "closed stderr does not leak" . B.null
  where
    replace old new = go
      where
        go [] = []
        go input | old `isPrefix` input = new <> go (drop (length old) input)
        go (x : xs) = x : go xs
        isPrefix prefix input = take (length prefix) input == prefix

runProcessSubstitution :: Context -> IO ()
runProcessSubstitution ctx = do
  probe <- invoke (runtime ctx) ["--abi", "2", "pipe-paths"] ""
  checkResult "pipe paths supported" (0, "", "") probe
  withWorkspace $ \directory -> do
    let destination = directory </> "output"
        body =
          linesOf
            [ "rpc substitution 7 input external /usr/bin/printf 'producer\\n'",
              "set job $response[3]",
              "set endpoint $response[4]",
              "rpc wait 7 source.sh 1 $job",
              "rpc run 7 external /bin/cat \"$endpoint\"",
              "printf 'input=%s\\n' $response[2]",
              "rpc substitution 7 output external /bin/sh -c 'cat > \"$1\"' child " <> quoteFish destination,
              "set job $response[3]",
              "set endpoint $response[4]",
              "rpc run 7 external /bin/sh -c 'printf consumer > \"$1\"' child \"$endpoint\"",
              "rpc wait 7 source.sh 1 $job",
              "printf 'output=%s\\n' $response[2]",
              "set payload (string repeat -n 300000 x)",
              "rpc substitution 7 input builtin source.sh 1 echo \"$payload\"",
              "set job $response[3]",
              "set endpoint $response[4]",
              "rpc run 7 external /usr/bin/head -c 1 \"$endpoint\"",
              "rpc wait 7 source.sh 1 $job",
              "printf ':signal=%s\\n' $response[2]"
            ]
    result <- runFish ctx directory body ""
    checkResult "process substitution endpoints" (0, "producer\ninput=0\noutput=0\nx:signal=141\n", "") result
    B.readFile destination >>= check "process substitution output" . (== "consumer")
  withWorkspace $ \directory -> do
    let body =
          linesOf
            [ "rpc fd-push 7",
              "rpc substitution 7 input external /usr/bin/printf 'redirected\\n'",
              "set job $response[3]",
              "rpc fd-endpoint 7 source.sh 1 0 $response[5]",
              "rpc read 7 source.sh 1 0 1 '" <> "\n" <> "' -1 '' scalar 1",
              "printf 'read=%s\\n' $response[4]",
              "rpc fd-pop 7",
              "rpc wait 7 source.sh 1 $job",
              "printf 'wait=%s\\n' $response[2]"
            ]
    result <- runFish ctx directory body ""
    checkResult "process substitution scoped stdin" (0, "read=redirected\nwait=0\n", "") result
  forM_ (["substitution", "background"] :: [String]) $ \nextKind -> withWorkspace $ \directory -> do
    let fifo = directory </> "release"
        script = directory </> "evaluate.fish"
    createNamedPipe fifo (ownerReadMode `unionFileModes` ownerWriteMode)
    releaseFd <- openFd fifo ReadWrite defaultFileFlags
    let later = if nextKind == "substitution" then "rpc substitution 7 output external /usr/bin/printf 'second\\n'" else "rpc spawn 7 external /usr/bin/true"
        body =
          linesOf
            [ "rpc substitution 7 output external /bin/sh -c 'printf \"first-start\\n\"; read value < \"$1\"; printf \"first-end\\n\"; exit 7' source " <> quoteFish fifo,
              "set first $response[3]",
              later,
              "rpc run 7 external /usr/bin/true",
              "rpc wait 7 source.sh 1",
              "printf 'parent\\n'",
              "rpc wait 7 source.sh 1 $first",
              "printf 'first-status=%s\\n' $response[2]"
            ]
    _ <- writeScript script (rpcHeader <> body)
    env <- sessionEnvironment (runtime ctx)
    (Just input, Just output, Just errors, process) <- createProcess (proc (runtime ctx) ["--abi", "2", "session-run", script]) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, env = Just env}
    hClose input
    observed <- timeout 5000000 (awaitLine output "parent")
    check ("wait-all blocked on old substitution: " <> nextKind) (observed == Just True)
    _ <- fdWrite releaseFd "release\n"
    closeFd releaseFd
    rest <- B.hGetContents output
    err <- B.hGetContents errors
    void $ evaluate (B.length rest + B.length err)
    ended <- waitForProcess process
    check ("old substitution explicit PID " <> nextKind <> ": " <> show (observed, rest, err, ended)) (ended == ExitSuccess && B.null err && "first-status=7\n" `B.isInfixOf` rest)

waitForFile :: FilePath -> IO Bool
waitForFile path = do
  exists <- doesFileExist path
  if exists
    then do
      contents <- B.readFile path
      if contents == "survived" then pure True else threadDelay 10000 >> waitForFile path
    else threadDelay 10000 >> waitForFile path

awaitLine :: Handle -> B.ByteString -> IO Bool
awaitLine handle wanted = do
  line <- C.hGetLine handle
  if line == wanted then pure True else awaitLine handle wanted

runSession :: Context -> IO ()
runSession ctx = do
  withWorkspace $ \directory -> do
    let script =
          linesOf
            [ "begin",
              " set -l response (printf '%s\\0' ping 0 | command \"$MONK_RUNTIME\" --abi 2 session-client | string split0)",
              " test \"$response[1]\" = ok; or exit 99",
              " printf 'owner=%s\\n' \"$response[3]\"",
              "end 3<&0 4>&1 5>&2"
            ]
    path <- writeScript (directory </> "evaluate.fish") script
    env <- sessionEnvironment (runtime ctx)
    (Just input, Just output, Just errors, process) <- createProcess (proc (runtime ctx) ["--abi", "2", "session-run", path]) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, env = Just env}
    hClose input
    owner <- getPid process
    stdoutBytes <- B.hGetContents output
    stderrBytes <- B.hGetContents errors
    void $ evaluate (B.length stdoutBytes + B.length stderrBytes)
    ended <- waitForProcess process
    check ("owner ping " <> show (owner, stdoutBytes, stderrBytes, ended)) $
      ended == ExitSuccess && maybe False (\pid -> stdoutBytes == b ("owner=" <> show pid <> "\n")) owner && B.null stderrBytes
  withWorkspace $ \directory -> do
    let fifo = directory </> "release"
        done = directory </> "done"
        script = directory </> "evaluate.fish"
    createNamedPipe fifo (ownerReadMode `unionFileModes` ownerWriteMode)
    releaseFd <- openFd fifo ReadWrite defaultFileFlags
    let childBody = "read -l release < " <> quoteFish fifo <> "; printf survived > " <> quoteFish done
    _ <- writeScript script (rpcHeader <> "rpc spawn 7 body " <> quoteFish childBody <> "\nset job $response[3]\nprintf '%s\\n' $job\n")
    env <- sessionEnvironment (runtime ctx)
    (Just input, Just output, Just errors, process) <- createProcess (proc (runtime ctx) ["--abi", "2", "session-run", script]) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, env = Just env}
    hClose input
    childPid <- timeout 10000000 (C.hGetLine output)
    check "background child PID" (maybe False (not . B.null) childPid)
    ownerExit <- timeout 10000000 (waitForProcess process)
    check "owner did not exit before background region" (ownerExit == Just ExitSuccess)
    _ <- fdWrite releaseFd "release\n"
    survived <- timeout 10000000 (waitForFile done)
    unless (survived == Just True) $ do
      partialOut <- B.hGetNonBlocking output 4096
      partialErr <- B.hGetNonBlocking errors 4096
      fail ("background child did not survive owner: " <> show (childPid, partialOut, partialErr, childBody))
    rest <- B.hGetContents output
    err <- B.hGetContents errors
    void $ evaluate (B.length rest + B.length err)
    closeFd releaseFd
    check "background owner streams" (B.null rest && B.null err)
  withWorkspace $ \directory -> do
    let body =
          linesOf
            [ "rpc run 7 external /bin/sh -c 'printf output; exit 7'",
              "set r $response",
              "printf 'status=%s\\n' $r[2]",
              "rpc spawn 7 external /bin/sh -c 'exit 9'",
              "set p $response",
              "rpc wait 7 source.sh 12 $p[3]",
              "set w $response",
              "rpc wait 7 source.sh 12 $p[3]",
              "set w2 $response",
              "printf 'wait=%s,%s\\n' $w[2] $w2[2]",
              "rpc wait 7 source.sh 12",
              "set all $response",
              "rpc wait 7 source.sh 12 $p[3]",
              "set missing $response",
              "printf 'all=%s;after=%s\\n' $all[2] $missing[2]"
            ]
    result <- runFish ctx directory body ""
    check "session wait ownership" $ code result == 0 && processStdout result == "outputstatus=7\nwait=9,9\nall=0;after=127\n" && "source.sh: line 12: wait: pid " `B.isInfixOf` processStderr result && "is not a child of this shell\n" `B.isInfixOf` processStderr result
  withWorkspace $ \directory -> do
    result <-
      runFish
        ctx
        directory
        ( linesOf
            [ "rpc run 7 pipeline 1 2 external 3 /bin/sh -c 'printf abc; exit 7' external 3 /bin/sh -c 'cat; exit 0'",
              "printf 'pipeline=%s\\n' $response[2]",
              "rpc run 7 pipeline 0 2 external 3 /bin/sh -c 'printf abc; exit 7' external 3 /bin/sh -c 'cat; exit 0'",
              "printf 'pipeline=%s\\n' $response[2]",
              "rpc spawn 7 pipeline 1 2 external 3 /bin/sh -c 'exit 4' external 3 /bin/sh -c 'exit 3'",
              "set job $response[3]",
              "rpc wait 7 source.sh 1 $job",
              "printf 'background=%s\\n' $response[2]"
            ]
        )
        ""
    checkResult "concurrent pipelines" (0, "abcpipeline=7\nabcpipeline=0\nbackground=3\n", "") result
  let script = "#" <> B.replicate 300000 120 <> "\nprintf \"%s|%s\" \"$argv[1]\" \"$argv[2]\"\n"
  prepared <- invoke (runtime ctx) ["--abi", "2", "session-prepare"] (script <> "\0")
  let packet = B.split 0 (processStdout prepared)
  check "large capsule metadata" (code prepared == 0 && take 2 packet == ["ok", "0"] && length packet == 5)
  case packet of
    _ : _ : capsule : token : _ -> do
      result <- invoke (runtime ctx) ["--abi", "2", "session-run", "--capsule", C.unpack capsule, C.unpack token, "", "value"] ""
      checkResult "capsule empty argv" (0, "|value", "") result
    _ -> fail "invalid capsule packet"
  withWorkspace $ \directory -> do
    result <-
      runFish
        ctx
        directory
        ( linesOf
            [ "rpc spawn 7 external /bin/cat",
              "set job $response[3]",
              "rpc wait 7 source.sh 1 $job",
              "read -l original",
              "printf 'stdin=%s\\n' \"$original\"",
              "set payload (string repeat -n 300000 x)",
              "rpc run 7 snapshot 'set state (string split0 < $argv[1]); printf \"%s:%s\" \"$state[1]\" (string length -- \"$state[2]\")' 4 '' \"$payload\"",
              "printf ':snapshot=%s\\n' $response[2]",
              "rpc run 7 pipeline 1 2 builtin 4 source.sh 1 echo \"$payload\" external 3 /usr/bin/head -c 1",
              "printf ':pipe=%s\\n' $response[2]"
            ]
        )
        "original\n"
    checkResult "large snapshot and pipeline SIGPIPE" (0, "stdin=original\n:300000:snapshot=0\nx:pipe=141\n", "") result
  withWorkspace $ \directory -> do
    probe <- writeScript (directory </> "closed.sh") "#!/bin/sh\n[ ! -e /dev/fd/$1 ]\n"
    permissions <- getPermissions probe
    setPermissions probe permissions {executable = True}
    forM_ ([0 .. 2] :: [Int]) $ \descriptor -> do
      let closedScript = directory </> "closed.fish"
          closer = "exec " <> show descriptor <> (if descriptor == 0 then "<&-; " else ">&-; ")
      _ <- writeScript closedScript ("command /bin/sh " <> quoteFish probe <> " " <> show descriptor <> "\nexit $status\n")
      env <- sessionEnvironment (runtime ctx)
      actual <- invokeWith "bash" ["-c", closer <> "exec \"$@\"", "bash", runtime ctx, "--abi", "2", "session-run", closedScript] "" Nothing (Just env) 10000000
      check ("closed evaluator stdio " <> show descriptor <> ": " <> show actual) (code actual == 0)
  withWorkspace $ \directory -> do
    result <-
      runFish
        ctx
        directory
        ( linesOf
            [ "rpc wait 7 source.sh 23 ''",
              "printf 'empty=%s\\n' $response[2]",
              "rpc wait 7 source.sh 23 0",
              "printf 'zero=%s\\n' $response[2]",
              "rpc wait 7 source.sh 23 2147483648",
              "printf 'large=%s\\n' $response[2]"
            ]
        )
        ""
    checkResult "wait operands" (0, "empty=1\nzero=127\nlarge=1\n", "source.sh: line 23: wait: `': not a pid or valid job spec\nsource.sh: line 23: wait: pid 0 is not a child of this shell\nsource.sh: line 23: wait: `2147483648': not a pid or valid job spec\n") result
  withWorkspace $ \_ -> do
    (readFd, writeFd) <- createPipe
    writer <- fdToHandle writeFd
    readHandle <- fdToHandle readFd
    (Just input, Just output, _, guardian) <- createProcess (proc "/bin/bash" ["-c", "exec 200>&2; exec 2>/dev/null; exec \"$1\" --abi 2 session-prepare", "bash", runtime ctx]) {std_in = CreatePipe, std_out = CreatePipe, std_err = UseHandle writer}
    hClose writer
    B.hPut input "true\0"
    hClose input
    preparedPacket <- B.hGetContents output
    void $ evaluate (B.length preparedPacket)
    guardianExit <- waitForProcess guardian
    eof <- timeout 5000000 (B.hGet readHandle 1)
    hClose readHandle
    check "capsule guardian retained high source fd" (guardianExit == ExitSuccess && eof == Just "")
    case B.split 0 preparedPacket of
      "ok" : "0" : capsule : token : _ -> do
        launched <- invoke (runtime ctx) ["--abi", "2", "session-run", "--capsule", C.unpack capsule, C.unpack token] ""
        check "capsule from high-fd guardian" (code launched == 0)
      _ -> fail "invalid high-fd capsule packet"
  withWorkspace $ \directory -> do
    result <- runFish ctx directory "rpc finish-signal 0 13\nexit 9" ""
    checkResult "pending signal overrides exit" (-13, "", "") result
    result2 <- runFish ctx directory "rpc fd-push 7\nrpc fd-close 7 1\nrpc run 7 builtin source.sh 12 echo value\nset code $response[2]\nrpc fd-pop 7\nprintf '%s' $code" ""
    checkResult "closed builtin stdout" (0, "1", "source.sh: line 12: echo: write error: Bad file descriptor\n") result2
  withWorkspace $ \directory -> do
    createDirectory (directory </> "bin")
    let command = directory </> "bin" </> "owned-command"
    _ <- writeScript command "#!/bin/sh\nprintf prefix-path; exit 6\n"
    permissions <- getPermissions command
    setPermissions command permissions {executable = True}
    result <- runFish ctx directory ("builtin cd " <> quoteFish directory <> "\nset -lx PATH bin\nrpc run 7 external owned-command\nprintf ':%s\\n' $response[2]\nrpc spawn 7 external absent-command\nset job $response[3]\nrpc wait 7 source.sh 1 $job\nprintf 'missing=%s\\n' $response[2]") ""
    checkResult "relative PATH after cwd" (0, "prefix-path:6\nmissing=127\n", "") result
  withWorkspace $ \directory -> do
    let vanished = directory </> "removed"
    createDirectory vanished
    result <- runFish ctx directory ("builtin cd " <> quoteFish vanished <> "\nrpc run 7 external /bin/rmdir " <> quoteFish vanished <> "\nprintf 'removed=%s\\n' $response[2]\nrpc run 7 builtin source.sh 1 printf after\nprintf ':%s\\n' $response[2]\nrpc run 7 external /usr/bin/true\nprintf 'external=%s\\n' $response[2]") ""
    checkResult "removed cwd identity" (0, "removed=0\nafter:0\nexternal=0\n", "") result
  withWorkspace $ \directory -> do
    let program = directory </> "program"
    _ <- writeScript program "#!/bin/sh\nexit 0\n"
    result <- runFish ctx directory ("rpc run 7 external-site source.sh 12 " <> quoteFish program <> "\nprintf 'denied=%s\\n' $response[2]\nrpc spawn 7 external-site source.sh 13 " <> quoteFish (program <> "-missing") <> "\nset job $response[3]\nrpc wait 7 source.sh 13 $job\nprintf 'missing=%s\\n' $response[2]") ""
    checkResult "exec source diagnostics" (0, "denied=126\nmissing=127\n", b ("source.sh: line 12: " <> program <> ": Permission denied\nsource.sh: line 13: " <> program <> "-missing: No such file or directory\n")) result
  withWorkspace $ \directory -> do
    createDirectory (directory </> "directory")
    B.writeFile (directory </> "plain") ""
    createSymbolicLink "loop" (directory </> "loop")
    B.writeFile (directory </> "binary") (B.pack [255, 0] <> "garbage")
    permissions <- getPermissions (directory </> "binary")
    setPermissions (directory </> "binary") permissions {executable = True}
    forM_ ([("directory", 126, "Is a directory"), ("plain/child", 126, "Not a directory"), ("loop", 126, "Too many levels of symbolic links"), ("absent", 127, "No such file or directory"), ("binary", 126, "cannot execute binary file: Exec format error")] :: [(String, Int, String)]) $ \(command, expected, reason) -> do
      let body = "builtin cd " <> quoteFish directory <> "\nrpc run 7 external-site source.sh 3 ./" <> command <> "\nprintf '%s' $response[2]"
      result <- runFish ctx directory body ""
      checkResult ("session exec classification " <> command) (0, b (show expected), b ("source.sh: line 3: ./" <> command <> ": " <> reason <> "\n")) result
