{-# LANGUAGE OverloadedStrings #-}

module Monk.Tooling.Runtime.Ownership
  ( runPortable,
    runExec,
    runDirectOutput,
    runNativeLauncher,
    runCallbackDiagnostics,
    runDirectorySignals,
    runSignals,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (IOException, catch, evaluate)
import Data.Bits ((.&.))
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.List (lookup, nub)
import Monk.Tooling.Process (ProcessResult (..))
import Monk.Tooling.Runtime.Common
import System.Directory (Permissions (..), copyFile, createDirectory, doesDirectoryExist, doesFileExist, getPermissions, listDirectory, removeFile, setPermissions)
import System.Exit (ExitCode (..))
import System.FilePath (splitSearchPath, takeDirectory, takeFileName, (</>))
import System.IO (hClose)
import System.Posix.Files (createNamedPipe, createSymbolicLink, fileMode, getFileStatus, ownerReadMode, ownerWriteMode, unionFileModes)
import System.Posix.IO (FdOption (CloseOnExec), OpenMode (ReadWrite), closeFd, createPipe, defaultFileFlags, fdToHandle, fdWrite, openFd, setFdOption)
import System.Posix.Signals (sigINT, sigQUIT, sigTERM, signalProcess)
import System.Posix.Types (CPid)
import System.Process (CreateProcess (..), StdStream (..), createProcess, getPid, proc, waitForProcess)
import System.Timeout (timeout)

b :: String -> B.ByteString
b = C.pack

maskCommand :: Int -> FilePath -> [String] -> (FilePath, [String])
maskCommand mask binary args =
  ( "/bin/bash",
    ["-c", concat ["exec " <> show fd <> (if fd == 0 then "<&-; " else ">&-; ") | fd <- ([0 .. 2] :: [Int]), mask `mod` (2 ^ (fd + 1)) < 2 ^ fd] <> "exec \"$@\"", "bash", binary] <> args
  )

invokeMasked :: Context -> Int -> FilePath -> [String] -> B.ByteString -> Maybe FilePath -> Maybe [(String, String)] -> IO ProcessResult
invokeMasked _ mask binary args input directory env = do
  let (wrapper, wrapped) = maskCommand mask binary args
  invokeWith wrapper wrapped input directory env 15000000

runPortable :: Context -> IO ()
runPortable ctx = do
  target <- invoke "uname" ["-sm"] ""
  let profile = case C.words (processStdout target) of
        ["Darwin", "arm64"] -> "aarch64-darwin"
        ["Linux", "aarch64"] -> "aarch64-linux"
        ["Linux", "x86_64"] -> "x86_64-linux"
        other -> error ("unsupported host " <> show other)
  description <- invoke (runtime ctx) ["--describe"] ""
  let linesOut = C.lines (processStdout description)
  check "runtime metadata shape" (case linesOut of [firstLine, _, third] -> code description == 0 && firstLine == "monk-runtime 2 bash53-i64" && third == b ("target " <> profile); _ -> False)
  old <- invoke (runtime ctx) ["--abi", "1", "echo"] "hello\0"
  check "ABI 1 rejection" (code old == 125)
  env <- referenceEnvironment
  let hostile = ("GHCRTS", "--definitely-invalid") : filter ((/= "GHCRTS") . fst) env
  unaffected <- invokeWith (runtime ctx) ["--abi", "2", "echo"] "hello\0" Nothing (Just hostile) 10000000
  checkResult "GHCRTS ignored" (0, "hello\n", "") unaffected
  forM_ captureCases $ \(script, scalarState, mask, original, expected) -> do
    let payload = "warning\0" <> b (show mask) <> "\0" <> script <> "\0" <> "2\0" <> scalarState
        setup = if original then "3</dev/null" else "3<&-"
    result <- invoke "bash" ["-c", "exec \"$1\" --abi 2 child-capture " <> setup, "bash", runtime ctx] payload
    checkResult ("portable capture " <> show script) expected result
  withWorkspace $ \directory -> do
    probe <- writeScript (directory </> "probe.sh") "#!/bin/sh\n[ ! -e /dev/fd/200 ]\n"
    permissions <- getPermissions probe
    setPermissions probe permissions {executable = True}
    let script = b ("command /bin/sh " <> quoteFish probe)
        payload = frames ["", "6", script, "2"]
    highFd <- invoke "bash" ["-c", "exec 200</dev/null; ulimit -n 64; exec \"$1\" --abi 2 child-capture", "bash", runtime ctx] payload
    check ("high descriptor cleanup " <> show highFd) (code highFd == 0 && processStdout highFd == B.concat ["ok\0", "0\0", "\0"])
  withWorkspace $ \directory -> do
    let workspace = directory </> "temporary"
        fifo = directory </> "input"
    createDirectory workspace
    createNamedPipe fifo (ownerReadMode `unionFileModes` ownerWriteMode)
    keepOpen <- openFd fifo ReadWrite defaultFileFlags
    let script = b ("printf ready >&2; read -l item < \"" <> fifo <> "\"")
        payload = frames ["", "6", script, "2"]
    (Just input, Just output, Just errors, process) <- createProcess (proc (runtime ctx) ["--abi", "2", "child-capture"]) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, env = Just (("TMPDIR", workspace) : filter ((/= "TMPDIR") . fst) env)}
    B.hPut input payload
    hClose input
    ready <- timeout 10000000 (B.hGet errors 5)
    check "child-capture ready" (ready == Just "ready")
    owned <- listDirectory workspace
    check "child-capture private dir count" (length owned == 1)
    forM_ owned $ \name -> do
      let private = workspace </> name
      mode <- fileMode <$> getFileStatus private
      check "child-capture private dir mode" ((mode .&. 0o777) == 0o700)
      children <- listDirectory private
      forM_ children $ \child -> do
        childMode <- fileMode <$> getFileStatus (private </> child)
        check "child-capture private file mode" ((childMode .&. 0o777) == 0o600)
    getPid process >>= maybe (fail "missing child-capture PID") (signalProcess sigTERM)
    remainingOut <- B.hGetContents output
    remainingErr <- B.hGetContents errors
    evaluate (B.length remainingOut + B.length remainingErr)
    ended <- waitForProcess process
    closeFd keepOpen
    check ("child-capture owner signal " <> show (ended, remainingOut, remainingErr)) (ended == ExitFailure 143)
    listDirectory workspace >>= check "child-capture workspace after signal" . null
  where
    captureCases :: [(B.ByteString, B.ByteString, Int, Bool, (Int, B.ByteString, B.ByteString))]
    captureCases =
      [ ("command cat $argv[1]; printf \"\\377\\376\\n\\n\"", "prefix\0state\0", 7, True, (0, B.concat ["ok\0", "0\0", "prefixstate", B.pack [255, 254, 0]], "warning")),
        ("command head -c 1048576 /dev/zero; printf x", "", 7, True, (0, B.concat ["ok\0", "0\0", "x\0"], "warning")),
        ("true", "", 7, False, (0, B.concat ["error\0", "125\0", "child-transport-failure\0"], "")),
        ("true", "", 6, False, (0, B.concat ["ok\0", "0\0", "\0"], "")),
        ("command kill -TERM $fish_pid", "", 7, True, (0, B.concat ["ok\0", "143\0", "\0"], ""))
      ]

runExec :: Context -> IO ()
runExec ctx = do
  env <- referenceEnvironment
  let probe = "printf '%s:' \"$$\"; printf '%s' \"$1\""
      source = "exec \"$1\" --abi 2 exec-site source.sh 1 \"$2\" -c \"$3\" probe \"$(printf '\\377')\""
  (Just input, Just output, Just errors, process) <- createProcess (proc "/bin/bash" ["-c", source, "bash", runtime ctx, "/bin/sh", probe]) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, env = Just env}
  hClose input
  pid <- getPid process
  out <- B.hGetContents output
  err <- B.hGetContents errors
  _ <- B.length out `seq` pure ()
  _ <- B.length err `seq` pure ()
  ended <- waitForProcess process
  check ("exec PID and byte argv " <> show (pid, out, err)) $ ended == ExitSuccess && maybe False (\value -> b (show value) <> B.pack [58, 255] == out) pid && B.null err
  let closedProbe = "[ ! -e /dev/fd/$1 ]"
  forM_ ([0 .. 2] :: [Int]) $ \descriptor -> do
    result <- invokeMasked ctx (7 - 2 ^ descriptor) (runtime ctx) ["--abi", "2", "exec-site", "source.sh", "1", "/bin/sh", "-c", closedProbe, "probe", show descriptor] "" Nothing (Just env)
    checkResult ("exec closed fd " <> show descriptor) (0, "", "") result
  withWorkspace $ \directory -> do
    let command = directory </> "program"
    _ <- writeScript command "#!/bin/sh\nexit 0\n"
    forM_ [(126, "Permission denied"), (127, "No such file or directory")] $ \(expected, reason) -> do
      when (expected == 127) (removeFile command)
      forM_ [7, 6, 5, 3] $ \mask -> do
        result <- invokeMasked ctx mask (runtime ctx) ["--abi", "2", "exec-site", "source.sh", "12", command] "" Nothing (Just env)
        let diagnostic = if mask `mod` 8 < 4 then "" else b ("source.sh: line 12: " <> command <> ": " <> reason <> "\n")
        checkResult ("exec errno " <> show (expected, mask)) (expected, "", diagnostic) result
  withWorkspace $ \directory -> do
    createDirectory (directory </> "directory")
    B.writeFile (directory </> "plain") ""
    createSymbolicLink "loop" (directory </> "loop")
    B.writeFile (directory </> "binary") (B.pack [255, 0] <> "garbage")
    permissions <- getPermissions (directory </> "binary")
    setPermissions (directory </> "binary") permissions {executable = True}
    forM_ [("directory", 126, "Is a directory"), ("plain/child", 126, "Not a directory"), ("loop", 126, "Too many levels of symbolic links"), ("absent", 127, "No such file or directory"), ("binary", 126, "cannot execute binary file: Exec format error")] $ \(command, expected, reason) -> do
      result <- invokeWith (runtime ctx) ["--abi", "2", "exec-site", "source.sh", "3", "./" <> command] "" (Just directory) (Just env) 10000000
      checkResult ("exec classification " <> command) (expected, "", b ("source.sh: line 3: ./" <> command <> ": " <> reason <> "\n")) result

runDirectOutput :: Context -> IO ()
runDirectOutput ctx = do
  env <- referenceEnvironment
  forM_ writerCases $ \(name, arguments, expected) -> do
    let request = frames (["source.sh", "12", name] <> arguments)
    result <- invokeWith (runtime ctx) ["--abi", "2", "write-builtin"] request Nothing (Just env) 10000000
    checkResult ("direct " <> C.unpack name) (0, expected, "") result
    forM_ [(5, b ("source.sh: line 12: " <> (if name == "echo-bytes" then "echo" else C.unpack name) <> ": write error: Bad file descriptor\n")), (1, "")] $ \(mask, diagnostic) -> do
      closed <- invokeMasked ctx mask (runtime ctx) ["--abi", "2", "write-builtin"] request Nothing (Just env)
      checkResult ("direct closed " <> show mask <> " " <> C.unpack name) (1, "", diagnostic) closed
  forM_ [("printf", [""]), ("echo", ["-n"])] $ \(name, arguments) -> do
    closed <- invokeMasked ctx 5 (runtime ctx) ["--abi", "2", "write-builtin"] (frames (["source.sh", "12", name] <> arguments)) Nothing (Just env)
    checkResult "zero byte direct writer" (0, "", "") closed
  forM_ ["unterminated", "", frames ["source.sh", "0", "echo"], frames ["source.sh", "01", "echo"], frames ["source.sh", "1", "eval", "printf x"], frames ["source.sh", "1", "echo-bytes", "a", "b"], frames ["source.sh", "1", "printf", "%q", "x"]] $ \payload -> do
    result <- invoke (runtime ctx) ["--abi", "2", "write-builtin"] payload
    check "malformed direct request rejected" (code result == 125 && B.null (processStdout result))
  forM_ [(["13"], -13), (["9"], 125), ([], 125)] $ \(arguments, expected) -> do
    result <- invoke (runtime ctx) (["--abi", "2", "raise-signal"] <> arguments) ""
    check "raise-signal semantics" (code result == expected && B.null (processStdout result))
  description <- invoke (runtime ctx) ["--describe"] ""
  check "describe complete" (code description == 0 && length (C.lines (processStdout description)) == 3 && B.null (processStderr description))
  forM_ writerCases $ \(name, arguments, _) -> do
    broken <- invokeBrokenPipe (runtime ctx) ["--abi", "2", "write-builtin"] (frames (["source.sh", "12", name] <> arguments)) 0
    check ("direct writer SIGPIPE " <> C.unpack name <> ": " <> show broken) (broken == (ExitFailure (-13), "", ""))
  partial <- invokeBrokenPipe (runtime ctx) ["--abi", "2", "write-builtin"] (frames ["source.sh", "12", "printf", "%s", B.replicate 1048576 122]) 64
  check ("partial direct writer SIGPIPE " <> show partial) (partial == (ExitFailure (-13), B.replicate 64 122, ""))
  metadataBroken <- invokeBrokenPipe (runtime ctx) ["--describe"] "" 0
  check "describe SIGPIPE" (metadataBroken == (ExitFailure (-13), "", ""))
  case monk ctx of
    Nothing -> pure ()
    Just translator -> withWorkspace $ \directory -> do
      let source = directory </> "input.bash"
          target = directory </> "input.fish"
      forM_ ["printf x", "echo x", "printf ''", "echo -n", "printf '%s' $'\\xff'", "echo 'a b'"] $ \body -> do
        _ <- writeScript source body
        translated <- invokeWith translator [source, "--strict"] "" Nothing (Just env) 15000000
        check "translate direct writer" (code translated == 0)
        B.writeFile target (processStdout translated)
        forM_ [7, 5, 1] $ \mask -> do
          reference <- invokeMasked ctx mask "bash" [source] "" Nothing (Just env)
          actual <- invokeMasked ctx mask (runtime ctx) ["--abi", "2", "launch", target] "" Nothing (Just env)
          checkResult ("compiled direct writer " <> body <> " mask " <> show mask) (code reference, processStdout reference, processStderr reference) actual
        referencePipe <- invokeBrokenPipe "bash" [source] "" 0
        actualPipe <- invokeBrokenPipe (runtime ctx) ["--abi", "2", "launch", target] "" 0
        check ("compiled direct writer no-reader " <> body <> ": " <> show (referencePipe, actualPipe)) (actualPipe == referencePipe)
  where
    writerCases =
      [ ("echo", ["one", "", "three"], "one  three\n"),
        ("echo-bytes", ["already expanded\n"], "already expanded\n"),
        ("echo", ["-ne", "a\\0b\\xff"], B.pack [97, 0, 98, 255]),
        ("printf", ["<%s>:%d\\n", B.singleton 255, "-42"], B.pack [60, 255, 62, 58, 45, 52, 50, 10]),
        ("printf", ["%s", B.replicate 1048576 120], B.replicate 1048576 120)
      ]

-- | Give a child a pipe whose reader is absent or closes after a prefix.
-- The payload writer runs independently so a large request cannot deadlock
-- while the child is blocked on the output pipe.
invokeBrokenPipe :: FilePath -> [String] -> B.ByteString -> Int -> IO (ExitCode, B.ByteString, B.ByteString)
invokeBrokenPipe binary args payload prefix = do
  (readerFd, writerFd) <- createPipe
  when (prefix == 0) (closeFd readerFd)
  when (prefix /= 0) (setFdOption readerFd CloseOnExec True)
  writer <- fdToHandle writerFd
  (Just input, _, Just errors, process) <- createProcess (proc binary args) {std_in = CreatePipe, std_out = UseHandle writer, std_err = CreatePipe}
  hClose writer
  _ <- forkIO ((B.hPut input payload >> hClose input) `catch` (\(_ :: IOException) -> pure ()))
  received <-
    if prefix == 0
      then pure ""
      else do
        readHandle <- fdToHandle readerFd
        bytes <- B.hGet readHandle prefix
        hClose readHandle
        pure bytes
  err <- B.hGetContents errors
  evaluate (B.length err)
  ended <- waitForProcess process
  pure (ended, received, err)

runCallbackDiagnostics :: Context -> IO ()
runCallbackDiagnostics ctx = case monk ctx of
  Nothing -> fail "callback-diagnostics requires --monk"
  Just translator -> withWorkspace $ \directory -> do
    env <- referenceEnvironment
    let source = directory </> "root.bash"
        moduleFile = directory </> "module.bash"
        generated = directory </> "generated.fish"
    forM_ (zip [0 :: Int ..] callbackCases) $ \(index, (body, dependency)) -> do
      _ <- writeScript source body
      _ <- writeScript moduleFile dependency
      translated <- invokeWith translator [source, "--strict", "--recursive", "--sources", "inline", "--runtime", runtime ctx] "" (Just directory) (Just env) 30000000
      check ("callback translation " <> show index <> ": " <> show translated) (code translated == 0)
      B.writeFile generated (processStdout translated)
      forM_ [7, 5, 1] $ \mask -> do
        oracle <- invokeMasked ctx mask "bash" ["--noprofile", "--norc", source] "" (Just directory) (Just env)
        actual <- invokeMasked ctx mask (runtime ctx) ["--abi", "2", "launch", generated] "" (Just directory) (Just env)
        checkResult ("callback " <> show index <> " mask " <> show mask) (code oracle, processStdout oracle, processStderr oracle) actual
  where
    callbackCases =
      [ ("trap 'printf x' ERR\nfalse\n", ""),
        ("trap 'true\nprintf x' ERR\nfalse\n", ""),
        ("set -- first 'two words'\ntrap 'printf \"<%s>\" \"$@\"' ERR\nfalse\nfalse\n", ""),
        ("trap 'printf x' ERR\n. ./module.bash\n", "false\n"),
        (". ./module.bash\ntrue\n", "trap 'printf x' EXIT\n"),
        (". ./module.bash\n", "trap 'printf x' EXIT\nexit 2\n"),
        ("trap 'printf x' EXIT\n. ./module.bash\nf\n", "f() { exit 2; }\n"),
        ("trap 'printf x' EXIT\n(trap 'printf child' EXIT; true)\n", ""),
        ("true\ntrue\ntrap 'printf x' EXIT\ntrue\n", ""),
        ("trap 'printf err' ERR\ntrap 'printf exit' EXIT\nfalse\n", ""),
        ("trap 'false; printf x' ERR\nfalse\n", "")
      ]

runNativeLauncher :: Context -> IO ()
runNativeLauncher ctx = do
  inherited <- referenceEnvironment
  withWorkspace $ \directory -> do
    let workspace = directory </> "workspace"
        script = directory </> "entry.fish"
        resultFile = directory </> "result"
        env = ("MONK_TEST_RUNTIME", runtime ctx) : ("TMPDIR", workspace) : filter (\(key, _) -> key /= "MONK_TEST_RUNTIME" && key /= "TMPDIR") inherited
    createDirectory workspace
    forM_ [0 .. 7] $ \mask -> do
      _ <- writeScript script ("command \"$MONK_TEST_RUNTIME\" --abi 2 descriptor-state\nset mask $status\nset --global argv replacement $argv\nprintf \"%s\\0\" $mask $argv > " <> quoteFish resultFile <> "\n")
      actual <- invokeMasked ctx mask (runtime ctx) ["--abi", "2", "launch", script, "", "two words"] "" Nothing (Just env)
      checkResult ("launcher stdio mask " <> show mask) (0, "", "") actual
      saved <- B.readFile resultFile
      check ("launcher original descriptor mask/argv " <> show mask) (saved == B.concat [b (show mask), "\0replacement\0\0two words\0"])
      listDirectory workspace >>= check "launcher leaked temporary files" . null
    _ <- writeScript script "printf '%s\\0' source.sh 7 printf x | command \"$MONK_TEST_RUNTIME\" --abi 2 write-builtin\nexit $status\n"
    closed <- invokeMasked ctx 5 (runtime ctx) ["--abi", "2", "launch", script] "" Nothing (Just env)
    checkResult "launcher writer sees original closed stdout" (1, "", "source.sh: line 7: printf: write error: Bad file descriptor\n") closed
    _ <- writeScript script "exec \"$MONK_TEST_RUNTIME\" --abi 2 raise-signal 13\n"
    signalled <- invokeMasked ctx 6 (runtime ctx) ["--abi", "2", "launch", script] "" Nothing (Just env)
    checkResult "launcher native signal" (-13, "", "") signalled
    listDirectory workspace >>= check "signal cleanup" . null
    forM_ ["MONK_LAUNCH_ORIGINAL", "MONK_LAUNCH_WRAPPER"] $ \key -> do
      bad <- invokeWith (runtime ctx) ["--abi", "2", "launch", script] "" Nothing (Just ((key, "") : env)) 10000000
      check ("launcher metadata injection " <> key) (code bad == 125)
    let probe = "printf '%s\\n' \"${MONK_LAUNCH_ORIGINAL-None}\" \"${MONK_LAUNCH_WRAPPER-None}\""
    _ <- writeScript script ("printf '%s\\0' \"$MONK_LAUNCH_ORIGINAL\" \"$MONK_LAUNCH_WRAPPER\" (status filename) > " <> quoteFish resultFile <> "\ncommand \"$MONK_TEST_RUNTIME\" --abi 2 exec-site source.sh 1 /bin/sh -c " <> quoteFish probe <> "\n")
    metadata <- invokeMasked ctx 6 (runtime ctx) ["--abi", "2", "launch", script] "" Nothing (Just env)
    checkResult "launcher metadata private to wrapper" (0, "None\nNone\n", "") metadata
    packet <- B.split 0 <$> B.readFile resultFile
    case packet of
      original : wrapper : actual : _ -> do
        check "original artifact metadata" (original == b script && wrapper == actual)
        doesFileExist (C.unpack wrapper) >>= check "wrapper was not cleaned" . not
      _ -> fail "invalid launcher metadata packet"
    let fifo = directory </> "release"
    createNamedPipe fifo (ownerReadMode `unionFileModes` ownerWriteMode)
    release <- openFd fifo ReadWrite defaultFileFlags
    _ <- writeScript script ("printf ready >&2\nread -l item < " <> quoteFish fifo <> "\n")
    let (launcher, arguments) = maskCommand 6 (runtime ctx) ["--abi", "2", "launch", script]
    (Just input, Just output, Just errors, process) <- createProcess (proc launcher arguments) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, env = Just env}
    hClose input
    ready <- timeout 10000000 (B.hGet errors 5)
    check "launcher readiness" (ready == Just "ready")
    owned <- listDirectory workspace
    check "launcher private directory count" (length owned == 1)
    case owned of
      [name] -> do
        let private = workspace </> name
        mode <- fileMode <$> getFileStatus private
        check "launcher private directory mode" ((mode .&. 0o777) == 0o700)
        children <- listDirectory private
        forM_ children $ \child -> do
          childMode <- fileMode <$> getFileStatus (private </> child)
          check "launcher private file mode" ((childMode .&. 0o777) == 0o600)
      _ -> fail "launcher temporary directory count changed"
    pid <- getPid process
    maybe (fail "launcher PID unavailable") (signalProcess sigTERM) pid
    outputBytes <- B.hGetContents output
    errorBytes <- B.hGetContents errors
    evaluate (B.length outputBytes + B.length errorBytes)
    ended <- waitForProcess process
    check ("launcher signal cleanup " <> show (ended, outputBytes, errorBytes)) (ended == ExitFailure (-15) && B.null outputBytes && B.null errorBytes)
    listDirectory workspace >>= check "launcher workspace after signal" . null
    let done = directory </> "done"
        background = "command /bin/sh -c " <> quoteFish ("read line < " <> quoteFish fifo <> "; printf survived > " <> quoteFish done) <> " &\nexit 0\n"
    _ <- writeScript script background
    let (backgroundLauncher, backgroundArgs) = maskCommand 6 (runtime ctx) ["--abi", "2", "launch", script]
    (Just backgroundInput, Just backgroundOut, Just backgroundErr, backgroundProcess) <- createProcess (proc backgroundLauncher backgroundArgs) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, env = Just env}
    hClose backgroundInput
    ownerExit <- timeout 10000000 (waitForProcess backgroundProcess)
    check "launcher owner blocked on background job" (ownerExit == Just ExitSuccess)
    _ <- fdWrite release "go\n"
    survived <- timeout 10000000 (waitForFile done)
    check "launcher background child survived" (survived == Just True)
    rest <- B.hGetContents backgroundOut
    restErr <- B.hGetContents backgroundErr
    evaluate (B.length rest + B.length restErr)
    closeFd release
    check "launcher background streams" (B.null rest && B.null restErr)
    listDirectory workspace >>= check "launcher workspace after completion" . null
  case monk ctx of
    Nothing -> pure ()
    Just translator -> forM_ ["absolute", "relative"] $ \sourceSpelling -> withWorkspace $ \directory -> do
      let root = directory </> "artifact directory\n"
          source = root </> "source.bash"
          target = root </> "entry.fish"
          moduleFile = root </> "module.bash"
          provider = root </> "provider"
      createDirectory root
      _ <- writeScript moduleFile "f() { printf \"<%s>\" \"$1\"; }; f \"$1\"\n"
      _ <- writeScript source (". " <> (if sourceSpelling == "absolute" then quoteFish moduleFile else "./module.bash") <> " \"$1\"; (printf \"%s\" \"$2\")\n")
      copyFile (runtime ctx) provider
      translated <- invokeWith translator [source, "--strict", "--recursive", "--managed", "--runtime", provider, "-o", target] "" (Just root) Nothing 30000000
      check ("managed launcher translation " <> sourceSpelling <> ": " <> show translated) (code translated == 0)
      providers <- findNamed "monk-runtime" root
      case providers of
        [captured] -> do
          let generation = takeDirectory (takeDirectory captured)
              generationEntry = generation </> "entry.fish"
          doesFileExist generationEntry >>= check "generation entry missing"
          generated <- listDirectory generation
          check "managed source modules missing" (any (\name -> takeFileName name /= "entry.fish" && ".fish" `C.isSuffixOf` b name) generated)
          removeFile provider
          ambient <- referenceEnvironment
          let searchPaths = splitSearchPath (fromMaybe "" (lookup "PATH" ambient))
          allowed <- filterM (\entry -> not <$> doesFileExist (entry </> "monk-runtime")) searchPaths
          let executionEnv = ("PATH", intercalate ":" allowed) : filter (\(key, _) -> key /= "PATH" && key /= "MONK_TEST_RUNTIME") ambient
          remainingProviders <- filterM (doesFileExist . (</> "monk-runtime")) allowed
          check "managed provider still on PATH" (null remainingProviders)
          forM_ [0 .. 7] $ \mask -> do
            oracle <- invokeMasked ctx mask "bash" [source, "first", "second"] "" (Just root) (Just executionEnv)
            forM_ [target, generationEntry] $ \artifact -> do
              actual <- invokeMasked ctx mask captured ["--abi", "2", "launch", artifact, "first", "second"] "" (Just root) (Just executionEnv)
              checkResult ("captured provider " <> sourceSpelling <> " mask " <> show mask) (code oracle, processStdout oracle, processStderr oracle) actual
        _ -> fail ("expected one captured runtime: " <> show providers)

waitForFile :: FilePath -> IO Bool
waitForFile path = do
  present <- doesFileExist path
  if present
    then do
      contents <- B.readFile path
      if contents == "survived" then pure True else threadDelay 10000 >> waitForFile path
    else do
      threadDelay 10000
      waitForFile path

findNamed :: String -> FilePath -> IO [FilePath]
findNamed wanted root = do
  entries <- listDirectory root
  fmap concat $ forM entries $ \entry -> do
    let path = root </> entry
    directory <- doesDirectoryExist path
    if directory then findNamed wanted path else pure [path | entry == wanted]

runDirectorySignals :: Context -> IO ()
runDirectorySignals ctx = case monk ctx of
  Nothing -> fail "directory-signals requires --monk"
  Just translator -> do
    inherited <- referenceEnvironment
    forM_ [("", "pwd"), ("", "pwd -P"), ("", "pushd /tmp"), ("OLDPWD=/tmp\n", "cd -"), ("pushd /tmp >/dev/null\n", "popd"), ("", "popd 2>&1"), ("", "cd /definitely-missing-monk-directory 2>&1")] $ \(prepare, operation) ->
      withWorkspace $ \directory -> do
        let source = directory </> "source.sh"
            target = directory </> "output.fish"
            ready = directory </> "ready"
            release = directory </> "release"
            continued = directory </> "continued"
            env = ("PWD", directory) : ("OLDPWD", "/tmp") : filter (\(key, _) -> key /= "PWD" && key /= "OLDPWD") inherited
            body = prepare <> "printf 'ready\\n' >" <> ready <> "\nread -r gate <" <> release <> "\nfalse\n" <> operation <> "\nprintf continued >" <> continued <> "\n"
        createNamedPipe ready (ownerReadMode `unionFileModes` ownerWriteMode)
        createNamedPipe release (ownerReadMode `unionFileModes` ownerWriteMode)
        _ <- writeScript source body
        translated <- invokeWith translator [source, "--strict", "--directory-contract", "stable", "--runtime", runtime ctx, "-o", target] "" (Just directory) (Just env) 30000000
        check ("directory signal translation " <> operation <> ": " <> show translated) (code translated == 0)
        oracle <- directorySignalCommand "bash" ["--noprofile", "--norc", source] directory env ready release continued
        check ("Bash source did not receive SIGPIPE: " <> operation <> " " <> show oracle) (oracle == (ExitFailure (-13), "", False))
        actual <- directorySignalCommand "fish" ["--no-config", target] directory env ready release continued
        check ("directory signal identity " <> operation <> ": " <> show (oracle, actual)) (actual == oracle)
    withWorkspace $ \directory -> do
      source <- writeScript (directory </> "trap-directory.sh") "trap 'printf callback' EXIT\npwd\n"
      rejected <- invokeWith translator [source, "--strict", "--directory-contract", "stable", "--runtime", runtime ctx] "" Nothing (Just inherited) 30000000
      check "directory/trap composition rejected" (code rejected /= 0)

directorySignalCommand :: FilePath -> [String] -> FilePath -> [(String, String)] -> FilePath -> FilePath -> FilePath -> IO (ExitCode, B.ByteString, Bool)
directorySignalCommand binary args directory env ready release continued = do
  present <- doesFileExist continued
  when present (removeFile continued)
  readyFd <- openFd ready ReadWrite defaultFileFlags
  readyHandle <- fdToHandle readyFd
  releaseFd <- openFd release ReadWrite defaultFileFlags
  (readerFd, writerFd) <- createPipe
  setFdOption readerFd CloseOnExec True
  writer <- fdToHandle writerFd
  (Just input, _, Just errors, process) <- createProcess (proc binary args) {cwd = Just directory, env = Just env, std_in = CreatePipe, std_out = UseHandle writer, std_err = CreatePipe}
  hClose input
  hClose writer
  readyBytes <- timeout 5000000 (C.hGetLine readyHandle)
  check ("directory signal readiness " <> show readyBytes) (readyBytes == Just "ready")
  closeFd readerFd
  _ <- fdWrite releaseFd "go\n"
  err <- B.hGetContents errors
  evaluate (B.length err)
  ended <- waitForProcess process
  hClose readyHandle
  closeFd releaseFd
  didContinue <- doesFileExist continued
  pure (ended, err, didContinue)

runSignals :: Context -> IO ()
runSignals ctx = do
  env <- sessionEnvironment (runtime ctx)
  forM_ (["external", "pipeline", "body", "substitution"] :: [String]) $ \kind ->
    forM_ [False, True] $ \inherited -> withWorkspace $ \directory -> do
      let ready = directory </> "ready"
          release = directory </> "release"
          script = directory </> "evaluate.fish"
      createNamedPipe ready (ownerReadMode `unionFileModes` ownerWriteMode)
      createNamedPipe release (ownerReadMode `unionFileModes` ownerWriteMode)
      readyFd <- openFd ready ReadWrite defaultFileFlags
      readyHandle <- fdToHandle readyFd
      releaseFd <- openFd release ReadWrite defaultFileFlags
      let probe = "printf '%s\\n' $$ > " <> quoteFish ready <> "; read gate < " <> quoteFish release <> "; printf 'survived\\n'"
          external = "external /bin/sh -c " <> quoteFish probe
          stage = case kind of
            "pipeline" -> "pipeline 0 2 external 1 /usr/bin/true external 3 /bin/sh -c " <> quoteFish probe
            "body" -> "body " <> quoteFish (rpcHeader <> "rpc run 7 " <> external <> "\nexit $response[2]\n")
            _ -> external
          command =
            if kind == "substitution"
              then "rpc substitution 7 input " <> stage <> "\nset child $response[3]\nset endpoint $response[4]\nprintf 'job=%s\\n' $child\nrpc run 7 external /bin/cat $endpoint\n"
              else "rpc " <> (if inherited then "run" else "spawn") <> " 7 " <> stage <> "\n"
          extra =
            if not inherited && kind /= "substitution"
              then "set child $response[3]\nprintf 'job=%s\\n' $child\nrpc wait 7 signal.sh 1 $child\n"
              else ""
      _ <- writeScript script (rpcHeader <> command <> extra <> "printf 'status=%s\\n' $response[2]\n")
      let (binary, args) =
            if inherited
              then ("/bin/bash", ["-c", "trap '' INT QUIT; exec \"$@\"", "bash", runtime ctx, "--abi", "2", "session-run", script])
              else (runtime ctx, ["--abi", "2", "session-run", script])
      (Just input, Just output, Just errors, process) <- createProcess (proc binary args) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, env = Just env}
      hClose input
      owner <- getPid process
      job <-
        if not inherited || kind == "substitution"
          then do
            line <- timeout 10000000 (C.hGetLine output)
            check ("signal job readiness " <> show (kind, inherited, line)) (maybe False (B.isPrefixOf "job=") line)
            pure (line >>= parsePid . C.drop 4)
          else pure Nothing
      readyMessage <- timeout 10000000 (C.hGetLine readyHandle)
      check ("signal child readiness " <> show (kind, inherited, readyMessage)) (maybe False (not . B.null) readyMessage)
      let child = readyMessage >>= parsePid
      forM_ (nub (catMaybes [child, if isJust job then job else owner])) $ \pid -> do
        signalProcess sigINT pid
        signalProcess sigQUIT pid
      _ <- fdWrite releaseFd "go\n"
      hClose readyHandle
      closeFd releaseFd
      out <- B.hGetContents output
      err <- B.hGetContents errors
      evaluate (B.length out + B.length err)
      ended <- waitForProcess process
      check ("ignored INT/QUIT " <> show (kind, inherited, ended, out, err)) (ended == ExitSuccess && out == "survived\nstatus=0\n" && B.null err)
  withWorkspace $ \directory -> do
    let script = directory </> "evaluate.fish"
    forM_ ([(2, "INT"), (3, "QUIT")] :: [(Int, String)]) $ \(number, name) -> do
      let probe = "kill -" <> name <> " $$"
      _ <- writeScript script (rpcHeader <> "rpc run 7 external /bin/sh -c " <> quoteFish probe <> "\nprintf '%s' $response[2]\n")
      result <- invokeWith (runtime ctx) ["--abi", "2", "session-run", script] "" (Just directory) (Just env) 10000000
      checkResult ("foreground signal " <> name) (0, b (show (128 + number)), "") result

parsePid :: B.ByteString -> Maybe CPid
parsePid bytes = case reads (C.unpack (C.takeWhile (/= '\n') bytes)) of
  [(number, "")] -> Just (fromIntegral (number :: Int))
  _ -> Nothing
