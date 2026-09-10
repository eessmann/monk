module Unit.PlannedDirectory (unitPlannedDirectoryTests) where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Monk.Output qualified as Output
import Monk.Source qualified as Source
import Monk.Translation
import Path (toFilePath)
import Path.IO qualified as PathIO
import ShellSupport (prepareEnv, shouldRunIntegration)
import System.Directory (createDirectory, createDirectoryLink, doesDirectoryExist)
import System.Exit (ExitCode (ExitFailure))
import System.FilePath ((</>))
import System.Process (CreateProcess (cwd, env), proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as H

unitPlannedDirectoryTests :: TestTree
unitPlannedDirectoryTests =
  testGroup
    "Stable directory contract"
    ( [ rejection "directory default rejects" strictConfig "pwd",
        rejection "v1 preserves directory restriction" (strictConfig {entryMode = Sourceable, callerContract = emptyCallerContract {callerAmbientEffects = NoRelevantAmbientEffects}}) "cd /tmp",
        rejection "interior missing parent cancellation rejects" standalone "cd /monk-missing/../tmp",
        rejection "oversized path component rejects" standalone ("cd /tmp/" <> T.replicate 256 "x"),
        rejection "oversized total path rejects" standalone ("cd /" <> T.intercalate "/" (replicate 22 (T.replicate 200 "x"))),
        rejection "implicit HOME cd remains deferred" standalone "cd",
        rejection "physical cd remains deferred" standalone "cd -P /tmp",
        rejection "direct PWD writes reject" standalone "PWD=/tmp",
        rejection "CDPATH mutation rejects" standalone "CDPATH=/tmp; cd /",
        rejection "unknown cd previous path rejects" standalone "cd -",
        rejection "read-only cwd contract cannot change cwd" (sourceable {callerContract = fullCaller {callerDirectory = Just (MkDirectoryPermissions ReadDirectory ReadDirectory NoDirectoryAccess NoDirectoryAccess)}}) "cd /tmp",
        sourceEdges,
        callerStackBridge,
        permissionsMatrix,
        testGroup "resolved logical path bound" [resolvedPathBound mode | mode <- [Standalone, Sourceable]],
        exportedDirectoryFunction,
        importedCwdInvalidatesSource,
        importedOldpwdInvalidatesPrevious,
        testGroup "executed directory source boundaries" [executedSource mode separate | mode <- [Standalone, Sourceable], separate <- [False, True]],
        exact Standalone "Bash startup unset exported OLDPWD" "printf '<%s>\\n' \"${OLDPWD+x}\"; cd /tmp; printenv OLDPWD",
        H.testCase "v2 explicit separated permissions decode" $ H.assertBool "v2 decoding" (isRight (parseCallerContract "{\"version\":2,\"ambientEffects\":\"none\",\"directory\":{\"contract\":\"stable\",\"cwd\":\"read-write\",\"PWD\":\"read-write\",\"OLDPWD\":\"read-write\",\"stack\":\"read-write\"}}"))
      ]
        <> [exact mode name source | mode <- [Standalone, Sourceable], (name, source) <- cases]
    )
  where
    cases =
      [ ("newline error operand", "cd '/monk\nmissing'; printf 'status:%s\\n' \"$?\""),
        ("nonascii error operand", "cd '/monké-missing'; printf 'status:%s\\n' \"$?\""),
        ("quoted error operand", "cd \"/monk'missing\"; printf 'status:%s\\n' \"$?\""),
        ("backslash error operand", "cd '/monk\\missing'; printf 'status:%s\\n' \"$?\""),
        ("failed pop preserves stack", "pushd '__SPACE__'; pushd /; rmdir '__SPACE__'; popd; popd; pwd"),
        ("exported OLDPWD remains exported", "export OLDPWD=/before; cd /tmp; printenv OLDPWD"),
        ("unexported OLDPWD remains unexported", "unset OLDPWD; OLDPWD=/before; cd /tmp; printenv OLDPWD; printf 'status:%s\\n' \"$?\""),
        ("failed cd preserves OLDPWD", "OLDPWD=/before; cd /monk-directory-does-not-exist; printf '%s\\n' \"$OLDPWD\""),
        ("space stack child isolation", "pushd '__SPACE__'; pushd /; (popd; pwd); popd; popd"),
        ("symlink logical and physical pwd", "cd '__LINK__'; pwd; pwd -P; cd ..; pwd"),
        ("logical and physical pwd", "pwd; cd -- /tmp; pwd -L; pwd -P"),
        ("leading parent path", "cd /tmp; cd ..; pwd"),
        ("actual missing directory status", "cd /monk-directory-does-not-exist; printf 'status:%s\\n' \"$?\"; pwd"),
        ("actual non-directory diagnostic", "cd /etc/passwd; printf 'status:%s\\n' \"$?\"; pwd"),
        ("push and pop order", "pushd /tmp; pushd /; popd; popd; pwd"),
        ("empty stack failure", "popd; printf 'status:%s\\n' \"$?\""),
        ("failed push leaves stack unchanged", "pushd /tmp; pushd /monk-directory-does-not-exist; popd; pwd"),
        ("success edge proves previous directory", "cd /tmp && cd -; pwd"),
        ("child cwd isolation", "cd /tmp; (cd /; pwd); pwd"),
        ("child stack isolation", "pushd /tmp; pushd /; (popd; pwd); popd; popd"),
        ("substitution stack isolation", "pushd /tmp; pushd /; printf '%s\\n' \"$(popd; pwd)\"; popd; popd")
      ]

standalone :: TranslateConfig
standalone = strictConfig {directoryContract = StableDirectoryContract}

fullCaller :: CallerContract
fullCaller = emptyCallerContract {callerAmbientEffects = NoRelevantAmbientEffects, callerFunctions = M.fromList [("rmdir", MkFunctionContract "host_rmdir" mempty mempty), ("printenv", MkFunctionContract "host_printenv" mempty mempty)], callerFunctionDirectories = M.singleton "printenv" (MkDirectoryPermissions NoDirectoryAccess NoDirectoryAccess ReadDirectory NoDirectoryAccess), callerDirectory = Just (MkDirectoryPermissions ReadWriteDirectory ReadWriteDirectory ReadWriteDirectory ReadWriteDirectory)}

sourceable :: TranslateConfig
sourceable = strictConfig {entryMode = Sourceable, callerContract = fullCaller}

rejection :: String -> TranslateConfig -> Text -> TestTree
rejection name config source = H.testCase name $ do
  result <- translateBashScript config "directory.bash" source
  H.assertBool "unsupported operation must reject" (isLeft result)

exact :: EntryMode -> String -> Text -> TestTree
exact mode name source = H.testCaseSteps (show mode <> " " <> name) $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> PathIO.withSystemTempDir "monk-directory" $ \temporary -> do
      let directory = toFilePath temporary
          bashPath = directory </> "input.bash"
          fishPath = directory </> "input.fish"
          config = if mode == Standalone then standalone else sourceable
      createDirectory (directory </> "space child")
      createDirectory (directory </> "physical")
      createDirectoryLink (directory </> "physical") (directory </> "link")
      let sourceText = T.replace "__LINK__" (toText (directory </> "link")) (T.replace "__SPACE__" (toText (directory </> "space child")) source)
      result <- translateBashScript config bashPath sourceText
      translated <- either (\failure -> H.assertFailure (show failure) >> error "unreachable") pure result
      TIO.writeFile bashPath sourceText
      TIO.writeFile fishPath (renderTranslation translated)
      environment <- filter ((/= "CDPATH") . fst) <$> prepareEnv
      let bashArgs = if mode == Standalone then [bashPath] else ["-c", ". \"$1\"", "caller.bash", bashPath]
          fishArgs = if mode == Standalone then [fishPath] else ["-c", "set -g dirstack; function host_rmdir; command rmdir $argv; end; function host_printenv; command printenv $argv; end; source \"$argv[1]\"", fishPath]
      bash <- readCreateProcessWithExitCode ((proc "bash" (["--noprofile", "--norc"] <> bashArgs)) {env = Just environment}) ""
      present <- doesDirectoryExist (directory </> "space child")
      unless present (createDirectory (directory </> "space child"))
      fish <- readCreateProcessWithExitCode ((proc "fish" ("--no-config" : fishArgs)) {env = Just environment}) ""
      H.assertEqual ("stdout/stderr/status for " <> T.unpack source) bash fish

sourceEdges :: TestTree
sourceEdges = H.testCase "relative sources consume success and failure cwd edges" $
  PathIO.withSystemTempDir "monk-directory-source" $ \temporary -> do
    let directory = toFilePath temporary
        target = directory </> "target"
        root = directory </> "root.bash"
        environment = Source.MkSourceEnvironment directory [] False
    createDirectory target
    TIO.writeFile (directory </> "child.bash") "printf original"
    TIO.writeFile (target </> "child.bash") "printf changed"
    forM_
      [ ("cd '" <> toText target <> "' && . ./child.bash", True, target </> "child.bash"),
        ("cd /monk-directory-does-not-exist || . ./child.bash", True, directory </> "child.bash"),
        ("for x in one two; do cd target && . ./child.bash; done", False, ""),
        ("cd '" <> toText target <> "'; . ./child.bash", False, "")
      ]
      $ \(source, admitted, dependency) -> do
        TIO.writeFile root source
        result <- Source.translateSourceGraphWithEnvironment standalone environment True root
        if admitted
          then case result of
            Right graph -> H.assertBool "source uses correct execution cwd" (dependency `elem` Source.sourcePaths graph)
            Left failure -> H.assertFailure (show failure)
          else H.assertBool "unguarded cd cannot assume success" (isLeft result)

callerStackBridge :: TestTree
callerStackBridge = H.testCaseSteps "sourceable entry consumes existing global caller stack" $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> PathIO.withSystemTempDir "monk-directory-caller-stack" $ \temporary -> do
      let directory = toFilePath temporary
          bashPath = directory </> "input.bash"
          fishPath = directory </> "input.fish"
          source = "popd; pwd; popd; pwd"
          caller = fullCaller {callerFunctions = mempty, callerFunctionDirectories = mempty}
      result <- translateBashScript (sourceable {callerContract = caller}) bashPath source
      translated <- either (\failure -> H.assertFailure (show failure) >> error "unreachable") pure result
      TIO.writeFile bashPath source
      TIO.writeFile fishPath (renderTranslation translated)
      environment <- filter ((/= "CDPATH") . fst) <$> prepareEnv
      bash <- readCreateProcessWithExitCode ((proc "bash" ["--noprofile", "--norc", "-c", "pushd /tmp >/dev/null; pushd / >/dev/null; . \"$1\"", "caller.bash", bashPath]) {env = Just environment}) ""
      fish <- readCreateProcessWithExitCode ((proc "fish" ["--no-config", "-c", "set -g dirstack /tmp \"$PWD\"; set -gx OLDPWD /tmp; builtin cd /; source \"$argv[1]\"", fishPath]) {env = Just environment}) ""
      H.assertEqual "caller directory stack output/status" bash fish

permissionsMatrix :: TestTree
permissionsMatrix =
  testGroup
    "independent per-state access matrix"
    [ H.testCase (label <> " " <> field <> " " <> show permission) $ do
        let allAccess = [ReadWriteDirectory, ReadWriteDirectory, ReadWriteDirectory, ReadWriteDirectory]
            selected = take slot allAccess <> [permission] <> drop (slot + 1) allAccess
            permissions = case selected of [cwd, pwd, oldpwd, stack] -> MkDirectoryPermissions cwd pwd oldpwd stack; _ -> error "four slots"
            caller = emptyCallerContract {callerAmbientEffects = NoRelevantAmbientEffects, callerDirectory = Just permissions}
            expected = all (\(neededSlot, needsRead, needsWrite) -> let access = if neededSlot == slot then permission else ReadWriteDirectory in (not needsRead || access `elem` [ReadDirectory, ReadWriteDirectory]) && (not needsWrite || access `elem` [WriteDirectory, ReadWriteDirectory])) requirements
        result <- translateBashScript (strictConfig {entryMode = Sourceable, callerContract = caller}) "permissions.bash" source
        H.assertEqual "operation admission matches independent state accesses" expected (isRight result)
    | (label, source, requirements) <-
        [ ("logical pwd", "pwd", [(0, True, False), (1, True, False)]),
          ("physical pwd", "pwd -P", [(0, True, False)]),
          ("cd", "cd /tmp", [(0, True, True), (1, True, True), (2, False, True)]),
          ("pushd", "pushd /tmp", [(0, True, True), (1, True, True), (2, False, True), (3, True, True)]),
          ("popd", "popd", [(0, True, True), (1, True, True), (2, False, True), (3, True, True)])
        ],
      (slot, field) <- zip [0 ..] ["cwd", "PWD", "OLDPWD", "stack"],
      permission <- [NoDirectoryAccess, ReadDirectory, WriteDirectory, ReadWriteDirectory]
    ]

exportedDirectoryFunction :: TestTree
exportedDirectoryFunction = H.testCaseSteps "exported directory function retains runtime after entry returns" $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> PathIO.withSystemTempDir "monk-directory-export" $ \temporary -> do
      let directory = toFilePath temporary
          bashPath = directory </> "input.bash"
          fishPath = directory </> "input.fish"
          source = "visit() { pushd /tmp; popd; pwd; }; return 7"
          caller = fullCaller {callerFunctions = mempty, callerFunctionDirectories = mempty, callerExportedFunctions = S.singleton "visit"}
      result <- translateBashScript (sourceable {callerContract = caller}) bashPath source
      translated <- either (\failure -> H.assertFailure (show failure) >> error "unreachable") pure result
      TIO.writeFile bashPath source
      TIO.writeFile fishPath (renderTranslation translated)
      environment <- filter ((/= "CDPATH") . fst) <$> prepareEnv
      bash <- readCreateProcessWithExitCode ((proc "bash" ["--noprofile", "--norc", "-c", ". \"$1\"; printf 'entry:%s\\n' \"$?\"; visit; visit", "caller.bash", bashPath]) {env = Just environment}) ""
      fish <- readCreateProcessWithExitCode ((proc "fish" ["--no-config", "-c", "set -g dirstack; source \"$argv[1]\"; builtin printf 'entry:%s\\n' \"$status\"; visit; visit", fishPath]) {env = Just environment}) ""
      H.assertEqual "deferred directory output/status" bash fish

executedSource :: EntryMode -> Bool -> TestTree
executedSource mode separate = H.testCaseSteps (show mode <> if separate then " separate" else " inline") $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> PathIO.withSystemTempDir "monk-directory-executed-source" $ \temporary -> do
      let directory = toFilePath temporary
          target = directory </> "target"
          bashPath = directory </> "root.bash"
          fishPath = directory </> "root.fish"
          caller = importedDirectoryCaller
          config = if mode == Standalone then standalone else sourceable {callerContract = caller}
          environment = Source.MkSourceEnvironment directory [] False
      createDirectory target
      TIO.writeFile (target </> "child.bash") "printf 'child:%s:%s\\n' \"$1\" \"$2\"; shift; printf 'shift:%s\\n' \"$1\"; cd /tmp; return 7; printf unreachable"
      TIO.writeFile bashPath ((if mode == Sourceable then "visit; " else "") <> "cd '" <> toText target <> "' && . ./child.bash alpha beta; printf 'root:%s:%s\\n' \"$?\" \"$#\"; pwd")
      discovered <- Source.translateSourceGraphWithEnvironment config environment True bashPath
      graph <- either (\failure -> H.assertFailure (show failure) >> error "unreachable") pure discovered
      planned <- if separate then Output.planSeparateOutputBundle fishPath graph else Output.planCombinedOutputBundle (Output.OutputPath fishPath) graph
      bundle <- either (\failure -> H.assertFailure (show failure) >> error "unreachable") pure planned
      published <- Output.publishOutputBundle bundle
      either (H.assertFailure . show) (const (pure ())) published
      processEnvironment <- filter ((/= "CDPATH") . fst) <$> prepareEnv
      let bashArgs = if mode == Standalone then [bashPath, "outside"] else ["-c", "visit() { cd /tmp; }; unset OLDPWD; . \"$1\" outside", "caller.bash", bashPath]
          fishArgs = if mode == Standalone then [fishPath, "outside"] else ["-c", "function host_visit; set -l previous \"$PWD\"; builtin cd /tmp; and builtin set -gu OLDPWD \"$previous\"; end; set -g dirstack; source \"$argv[1]\" outside", fishPath]
      bash <- readCreateProcessWithExitCode ((proc "bash" (["--noprofile", "--norc"] <> bashArgs)) {env = Just processEnvironment}) ""
      fish <- readCreateProcessWithExitCode ((proc "fish" ("--no-config" : fishArgs)) {env = Just processEnvironment}) ""
      H.assertEqual "source cwd/argv/return/status behavior" bash fish

importedDirectoryCaller :: CallerContract
importedDirectoryCaller =
  fullCaller
    { callerFunctions = M.singleton "visit" (MkFunctionContract "host_visit" mempty mempty),
      callerFunctionDirectories = M.singleton "visit" (MkDirectoryPermissions ReadWriteDirectory ReadWriteDirectory WriteDirectory NoDirectoryAccess)
    }

importedCwdInvalidatesSource :: TestTree
importedCwdInvalidatesSource = H.testCase "declared imported cwd change invalidates relative source discovery" $ do
  result <- translateBashScript (sourceable {callerContract = importedDirectoryCaller}) "import-cwd.bash" "visit; . ./child.bash"
  case result of
    Left failure -> H.assertBool "reject at cwd resolution, not missing source graph" (any (T.isSuffixOf "source-directory-state" . diagnosticCodeText . diagnosticCode) (failureDiagnostics failure))
    Right _ -> H.assertFailure "unknown imported cwd was treated as the initial execution cwd"

resolvedPathBound :: EntryMode -> TestTree
resolvedPathBound mode = H.testCaseSteps (show mode) $ \step -> do
  readiness <- shouldRunIntegration
  case readiness of
    Left reason -> step ("skipped: " <> reason)
    Right () -> PathIO.withSystemTempDir "monk-directory-bound" $ \temporary -> do
      let directory = toFilePath temporary
          fishPath = directory </> "input.fish"
          caller = fullCaller {callerFunctions = mempty, callerFunctionDirectories = mempty}
          config = if mode == Standalone then standalone else sourceable {callerContract = caller}
          grow path = do
            let suffixLength = if length path >= 3880 then 4090 - length path - 1 else 200
                next = path </> replicate suffixLength 'x'
            createDirectory next
            if length next == 4090 then pure next else grow next
      deep <- grow directory
      result <- translateBashScript config "path-bound.bash" "cd missing-directory; printf unreachable"
      translated <- either (\failure -> H.assertFailure (show failure) >> error "unreachable") pure result
      TIO.writeFile fishPath (renderTranslation translated)
      environment <- filter ((/= "CDPATH") . fst) <$> prepareEnv
      let arguments = if mode == Standalone then [fishPath] else ["-c", "set -g dirstack; source \"$argv[1]\"", fishPath]
      (status, output, errors) <- readCreateProcessWithExitCode ((proc "fish" ("--no-config" : arguments)) {env = Just environment, cwd = Just deep}) ""
      H.assertEqual "contract violation status" (ExitFailure 125) status
      H.assertEqual "no body effect" "" output
      H.assertBool "lexical bound fails before parent cd diagnostic" ("resolved logical directory path exceeds 4095 bytes" `T.isInfixOf` toText errors && not ("cd:" `T.isInfixOf` toText errors))

importedOldpwdInvalidatesPrevious :: TestTree
importedOldpwdInvalidatesPrevious = H.testCase "declared imported OLDPWD write invalidates previous-directory proof" $ do
  let caller = importedDirectoryCaller {callerFunctionDirectories = M.singleton "visit" (MkDirectoryPermissions NoDirectoryAccess NoDirectoryAccess WriteDirectory NoDirectoryAccess)}
  result <- translateBashScript (sourceable {callerContract = caller}) "import-oldpwd.bash" "cd /tmp && { visit; cd -; }"
  case result of
    Left failure -> H.assertBool "unknown imported previous path rejects" (any (T.isSuffixOf "directory-oldpwd" . diagnosticCodeText . diagnosticCode) (failureDiagnostics failure))
    Right _ -> H.assertFailure "imported OLDPWD mutation retained an invalid path proof"
