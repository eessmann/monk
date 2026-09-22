module Unit.Source (unitSourceTests) where

import Monk.Source
import Monk.Translation
import SourceTestSupport
import System.Directory (createFileLink)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

unitSourceTests :: TestTree
unitSourceTests =
  testGroup
    "Owned source graphs"
    [ testCase "literal dependency identity and source occurrence are retained" $
        withSources ". ./child.bash\nprintf 'root\\n'\n" [("child.bash", "printf 'child\\n'\n")] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          sourcePaths graph @?= [root, sourceWorkingDirectory environment </> "child.bash"]
          length (sourceDependencies graph) @?= 2
          map sourceOccurrenceId (sourceOccurrences graph) @?= [0]
          assertBool "source location missing" (all (isJust . sourceOccurrenceRange) (sourceOccurrences graph))
          assertSourceEquivalent root environment graph [],
      testCase "nested occurrences identify their immediate parent" $
        withSources ". ./child.bash\n" [("child.bash", ". ./grandchild.bash\n"), ("grandchild.bash", "true\n")] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          map sourceOccurrenceParent (sourceOccurrences graph) @?= [root, sourceWorkingDirectory environment </> "child.bash"],
      testCase "repeated dependency executes at each occurrence" $
        withSources ". ./child.bash\n. ./child.bash\n" [("child.bash", "printf 'again\\n'\n")] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          length (sourceDependencies graph) @?= 2
          map sourceOccurrenceId (sourceOccurrences graph) @?= [0, 1]
          assertSourceEquivalent root environment graph [],
      testCase "a sourced definition affects subsequent dispatch" $
        withSources ". ./child.bash\nvisit\n" [("child.bash", "visit() { printf 'visited\\n'; }\n")] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          assertSourceEquivalent root environment graph [],
      testCase "source ERR callback restores invocation redirection after inner callbacks" $
        withSources "trap 'printf \"err:%s\\n\" \"$?\"' ERR; . ./child.bash >log; printf 'parent\\n'; cat log" [("child.bash", "false")] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          assertSourceEquivalent root environment graph [],
      testCase "each source alias retains its runtime diagnostic spelling" $
        withSources ". ./child.bash; . ././child.bash" [("child.bash", "./missing")] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          length (sourceDependencies graph) @?= 2
          assertSourceEquivalent root environment graph [],
      testCase "sourced function diagnostics retain definition occurrence spelling" $
        withSources ". ./child.bash; visit" [("child.bash", "visit() { ./missing; }")] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          assertSourceEquivalent root environment graph [],
      testCase "sourced arithmetic diagnostics retain source operand spelling" $
        withSources ". ./child.bash" [("child.bash", "((1/0))")] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          assertSourceEquivalent root environment graph [],
      testCase "redefined function keeps each alias origin without changing dependency identity" $
        withSources ". ./child.bash; visit; . sub/../child.bash; visit" [("child.bash", "visit() { ./missing; }"), ("sub/unused", "")] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          length (sourceDependencies graph) @?= 2
          assertSourceEquivalent root environment graph [],
      testGroup
        "PATH source diagnostic spelling"
        [ testCase (show search) $
            withSources ". child.bash" [("child.bash", "./missing"), ("sub/child.bash", "./missing")] $ \root environment -> do
              let selected = environment {sourceSearchPath = [search]}
              graph <- requireGraph strictConfig selected root
              assertSourceEquivalent root selected graph []
        | search <- ["sub", "./sub", "", "absent"]
        ],
      testCase "symlink source diagnostic spelling does not replace canonical identity" $
        withSources ". ./link.bash" [("child.bash", "./missing")] $ \root environment -> do
          let directory = sourceWorkingDirectory environment
          createFileLink (directory </> "child.bash") (directory </> "link.bash")
          graph <- requireGraph strictConfig environment root
          sourcePaths graph @?= [root, directory </> "child.bash"]
          assertSourceEquivalent root environment graph [],
      testCase "root symlink spelling remains the runtime diagnostic origin" $
        withSources "./missing" [] $ \root environment -> do
          let alias = sourceWorkingDirectory environment </> "alias.bash"
          createFileLink root alias
          graph <- requireGraph strictConfig environment alias
          sourceRoot graph @?= root
          assertSourceEquivalent alias environment graph [],
      testCase "source cycles reject without an output product" $
        withSources ". ./child.bash\n" [("child.bash", ". ./root.bash\n")] $ \root environment -> do
          result <- translateSourceGraphWithEnvironment strictConfig environment True root
          assertRejected result,
      testCase "computed sources reject" $
        withSources "dependency=./child.bash\n. \"$dependency\"\n" [("child.bash", "true\n")] $ \root environment -> do
          result <- translateSourceGraphWithEnvironment strictConfig environment True root
          assertRejected result,
      testCase "missing dependencies reject rather than retaining Bash source spelling" $
        withSources ". ./missing.bash\n" [] $ \root environment -> do
          result <- translateSourceGraphWithEnvironment strictConfig environment True root
          assertRejected result,
      testCase "repeated entry contexts normalize independently" $
        withSources "x=one\n. ./child.bash\nx=two\n. ./child.bash\n" [("child.bash", "printf '%s\\n' \"$x\"\n")] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          length (sourceDependencies graph) @?= 2
          assertSourceEquivalent root environment graph [],
      testCase "literal source in child keeps scalar writes isolated" $
        withSources "x=parent; (. ./child.bash); printf '%s' \"$x\"" [("child.bash", "x=child; printf '%s:' \"$x\"")] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          assertSourceEquivalent root environment graph [],
      testCase "literal source in substitution keeps scalar writes isolated" $
        withSources "x=parent; value=$(. ./child.bash); printf '%s:%s' \"$value\" \"$x\"" [("child.bash", "x=child; printf '%s' \"$x\"")] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          assertSourceEquivalent root environment graph [],
      testCase "absolute source in function updates caller local and returns only from source" $
        withSources "" [("child.bash", "x=changed; return 7; x=bad")] $ \root environment -> do
          let child = toText (sourceWorkingDirectory environment </> "child.bash")
          writeFileText root ("x=global; f() { local x=local; . '" <> child <> "'; printf '%s:%s:' \"$?\" \"$x\"; }; f; printf '%s' \"$x\"")
          graph <- requireGraph strictConfig environment root
          assertSourceEquivalent root environment graph [],
      testCase "function source cannot assume definition-time relative cwd" $
        withSources "f() { . ./child.bash; }; f" [("child.bash", "true")] $ \root environment -> do
          result <- translateSourceGraphWithEnvironment strictConfig environment True root
          assertRejected result,
      testCase "sourced local declaration cannot acquire wrapper-local scope" $
        withSources "" [("child.bash", "local x=bad")] $ \root environment -> do
          let child = toText (sourceWorkingDirectory environment </> "child.bash")
          writeFileText root ("f() { . '" <> child <> "'; }; f")
          result <- translateSourceGraphWithEnvironment strictConfig environment True root
          assertRejected result
    ]

assertRejected :: Either SourceGraphFailure SourceGraph -> Assertion
assertRejected (Left (MkSourceGraphFailure _ failure)) = assertBool "empty diagnostics" (not (null (failureDiagnostics failure)))
assertRejected (Right _) = assertFailure "unsupported graph was admitted"
