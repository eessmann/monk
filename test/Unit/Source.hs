module Unit.Source (unitSourceTests) where

import Monk.Source
import Monk.Translation
import SourceTestSupport
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
      testCase "incompatible repeated entry contexts reject" $
        withSources "x=one\n. ./child.bash\nx=two\n. ./child.bash\n" [("child.bash", "printf '%s\\n' \"$x\"\n")] $ \root environment -> do
          result <- translateSourceGraphWithEnvironment strictConfig environment True root
          assertRejected result
    ]

assertRejected :: Either SourceGraphFailure SourceGraph -> Assertion
assertRejected (Left (MkSourceGraphFailure _ failure)) = assertBool "empty diagnostics" (not (null (failureDiagnostics failure)))
assertRejected (Right _) = assertFailure "unsupported graph was admitted"
