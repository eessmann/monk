module Unit.Inline (unitInlineTests) where

import Monk.Translation (strictConfig)
import SourceTestSupport
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

unitInlineTests :: TestTree
unitInlineTests =
  testGroup
    "Owned source execution"
    [ testCase "source return exits only its body and preserves final status" $
        withSources "false\n. ./child.bash\nprintf 'result=%s\\n' \"$?\"\n" [("child.bash", "printf 'incoming=%s\\n' \"$?\"\nreturn 7\nprintf 'unreachable\\n'\n")] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          assertSourceEquivalent root environment graph [],
      testCase "source without arguments inherits positional parameters" $
        withSources ". ./child.bash\nprintf 'after=<%s>\\n' \"$@\"\n" [("child.bash", "printf 'child=<%s>\\n' \"$@\"\n")] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          assertSourceEquivalent root environment graph ["one", "two words", ""],
      testCase "explicit source arguments do not replace caller arguments" $
        withSources ". ./child.bash inner 'two words'\nprintf 'after=<%s>\\n' \"$@\"\n" [("child.bash", "printf 'child=<%s>\\n' \"$@\"\nreturn 3\n")] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          assertSourceEquivalent root environment graph ["outer"],
      testCase "empty source returns success" $
        withSources "false\n. ./empty.bash\nprintf '%s\\n' \"$?\"\n" [("empty.bash", "")] $ \root environment -> do
          graph <- requireGraph strictConfig environment root
          assertSourceEquivalent root environment graph []
    ]
