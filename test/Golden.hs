{-# LANGUAGE OverloadedStrings #-}

module Golden
  ( goldenTests,
  )
where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import FixtureAdmission
import Monk.Translation
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

goldenTests :: TestTree
goldenTests =
  testGroup "Golden translations" (map goldenTest goldenFixtures)

data GoldenFixture = MkGoldenFixture
  { gfName :: String,
    gfBashPath :: FilePath,
    gfFishPath :: FilePath
  }

goldenFixtures :: [GoldenFixture]
goldenFixtures =
  [ MkGoldenFixture "echo-exit" "test/fixtures/golden/echo-exit.bash" "test/fixtures/golden/echo-exit.fish",
    MkGoldenFixture "echo-echo" "test/fixtures/golden/echo-echo.bash" "test/fixtures/golden/echo-echo.fish",
    MkGoldenFixture "pipeline" "test/fixtures/golden/pipeline.bash" "test/fixtures/golden/pipeline.fish",
    MkGoldenFixture "assignments" "test/fixtures/golden/assignments.bash" "test/fixtures/golden/assignments.fish",
    MkGoldenFixture "read-prompt" "test/fixtures/golden/read-prompt.bash" "test/fixtures/golden/read-prompt.fish",
    MkGoldenFixture "double-bracket-eq" "test/fixtures/golden/double-bracket-eq.bash" "test/fixtures/golden/double-bracket-eq.fish",
    MkGoldenFixture "glob-basic" "test/fixtures/golden/glob-basic.bash" "test/fixtures/golden/glob-basic.fish",
    MkGoldenFixture "extglob-basic" "test/fixtures/golden/extglob-basic.bash" "test/fixtures/golden/extglob-basic.fish",
    MkGoldenFixture
      "case-pattern-expansion-glob"
      "test/fixtures/golden/case-pattern-expansion-glob.bash"
      "test/fixtures/golden/case-pattern-expansion-glob.fish"
  ]

goldenTest :: GoldenFixture -> TestTree
goldenTest MkGoldenFixture {gfName, gfBashPath, gfFishPath} =
  H.testCase gfName $ do
    bashSrc <- TIO.readFile gfBashPath
    policy <- loadFixtureAdmission gfBashPath
    result <- translateBashScript strictConfig gfBashPath bashSrc
    case (policy, result) of
      (RejectedFixture prefix rationale, Left failure) ->
        H.assertBool
          (toString rationale)
          (any (T.isPrefixOf prefix . diagnosticCodeText . diagnosticCode) (failureDiagnostics failure))
      (RejectedFixture _ rationale, Right _) -> H.assertFailure ("excluded golden fixture emitted executable output: " <> toString rationale)
      (ExactFixture, Left failure) -> H.assertFailure (show failure)
      (ExactFixture, Right translated) -> do
        expected <- TIO.readFile gfFishPath
        normalize (renderTranslation translated) @?= normalize expected

normalize :: Text -> Text
normalize = T.stripEnd . T.replace "\r\n" "\n"
