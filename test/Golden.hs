{-# LANGUAGE OverloadedStrings #-}

module Golden
  ( goldenTests
  ) where

import Monk
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

goldenTests :: TestTree
goldenTests =
  testGroup "Golden translations" (map goldenTest goldenFixtures)

data GoldenFixture = GoldenFixture
  { gfName :: String
  , gfBashPath :: FilePath
  , gfFishPath :: FilePath
  }

goldenFixtures :: [GoldenFixture]
goldenFixtures =
  [ GoldenFixture "echo-exit" "test/fixtures/golden/echo-exit.bash" "test/fixtures/golden/echo-exit.fish"
  , GoldenFixture "echo-echo" "test/fixtures/golden/echo-echo.bash" "test/fixtures/golden/echo-echo.fish"
  , GoldenFixture "assignments" "test/fixtures/golden/assignments.bash" "test/fixtures/golden/assignments.fish"
  , GoldenFixture "read-prompt" "test/fixtures/golden/read-prompt.bash" "test/fixtures/golden/read-prompt.fish"
  , GoldenFixture "double-bracket-eq" "test/fixtures/golden/double-bracket-eq.bash" "test/fixtures/golden/double-bracket-eq.fish"
  , GoldenFixture "glob-basic" "test/fixtures/golden/glob-basic.bash" "test/fixtures/golden/glob-basic.fish"
  , GoldenFixture "extglob-basic" "test/fixtures/golden/extglob-basic.bash" "test/fixtures/golden/extglob-basic.fish"
  ]

goldenTest :: GoldenFixture -> TestTree
goldenTest GoldenFixture { gfName, gfBashPath, gfFishPath } =
  H.testCase gfName $ do
    bashSrc <- TIO.readFile gfBashPath
    expected <- TIO.readFile gfFishPath
    result <- translateScriptText gfBashPath bashSrc
    case result of
      Left err -> H.assertFailure err
      Right actual -> normalize actual @?= normalize expected

translateScriptText :: FilePath -> Text -> IO (Either String Text)
translateScriptText path script = do
  parseResult <- parseBashScript path script
  case translateParseResult defaultConfig parseResult of
    Left err -> pure (Left ("translateParseResult failed: " <> show err))
    Right (stmt, _) -> pure (Right (renderFish [stmt]))

normalize :: Text -> Text
normalize = T.stripEnd . T.replace "\r\n" "\n"
