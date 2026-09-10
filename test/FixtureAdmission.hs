module FixtureAdmission
  ( FixtureAdmission (..),
    loadFixtureAdmission,
    fixtureAdmissionTests,
  )
where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (normalise, takeExtension, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

data FixtureAdmission = ExactFixture | RejectedFixture Text Text
  deriving stock (Eq, Show)

loadFixtureAdmission :: FilePath -> IO FixtureAdmission
loadFixtureAdmission path = do
  policies <- loadPolicies
  case M.lookup (normalise path) policies of
    Just policy -> pure policy
    Nothing -> H.assertFailure ("fixture has no reviewed admission policy: " <> path) >> pure ExactFixture

fixtureAdmissionTests :: TestTree
fixtureAdmissionTests =
  testGroup
    "Fixture policy inventory"
    [ H.testCase "every Bash fixture has exactly one reviewed policy" $ do
        policies <- loadPolicies
        paths <- sort <$> bashFiles "test/fixtures"
        H.assertEqual "missing or obsolete fixture policies" paths (M.keys policies)
    ]

loadPolicies :: IO (M.Map FilePath FixtureAdmission)
loadPolicies = do
  contents <- decodeUtf8 <$> readFileBS "test/fixtures/admission.tsv"
  rows <- traverse parseRow (drop 1 (T.lines contents))
  let policies = M.fromList rows
  H.assertEqual "duplicate fixture policy" (length rows) (M.size policies)
  pure policies
  where
    parseRow row = case T.splitOn "\t" row of
      [path, "exact", "-", rationale] | not (T.null rationale) -> pure (normalise (toString path), ExactFixture)
      [path, "reject", prefix, rationale]
        | T.isPrefixOf "monk." prefix && not (T.null rationale) ->
            pure (normalise (toString path), RejectedFixture prefix rationale)
      _ -> H.assertFailure ("invalid reviewed fixture policy: " <> toString row) >> pure ("", ExactFixture)

bashFiles :: FilePath -> IO [FilePath]
bashFiles directory = do
  names <- listDirectory directory
  concat <$> traverse child names
  where
    child name = do
      let path = directory </> name
      nested <- doesDirectoryExist path
      if nested then bashFiles path else pure [normalise path | takeExtension path == ".bash"]
