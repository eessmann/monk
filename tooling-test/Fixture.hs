module Fixture (tests) where

import Control.Exception (IOException, try)
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy qualified as BL
import Monk.Internal.Fixture
import Monk.Internal.Shell (ShellRunMode (..))
import Path.IO qualified as PathIO
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "fixture metadata"
    [ testCase "versioned JSON preserves exact argument boundaries" $
        withSystemTempDirectory "monk-fixture-" $ \directory -> do
          let base = directory </> "example"
              args = ["", "two words", "line\nbreak", "tab\there", "quote'\\"] :: [Text]
          BL.writeFile (base <> ".fixture.json") (encode (object ["version" .= (1 :: Int), "args" .= args, "mode" .= ("exec" :: Text)]))
          path <- PathIO.resolveFile' (base <> ".bash")
          metadata <- loadFixtureMetadata path
          fmArgs metadata @?= args
          fmMode metadata @?= ShellRunExec,
      testCase "legacy mode rejects unknown values" $
        withSystemTempDirectory "monk-fixture-" $ \directory -> do
          let base = directory </> "example"
          writeFile (base <> ".mode") "exce\n"
          path <- PathIO.resolveFile' (base <> ".bash")
          rejected =<< try @IOException (loadFixtureMetadata path),
      testCase "versioned JSON rejects unknown keys and versions" $
        withSystemTempDirectory "monk-fixture-" $ \directory -> do
          let base = directory </> "example"
          path <- PathIO.resolveFile' (base <> ".bash")
          forM_ [object ["version" .= (2 :: Int)], object ["version" .= (1 :: Int), "argz" .= ([] :: [Text])]] $ \value -> do
            BL.writeFile (base <> ".fixture.json") (encode value)
            rejected =<< try @IOException (loadFixtureMetadata path),
      testCase "JSON cannot silently override legacy sidecars" $
        withSystemTempDirectory "monk-fixture-" $ \directory -> do
          let base = directory </> "example"
          path <- PathIO.resolveFile' (base <> ".bash")
          BL.writeFile (base <> ".fixture.json") (encode (object ["version" .= (1 :: Int)]))
          writeFile (base <> ".args") "legacy"
          rejected =<< try @IOException (loadFixtureMetadata path)
    ]
  where
    rejected (Left _) = pure ()
    rejected (Right _) = assertFailure "malformed fixture metadata was accepted"
