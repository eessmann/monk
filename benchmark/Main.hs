{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, env, nfIO)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Monk.Translation (projectName, renderTranslation, strictConfig, translateBashScript)

fixtures :: [(String, FilePath, Bool)]
fixtures =
  [ ("exact/small", "benchmark/fixtures/small.bash", True),
    ("exact/medium", "benchmark/fixtures/medium-exact.bash", True),
    ("exact/large", "benchmark/fixtures/large-exact.bash", True),
    ("rejection/legacy-medium", "benchmark/fixtures/medium.bash", False),
    ("rejection/legacy-large", "benchmark/fixtures/large.bash", False)
  ]

main :: IO ()
main =
  defaultMain
    [ bgroup
        ("translate/" <> T.unpack projectName)
        [ env (TIO.readFile path) $ \src ->
            bench name $
              nfIO (translateFixture expectExact path src)
        | (name, path, expectExact) <- fixtures
        ]
    ]

translateFixture :: Bool -> FilePath -> Text -> IO Text
translateFixture expectExact path src = do
  result <- translateBashScript strictConfig path src
  case result of
    Left failure
      | not expectExact -> pure (show failure)
      | otherwise -> error ("Mandatory benchmark admission failed: " <> show failure)
    Right translation
      | expectExact -> pure (renderTranslation translation)
      | otherwise -> error "Excluded legacy benchmark unexpectedly produced executable output"
