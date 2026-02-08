{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, env, nfIO)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Monk (FishStatement (..), defaultConfig, parseBashScript, projectName, renderFish, translateParseResult)

fixtures :: [(String, FilePath)]
fixtures =
  [ ("small", "benchmark/fixtures/small.bash"),
    ("medium", "benchmark/fixtures/medium.bash"),
    ("large", "benchmark/fixtures/large.bash")
  ]

main :: IO ()
main =
  defaultMain
    [ bgroup
        ("translate/" <> T.unpack projectName)
        [ env (TIO.readFile path) $ \src ->
            bench name $
              nfIO (translateFixture path src)
          | (name, path) <- fixtures
        ]
    ]

translateFixture :: FilePath -> Text -> IO Text
translateFixture path src = do
  parseResult <- parseBashScript path src
  case translateParseResult defaultConfig parseResult of
    Left err ->
      let msg = "translateParseResult failed for " <> path <> ": " <> show err
       in error (T.pack msg)
    Right (stmt, _) ->
      pure (renderFish (flattenStatements stmt))

flattenStatements :: FishStatement -> [FishStatement]
flattenStatements = \case
  StmtList xs -> xs
  stmt -> [stmt]
