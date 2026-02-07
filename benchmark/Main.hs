{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_, replicateM_)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Monk (defaultConfig, parseBashScript, projectName, translateParseResult)
import System.CPUTime (getCPUTime)

main :: IO ()
main = do
  let banner = "Benchmarks for " <> projectName
      fixtures =
        [ ("small", "benchmark/fixtures/small.bash", 50),
          ("medium", "benchmark/fixtures/medium.bash", 20),
          ("large", "benchmark/fixtures/large.bash", 5)
        ]
  putTextLn banner
  forM_ fixtures $ \(name, path, iters) -> do
    src <- TIO.readFile path
    start <- getCPUTime
    replicateM_ iters $ do
      parseResult <- parseBashScript path src
      case translateParseResult defaultConfig parseResult of
        Left err -> error ("translateParseResult failed: " <> show err)
        Right _ -> pure ()
    end <- getCPUTime
    let diffMs :: Double
        diffMs = fromIntegral (end - start) / 1e9
        perIter = diffMs / fromIntegral iters
        line =
          T.intercalate
            " "
            [ name <> ":",
              T.pack (show iters) <> " iters,",
              T.pack (show (round diffMs :: Int)) <> " ms total,",
              T.pack (show (realToFrac (fromIntegral (round (perIter * 100)) / 100 :: Double) :: Double)) <> " ms/iter"
            ]
    putTextLn line
