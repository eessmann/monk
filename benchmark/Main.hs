module Main (main) where

import Monk (projectName)


main :: IO ()
main = putStrLn ("Benchmarks for " ++ projectName)
