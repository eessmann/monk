module Main (main) where

import Clam (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
