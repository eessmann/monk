module Main (main) where

import Clam (projectName)


main :: IO ()
main = putTextLn ("Tests for " <> projectName)
