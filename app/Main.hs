module Main (main) where

import Clam (projectName)


main :: IO ()
main = putStrLn ("Executable for " ++ projectName)
