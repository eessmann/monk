module Main (main) where

import Clam (projectName, prettyFish)
import FishAST
import Prettyprinter

main :: IO ()
main = do
  putTextLn ("Executable for " <> projectName)
  putTextLn "\nDone!"