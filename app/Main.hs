module Main (main) where

import Monk
import ShellCheck.Interface


main :: IO ()
main = do
  putTextLn ("Executable for " <> projectName)
  putTextLn "Reading script:"
  script <- readFileBS "test.sh"
  putBSLn script
  putTextLn "Parsing script:"
  resultE <- parseBashFile "test.sh"
  case resultE of
    Left errs -> putStrLn ("Parse failed: " <> show errs)
    Right parseRes -> do
      print (prRoot parseRes)            -- The AST root
      print (prComments parseRes)        -- Any warnings/comments
      putTextLn "Translating to Fish:"
      let fishAST = do
            bashAST <- prRoot parseRes
            pure (translateToken bashAST)
      putTextLn $ show fishAST

  putTextLn "\nDone!"