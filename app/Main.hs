module Main (main) where

import Monk
import Language.Fish.Translator.Monad (defaultConfig)
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
      let fishAST = translateParseResult defaultConfig parseRes
      putTextLn $ show fishAST

  putTextLn "\nDone!"
