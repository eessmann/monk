{-# LANGUAGE OverloadedStrings #-}

module Unit.Refactor
  ( unitRefactorTests,
  )
where

import Data.Char (isAlphaNum)
import Data.Text qualified as T
import System.Directory (doesDirectoryExist, listDirectory, makeAbsolute)
import System.FilePath ((</>), takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H
import TestSupport (translateScript)

unitRefactorTests :: TestTree
unitRefactorTests =
  testGroup
    "Refactor Seams"
    [ H.testCase "Null-delimited array reads still lower through the exact helper path" $ do
        out <- translateScript "read -d '' -a items"
        T.isInfixOf "__monk_read_capture_delim" out H.@? "expected capture helper"
        T.isInfixOf "__monk_read_assign" out H.@? "expected array assignment helper",
      H.testCase "Delimited multi-var reads keep the generalized assignment helper path" $ do
        out <- translateScript "IFS=: read -u 3 -d : left right"
        T.isInfixOf "__monk_read_capture_delim" out H.@? "expected capture helper"
        T.isInfixOf "__monk_read_assign" out H.@? "expected multi-var assignment helper",
      H.testCase "Default assignment expansions still lower through conditional set logic" $ do
        out <- translateScript "echo ${name=value}"
        T.isInfixOf "set '-q' 'name'" out H.@? "expected set-check guard"
        T.isInfixOf "set --global name" out H.@? "expected assignment branch",
      H.testCase "Case-modification expansions still lower through string helpers" $ do
        out <- translateScript "echo ${name^^}"
        T.isInfixOf "string upper" out H.@? "expected case-modifier lowering"
    ,
      H.testCase "Tracked Haskell sources do not use punning constructors" $ do
        repoRoot <- makeAbsolute "."
        files <- fmap concat (traverse (collectHsFiles . (repoRoot </>)) ["src", "app", "test", "scripts"])
        offenders <- fmap concat (traverse punningConstructors files)
        offenders H.@?= []
    ]

collectHsFiles :: FilePath -> IO [FilePath]
collectHsFiles dir = do
  entries <- sort <$> listDirectory dir
  fmap concat (traverse go entries)
  where
    go name = do
      let path = dir </> name
      isDir <- doesDirectoryExist path
      if isDir
        then collectHsFiles path
        else pure [path | takeExtension path == ".hs"]

punningConstructors :: FilePath -> IO [String]
punningConstructors path = do
  contents <- decodeUtf8 <$> readFileBS path
  pure
    [ path <> ":" <> show lineNo <> ": " <> toString typeName
    | (lineNo, lineText) <- zip [1 :: Int ..] (T.lines contents),
      Just typeName <- [punningTypeName lineText]
    ]

punningTypeName :: Text -> Maybe Text
punningTypeName rawLine = do
  (keyword, typeName, rest) <- declarationParts (stripComment rawLine)
  guard (keyword == "data" || keyword == "newtype")
  constructorName <- listToMaybe rest
  let normalizedType = trimIdent typeName
      normalizedCtor = trimIdent constructorName
  guard (not (T.null normalizedType))
  guard (normalizedType == normalizedCtor)
  pure normalizedType

declarationParts :: Text -> Maybe (Text, Text, [Text])
declarationParts line =
  case T.words line of
    keyword : typeName : "=" : rest -> Just (keyword, typeName, rest)
    _ -> Nothing

stripComment :: Text -> Text
stripComment = fst . T.breakOn "--"

trimIdent :: Text -> Text
trimIdent = T.takeWhile isIdentChar

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''
