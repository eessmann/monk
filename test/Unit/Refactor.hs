{-# LANGUAGE OverloadedStrings #-}

module Unit.Refactor
  ( unitRefactorTests,
  )
where

import Data.Char (isAlphaNum)
import Data.Text qualified as T
import System.Directory (doesDirectoryExist, listDirectory, makeAbsolute)
import System.FilePath (makeRelative, normalise, takeExtension, (</>))
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
        T.isInfixOf "string upper" out H.@? "expected case-modifier lowering",
      H.testCase "Tracked Haskell sources do not use punning constructors" $ do
        repoRoot <- makeAbsolute "."
        files <- fmap concat (traverse (collectHsFiles . (repoRoot </>)) ["src", "app", "test", "scripts"])
        offenders <- fmap concat (traverse punningConstructors files)
        offenders H.@?= [],
      H.testCase "Translator implementation modules do not import raw AST, DSL internals, or transitional facade" $ do
        repoRoot <- makeAbsolute "."
        files <- translatorImplementationFiles repoRoot
        offenders <- fmap concat (traverse forbiddenTranslatorImports files)
        offenders H.@?= [],
      H.testCase "Translator syntax boundary import footprint does not grow" $ do
        repoRoot <- makeAbsolute "."
        files <- translatorImplementationFiles repoRoot
        users <- fmap concat (traverse syntaxBoundaryImports files)
        H.assertBool
          ( "translator syntax boundary import count grew above "
              <> show translatorSyntaxImportLimit
              <> ": "
              <> show users
          )
          (length users <= translatorSyntaxImportLimit),
      H.testCase "Test support constructs Fish through DSL except explicit raw backend tests" $ do
        repoRoot <- makeAbsolute "."
        testFiles <- collectHsFiles (repoRoot </> "test")
        offenders <- fmap concat (traverse (forbiddenRawTestImports repoRoot) testFiles)
        offenders H.@?= [],
      H.testCase "Public Fish DSL export list hides unsafe constructors and lowerers" $ do
        repoRoot <- makeAbsolute "."
        contents <- decodeUtf8 <$> readFileBS (repoRoot </> "src" </> "Language" </> "Fish" </> "DSL.hs")
        let exports = dslExportList contents
            forbidden = ["Unsafe", "lower"]
            offenders = filter (`T.isInfixOf` exports) forbidden
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

translatorImplementationFiles :: FilePath -> IO [FilePath]
translatorImplementationFiles repoRoot = do
  subtree <- collectHsFiles (repoRoot </> "src" </> "Language" </> "Fish" </> "Translator")
  pure
    ( (repoRoot </> "src" </> "Language" </> "Fish" </> "Translator.hs")
        : filter (not . isTranslatorBoundary) subtree
    )
  where
    isTranslatorBoundary path =
      normalise path
        == normalise (repoRoot </> "src" </> "Language" </> "Fish" </> "Translator" </> "Syntax.hs")

forbiddenTranslatorImports :: FilePath -> IO [String]
forbiddenTranslatorImports path = do
  contents <- decodeUtf8 <$> readFileBS path
  pure
    [ path <> ":" <> show lineNo <> ": " <> toString lineText
    | (lineNo, lineText) <- zip [1 :: Int ..] (T.lines contents),
      any (`isImportUnder` lineText) forbiddenTranslatorImportRoots
    ]

forbiddenTranslatorImportRoots :: [Text]
forbiddenTranslatorImportRoots =
  [ "Language.Fish.AST",
    "Monk.AST.Raw",
    "Language.Fish.DSL.Internal",
    "Language.Fish.DSL.Lower",
    "Language.Fish.Translator.DSL"
  ]

translatorSyntaxImportLimit :: Int
translatorSyntaxImportLimit = 57

syntaxBoundaryImports :: FilePath -> IO [FilePath]
syntaxBoundaryImports path = do
  contents <- decodeUtf8 <$> readFileBS path
  pure
    [ path
    | lineText <- T.lines contents,
      importedModule lineText == Just "Language.Fish.Translator.Syntax"
    ]

forbiddenRawTestImports :: FilePath -> FilePath -> IO [String]
forbiddenRawTestImports repoRoot path
  | makeRelative repoRoot path `elem` rawBackendTestAllowlist = pure []
  | otherwise = do
      contents <- decodeUtf8 <$> readFileBS path
      pure
        [ path <> ":" <> show lineNo <> ": " <> toString lineText
        | (lineNo, lineText) <- zip [1 :: Int ..] (T.lines contents),
          any (`isImportUnder` lineText) forbiddenTestImportRoots
        ]

rawBackendTestAllowlist :: [FilePath]
rawBackendTestAllowlist = []

forbiddenTestImportRoots :: [Text]
forbiddenTestImportRoots =
  [ "Language.Fish.AST",
    "Monk.AST.Raw"
  ]

isImportUnder :: Text -> Text -> Bool
isImportUnder moduleRoot rawLine =
  case importedModule rawLine of
    Just imported ->
      imported == moduleRoot || (moduleRoot <> ".") `T.isPrefixOf` imported
    Nothing -> False

importedModule :: Text -> Maybe Text
importedModule rawLine =
  case T.words (T.strip rawLine) of
    "import" : "qualified" : imported : _ -> Just imported
    "import" : imported : _ -> Just imported
    _ -> Nothing

dslExportList :: Text -> Text
dslExportList contents =
  case T.breakOn "\nwhere" contents of
    (header, _) -> header
