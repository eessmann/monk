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
      H.testCase "Translator implementation modules keep raw and lowering imports behind boundaries" $ do
        repoRoot <- makeAbsolute "."
        files <- translatorImplementationFiles repoRoot
        offenders <- fmap concat (traverse (forbiddenTranslatorImports repoRoot) files)
        offenders H.@?= [],
      H.testCase "Translator syntax bridge stays retired" $ do
        repoRoot <- makeAbsolute "."
        files <- translatorImplementationFiles repoRoot
        users <- fmap concat (traverse (moduleImports "Language.Fish.Translator.Syntax") files)
        H.assertBool
          ( "translator syntax bridge import count is above "
              <> show translatorSyntaxImportLimit
              <> ": "
              <> show users
          )
          (length users <= translatorSyntaxImportLimit),
      H.testCase "Translator Args adapter avoids bridge internals" $ do
        repoRoot <- makeAbsolute "."
        offenders <- forbiddenArgsAdapterImports repoRoot
        offenders H.@?= [],
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
        offenders H.@?= [],
      H.testCase "Translator construction boundary export list hides unsafe constructors" $ do
        repoRoot <- makeAbsolute "."
        contents <- decodeUtf8 <$> readFileBS (repoRoot </> "src" </> "Language" </> "Fish" </> "Translator" </> "Construction.hs")
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
  pure ((repoRoot </> "src" </> "Language" </> "Fish" </> "Translator.hs") : subtree)

forbiddenTranslatorImports :: FilePath -> FilePath -> IO [String]
forbiddenTranslatorImports repoRoot path = do
  contents <- decodeUtf8 <$> readFileBS path
  let relativePath = normalise (makeRelative repoRoot path)
  pure
    [ path <> ":" <> show lineNo <> ": " <> toString lineText
    | (lineNo, lineText) <- zip [1 :: Int ..] (T.lines contents),
      any (`isImportUnder` lineText) alwaysForbiddenTranslatorImportRoots
        || ( relativePath `notElem` translatorRawTypeBoundaryAllowlist
               && any (`isImportUnder` lineText) rawAstImportRoots
           )
        || ( relativePath `notElem` translatorConstructionBoundaryAllowlist
               && any (`isImportUnder` lineText) constructionBoundaryOnlyImportRoots
           )
    ]

alwaysForbiddenTranslatorImportRoots :: [Text]
alwaysForbiddenTranslatorImportRoots =
  [ "Language.Fish.Translator.Syntax",
    "Language.Fish.Translator.DSL"
  ]

rawAstImportRoots :: [Text]
rawAstImportRoots =
  [ "Language.Fish.AST",
    "Monk.AST.Raw"
  ]

constructionBoundaryOnlyImportRoots :: [Text]
constructionBoundaryOnlyImportRoots =
  [ "Language.Fish.DSL.Internal",
    "Language.Fish.DSL.Lower"
  ]

translatorConstructionBoundaryAllowlist :: [FilePath]
translatorConstructionBoundaryAllowlist =
  [normalise "src/Language/Fish/Translator/Construction.hs"]

translatorRawTypeBoundaryAllowlist :: [FilePath]
translatorRawTypeBoundaryAllowlist =
  [normalise "src/Language/Fish/Translator/Types.hs"]

translatorSyntaxImportLimit :: Int
translatorSyntaxImportLimit = 0

moduleImports :: Text -> FilePath -> IO [FilePath]
moduleImports moduleName path = do
  contents <- decodeUtf8 <$> readFileBS path
  pure
    [ path
    | lineText <- T.lines contents,
      importedModule lineText == Just moduleName
    ]

forbiddenArgsAdapterImports :: FilePath -> IO [String]
forbiddenArgsAdapterImports repoRoot =
  forbiddenTranslatorImports repoRoot (repoRoot </> "src" </> "Language" </> "Fish" </> "Translator" </> "Args.hs")

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
