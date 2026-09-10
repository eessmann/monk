module Unit.Refactor (unitRefactorTests) where

import Data.Text qualified as T
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

unitRefactorTests :: TestTree
unitRefactorTests =
  testGroup
    "Architecture boundaries"
    [ H.testCase "superseded semantic entry and rewrite walkers are retired" $ do
        forM_ ["src/Language/Fish/Translator.hs", "src/Language/Fish/Inline.hs", "src/Language/Fish/Translator/Hoist.hs", "src/Language/Fish/Translator/Commands/CommandTokens.hs"] $ \path -> do
          present <- doesFileExist path
          H.assertBool ("superseded semantic owner remains: " <> path) (not present),
      H.testCase "materialization does not lower raw AST or walk ShellCheck tokens" $ do
        files <- collectHsFiles "src/Language/Fish/Translator"
        forM_ files $ \path -> do
          contents <- source path
          let imports = mapMaybe importedModule (T.lines contents)
          H.assertEqual
            ("forbidden materializer import: " <> path)
            []
            (filter (\name -> any (`underModule` name) ["Language.Fish.AST", "Language.Fish.DSL.Lower", "ShellCheck.AST"]) imports),
      H.testCase "semantic plan owns meaning without executable parser nodes" $ do
        contents <- source "src/Language/Bash/Plan.hs"
        H.assertBool "raw parser token import in semantic plan" (not ("import ShellCheck" `T.isInfixOf` contents))
        H.assertBool "raw executable token field in semantic plan" (not (":: Token" `T.isInfixOf` contents)),
      H.testCase "source discovery uses the authoritative normalization continuation" $ do
        contents <- source "src/Monk/Source.hs"
        H.assertBool "missing source normalization owner" ("beginNormalization" `T.isInfixOf` contents)
        H.assertBool "second source AST walker" (not ("import ShellCheck.AST" `T.isInfixOf` contents))
        H.assertBool "source rewrite after admission" (not ("rewriteSources" `T.isInfixOf` contents)),
      H.testCase "public Fish DSL hides raw constructors and lowering" $ do
        contents <- source "src/Language/Fish/DSL.hs"
        let exports = fst (T.breakOn "\nwhere" contents)
        H.assertEqual "unsafe public DSL export" [] (filter (`T.isInfixOf` exports) ["Unsafe", "lower"]),
      H.testCase "structural DSL nodes do not embed the private renderer AST" $ do
        contents <- source "src/Language/Fish/DSL/Internal.hs"
        H.assertEqual
          "raw AST embedded in structural DSL"
          []
          (filter (`T.isInfixOf` contents) ["import Language.Fish.AST", "Raw.Fish", "UnsafeExpr", "UnsafeCommand", "UnsafeStmt"]),
      H.testCase "private plan and publication modules remain private library modules" $ do
        contents <- source "monk.cabal"
        let public = fst (T.breakOn "  build-depends:" (snd (T.breakOn "\nlibrary\n" contents)))
        H.assertEqual
          "private implementation exposed"
          []
          (filter (`T.isInfixOf` public) ["Language.Bash.Plan", "Language.Fish.DSL.Internal", "Language.Fish.DSL.Lower", "Monk.Output.Publication", "Monk.Source.Product"]),
      H.testCase "CLI publishes only a planned output product" $ do
        contents <- source "app/Main.hs"
        H.assertBool "CLI bypassed combined planning" ("planCombinedOutputBundle" `T.isInfixOf` contents)
        H.assertBool "CLI bypassed publication" ("publishOutputBundle" `T.isInfixOf` contents)
        H.assertBool "legacy source rewrite path returned" (not ("inlineSourceGraph" `T.isInfixOf` contents))
    ]

source :: FilePath -> IO Text
source path = decodeUtf8 <$> readFileBS path

collectHsFiles :: FilePath -> IO [FilePath]
collectHsFiles directory = do
  names <- listDirectory directory
  concat <$> traverse child names
  where
    child name = do
      let path = directory </> name
      nested <- doesDirectoryExist path
      if nested then collectHsFiles path else pure [path | takeExtension path == ".hs"]

importedModule :: Text -> Maybe Text
importedModule line = case T.words line of
  "import" : "qualified" : name : _ -> Just name
  "import" : name : _ -> Just name
  _ -> Nothing

underModule :: Text -> Text -> Bool
underModule root name = name == root || (root <> ".") `T.isPrefixOf` name
