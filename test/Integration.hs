{-# LANGUAGE OverloadedStrings #-}

module Integration
  ( integrationTests,
  )
where

import Control.Monad (foldM)
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import FixtureSupport
  ( loadFixtureArgs,
    loadFixtureMode,
    loadFixturePlatforms,
    loadFixturePrereqs,
    loadFixtureRecursive,
    loadFixtureStdin,
  )
import Monk
  ( Translation (..),
    defaultConfig,
    inlineStatements,
    parseBashFile,
    parseBashScript,
    renderFish,
    renderTranslation,
    translateParseResult,
    translationState,
    translationStatements,
  )
import ShellSupport
  ( RunResult (..),
    Shell (..),
    diffEnv,
    prepareEnv,
    runShell,
    runShellWithMode,
    shouldRunIntegration,
  )
import ShellCheck.AST
import ShellCheck.Interface (prRoot)
import ShellCheck.ASTLib (getLiteralStringDef)
import System.Directory (canonicalizePath, doesFileExist, findExecutable)
import System.FilePath ((</>), isRelative, takeDirectory)
import System.Info qualified as SysInfo
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as H

integrationTests :: TestTree
integrationTests =
  testGroup "Integration (bash vs fish)" (map integrationTest integrationFixtures)

integrationFixtures :: [IntegrationFixture]
integrationFixtures =
  [ IntegrationFixture "pwd-cd" "test/fixtures/integration/pwd-cd.bash",
    IntegrationFixture "stdout-stderr-exit" "test/fixtures/integration/stdout-stderr-exit.bash",
    IntegrationFixture "cd-tmp" "test/fixtures/integration/cd-tmp.bash",
    IntegrationFixture "pushd-popd" "test/fixtures/integration/pushd-popd.bash",
    IntegrationFixture "errexit-basic" "test/fixtures/integration/errexit-basic.bash",
    IntegrationFixture "errexit-andor" "test/fixtures/integration/errexit-andor.bash",
    IntegrationFixture "errexit-conditionals" "test/fixtures/integration/errexit-conditionals.bash",
    IntegrationFixture "pipefail-basic" "test/fixtures/integration/pipefail-basic.bash",
    IntegrationFixture "pipefail-toggle" "test/fixtures/integration/pipefail-toggle.bash",
    IntegrationFixture "read-flags" "test/fixtures/integration/read-flags.bash",
    IntegrationFixture "here-string-basic" "test/fixtures/integration/here-string-basic.bash",
    IntegrationFixture "param-expansion-args" "test/fixtures/integration/param-expansion-args.bash",
    IntegrationFixture "param-expansion-redirection" "test/fixtures/integration/param-expansion-redirection.bash",
    IntegrationFixture "param-expansion-case" "test/fixtures/integration/param-expansion-case.bash",
    IntegrationFixture "procsub-input" "test/fixtures/integration/procsub-input.bash",
    IntegrationFixture "procsub-output" "test/fixtures/integration/procsub-output.bash",
    IntegrationFixture "source-recursive" "test/fixtures/integration/source-recursive.bash",
    IntegrationFixture "trap-exit" "test/fixtures/integration/trap-exit.bash",
    IntegrationFixture "trap-exit-expansion" "test/fixtures/integration/trap-exit-expansion.bash",
    IntegrationFixture "arith-short-circuit" "test/fixtures/integration/arith-short-circuit.bash",
    IntegrationFixture "time-prefix" "test/fixtures/integration/time-prefix.bash",
    IntegrationFixture "corpus/simple-echo" "test/fixtures/corpus/simple-echo.bash",
    IntegrationFixture "corpus/if-then" "test/fixtures/corpus/if-then.bash",
    IntegrationFixture "realworld/hello-world" "test/fixtures/realworld/hello-world.bash",
    IntegrationFixture "realworld/echo-args" "test/fixtures/realworld/echo-args.bash",
    IntegrationFixture "realworld/a2l" "test/fixtures/realworld/a2l.bash",
    IntegrationFixture "realworld/coat" "test/fixtures/realworld/coat.bash",
    IntegrationFixture "realworld/taoc" "test/fixtures/realworld/taoc.bash",
    IntegrationFixture "realworld/pyramid-right" "test/fixtures/realworld/pyramid-right.bash",
    IntegrationFixture "realworld/pyramid-left" "test/fixtures/realworld/pyramid-left.bash",
    IntegrationFixture "realworld/version-compare" "test/fixtures/realworld/version-compare.bash"
  ]

data IntegrationFixture = IntegrationFixture
  { ifName :: String,
    ifPath :: FilePath
  }

integrationTest :: IntegrationFixture -> TestTree
integrationTest IntegrationFixture {ifName, ifPath} = H.testCase ifName $ do
  runnable <- shouldRunIntegration
  case runnable of
    Left _reason -> pure ()
    Right () -> do
      supportedPlatform <- fixtureSupportedOnCurrentPlatform ifPath
      prereqsMet <- fixturePrereqsAvailable ifPath
      when (supportedPlatform && prereqsMet) $ do
        bashSrc <- TIO.readFile ifPath
        translation <- translateScriptText ifPath bashSrc
        args <- loadFixtureArgs ifPath
        runMode <- loadFixtureMode ifPath
        stdinInput <- loadFixtureStdin ifPath
        case translation of
          Left err -> H.assertFailure err
          Right fishSrc -> do
            baseEnv <- prepareEnv
            baseBash <- runShell ShellBash baseEnv ""
            baseFish <- runShell ShellFish baseEnv ""
            bashRes <- runShellWithMode runMode ShellBash baseEnv bashSrc args stdinInput
            fishRes <- runShellWithMode runMode ShellFish baseEnv fishSrc args stdinInput
            let bashDelta = diffEnv (rrEnv baseBash) (rrEnv bashRes)
                fishDelta = diffEnv (rrEnv baseFish) (rrEnv fishRes)
            rrExit bashRes @?= rrExit fishRes
            rrStdout bashRes @?= rrStdout fishRes
            rrStderr bashRes @?= rrStderr fishRes
            bashDelta @?= fishDelta

fixturePrereqsAvailable :: FilePath -> IO Bool
fixturePrereqsAvailable path = do
  prereqs <- loadFixturePrereqs path
  and <$> mapM (fmap isJust . findExecutable) prereqs

fixtureSupportedOnCurrentPlatform :: FilePath -> IO Bool
fixtureSupportedOnCurrentPlatform path = do
  mPlatforms <- loadFixturePlatforms path
  pure $
    case mPlatforms of
      Nothing -> True
      Just platforms -> toText SysInfo.os `elem` platforms

translateScriptText :: FilePath -> Text -> IO (Either String Text)
translateScriptText path script = do
  recursive <- loadFixtureRecursive path
  if recursive
    then translateScriptTextRecursive path
    else do
      parseResult <- parseBashScript path script
      case translateParseResult defaultConfig parseResult of
        Left err -> pure (Left ("translateParseResult failed: " <> show err))
        Right translation -> pure (Right (renderTranslation translation))

translateScriptTextRecursive :: FilePath -> IO (Either String Text)
translateScriptTextRecursive path = do
  rootPath <- canonicalizePath path
  translationsOrErr <- collectTranslations M.empty rootPath
  case translationsOrErr of
    Left err -> pure (Left err)
    Right translations -> do
      stmts <- inlineStatements (\_ -> pure ()) translations Set.empty rootPath
      pure (Right (renderFish stmts))

collectTranslations ::
  M.Map FilePath Translation ->
  FilePath ->
  IO (Either String (M.Map FilePath Translation))
collectTranslations acc path
  | M.member path acc = pure (Right acc)
  | otherwise = do
      parseResultE <- parseBashFile path
      case parseResultE of
        Left errs -> pure (Left ("parseBashFile failed: " <> show errs))
        Right parseResult ->
          case translateParseResult defaultConfig parseResult of
            Left err -> pure (Left ("translateParseResult failed: " <> show err))
            Right result -> do
              sourceMap <- collectSourceMap path (prRoot parseResult)
              let translation =
                    Translation
                      { trPath = path,
                        trStatements = translationStatements result,
                        trState = translationState result,
                        trSourceMap = sourceMap
                      }
                  acc' = M.insert path translation acc
              foldM collectChild (Right acc') (catMaybes (M.elems sourceMap))
  where
    collectChild (Left err) _ = pure (Left err)
    collectChild (Right seen) child = collectTranslations seen child

collectSourceMap :: FilePath -> Maybe Token -> IO (M.Map T.Text (Maybe FilePath))
collectSourceMap path mRoot = do
  let sources = maybe [] collectSourceArgs mRoot
      baseDir = takeDirectory path
      literals = map tokenToLiteralText sources
  foldM (resolveSource baseDir) M.empty literals
  where
    resolveSource base acc txt
      | T.null txt = pure acc
      | M.member txt acc = pure acc
      | otherwise = do
          resolved <- resolveSourcePath base txt
          pure (M.insert txt resolved acc)

resolveSourcePath :: FilePath -> T.Text -> IO (Maybe FilePath)
resolveSourcePath base txt = resolveCandidates candidates
  where
    raw = toString txt
    primary = if isRelative raw then base </> raw else raw
    candidates =
      if isRelative raw
        then [primary, raw]
        else [primary]

    resolveCandidates [] = pure Nothing
    resolveCandidates (candidate : rest) = do
      exists <- doesFileExist candidate
      if exists
        then Just <$> canonicalizePath candidate
        else resolveCandidates rest

collectSourceArgs :: Token -> [Token]
collectSourceArgs tok =
  let direct =
        case tok of
          T_SourceCommand _ _ pathTok -> [pathTok]
          T_SimpleCommand _ _ (cmdTok : argTok : _)
            | isSourceCmd cmdTok -> [argTok]
          _ -> []
   in direct <> concatMap collectSourceArgs (tokenChildren tok)

tokenChildren :: Token -> [Token]
tokenChildren (OuterToken _ inner) = toList inner

isSourceCmd :: Token -> Bool
isSourceCmd tok =
  let name = tokenToLiteralText tok
   in name == "source" || name == "."

tokenToLiteralText :: Token -> T.Text
tokenToLiteralText = T.pack . getLiteralStringDef ""
