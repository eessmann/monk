{-# LANGUAGE LambdaCase #-}

-- | Typed planning for stdout, combined recursive output, and separate files.
-- Rendering is deliberately separate from filesystem writes.
module Monk.Output
  ( OutputTarget (..),
    GeneratedFile (..),
    OutputBundle (..),
    planCombinedOutputBundle,
    planSeparateOutputBundle,
    renderOutputBundle,
  )
where

import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Typeable (cast)
import Language.Fish.DSL (Script, renderScript)
import Language.Fish.DSL.Internal
  ( CaseItem (..),
    ExprOrRedirect (..),
    FishCommand (..),
    FishExpr (ExprFileRelative, ExprListLiteral, ExprLiteral),
    FishFunction (..),
    FishJobConjCont (..),
    FishJobConjunction (..),
    FishJobList (..),
    FishJobPipeline (..),
    FishStatement (..),
    FishType (TList, TStr),
    JobPipeCont (..),
    Script (MkScript),
  )
import Monk.Source
  ( SourceGraph (..),
    Translation (..),
    inlineSourceGraph,
    rewriteSources,
  )
import Monk.Translation
  ( Diagnostic (..),
    DiagnosticCode (..),
    DiagnosticPhase (PhaseSource),
    DiagnosticSeverity (DiagnosticError),
    ReviewRisk (Unsafe),
    RuntimeRequirement (..),
  )
import System.FilePath qualified as FP

data OutputTarget
  = OutputStdout
  | OutputPath FilePath
  deriving stock (Show, Eq, Ord)

data GeneratedFile = MkGeneratedFile
  { generatedTarget :: OutputTarget,
    generatedScript :: Script,
    generatedDiagnostics :: [Diagnostic],
    generatedRuntimeRequirements :: [RuntimeRequirement]
  }
  deriving stock (Show, Eq)

data OutputBundle = MkOutputBundle
  { bundleUserFiles :: NonEmpty GeneratedFile,
    bundleRuntimeFile :: Maybe GeneratedFile
  }
  deriving stock (Show, Eq)

planCombinedOutputBundle ::
  OutputTarget ->
  FilePath ->
  SourceGraph ->
  IO (Either Diagnostic OutputBundle)
planCombinedOutputBundle target rootPath graph =
  case M.lookup rootPath (sgTranslations graph) of
    Nothing -> pure (Left (missingRootDiagnostic rootPath))
    Just _ -> do
      (script, inlineDiagnostics) <- inlineSourceGraph graph rootPath
      let translations = orderedTranslations graph
          generated =
            MkGeneratedFile
              { generatedTarget = target,
                generatedScript = dedupeGeneratedRuntime script,
                generatedDiagnostics = concatMap trDiagnostics translations <> inlineDiagnostics,
                generatedRuntimeRequirements = mergeRuntimeRequirements (concatMap trRuntimeRequirements translations)
              }
      pure (Right (MkOutputBundle (generated :| []) Nothing))

planSeparateOutputBundle ::
  FilePath ->
  FilePath ->
  SourceGraph ->
  Either Diagnostic OutputBundle
planSeparateOutputBundle rootOutput rootPath graph = do
  unless (M.member rootPath translations) (Left (missingRootDiagnostic rootPath))
  userFiles <- maybe (Left (missingRootDiagnostic rootPath)) Right (NE.nonEmpty plannedUserFiles)
  case duplicateOutputTarget (toList userFiles <> maybeToList runtimeFile) of
    Just target -> Left (duplicateOutputDiagnostic target)
    Nothing -> pure ()
  pure
    MkOutputBundle
      { bundleUserFiles = userFiles,
        bundleRuntimeFile = runtimeFile
      }
  where
    translations = relocateTranslations rootOutput rootPath graph
    runtimePath = FP.combine (FP.takeDirectory rootOutput) "_monk_runtime.fish"
    prepared =
      [ (sourcePath, translation, splitRuntime (anchorRelativeSources (rewriteSources translations translation)))
      | sourcePath <- sgOrder graph,
        Just translation <- [M.lookup sourcePath translations]
      ]
    runtimeStatements = L.nub (concatMap (fst . third) prepared)
    runtimeRequirements = mergeRuntimeRequirements (concatMap (\(_, translation, _) -> trRuntimeRequirements translation) prepared)
    runtimeFile
      | null runtimeStatements = Nothing
      | otherwise =
          Just
            MkGeneratedFile
              { generatedTarget = OutputPath runtimePath,
                generatedScript = MkScript runtimeStatements,
                generatedDiagnostics = [],
                generatedRuntimeRequirements = runtimeRequirements
              }
    plannedUserFiles = map makeUserFile prepared
    makeUserFile (_, translation, (runtimePrefix, userStatements)) =
      let outputPath = trPath translation
          runtimeSource =
            [ Stmt (Source (ExprFileRelative (toText (relativePath (FP.takeDirectory outputPath) runtimePath))))
            | not (null runtimePrefix)
            ]
       in MkGeneratedFile
            { generatedTarget = OutputPath outputPath,
              generatedScript = MkScript (runtimeSource <> userStatements),
              generatedDiagnostics = trDiagnostics translation,
              generatedRuntimeRequirements = trRuntimeRequirements translation
            }

renderOutputBundle :: OutputBundle -> [(OutputTarget, Text)]
renderOutputBundle bundle =
  map
    renderFile
    (toList (bundleUserFiles bundle) <> maybeToList (bundleRuntimeFile bundle))
  where
    renderFile generated =
      (generatedTarget generated, renderScript (generatedScript generated))

orderedTranslations :: SourceGraph -> [Translation]
orderedTranslations graph =
  mapMaybe (`M.lookup` sgTranslations graph) (sgOrder graph)

relocateTranslations :: FilePath -> FilePath -> SourceGraph -> M.Map FilePath Translation
relocateTranslations rootOutput rootPath graph =
  M.mapWithKey relocate (sgTranslations graph)
  where
    sourceRoot = commonAncestorDir (sgOrder graph)
    outputRoot = FP.takeDirectory rootOutput
    relocate sourcePath translation
      | sourcePath == rootPath = translation {trPath = rootOutput}
      | otherwise =
          let relativeSource = FP.makeRelative sourceRoot sourcePath
              outputPath = FP.combine outputRoot (FP.replaceExtension relativeSource "fish")
           in translation {trPath = outputPath}

commonAncestorDir :: [FilePath] -> FilePath
commonAncestorDir = \case
  [] -> "."
  path : rest ->
    foldl' sharedDirectory (FP.takeDirectory path) (map FP.takeDirectory rest)
  where
    sharedDirectory left right =
      case map fst (takeWhile (uncurry (==)) (zip (segments left) (segments right))) of
        [] -> "."
        common -> FP.joinPath common
    segments = FP.splitDirectories . FP.normalise

relativePath :: FilePath -> FilePath -> FilePath
relativePath fromDirectory targetPath =
  case replicate (length fromRest) ".." <> targetRest of
    [] -> "."
    parts -> FP.joinPath parts
  where
    fromParts = FP.splitDirectories (FP.normalise fromDirectory)
    targetParts = FP.splitDirectories (FP.normalise targetPath)
    commonCount = length (takeWhile (uncurry (==)) (zip fromParts targetParts))
    fromRest = drop commonCount fromParts
    targetRest = drop commonCount targetParts

anchorRelativeSources :: Script -> Script
anchorRelativeSources (MkScript statements) =
  MkScript (map anchorStatement statements)

anchorStatement :: FishStatement -> FishStatement
anchorStatement = \case
  Stmt command -> Stmt (anchorCommand command)
  StmtList statements -> StmtList (map anchorStatement statements)
  other -> other

anchorCommand :: FishCommand t -> FishCommand t
anchorCommand = \case
  Command name args
    | name == "source" || name == "." -> Command name (anchorSourceArgs args)
  Source expr -> Source (anchorSourceExpr expr)
  Begin body suffix -> Begin (NE.map anchorStatement body) suffix
  If condition thenBody elseBody suffix ->
    If
      (anchorJobList condition)
      (NE.map anchorStatement thenBody)
      (map anchorStatement elseBody)
      suffix
  While condition body suffix ->
    While (anchorJobList condition) (NE.map anchorStatement body) suffix
  For name values body suffix ->
    For name values (NE.map anchorStatement body) suffix
  Switch expr cases suffix ->
    Switch expr (NE.map anchorCaseItem cases) suffix
  Function fishFunction ->
    Function fishFunction {funcBody = NE.map anchorStatement (funcBody fishFunction)}
  Pipeline pipeline -> Pipeline (anchorPipeline pipeline)
  JobConj conjunction -> JobConj (anchorConjunction conjunction)
  Semicolon left right -> Semicolon (anchorCommand left) (anchorCommand right)
  Not command -> Not (anchorCommand command)
  Background command -> Background (anchorCommand command)
  Decorated decoration command -> Decorated decoration (anchorCommand command)
  other -> other

anchorSourceExpr :: FishExpr TStr -> FishExpr TStr
anchorSourceExpr = \case
  ExprLiteral path
    | FP.isRelative (toString path) -> ExprFileRelative path
  other -> other

anchorSourceArgs :: [ExprOrRedirect] -> [ExprOrRedirect]
anchorSourceArgs = \case
  ExprVal expr : rest ->
    case cast expr of
      Just stringExpr -> ExprVal (anchorSourceExpr stringExpr) : rest
      Nothing ->
        case cast expr of
          Just listExpr -> ExprVal (anchorSourceListExpr listExpr) : rest
          Nothing -> ExprVal expr : rest
  other -> other

anchorSourceListExpr :: FishExpr (TList TStr) -> FishExpr (TList TStr)
anchorSourceListExpr = \case
  ExprListLiteral [expr] -> ExprListLiteral [anchorSourceExpr expr]
  other -> other

anchorCaseItem :: CaseItem -> CaseItem
anchorCaseItem (MkCaseItem patterns body) =
  MkCaseItem patterns (NE.map anchorStatement body)

anchorJobList :: FishJobList -> FishJobList
anchorJobList (MkFishJobList conjunctions) =
  MkFishJobList (NE.map anchorConjunction conjunctions)

anchorConjunction :: FishJobConjunction -> FishJobConjunction
anchorConjunction conjunction =
  conjunction
    { jcJob = anchorPipeline (jcJob conjunction),
      jcContinuations = map anchorContinuation (jcContinuations conjunction)
    }

anchorContinuation :: FishJobConjCont -> FishJobConjCont
anchorContinuation = \case
  JCAnd pipeline -> JCAnd (anchorPipeline pipeline)
  JCOr pipeline -> JCOr (anchorPipeline pipeline)

anchorPipeline :: FishJobPipeline -> FishJobPipeline
anchorPipeline pipeline =
  pipeline
    { jpStatement = anchorStatement (jpStatement pipeline),
      jpCont = map anchorPipeContinuation (jpCont pipeline)
    }
  where
    anchorPipeContinuation continuation =
      continuation {jpcStatement = anchorStatement (jpcStatement continuation)}

splitRuntime :: Script -> ([FishStatement], [FishStatement])
splitRuntime (MkScript statements) = span isGeneratedRuntimeStatement statements

isGeneratedRuntimeStatement :: FishStatement -> Bool
isGeneratedRuntimeStatement = \case
  Stmt (Function fishFunction) -> "__monk_" `T.isPrefixOf` funcName fishFunction
  Stmt (Set _ name _) -> "__monk_" `T.isPrefixOf` name
  _ -> False

dedupeGeneratedRuntime :: Script -> Script
dedupeGeneratedRuntime (MkScript statements) = MkScript (go [] statements)
  where
    go _ [] = []
    go seen (statement : rest)
      | isGeneratedRuntimeStatement statement && statement `elem` seen = go seen rest
      | isGeneratedRuntimeStatement statement = statement : go (seen <> [statement]) rest
      | otherwise = statement : go seen rest

mergeRuntimeRequirements :: [RuntimeRequirement] -> [RuntimeRequirement]
mergeRuntimeRequirements requirements =
  map (uncurry MkRuntimeRequirement) (M.toAscList merged)
  where
    merged = foldl' insertRequirement mempty requirements
    insertRequirement acc requirement =
      M.insertWith
        mergeUses
        (requirementProgram requirement)
        (requirementUses requirement)
        acc
    mergeUses new existing =
      fromMaybe existing (NE.nonEmpty (L.nub (toList existing <> toList new)))

missingRootDiagnostic :: FilePath -> Diagnostic
missingRootDiagnostic path =
  MkDiagnostic
    { diagnosticCode = MkDiagnosticCode "monk.output.missing-root",
      diagnosticPhase = PhaseSource,
      diagnosticSeverity = DiagnosticError,
      diagnosticRisk = Unsafe,
      diagnosticMessage = "No translated source was found for output root: " <> toText path,
      diagnosticRange = Nothing
    }

duplicateOutputTarget :: [GeneratedFile] -> Maybe OutputTarget
duplicateOutputTarget files =
  listToMaybe
    [ target
    | target : _ : _ <- L.group (L.sort (map generatedTarget files))
    ]

duplicateOutputDiagnostic :: OutputTarget -> Diagnostic
duplicateOutputDiagnostic target =
  MkDiagnostic
    { diagnosticCode = MkDiagnosticCode "monk.output.duplicate-target",
      diagnosticPhase = PhaseSource,
      diagnosticSeverity = DiagnosticError,
      diagnosticRisk = Unsafe,
      diagnosticMessage = "Multiple generated files resolve to the same output target: " <> renderTarget target,
      diagnosticRange = Nothing
    }
  where
    renderTarget = \case
      OutputStdout -> "stdout"
      OutputPath path -> toText path

third :: (a, b, c) -> c
third (_, _, value) = value
