-- | Authoritative literal-source discovery. Each dependency is read once, while
-- each source occurrence resumes normalization and executes at its own boundary.
module Monk.Source
  ( SourceMode (..),
    SourceEnvironment (..),
    captureSourceEnvironment,
    resolveSourcePathIn,
    SourceGraph,
    SourceDependency,
    SourceOccurrence,
    SourceGraphFailure (..),
    sourceRoot,
    sourcePaths,
    sourceDependencies,
    sourceDependencyPath,
    sourceDependencyIdentity,
    sourceOccurrences,
    sourceOccurrenceId,
    sourceOccurrenceParent,
    sourceOccurrenceDependency,
    sourceOccurrenceRange,
    sourceGraphDiagnostics,
    sourceGraphRuntimeRequirements,
    sourceGraphStatistics,
    translateSourceGraph,
    translateSourceGraphWithEnvironment,
  )
where

import Control.Exception (IOException, try)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Language.Bash.Plan qualified as P
import Language.Bash.Plan.Normalize
  ( NormalizationResult (..),
    SourceDocument (..),
    beginNormalization,
  )
import Language.Fish.DSL (SourceRange)
import Language.Fish.Translator.Plan (compileSourcePlan, plannedDiagnostics, plannedRequirements, plannedStatistics)
import Monk.Source.Environment
import Monk.Source.Product
import Monk.Translation (TranslationFailure (..), parseBashScript)
import Monk.Translation.ParseDiagnostics (genericParseDiagnostic, positionedCommentDiagnostic)
import Monk.Translation.Types
import ShellCheck.Interface (ParseResult, prComments, prRoot)

data SourceMode = SourceInline | SourceSeparate
  deriving stock (Show, Eq)

data SourceGraphFailure = MkSourceGraphFailure FilePath TranslationFailure
  deriving stock (Show, Eq)

sourceRoot :: SourceGraph -> FilePath
sourceRoot = graphRoot

sourcePaths :: SourceGraph -> [FilePath]
sourcePaths = map snapshotPath . graphSnapshots

sourceDependencies :: SourceGraph -> [SourceDependency]
sourceDependencies = map (\input -> MkSourceDependency (snapshotPath input) (snapshotIdentity input)) . graphSnapshots

sourceDependencyPath :: SourceDependency -> FilePath
sourceDependencyPath (MkSourceDependency path _) = path

sourceDependencyIdentity :: SourceDependency -> Text
sourceDependencyIdentity (MkSourceDependency _ label) = label

sourceOccurrences :: SourceGraph -> [SourceOccurrence]
sourceOccurrences = graphOccurrences

sourceOccurrenceId :: SourceOccurrence -> Int
sourceOccurrenceId = occurrenceSequence

sourceOccurrenceParent :: SourceOccurrence -> FilePath
sourceOccurrenceParent = occurrenceParent

sourceOccurrenceDependency :: SourceOccurrence -> FilePath
sourceOccurrenceDependency = occurrenceDependency

sourceOccurrenceRange :: SourceOccurrence -> Maybe SourceRange
sourceOccurrenceRange = occurrenceRange

sourceGraphDiagnostics :: SourceGraph -> [Diagnostic]
sourceGraphDiagnostics graph = graphParseDiagnostics graph <> plannedDiagnostics (graphTranslation graph)

sourceGraphRuntimeRequirements :: SourceGraph -> [RuntimeRequirement]
sourceGraphRuntimeRequirements = plannedRequirements . graphTranslation

sourceGraphStatistics :: SourceGraph -> TranslationStatistics
sourceGraphStatistics = plannedStatistics . graphTranslation

translateSourceGraph :: TranslateConfig -> Bool -> FilePath -> IO (Either SourceGraphFailure SourceGraph)
translateSourceGraph cfg recursive rootPath = do
  environment <- try @IOException captureSourceEnvironment
  case environment of
    Left err -> pure (Left (oneFailure rootPath (sourceDiagnostic Nothing "environment" (show err))))
    Right value -> translateSourceGraphWithEnvironment cfg value recursive rootPath

translateSourceGraphWithEnvironment ::
  TranslateConfig -> SourceEnvironment -> Bool -> FilePath -> IO (Either SourceGraphFailure SourceGraph)
translateSourceGraphWithEnvironment cfg environment recursive rootPath = do
  rootInput <- readSourceSnapshot rootPath
  case rootInput of
    Left diagnostic -> pure (Left (oneFailure rootPath diagnostic))
    Right input -> do
      parsed <- parseBashScript (snapshotPath input) (snapshotText input)
      case parsedFailure parsed of
        Just failure -> pure (Left (MkSourceGraphFailure (snapshotPath input) failure))
        Nothing ->
          drive
            (snapshotPath input)
            (M.singleton (snapshotPath input) (input, parsed, Nothing))
            [input]
            []
            (map positionedCommentDiagnostic (prComments parsed))
            (beginNormalization cfg (snapshotText input) parsed)
  where
    drive root cache inputs occurrences diagnostics = \case
      NormalizationFailed errors -> pure (Left (MkSourceGraphFailure root (MkTranslationFailure errors)))
      NormalizationComplete plan -> pure $ case compileSourcePlan plan of
        Left errors -> Left (MkSourceGraphFailure root (MkTranslationFailure errors))
        Right translated -> Right (MkSourceGraph root environment inputs occurrences plan translated diagnostics)
      NormalizationNeedsSource request resume
        | not recursive ->
            pure
              ( Left
                  ( oneFailure
                      root
                      (sourceDiagnostic (P.sourceRequestRange request) "disabled" "Literal source requires recursive graph translation")
                  )
              )
        | otherwise -> do
            let resolveDirectory path =
                  let absolute = if T.isPrefixOf "/" path then path else toText (sourceWorkingDirectory environment) <> "/" <> path
                      component parts "" = parts
                      component parts "." = parts
                      component parts ".." = drop 1 parts
                      component parts value = value : parts
                   in "/" <> T.intercalate "/" (reverse (foldl' component [] (T.splitOn "/" absolute)))
                executionEnvironment = maybe environment (\path -> environment {sourceWorkingDirectory = toString (resolveDirectory path)}) (P.sourceRequestWorkingDirectory request)
            resolved <- resolveSourcePathIn executionEnvironment (P.sourceRequestTarget request)
            case resolved of
              Left diagnostic -> pure (Left (oneFailure root diagnostic {diagnosticRange = P.sourceRequestRange request}))
              Right path
                | toText path `elem` P.sourceRequestStack request ->
                    pure
                      ( Left
                          ( oneFailure
                              path
                              (sourceDiagnostic (P.sourceRequestRange request) "cycle" "Source cycles are outside the acyclic execution contract")
                          )
                      )
                | otherwise -> do
                    loaded <- case M.lookup path cache of
                      Just value -> pure (Right value)
                      Nothing ->
                        readSourceSnapshot path >>= \case
                          Left diagnostic -> pure (Left diagnostic)
                          Right input -> do
                            parsed <- parseBashScript (snapshotPath input) (snapshotText input)
                            pure (Right (input, parsed, Just (P.sourceRequestEntryContext request)))
                    case loaded of
                      Left diagnostic -> pure (Left (oneFailure path diagnostic {diagnosticRange = P.sourceRequestRange request}))
                      Right (input, parsed, priorContext)
                        | maybe False (/= P.sourceRequestEntryContext request) priorContext ->
                            pure
                              ( Left
                                  ( oneFailure
                                      path
                                      (sourceDiagnostic (P.sourceRequestRange request) "entry-context" "A dependency was reached under incompatible binding or dispatch facts")
                                  )
                              )
                        | otherwise -> case parsedFailure parsed of
                            Just failure -> pure (Left (MkSourceGraphFailure path failure))
                            Nothing -> do
                              let fresh = not (M.member path cache)
                                  parent = maybe root toString (listToMaybe (reverse (P.sourceRequestStack request)))
                                  occurrence = MkSourceOccurrence (length occurrences) (P.sourceRequestId request) parent path (P.sourceRequestRange request)
                              drive
                                root
                                (M.insert path (input, parsed, Just (P.sourceRequestEntryContext request)) cache)
                                (inputs <> [input | fresh])
                                (occurrences <> [occurrence])
                                (diagnostics <> [positionedCommentDiagnostic comment | fresh, comment <- prComments parsed])
                                (resume (SourceDocument (snapshotText input) parsed))

parsedFailure :: ParseResult -> Maybe TranslationFailure
parsedFailure parsed
  | isJust (prRoot parsed) = Nothing
  | otherwise =
      Just
        ( MkTranslationFailure
            ( fromMaybe
                (genericParseDiagnostic :| [])
                (NE.nonEmpty (map positionedCommentDiagnostic (prComments parsed)))
            )
        )

oneFailure :: FilePath -> Diagnostic -> SourceGraphFailure
oneFailure path diagnostic = MkSourceGraphFailure path (MkTranslationFailure (diagnostic :| []))

sourceDiagnostic :: Maybe SourceRange -> Text -> Text -> Diagnostic
sourceDiagnostic range code message = MkDiagnostic (MkDiagnosticCode ("monk.source." <> code)) PhaseSource DiagnosticError Unsafe message range
