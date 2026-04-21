-- |
-- Copyright: (c) 2025 Erich Essmann
-- SPDX-License-Identifier: MIT
-- Maintainer: Erich Essmann <essmanne@gmail.com>
--
-- Recursive source discovery and source-path rewriting helpers.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Monk.Source
  ( SourceMode (..),
    SourceGraph (..),
    SourceGraphFailure (..),
    translateSourceGraph,
    rewriteSources,
    collectSourceMap,
    resolveSourcePath,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Typeable (cast)
import Control.Monad (foldM)
import Language.Fish.AST
import Monk.Translation
  ( TranslateConfig,
    TranslateError,
    Translation (..),
    parseBashFile,
    translateParseResult,
    translationStatements,
    translationState,
  )
import ShellCheck.AST
import ShellCheck.ASTLib (getLiteralStringDef)
import ShellCheck.Interface (PositionedComment, prComments, prRoot)
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (isRelative, replaceExtension, takeDirectory, (</>))

data SourceMode
  = SourceInline
  | SourceSeparate
  deriving stock (Show, Eq)

data SourceGraph = MkSourceGraph
  { sgOrder :: [FilePath],
    sgParseComments :: M.Map FilePath [PositionedComment],
    sgTranslations :: M.Map FilePath Translation
  }
  deriving stock (Show, Eq)

data SourceGraphFailure
  = SourceGraphParseErrors FilePath [PositionedComment]
  | SourceGraphTranslateFailure FilePath TranslateError
  deriving stock (Show, Eq)

translateSourceGraph ::
  TranslateConfig ->
  Bool ->
  FilePath ->
  IO (Either SourceGraphFailure SourceGraph)
translateSourceGraph cfg recursive rootPath =
  go Set.empty [] mempty mempty [rootPath]
  where
    go _ order comments translations [] =
      pure
        ( Right
            MkSourceGraph
              { sgOrder = order,
                sgParseComments = comments,
                sgTranslations = translations
              }
        )
    go seen order comments translations (path : rest)
      | Set.member path seen = go seen order comments translations rest
      | otherwise = do
          parseResE <- parseBashFile path
          case parseResE of
            Left errs ->
              pure (Left (SourceGraphParseErrors path errs))
            Right parseRes ->
              case translateParseResult cfg parseRes of
                Left err ->
                  pure (Left (SourceGraphTranslateFailure path err))
                Right result -> do
                  sourceMap <-
                    if recursive
                      then collectSourceMap path (prRoot parseRes)
                      else pure mempty
                  let translation =
                        MkTranslation
                          { trPath = path,
                            trStatements = translationStatements result,
                            trState = translationState result,
                            trSourceMap = sourceMap
                          }
                      next =
                        if recursive
                          then catMaybes (M.elems sourceMap)
                          else []
                  go
                    (Set.insert path seen)
                    (order <> [path])
                    (M.insert path (prComments parseRes) comments)
                    (M.insert path translation translations)
                    (rest <> next)

rewriteSources :: M.Map FilePath Translation -> Translation -> [FishStatement]
rewriteSources translations tr =
  map (rewriteStatement sourceRewrite) (trStatements tr)
  where
    sourceRewrite txt =
      case join (M.lookup txt (trSourceMap tr)) of
        Just resolved
          | M.member resolved translations ->
              toText (replaceExtension (toString txt) "fish")
        _ -> txt

collectSourceMap :: FilePath -> Maybe Token -> IO (M.Map Text (Maybe FilePath))
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
          mPath <- resolveSourcePath base txt
          pure (M.insert txt mPath acc)

resolveSourcePath :: FilePath -> Text -> IO (Maybe FilePath)
resolveSourcePath base txt
  | T.null txt = pure Nothing
  | otherwise = do
      let raw = toString txt
          candidates
            | isRelative raw = ordNub [raw, base </> raw]
            | otherwise = [raw]
      resolveExisting candidates
  where
    resolveExisting = \case
      [] -> pure Nothing
      (candidate : rest) -> do
        exists <- doesFileExist candidate
        if exists
          then Just <$> canonicalizePath candidate
          else resolveExisting rest

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
tokenChildren = \case
  T_Script _ _ stmts -> stmts
  T_SimpleCommand _ assignments cmdToks -> assignments <> cmdToks
  T_Pipeline _ _ cmds -> cmds
  T_IfExpression _ conditionBranches elseBranch ->
    concatMap (uncurry (<>)) conditionBranches <> elseBranch
  T_WhileExpression _ cond body -> cond <> body
  T_UntilExpression _ cond body -> cond <> body
  T_Arithmetic _ exprTok -> [exprTok]
  T_ForArithmetic _ initTok condTok incTok body -> [initTok, condTok, incTok] <> body
  T_Function _ _ _ _ body -> [body]
  T_BraceGroup _ tokens -> tokens
  T_Subshell _ tokens -> tokens
  T_AndIf _ left right -> [left, right]
  T_OrIf _ left right -> [left, right]
  T_Backgrounded _ inner -> [inner]
  T_Annotation _ _ inner -> [inner]
  T_ForIn _ _ tokens body -> tokens <> body
  T_SelectIn _ _ tokens body -> tokens <> body
  T_CaseExpression _ switchExpr cases ->
    switchExpr : concatMap (\(_, pats, body) -> pats <> body) cases
  T_Redirecting _ redirs inner -> redirs <> [inner]
  T_NormalWord _ parts -> parts
  T_DoubleQuoted _ parts -> parts
  T_DollarBraced _ _ inner -> [inner]
  _ -> []

isSourceCmd :: Token -> Bool
isSourceCmd tok =
  let name = tokenToLiteralText tok
   in name == "source" || name == "."

tokenToLiteralText :: Token -> Text
tokenToLiteralText = T.pack . getLiteralStringDef ""

rewriteStatement :: (Text -> Text) -> FishStatement -> FishStatement
rewriteStatement f = \case
  Stmt cmd -> Stmt (rewriteCommand f cmd)
  StmtList xs -> StmtList (map (rewriteStatement f) xs)
  other -> other

rewriteCommand :: (Text -> Text) -> FishCommand t -> FishCommand t
rewriteCommand f = \case
  Command name args
    | name == "source" || name == "." ->
        Command name (rewriteSourceArgs f args)
  Source expr -> Source (rewriteSourceExpr f expr)
  Begin body suffix -> Begin (NE.map (rewriteStatement f) body) suffix
  If cond thn els suffix ->
    If
      (rewriteJobList f cond)
      (NE.map (rewriteStatement f) thn)
      (map (rewriteStatement f) els)
      suffix
  While cond body suffix ->
    While (rewriteJobList f cond) (NE.map (rewriteStatement f) body) suffix
  For var listExpr body suffix ->
    For var listExpr (NE.map (rewriteStatement f) body) suffix
  Switch expr cases suffix ->
    Switch expr (NE.map (rewriteCaseItem f) cases) suffix
  Function func ->
    Function func {funcBody = NE.map (rewriteStatement f) (funcBody func)}
  Pipeline pipe -> Pipeline (rewritePipeline f pipe)
  JobConj jc -> JobConj (rewriteConjunction f jc)
  Semicolon c1 c2 -> Semicolon (rewriteCommand f c1) (rewriteCommand f c2)
  Not cmd -> Not (rewriteCommand f cmd)
  Background cmd -> Background (rewriteCommand f cmd)
  Decorated dec cmd -> Decorated dec (rewriteCommand f cmd)
  other -> other

rewriteSourceExpr :: (Text -> Text) -> FishExpr TStr -> FishExpr TStr
rewriteSourceExpr f = \case
  ExprLiteral txt -> ExprLiteral (f txt)
  other -> other

rewriteSourceArgs :: (Text -> Text) -> [ExprOrRedirect] -> [ExprOrRedirect]
rewriteSourceArgs f = \case
  ExprVal expr : rest ->
    case cast expr of
      Just stringExpr -> ExprVal (rewriteSourceExpr f stringExpr) : rest
      Nothing ->
        case cast expr of
          Just listExpr -> ExprVal (rewriteSourceListExpr f listExpr) : rest
          Nothing -> ExprVal expr : rest
  other -> other

rewriteSourceListExpr :: (Text -> Text) -> FishExpr (TList TStr) -> FishExpr (TList TStr)
rewriteSourceListExpr f = \case
  ExprListLiteral [ExprLiteral txt] -> ExprListLiteral [ExprLiteral (f txt)]
  other -> other

rewriteCaseItem :: (Text -> Text) -> CaseItem -> CaseItem
rewriteCaseItem f (MkCaseItem pats body) =
  MkCaseItem pats (NE.map (rewriteStatement f) body)

rewriteJobList :: (Text -> Text) -> FishJobList -> FishJobList
rewriteJobList f (MkFishJobList conj) =
  MkFishJobList (NE.map (rewriteConjunction f) conj)

rewriteConjunction :: (Text -> Text) -> FishJobConjunction -> FishJobConjunction
rewriteConjunction f jc =
  jc
    { jcJob = rewritePipeline f (jcJob jc),
      jcContinuations = map (rewriteConjCont f) (jcContinuations jc)
    }

rewriteConjCont :: (Text -> Text) -> FishJobConjCont -> FishJobConjCont
rewriteConjCont f = \case
  JCAnd pipe -> JCAnd (rewritePipeline f pipe)
  JCOr pipe -> JCOr (rewritePipeline f pipe)

rewritePipeline :: (Text -> Text) -> FishJobPipeline -> FishJobPipeline
rewritePipeline f pipe =
  pipe
    { jpStatement = rewriteStatement f (jpStatement pipe),
      jpCont = map rewritePipeCont (jpCont pipe)
    }
  where
    rewritePipeCont cont =
      cont {jpcStatement = rewriteStatement f (jpcStatement cont)}
