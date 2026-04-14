{-# LANGUAGE LambdaCase #-}

module Language.Fish.Translator.Simplify
  ( simplifyFishStatement,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.AST

simplifyFishStatement :: FishStatement -> FishStatement
simplifyFishStatement = simplifyStmt

simplifyStmt :: FishStatement -> FishStatement
simplifyStmt stmt =
  case stmt of
    Stmt cmd -> simplifyCommandStmt (simplifyCommand cmd)
    StmtList stmts ->
      case simplifyStmtList stmts of
        [] -> EmptyStmt
        [single] -> single
        simplified -> StmtList simplified
    Comment _ -> stmt
    EmptyStmt -> EmptyStmt

simplifyStmtList :: [FishStatement] -> [FishStatement]
simplifyStmtList = concatMap flatten
  where
    flatten stmt =
      case simplifyStmt stmt of
        EmptyStmt -> []
        StmtList inner -> inner
        other -> [other]

simplifyCommandStmt :: Typeable t => FishCommand t -> FishStatement
simplifyCommandStmt cmd =
  case cmd of
    Begin body suffix
      | null suffix ->
          let simplifiedBody = simplifyNE body
           in case simplifyPreludeBody simplifiedBody of
                Just flattened -> flattened
                Nothing ->
                  case NE.toList simplifiedBody of
                    [single] | safeToElideBegin single -> single
                    _ -> Stmt (Begin simplifiedBody [])
    _ -> Stmt cmd

simplifyPreludeBody :: NE.NonEmpty FishStatement -> Maybe FishStatement
simplifyPreludeBody body =
  let flattened = simplifyStmtList (NE.toList body)
   in if all isPreludeStmt flattened
        then Just $
          case flattened of
            [] -> EmptyStmt
            [single] -> single
            xs -> StmtList xs
        else Nothing

isPreludeStmt :: FishStatement -> Bool
isPreludeStmt = \case
  Stmt (Set {}) -> True
  Comment _ -> True
  _ -> False

safeToElideBegin :: FishStatement -> Bool
safeToElideBegin = \case
  Comment _ -> True
  Stmt cmd -> safeCommand cmd
  _ -> False
  where
    safeCommand = \case
      Command {} -> True
      Echo {} -> True
      Printf {} -> True
      Read {} -> True
      Eval {} -> True
      Source {} -> True
      Decorated _ inner -> safeCommand inner
      _ -> False

simplifyNE :: NE.NonEmpty FishStatement -> NE.NonEmpty FishStatement
simplifyNE body =
  case simplifyStmtList (NE.toList body) of
    [] -> EmptyStmt NE.:| []
    (x : xs) -> x NE.:| xs

simplifyCommand :: FishCommand t -> FishCommand t
simplifyCommand = \case
  Function fn ->
    let body' = simplifyNE (funcBody fn)
     in Function fn {funcBody = body'}
  For var listExpr body suffix ->
    For var listExpr (simplifyNE body) suffix
  While cond body suffix ->
    While (simplifyJobList cond) (simplifyNE body) suffix
  Begin body suffix ->
    Begin (simplifyNE body) suffix
  If cond thn els suffix ->
    If (simplifyJobList cond) (simplifyNE thn) (simplifyStmtList els) suffix
  Switch expr cases suffix ->
    Switch expr (fmap simplifyCaseItem cases) suffix
  Pipeline pipe ->
    Pipeline (simplifyPipeline pipe)
  JobConj conj ->
    JobConj (simplifyConjunction conj)
  Semicolon cmd1 cmd2 ->
    Semicolon (simplifyCommand cmd1) (simplifyCommand cmd2)
  Not inner ->
    Not (simplifyCommand inner)
  Background inner ->
    Background (simplifyCommand inner)
  Decorated dec inner ->
    Decorated dec (simplifyCommand inner)
  other -> other

simplifyCaseItem :: CaseItem -> CaseItem
simplifyCaseItem item =
  item {caseBody = simplifyNE (caseBody item)}

simplifyPipeline :: FishJobPipeline -> FishJobPipeline
simplifyPipeline pipe =
  pipe
    { jpStatement = simplifyStmt (jpStatement pipe),
      jpCont = map simplifyPipeCont (jpCont pipe)
    }

simplifyPipeCont :: JobPipeCont -> JobPipeCont
simplifyPipeCont cont =
  cont {jpcStatement = simplifyStmt (jpcStatement cont)}

simplifyConjunction :: FishJobConjunction -> FishJobConjunction
simplifyConjunction conj =
  conj
    { jcJob = simplifyPipeline (jcJob conj),
      jcContinuations = map simplifyConjCont (jcContinuations conj)
    }

simplifyConjCont :: FishJobConjCont -> FishJobConjCont
simplifyConjCont = \case
  JCAnd pipe -> JCAnd (simplifyPipeline pipe)
  JCOr pipe -> JCOr (simplifyPipeline pipe)

simplifyJobList :: FishJobList -> FishJobList
simplifyJobList (FishJobList conjs) =
  FishJobList (fmap simplifyConjunction conjs)
