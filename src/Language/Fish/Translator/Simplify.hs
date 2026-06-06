{-# LANGUAGE LambdaCase #-}

module Language.Fish.Translator.Simplify
  ( simplifyFishStatement,
  )
where

import Data.List.NonEmpty qualified as NE
import Language.Fish.Translator.DSL

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

simplifyCommandStmt :: (Typeable t) => FishCommand t -> FishStatement
simplifyCommandStmt cmd =
  case cmd of
    Begin body suffix
      | null suffix ->
          let simplifiedBody = normalizeBeginBody (simplifyNE body)
           in case simplifyPreludeBody simplifiedBody of
                Just flattened -> flattened
                Nothing ->
                  case simplifySingleSafeBody simplifiedBody of
                    Just flattened -> flattened
                    Nothing -> Stmt (Begin simplifiedBody [])
    _ -> Stmt cmd

simplifyPreludeBody :: NE.NonEmpty FishStatement -> Maybe FishStatement
simplifyPreludeBody body =
  let flattened = simplifyStmtList (NE.toList body)
   in if all isScopeNeutralPreludeStmt flattened
        then Just $
          case flattened of
            [] -> EmptyStmt
            [single] -> single
            xs -> StmtList xs
        else Nothing

normalizeBeginBody :: NE.NonEmpty FishStatement -> NE.NonEmpty FishStatement
normalizeBeginBody =
  nonEmptyStmtList
    . concatMap normalizeBeginStmt
    . NE.toList

normalizeBeginStmt :: FishStatement -> [FishStatement]
normalizeBeginStmt = \case
  Stmt (Begin body suffix)
    | null suffix ->
        let normalizedBody = normalizeBeginBody body
         in if canSpliceNestedBeginBody normalizedBody
              then NE.toList normalizedBody
              else [Stmt (Begin normalizedBody [])]
  other -> [other]

canSpliceNestedBeginBody :: NE.NonEmpty FishStatement -> Bool
canSpliceNestedBeginBody body =
  commentsOnlyBody body
    || scopeNeutralPreludeBody body
    || singleSafeBody body

commentsOnlyBody :: NE.NonEmpty FishStatement -> Bool
commentsOnlyBody = all isCommentStmt . NE.toList

scopeNeutralPreludeBody :: NE.NonEmpty FishStatement -> Bool
scopeNeutralPreludeBody = all isScopeNeutralPreludeStmt . NE.toList

isScopeNeutralPreludeStmt :: FishStatement -> Bool
isScopeNeutralPreludeStmt = \case
  Comment _ -> True
  Stmt (Set flags _ _) -> scopeNeutralSetFlags flags
  _ -> False

scopeNeutralSetFlags :: [SetFlag] -> Bool
scopeNeutralSetFlags flags =
  SetLocal `notElem` flags
    && SetFunction `notElem` flags

singleSafeBody :: NE.NonEmpty FishStatement -> Bool
singleSafeBody body =
  case nonCommentStatements (NE.toList body) of
    [single] -> safeToElideBegin single
    _ -> False

simplifySingleSafeBody :: NE.NonEmpty FishStatement -> Maybe FishStatement
simplifySingleSafeBody body =
  case nonCommentStatements (NE.toList body) of
    [single]
      | safeToElideBegin single || isElidableNestedBeginStmt single ->
          Just (stmtListToStatement (NE.toList body))
    _ -> Nothing

isElidableNestedBeginStmt :: FishStatement -> Bool
isElidableNestedBeginStmt = \case
  Stmt (Begin body suffix) ->
    null suffix
      && all nestedBeginBodyStmtSafe (NE.toList body)
  _ -> False

nestedBeginBodyStmtSafe :: FishStatement -> Bool
nestedBeginBodyStmtSafe = \case
  Comment _ -> True
  Stmt cmd -> nestedBeginCommandSafe cmd
  _ -> False

nestedBeginCommandSafe :: FishCommand t -> Bool
nestedBeginCommandSafe = \case
  Command {} -> True
  Echo {} -> True
  Printf {} -> True
  Read {} -> True
  Eval {} -> True
  Source {} -> True
  Set {} -> True
  Decorated _ inner -> nestedBeginCommandSafe inner
  Begin inner suffix -> null suffix && all nestedBeginBodyStmtSafe (NE.toList inner)
  _ -> False

isCommentStmt :: FishStatement -> Bool
isCommentStmt = \case
  Comment _ -> True
  _ -> False

nonCommentStatements :: [FishStatement] -> [FishStatement]
nonCommentStatements = filter (not . isCommentStmt)

stmtListToStatement :: [FishStatement] -> FishStatement
stmtListToStatement = \case
  [] -> EmptyStmt
  [single] -> single
  stmts -> StmtList stmts

nonEmptyStmtList :: [FishStatement] -> NE.NonEmpty FishStatement
nonEmptyStmtList = \case
  [] -> EmptyStmt NE.:| []
  (x : xs) -> x NE.:| xs

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
  nonEmptyStmtList (simplifyStmtList (NE.toList body))

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
    If (simplifyJobList cond) (simplifyNE thn) (simplifyElseBranch els) suffix
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

simplifyElseBranch :: [FishStatement] -> [FishStatement]
simplifyElseBranch els =
  let simplified = simplifyStmtList els
   in if isSyntheticTrueElse simplified
        then []
        else simplified

isSyntheticTrueElse :: [FishStatement] -> Bool
isSyntheticTrueElse = \case
  [stmt] -> stmtIsSyntheticTrue stmt
  _ -> False

stmtIsSyntheticTrue :: FishStatement -> Bool
stmtIsSyntheticTrue = \case
  Stmt cmd -> commandIsSyntheticTrue cmd
  _ -> False

commandIsSyntheticTrue :: FishCommand t -> Bool
commandIsSyntheticTrue = \case
  Command "true" [] -> True
  Begin body suffix ->
    null suffix
      && case NE.toList body of
        [stmt] -> stmtIsSyntheticTrue stmt
        _ -> False
  _ -> False

simplifyPipeline :: FishJobPipeline -> FishJobPipeline
simplifyPipeline pipe =
  pipe
    { jpStatement = simplifyPipelineStage (jpStatement pipe),
      jpCont = map simplifyPipeCont (jpCont pipe)
    }

simplifyPipeCont :: JobPipeCont -> JobPipeCont
simplifyPipeCont cont =
  cont {jpcStatement = simplifyPipelineStage (jpcStatement cont)}

simplifyPipelineStage :: FishStatement -> FishStatement
simplifyPipelineStage stmt =
  case simplifyStmt stmt of
    StmtList [] -> EmptyStmt
    StmtList [single] -> unwrapTrivialPipelineStage single
    StmtList (stageHead : stageRest) -> Stmt (Begin (stageHead NE.:| stageRest) [])
    other -> unwrapTrivialPipelineStage other

unwrapTrivialPipelineStage :: FishStatement -> FishStatement
unwrapTrivialPipelineStage stmt =
  let stripped = stripTrivialPipelineStage stmt
   in if safeToElideBegin stripped
        then stripped
        else stmt

stripTrivialPipelineStage :: FishStatement -> FishStatement
stripTrivialPipelineStage = \case
  Stmt (Begin body suffix)
    | null suffix,
      Just single <- exactSingleBodyStmt body ->
        stripTrivialPipelineStage single
  Stmt (Pipeline pipe)
    | trivialSingleStagePipeline pipe ->
        stripTrivialPipelineStage (jpStatement pipe)
  other -> other

exactSingleBodyStmt :: NE.NonEmpty FishStatement -> Maybe FishStatement
exactSingleBodyStmt body =
  case NE.toList body of
    [single] -> Just single
    _ -> Nothing

trivialSingleStagePipeline :: FishJobPipeline -> Bool
trivialSingleStagePipeline pipe =
  not (jpTime pipe)
    && null (jpVariables pipe)
    && null (jpCont pipe)
    && not (jpBackgrounded pipe)

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
simplifyJobList (MkFishJobList conjs) =
  MkFishJobList (fmap simplifyConjunction conjs)
