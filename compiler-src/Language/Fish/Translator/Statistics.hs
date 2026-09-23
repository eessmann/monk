{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- | Structural materialization statistics. Literal text is never executable
-- syntax here, including rendered child programs carried as transport frames.
module Language.Fish.Translator.Statistics
  ( materializationStatistics,
    commandReferences,
  )
where

import Data.ByteString qualified as BS
import Data.Set qualified as S
import Data.Text qualified as T
import Language.Fish.DSL (renderScript)
import Language.Fish.DSL.Internal
import Monk.Translation.Types (TranslationStatistics (..))

-- | The prefix and provider name are allocated by materialization, disjoint
-- from user function names. Counts include sites in function bodies once per
-- definition, whether reached or not. They never represent execution counts.
materializationStatistics :: Text -> Text -> Script -> TranslationStatistics
materializationStatistics prefix provider script@(MkScript statements) =
  let Sites definitions calls native bindings nodes captures external = collectSites provider statements
      owned name = not (T.null prefix) && T.isPrefixOf prefix name
   in MkTranslationStatistics (length (filter owned definitions)) (length (filter owned calls)) native (BS.length (encodeUtf8 (renderScript script))) nodes (length (filter owned bindings)) captures external

-- | Literal command identities in the structural tree, including substitutions
-- and function bodies. Expression data is never interpreted as source.
commandReferences :: [FishStatement] -> Set Text
commandReferences statements = let Sites _ calls _ _ _ _ _ = collectSites "" statements in S.fromList calls

collectSites :: Text -> [FishStatement] -> Sites
collectSites provider = foldMap statement
  where
    statement = \case
      Stmt cmd -> Sites [] [] 0 [] 1 0 0 <> command cmd
      StmtList items -> foldMap statement items
      Comment _ -> mempty
      EmptyStmt -> mempty
    command :: FishCommand grammar t -> Sites
    command = \case
      CommandSearch _ -> mempty
      Command name arguments -> Sites [] [commandNameText name] (if commandNameText name == provider then 1 else 0) [] 0 0 0 <> foldMap argument arguments
      CommandExpr headExpr arguments -> Sites [] [] (nativePathCall headExpr arguments) [] 0 0 0 <> foldMap argument arguments
      Set _ name value -> Sites [] [] 0 [identifierText name] 0 (statusCapture value) 0 <> expression value
      Function function -> Sites [funcName function] [] 0 [] 0 0 0 <> foldMap statement (funcBody function)
      For _ values body redirects -> expression values <> foldMap statement body <> foldMap redirection redirects
      While condition body redirects -> jobs condition <> foldMap statement body <> foldMap redirection redirects
      Begin body redirects -> foldMap statement body <> foldMap redirection redirects
      If condition yes no redirects -> jobs condition <> foldMap statement yes <> foldMap statement no <> foldMap redirection redirects
      Switch value cases redirects -> expression value <> foldMap caseItem cases <> foldMap redirection redirects
      Break -> mempty
      Continue -> mempty
      ReturnScalar value -> expression value
      Return value -> foldMap expression value
      Exit value -> foldMap expression value
      Source value -> expression value
      Eval value -> expression value
      Read _ _ -> mempty
      Echo values -> foldMap expression values
      Printf format values -> expression format <> foldMap expression values
      Pipeline value -> pipeline value
      JobConj value -> conjunction value
      Semicolon left right -> command left <> command right
      Not value -> command value
      Background value -> command value
      Wait value -> foldMap expression value
      Exec value arguments -> Sites [] [] (nativePathCall value arguments) [] 0 0 0 <> foldMap argument arguments
      Decorated DecCommand value -> Sites [] [] 0 [] 0 0 (externalDispatch value) <> command value
      Decorated _ value -> command value
    runtimePath :: Executable -> Bool
    runtimePath = foldExecutable (const False) (\name -> not (T.null provider) && identifierText name == provider <> "_path")
    operationName :: [ExprOrRedirect] -> Maybe Text
    operationName (ExprVal (ExprLiteral "--abi") : ExprVal (ExprLiteral _) : ExprVal (ExprLiteral operation) : _) = Just operation
    operationName _ = Nothing
    nativePathCall :: Executable -> [ExprOrRedirect] -> Int
    nativePathCall headExpr arguments = if runtimePath headExpr && isJust (operationName arguments) then 1 else 0
    externalDispatch :: FishCommand grammar t -> Int
    externalDispatch (CommandExpr headExpr arguments)
      | runtimePath headExpr = if operationName arguments == Just "exec-site" then 1 else 0
    externalDispatch _ = 1
    statusCapture :: FishExpr t -> Int
    statusCapture (ExprListLiteral [ExprQuotedVariable (VarScalar "status")]) = 1
    statusCapture _ = 0
    expression :: FishExpr t -> Sites
    expression = \case
      ExprLiteral _ -> mempty
      ExprEmbeddedScript (MkScript body) -> foldMap statement body
      ExprNumLiteral _ -> mempty
      ExprVariable value -> variable value
      ExprQuotedVariable value -> variable value
      ExprSpecialVar _ -> mempty
      ExprStringConcat a b -> expression a <> expression b
      ExprStringOp _ value -> expression value
      ExprJoinList value -> expression value
      ExprFileRelative _ -> mempty
      ExprMath values -> foldMap expression values
      ExprCommandSubst body -> foldMap statement body
      ExprQuotedCommandSubst body -> foldMap statement body
      ExprListLiteral values -> foldMap expression values
      ExprListConcat a b -> expression a <> expression b
      ExprGlob _ -> mempty
      ExprProcessSubst body -> foldMap statement body
    variable :: FishVarRef t -> Sites
    variable = \case
      VarAll _ -> mempty
      VarScalar _ -> mempty
      VarIndex _ index -> case index of
        IndexSingle value -> expression value
        IndexRange start end -> foldMap expression start <> foldMap expression end
        IndexList values -> foldMap expression values
    argument (ExprVal value) = expression value
    argument (RedirectVal redirect) = redirection redirect
    redirection = \case
      FileRedirect _ _ value -> expression value
      BothFileRedirect _ value -> expression value
      DuplicateRedirect {} -> mempty
      CloseRedirect {} -> mempty
    caseItem item = foldMap expression (casePatterns item) <> foldMap statement (caseBody item)
    assignment = foldMap expression . vaValue
    pipeline job = foldMap assignment (jpVariables job) <> statement (stageToStatement (jpStatement job)) <> foldMap continuation (jpCont job)
    continuation value = foldMap assignment (jpcVariables value) <> statement (stageToStatement (jpcStatement value))
    conjunction value = pipeline (jcJob value) <> foldMap (\case JCAnd job -> pipeline job; JCOr job -> pipeline job) (jcContinuations value)
    jobs (MkFishJobList values) = foldMap conjunction values

data Sites = Sites [Text] [Text] Int [Text] Int Int Int

instance Semigroup Sites where
  Sites a b c d e f g <> Sites h i j k l m n = Sites (a <> h) (b <> i) (c + j) (d <> k) (e + l) (f + m) (g + n)

instance Monoid Sites where
  mempty = Sites [] [] 0 [] 0 0 0
