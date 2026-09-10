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
  let Sites definitions calls native = collectSites provider statements
      owned name = not (T.null prefix) && T.isPrefixOf prefix name
   in MkTranslationStatistics (length (filter owned definitions)) (length (filter owned calls)) native (BS.length (encodeUtf8 (renderScript script)))

-- | Literal command identities in the structural tree, including substitutions
-- and function bodies. Expression data is never interpreted as source.
commandReferences :: [FishStatement] -> Set Text
commandReferences statements = let Sites _ calls _ = collectSites "" statements in S.fromList calls

collectSites :: Text -> [FishStatement] -> Sites
collectSites provider = foldMap statement
  where
    statement = \case
      Stmt cmd -> command cmd
      StmtList items -> foldMap statement items
      Comment _ -> mempty
      EmptyStmt -> mempty
    command :: FishCommand t -> Sites
    command = \case
      Command name arguments -> Sites [] [name] (if name == provider then 1 else 0) <> foldMap argument arguments
      CommandExpr headExpr arguments -> expression headExpr <> foldMap argument arguments
      Set _ _ value -> expression value
      Function function -> Sites [funcName function] [] 0 <> foldMap statement (funcBody function)
      For _ values body redirects -> expression values <> foldMap statement body <> foldMap argument redirects
      While condition body redirects -> jobs condition <> foldMap statement body <> foldMap argument redirects
      Begin body redirects -> foldMap statement body <> foldMap argument redirects
      If condition yes no redirects -> jobs condition <> foldMap statement yes <> foldMap statement no <> foldMap argument redirects
      Switch value cases redirects -> expression value <> foldMap caseItem cases <> foldMap argument redirects
      Break -> mempty
      Continue -> mempty
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
      Exec value arguments -> expression value <> foldMap argument arguments
      Decorated _ value -> command value
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
    argument (RedirectVal redirect) = case redirTarget redirect of
      RedirectFile value -> expression value
      RedirectTargetFD _ -> mempty
      RedirectClose -> mempty
    caseItem item = foldMap expression (casePatterns item) <> foldMap statement (caseBody item)
    assignment = foldMap expression . vaValue
    pipeline job = foldMap assignment (jpVariables job) <> statement (jpStatement job) <> foldMap continuation (jpCont job)
    continuation value = foldMap assignment (jpcVariables value) <> statement (jpcStatement value)
    conjunction value = pipeline (jcJob value) <> foldMap (\case JCAnd job -> pipeline job; JCOr job -> pipeline job) (jcContinuations value)
    jobs (MkFishJobList values) = foldMap conjunction values

data Sites = Sites [Text] [Text] Int

instance Semigroup Sites where
  Sites a b c <> Sites d e f = Sites (a <> d) (b <> e) (c + f)

instance Monoid Sites where
  mempty = Sites [] [] 0
