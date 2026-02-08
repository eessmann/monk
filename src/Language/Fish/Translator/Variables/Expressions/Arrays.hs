{-# LANGUAGE LambdaCase #-}

module Language.Fish.Translator.Variables.Expressions.Arrays
  ( translateArrayElementsWith,
    translateArrayElementsMWith,
    translateArrayAssignmentWith,
    translateArrayAssignmentMWith,
  )
where

import Language.Fish.AST
import Language.Fish.Translator.Hoist (Hoisted (..))
import Language.Fish.Translator.Hoist.Monad (HoistedM, hoistM)
import Language.Fish.Translator.Monad (TranslateM)
import Language.Fish.Translator.Variables.Index (indexedVarText)
import ShellCheck.AST

translateArrayElementsWith ::
  (Token -> FishExpr (TList TStr)) ->
  [Token] ->
  FishExpr (TList TStr)
translateArrayElementsWith translateTokenToListExpr elems =
  case elems of
    [] -> ExprListLiteral []
    _ ->
      case map elementToListExpr elems of
        (x : xs) -> foldl' ExprListConcat x xs
        [] -> ExprListLiteral []
  where
    elementToListExpr t = case t of
      T_IndexedElement _ _ v -> translateTokenToListExpr v
      _ -> translateTokenToListExpr t

translateArrayElementsMWith ::
  (Token -> HoistedM (FishExpr (TList TStr))) ->
  [Token] ->
  HoistedM (FishExpr (TList TStr))
translateArrayElementsMWith translateTokenToListExprM =
  go
  where
    go :: [Token] -> HoistedM (FishExpr (TList TStr))
    go elems =
      case elems of
        [] -> hoistM [] (ExprListLiteral [])
        _ -> do
          parts <- mapM elementToListExpr elems
          let Hoisted pre exprs = sequenceA parts
          case exprs of
            [] -> hoistM pre (ExprListLiteral [])
            (x : xs) -> hoistM pre (foldl' ExprListConcat x xs)

    elementToListExpr t = case t of
      T_IndexedElement _ _ v -> translateTokenToListExprM v
      _ -> translateTokenToListExprM t

translateArrayAssignmentWith ::
  (Token -> FishExpr (TList TStr)) ->
  (Token -> FishExpr (TList TStr)) ->
  Text ->
  [SetFlag] ->
  [Token] ->
  [FishStatement]
translateArrayAssignmentWith translateTokenToListExpr translateTokenToListExprNoSplit name flags elems =
  case indexedElements elems of
    [] ->
      [Stmt (Set flags name (translateArrayElementsWith translateTokenToListExpr elems))]
    indexed ->
      map (indexedSet name) indexed
  where
    indexedElements = mapMaybe asIndexed
    asIndexed = \case
      T_IndexedElement _ indices value -> Just (indices, value)
      _ -> Nothing

    indexedSet var (idxTokens, value) =
      case indexedVarText var idxTokens of
        Just varTxt ->
          Stmt (Set flags varTxt (translateTokenToListExprNoSplit value))
        Nothing ->
          Stmt (Set flags var (translateTokenToListExprNoSplit value))

translateArrayAssignmentMWith ::
  (Token -> HoistedM (FishExpr (TList TStr))) ->
  (Token -> HoistedM (FishExpr (TList TStr))) ->
  Text ->
  [SetFlag] ->
  [Token] ->
  TranslateM [FishStatement]
translateArrayAssignmentMWith translateTokenToListExprM translateTokenToListExprMNoSplit name flags elems = do
  Hoisted pre stmts <- go
  pure (pre <> stmts)
  where
    go :: HoistedM [FishStatement]
    go =
      case indexedElements elems of
        [] -> do
          Hoisted pre expr <- translateArrayElementsMWith translateTokenToListExprM elems
          hoistM pre [Stmt (Set flags name expr)]
        indexed -> do
          parts <- mapM (indexedSet name) indexed
          let Hoisted pre stmts = sequenceA parts
          hoistM pre (concat stmts)

    indexedElements = mapMaybe asIndexed
    asIndexed = \case
      T_IndexedElement _ indices value -> Just (indices, value)
      _ -> Nothing

    indexedSet var (idxTokens, value) = do
      let target = fromMaybe var (indexedVarText var idxTokens)
      Hoisted pre expr <- translateTokenToListExprMNoSplit value
      hoistM pre [Stmt (Set flags target expr)]
