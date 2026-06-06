{-# LANGUAGE LambdaCase #-}

module Language.Fish.Translator.Cond
  ( Cond (..),
    condToCommand,
    condFromTokenWith,
    condFromTokenMWith,
    condUnaryCommand,
    condBinaryCommand,
    condNullaryCommand,
    testUnaryCommand,
    testBinaryCommand,
    testNullaryCommand,
    testNonZeroCommand,
  )
where

import Language.Fish.Translator.DSL
import Language.Fish.Translator.Hoist (Hoisted (..), beginIfNeeded)
import Language.Fish.Translator.Hoist.Monad (HoistedM, hoistM)
import Language.Fish.Translator.Statement (statusConjunction)
import ShellCheck.AST

data Cond
  = CondTrue
  | CondNot Cond
  | CondAnd Cond Cond
  | CondOr Cond Cond
  | CondTest (FishCommand TStatus)
  deriving stock (Eq, Show)

condToCommand :: Cond -> FishCommand TStatus
condToCommand = \case
  CondTrue -> Command "true" []
  CondNot inner -> Not (condToCommand inner)
  CondAnd l r -> statusConjunction ConjAnd (condToCommand l) (condToCommand r)
  CondOr l r -> statusConjunction ConjOr (condToCommand l) (condToCommand r)
  CondTest cmd -> cmd

condUnaryCommand :: Text -> FishExpr TStr -> FishCommand TStatus
condUnaryCommand = testUnaryCommand

condNullaryCommand :: FishExpr TStr -> FishCommand TStatus
condNullaryCommand = testNullaryCommand

condBinaryCommand :: Text -> FishExpr TStr -> FishExpr TStr -> FishCommand TStatus
condBinaryCommand op lhs rhs
  | op == "=~" = stringMatch "-qr"
  | op == "==" || op == "=" = stringMatch "-q"
  | op == "!=" = Not (stringMatch "-q")
  | otherwise = testBinaryCommand op lhs rhs
  where
    stringMatch flag =
      Command
        "string"
        [ ExprVal (ExprLiteral "match"),
          ExprVal (ExprLiteral flag),
          ExprVal (ExprLiteral "--"),
          ExprVal rhs,
          ExprVal lhs
        ]

testUnaryCommand :: (Typeable t) => Text -> FishExpr t -> FishCommand TStatus
testUnaryCommand op expr =
  Command "test" [ExprVal (ExprLiteral op), ExprVal expr]

testNullaryCommand :: (Typeable t) => FishExpr t -> FishCommand TStatus
testNullaryCommand expr =
  Command "test" [ExprVal expr]

testBinaryCommand :: (Typeable a, Typeable b) => Text -> FishExpr a -> FishExpr b -> FishCommand TStatus
testBinaryCommand op lhs rhs =
  Command
    "test"
    [ ExprVal lhs,
      ExprVal (ExprLiteral op),
      ExprVal rhs
    ]

testNonZeroCommand :: (Typeable t) => FishExpr t -> FishCommand TStatus
testNonZeroCommand expr =
  testBinaryCommand "-ne" expr (ExprLiteral "0")

condFromTokenWith ::
  (Token -> FishExpr TStr) ->
  (Token -> FishExpr TStr) ->
  (Token -> FishExpr TStr) ->
  Token ->
  Cond
condFromTokenWith translateExpr translateRegex literalExpr = go
  where
    go = \case
      TC_Group _ _ inner -> go inner
      TC_And _ _ _ l r -> CondAnd (go l) (go r)
      TC_Or _ _ _ l r -> CondOr (go l) (go r)
      TC_Unary _ _ "!" inner -> CondNot (go inner)
      TC_Unary _ _ op inner ->
        CondTest (condUnaryCommand (toText op) (translateExpr inner))
      TC_Binary _ _ op lhs rhs ->
        let opText = toText op
            rhsExpr =
              if opText == "=~"
                then translateRegex rhs
                else translateExpr rhs
         in CondTest (condBinaryCommand opText (translateExpr lhs) rhsExpr)
      TC_Nullary _ _ tok ->
        CondTest (condNullaryCommand (translateExpr tok))
      TC_Empty {} ->
        CondTrue
      other ->
        CondTest (condNullaryCommand (literalExpr other))

condFromTokenMWith ::
  (Token -> HoistedM (FishExpr TStr)) ->
  (Token -> HoistedM (FishExpr TStr)) ->
  (Token -> FishExpr TStr) ->
  Token ->
  HoistedM Cond
condFromTokenMWith translateExprM translateRegexM literalExpr = go
  where
    go = \case
      TC_Group _ _ inner -> go inner
      TC_And _ _ _ l r -> do
        MkHoisted _ condL <- go l
        MkHoisted _ condR <- go r
        hoistM [] (CondAnd condL condR)
      TC_Or _ _ _ l r -> do
        MkHoisted _ condL <- go l
        MkHoisted _ condR <- go r
        hoistM [] (CondOr condL condR)
      TC_Unary _ _ "!" inner -> do
        MkHoisted _ cond <- go inner
        hoistM [] (CondNot cond)
      TC_Unary _ _ op inner -> do
        MkHoisted pre expr <- translateExprM inner
        let cmd = beginIfNeeded pre (condUnaryCommand (toText op) expr)
        hoistM [] (CondTest cmd)
      TC_Binary _ _ op lhs rhs -> do
        let opText = toText op
        MkHoisted preL lhsExpr <- translateExprM lhs
        MkHoisted preR rhsExpr <-
          if opText == "=~"
            then translateRegexM rhs
            else translateExprM rhs
        let cmd = beginIfNeeded (preL <> preR) (condBinaryCommand opText lhsExpr rhsExpr)
        hoistM [] (CondTest cmd)
      TC_Nullary _ _ tok -> do
        MkHoisted pre expr <- translateExprM tok
        let cmd = beginIfNeeded pre (condNullaryCommand expr)
        hoistM [] (CondTest cmd)
      TC_Empty {} ->
        hoistM [] CondTrue
      other ->
        hoistM [] (CondTest (condNullaryCommand (literalExpr other)))
