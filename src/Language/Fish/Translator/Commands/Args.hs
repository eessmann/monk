module Language.Fish.Translator.Commands.Args
  ( translateArgsM,
    translateEvalM,
    concatWithSpaces,
  )
where

import Language.Fish.AST
import Language.Fish.Translator.Args (Arg)
import Language.Fish.Translator.Hoist (Hoisted (..))
import Language.Fish.Translator.Hoist.Monad (HoistedM, hoistM)
import Language.Fish.Translator.Variables
  ( translateTokenToArgM,
    translateTokenToExprM,
  )
import ShellCheck.AST (Token)

translateArgsM :: [Token] -> HoistedM [Arg]
translateArgsM = translateArgsHoistedM

translateArgsHoistedM :: [Token] -> HoistedM [Arg]
translateArgsHoistedM toks = do
  parts <- mapM translateTokenToArgM toks
  let Hoisted pre exprs = sequenceA parts
  hoistM pre exprs

translateEvalM :: [Token] -> HoistedM (FishExpr TStr)
translateEvalM = translateEvalHoistedM

translateEvalHoistedM :: [Token] -> HoistedM (FishExpr TStr)
translateEvalHoistedM toks = do
  parts <- mapM translateTokenToExprM toks
  let Hoisted pre exprs = sequenceA parts
  hoistM pre (concatWithSpaces exprs)

concatWithSpaces :: [FishExpr TStr] -> FishExpr TStr
concatWithSpaces [] = ExprLiteral ""
concatWithSpaces [e] = e
concatWithSpaces (e : es) = foldl' (\acc x -> ExprStringConcat (ExprStringConcat acc (ExprLiteral " ")) x) e es
