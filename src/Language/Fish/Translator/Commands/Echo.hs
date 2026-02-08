module Language.Fish.Translator.Commands.Echo
  ( translateEcho,
    translateEchoM,
  )
where

import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Args
  ( Arg,
    argExpr,
    renderArgs,
  )
import Language.Fish.Translator.Commands.Args (translateArgsM)
import Language.Fish.Translator.Hoist (Hoisted (..))
import Language.Fish.Translator.Hoist.Monad (HoistedM, hoistM)
import Language.Fish.Translator.Token (tokenHasExpansion, tokenToLiteralText)
import Language.Fish.Translator.Variables
  ( translateTokenToArg,
    translateTokenToListExpr,
    translateTokenToListExprM,
  )
import ShellCheck.AST

--------------------------------------------------------------------------------
-- Echo handling
--------------------------------------------------------------------------------

data EchoOptions = EchoOptions
  { echoNewline :: Bool,
    echoEscapes :: Bool,
    echoHadOptions :: Bool
  }
  deriving stock (Show, Eq)

defaultEchoOptions :: EchoOptions
defaultEchoOptions = EchoOptions True False False

parseEchoOptions :: [Token] -> Maybe (EchoOptions, [Token])
parseEchoOptions = go defaultEchoOptions
  where
    go opts [] = Just (opts, [])
    go opts (t : ts)
      | tokenHasExpansion t = Nothing
      | otherwise =
          let txt = tokenToLiteralText t
           in if T.isPrefixOf "-" txt
                && T.length txt > 1
                && T.all (\c -> c == 'n' || c == 'e' || c == 'E') (T.drop 1 txt)
                then
                  let opts' = T.foldl' applyOpt (opts {echoHadOptions = True}) (T.drop 1 txt)
                   in go opts' ts
                else Just (opts, t : ts)

    applyOpt acc c =
      case c of
        'n' -> acc {echoNewline = False}
        'e' -> acc {echoEscapes = True}
        'E' -> acc {echoEscapes = False}
        _ -> acc

echoFormat :: EchoOptions -> Text
echoFormat opts =
  let base = if echoEscapes opts then "%b" else "%s"
   in if echoNewline opts then base <> "\\n" else base

listExprToString :: FishExpr (TList TStr) -> FishExpr TStr
listExprToString = \case
  ExprListLiteral [] -> ExprLiteral ""
  ExprListLiteral [expr] -> expr
  other -> ExprJoinList other

translateTokensToListExpr :: [Token] -> FishExpr (TList TStr)
translateTokensToListExpr [] = ExprListLiteral []
translateTokensToListExpr [t] = translateTokenToListExpr t
translateTokensToListExpr (t : ts) =
  foldl' ExprListConcat (translateTokenToListExpr t) (map translateTokenToListExpr ts)

translateTokensToListExprM :: [Token] -> HoistedM (FishExpr (TList TStr))
translateTokensToListExprM =
  translateTokensToListExprHoisted

translateTokensToListExprHoisted :: [Token] -> HoistedM (FishExpr (TList TStr))
translateTokensToListExprHoisted tokens =
  case tokens of
    [] -> hoistM [] (ExprListLiteral [])
    _ -> do
      translated <- mapM translateTokenToListExprM tokens
      let Hoisted pre exprs = sequenceA translated
      case exprs of
        [] -> hoistM pre (ExprListLiteral [])
        (x : xs) -> hoistM pre (foldl' ExprListConcat x xs)

translateEcho :: [Token] -> [Arg] -> FishCommand TStatus
translateEcho plainArgs redirs =
  case parseEchoOptions plainArgs of
    Just (opts, rest)
      | echoEscapes opts ->
          let listExpr = translateTokensToListExpr rest
              msgExpr = listExprToString listExpr
              fmt = echoFormat opts
           in Command "printf" (renderArgs (argExpr (ExprLiteral fmt) : argExpr msgExpr : redirs))
      | otherwise ->
          let optExprs =
                if echoHadOptions opts && not (echoNewline opts)
                  then [argExpr (ExprLiteral "-n")]
                  else []
              argExprs = map translateTokenToArg rest
           in Command "echo" (renderArgs (optExprs ++ argExprs ++ redirs))
    _ ->
      let argExprs = map translateTokenToArg plainArgs
       in Command "echo" (renderArgs (argExprs ++ redirs))

translateEchoM :: [Token] -> [Arg] -> HoistedM (FishCommand TStatus)
translateEchoM plainArgs redirs =
  case parseEchoOptions plainArgs of
    Just (opts, rest)
      | echoEscapes opts -> do
          Hoisted pre listExpr <- translateTokensToListExprM rest
          let msgExpr = listExprToString listExpr
              fmt = echoFormat opts
              cmd = Command "printf" (renderArgs (argExpr (ExprLiteral fmt) : argExpr msgExpr : redirs))
          hoistM pre cmd
      | otherwise -> do
          Hoisted pre argExprs <- translateArgsM rest
          let optExprs =
                if echoHadOptions opts && not (echoNewline opts)
                  then [argExpr (ExprLiteral "-n")]
                  else []
          hoistM pre (Command "echo" (renderArgs (optExprs ++ argExprs ++ redirs)))
    _ -> do
      Hoisted pre argExprs <- translateArgsM plainArgs
      hoistM pre (Command "echo" (renderArgs (argExprs ++ redirs)))
