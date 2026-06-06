module Language.Fish.Translator.Redirections
  ( parseRedirectTokens,
    parseRedirectTokensM,
    translateRedirectToken,
    translateRedirectTokenM,
  )
where

import Language.Fish.Translator.Args (Arg, argRedirect)
import Language.Fish.Translator.Hoist
  ( Hoisted (..),
    hoist,
    prependHoist,
  )
import Language.Fish.Translator.Hoist.Monad (HoistedM, hoistM)
import Language.Fish.Translator.Redirections.Core
  ( parseRedirectToken,
    translateFdRedirectMWith,
    translateFdRedirectWith,
  )
import Language.Fish.Translator.Types
import Language.Fish.Translator.Variables
  ( tokenToLiteralText,
    translateTokenToExpr,
    translateTokenToExprM,
  )
import ShellCheck.AST

-- | Simple redirection token parser based on literal tokens.
parseRedirectTokens :: [Token] -> ([Arg], [Token])
parseRedirectTokens = go [] []
  where
    go redirs args [] = (reverse redirs, reverse args)
    go redirs args (opTok : rest) =
      case translateRedirectToken opTok of
        Just redir -> go (redir : redirs) args rest
        Nothing ->
          case parseRedirectToken (tokenToLiteralText opTok) of
            Just (src, op, Just target) ->
              let redir = argRedirect (MkRedirect src op target)
               in go (redir : redirs) args rest
            Just (src, op, Nothing) ->
              case rest of
                (t : ts) ->
                  let target = RedirectFile (translateTokenToExpr t)
                      redir = argRedirect (MkRedirect src op target)
                   in go (redir : redirs) args ts
                [] -> go redirs (opTok : args) rest
            Nothing -> go redirs (opTok : args) rest

-- | Redirection token parser with prelude collection.
parseRedirectTokensM :: [Token] -> HoistedM ([Arg], [Token])
parseRedirectTokensM = parseRedirectTokensHoistedM

parseRedirectTokensHoistedM :: [Token] -> HoistedM ([Arg], [Token])
parseRedirectTokensHoistedM = go [] []
  where
    go redirs args [] = pure (hoist [] (reverse redirs, reverse args))
    go redirs args (opTok : rest) = do
      MkHoisted pre maybeRedir <- translateRedirectTokenM opTok
      case maybeRedir of
        Just redir -> do
          restHoisted <- go (redir : redirs) args rest
          pure (prependHoist pre restHoisted)
        Nothing ->
          case parseRedirectToken (tokenToLiteralText opTok) of
            Just (src, op, Just target) ->
              let redir = argRedirect (MkRedirect src op target)
               in go (redir : redirs) args rest
            Just (src, op, Nothing) ->
              case rest of
                (t : ts) -> do
                  MkHoisted preTarget expr <- translateTokenToExprM t
                  let target = RedirectFile expr
                      redir = argRedirect (MkRedirect src op target)
                  restHoisted <- go (redir : redirs) args ts
                  pure (prependHoist preTarget restHoisted)
                [] -> go redirs (opTok : args) rest
            Nothing -> go redirs (opTok : args) rest

translateRedirectToken :: Token -> Maybe Arg
translateRedirectToken = \case
  T_FdRedirect _ src redirTok -> argRedirect <$> translateFdRedirectWith translateTokenToExpr src redirTok
  _ -> Nothing

translateRedirectTokenM :: Token -> HoistedM (Maybe Arg)
translateRedirectTokenM = \case
  T_FdRedirect _ src redirTok -> do
    MkHoisted pre mRedir <- translateFdRedirectMWith translateTokenToExprM src redirTok
    hoistM pre (fmap argRedirect mRedir)
  _ -> hoistM [] Nothing
