{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Redirections
  ( parseRedirectTokens
  ) where

import Relude
import Language.Fish.AST
import Language.Fish.Translator.Variables
import ShellCheck.AST

-- | Very simple redirection token parser based on literal tokens
parseRedirectTokens :: [Token] -> ([ExprOrRedirect], [Token])
parseRedirectTokens [] = ([], [])
parseRedirectTokens (opTok:rest) =
  let opTxt = tokenToLiteralText opTok
   in case opTxt of
        ">"  -> case rest of
                  (t:ts) -> let (more, rem) = parseRedirectTokens ts
                                e = translateTokenToExpr t
                             in (RedirectVal RedirectOut e : more, rem)
                  []     -> ([], opTok:rest)
        ">>" -> case rest of
                  (t:ts) -> let (more, rem) = parseRedirectTokens ts
                                e = translateTokenToExpr t
                             in (RedirectVal RedirectOutAppend e : more, rem)
                  []     -> ([], opTok:rest)
        "<"  -> case rest of
                  (t:ts) -> let (more, rem) = parseRedirectTokens ts
                                e = translateTokenToExpr t
                             in (RedirectVal RedirectIn e : more, rem)
                  []     -> ([], opTok:rest)
        "2>" -> case rest of
                  (t:ts) -> let (more, rem) = parseRedirectTokens ts
                                e = translateTokenToExpr t
                             in (RedirectVal RedirectErr e : more, rem)
                  []     -> ([], opTok:rest)
        "2>>"-> case rest of
                  (t:ts) -> let (more, rem) = parseRedirectTokens ts
                                e = translateTokenToExpr t
                             in (RedirectVal RedirectErrAppend e : more, rem)
                  []     -> ([], opTok:rest)
        "&>" -> case rest of
                  (t:ts) -> let (more, rem) = parseRedirectTokens ts
                                e = translateTokenToExpr t
                             in (RedirectVal RedirectBoth e : more, rem)
                  []     -> ([], opTok:rest)
        "&>>"-> case rest of
                  (t:ts) -> let (more, rem) = parseRedirectTokens ts
                                e = translateTokenToExpr t
                             in (RedirectVal RedirectBothAppend e : more, rem)
                  []     -> ([], opTok:rest)
        _     -> ([], opTok:rest)

