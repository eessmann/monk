{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Redirections
  ( parseRedirectTokens
  , translateRedirectToken
  ) where

import Data.Char (isDigit)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Language.Fish.AST
import Language.Fish.Translator.Variables
import ShellCheck.AST

-- | Simple redirection token parser based on literal tokens.
parseRedirectTokens :: [Token] -> ([ExprOrRedirect], [Token])
parseRedirectTokens = go [] []
  where
    go redirs args [] = (reverse redirs, reverse args)
    go redirs args (opTok:rest) =
      case translateRedirectToken opTok of
        Just redir -> go (redir : redirs) args rest
        Nothing ->
          case parseRedirectToken (tokenToLiteralText opTok) of
            Just (src, op, Just target) ->
              let redir = RedirectVal (Redirect src op target)
              in go (redir : redirs) args rest
            Just (src, op, Nothing) ->
              case rest of
                (t:ts) ->
                  let target = RedirectFile (translateTokenToExpr t)
                      redir = RedirectVal (Redirect src op target)
                  in go (redir : redirs) args ts
                [] -> go redirs (opTok : args) rest
            Nothing -> go redirs (opTok : args) rest

translateRedirectToken :: Token -> Maybe ExprOrRedirect
translateRedirectToken = \case
  T_FdRedirect _ src redirTok -> RedirectVal <$> translateFdRedirect src redirTok
  _ -> Nothing

translateFdRedirect :: String -> Token -> Maybe Redirect
translateFdRedirect src redirTok =
  case redirTok of
    T_IoFile _ op file -> do
      (redirOp, dir) <- redirectOpFromToken op
      let source = sourceFromFd src dir
      pure (Redirect source redirOp (RedirectFile (translateTokenToExpr file)))
    T_IoDuplicate _ op target -> do
      (redirOp, dir) <- redirectOpFromToken op
      let source = sourceFromFd src dir
      targetRef <- redirectTargetFromDup target
      pure (Redirect source redirOp targetRef)
    T_HereString _ word -> do
      let expr = hereStringExpr [word]
      pure (Redirect (sourceFromFd src InputRedirect) RedirectIn (RedirectFile expr))
    T_HereDoc _ _ _ _ toks -> do
      let expr = hereDocExpr toks
      pure (Redirect (sourceFromFd src InputRedirect) RedirectIn (RedirectFile expr))
    _ -> Nothing

data RedirectDir = InputRedirect | OutputRedirect

redirectOpFromToken :: Token -> Maybe (RedirectOp, RedirectDir)
redirectOpFromToken = \case
  T_Less {} -> Just (RedirectIn, InputRedirect)
  T_Greater {} -> Just (RedirectOut, OutputRedirect)
  T_DGREAT {} -> Just (RedirectOutAppend, OutputRedirect)
  T_CLOBBER {} -> Just (RedirectClobber, OutputRedirect)
  T_LESSGREAT {} -> Just (RedirectReadWrite, InputRedirect)
  T_GREATAND {} -> Just (RedirectOut, OutputRedirect)
  T_LESSAND {} -> Just (RedirectIn, InputRedirect)
  _ -> Nothing

sourceFromFd :: String -> RedirectDir -> RedirectSource
sourceFromFd src dir =
  case src of
    "" -> case dir of
            InputRedirect -> RedirectStdin
            OutputRedirect -> RedirectStdout
    "&" -> RedirectBoth
    _ | Just n <- readMaybe src -> RedirectFD n
    _ -> RedirectStdout

redirectTargetFromDup :: String -> Maybe RedirectTarget
redirectTargetFromDup tgt =
  case tgt of
    "-" -> Just RedirectClose
    _ | Just n <- readMaybe tgt -> Just (RedirectTargetFD n)
    _ -> Nothing

hereDocExpr :: [Token] -> FishExpr TStr
hereDocExpr = hereExpr "%s"

hereStringExpr :: [Token] -> FishExpr TStr
hereStringExpr = hereExpr "%s\n"

hereExpr :: Text -> [Token] -> FishExpr TStr
hereExpr fmt toks =
  let expr = concatHereDoc toks
      printfStmt = Stmt (Command "printf"
        [ ExprVal (ExprLiteral fmt)
        , ExprVal expr
        ])
  in ExprProcessSubst (printfStmt NE.:| [])

concatHereDoc :: [Token] -> FishExpr TStr
concatHereDoc [] = ExprLiteral ""
concatHereDoc [t] = translateTokenToExpr t
concatHereDoc (t:ts) =
  foldl' ExprStringConcat (translateTokenToExpr t) (map translateTokenToExpr ts)

parseRedirectToken :: Text -> Maybe (RedirectSource, RedirectOp, Maybe RedirectTarget)
parseRedirectToken txt =
  parseDupOut <|> parseDupIn <|> parseSimple
  where
    parseDupOut = parseDup ">&" RedirectOut RedirectStdout
    parseDupIn = parseDup "<&" RedirectIn RedirectStdin

    parseDup sym op defaultSrc = do
      let (fdTxt, rest) = T.breakOn sym txt
      if T.null rest
        then Nothing
        else do
          let targetTxt = T.drop (T.length sym) rest
          if T.null targetTxt
            then Nothing
            else do
              src <- parseSource fdTxt defaultSrc
              target <- parseTarget targetTxt
              pure (src, op, Just target)

    parseSimple =
      case txt of
        "&>" -> Just (RedirectBoth, RedirectOut, Nothing)
        "&>>" -> Just (RedirectBoth, RedirectOutAppend, Nothing)
        ">" -> Just (RedirectStdout, RedirectOut, Nothing)
        ">>" -> Just (RedirectStdout, RedirectOutAppend, Nothing)
        ">|" -> Just (RedirectStdout, RedirectClobber, Nothing)
        "<" -> Just (RedirectStdin, RedirectIn, Nothing)
        "<>" -> Just (RedirectStdin, RedirectReadWrite, Nothing)
        _ -> parseWithFdPrefix txt

    parseWithFdPrefix t = do
      let (fdTxt, rest) = T.span isDigit t
      if T.null fdTxt
        then Nothing
        else do
          fd <- readMaybe (toString fdTxt)
          op <- case rest of
            ">" -> Just RedirectOut
            ">>" -> Just RedirectOutAppend
            ">|" -> Just RedirectClobber
            "<" -> Just RedirectIn
            "<>" -> Just RedirectReadWrite
            _ -> Nothing
          pure (RedirectFD fd, op, Nothing)

    parseSource fdTxt defaultSrc
      | T.null fdTxt = Just defaultSrc
      | otherwise = RedirectFD <$> readMaybe (toString fdTxt)

    parseTarget t
      | t == "-" = Just RedirectClose
      | otherwise = RedirectTargetFD <$> readMaybe (toString t)
