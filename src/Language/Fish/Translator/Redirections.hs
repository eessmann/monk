{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Redirections
  ( parseRedirectTokens,
    parseRedirectTokensM,
    translateRedirectToken,
    translateRedirectTokenM,
  )
where

import Data.Char (isDigit)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Monad (TranslateM)
import Language.Fish.Translator.Variables
  ( translateTokenToExpr,
    translateTokenToExprM,
    tokenToLiteralText,
  )
import ShellCheck.AST

-- | Simple redirection token parser based on literal tokens.
parseRedirectTokens :: [Token] -> ([ExprOrRedirect], [Token])
parseRedirectTokens = go [] []
  where
    go redirs args [] = (reverse redirs, reverse args)
    go redirs args (opTok : rest) =
      case translateRedirectToken opTok of
        Just redir -> go (redir : redirs) args rest
        Nothing ->
          case parseRedirectToken (tokenToLiteralText opTok) of
            Just (src, op, Just target) ->
              let redir = RedirectVal (Redirect src op target)
               in go (redir : redirs) args rest
            Just (src, op, Nothing) ->
              case rest of
                (t : ts) ->
                  let target = RedirectFile (translateTokenToExpr t)
                      redir = RedirectVal (Redirect src op target)
                   in go (redir : redirs) args ts
                [] -> go redirs (opTok : args) rest
            Nothing -> go redirs (opTok : args) rest

-- | Redirection token parser with prelude collection.
parseRedirectTokensM :: [Token] -> TranslateM ([FishStatement], [ExprOrRedirect], [Token])
parseRedirectTokensM = go [] [] []
  where
    go pre redirs args [] = pure (pre, reverse redirs, reverse args)
    go pre redirs args (opTok : rest) = do
      mRedir <- translateRedirectTokenM opTok
      case mRedir of
        Just (pre', redir) -> go (pre <> pre') (redir : redirs) args rest
        Nothing ->
          case parseRedirectToken (tokenToLiteralText opTok) of
            Just (src, op, Just target) ->
              let redir = RedirectVal (Redirect src op target)
               in go pre (redir : redirs) args rest
            Just (src, op, Nothing) ->
              case rest of
                (t : ts) -> do
                  (preT, expr) <- translateTokenToExprM t
                  let target = RedirectFile expr
                      redir = RedirectVal (Redirect src op target)
                  go (pre <> preT) (redir : redirs) args ts
                [] -> go pre redirs (opTok : args) rest
            Nothing -> go pre redirs (opTok : args) rest

translateRedirectToken :: Token -> Maybe ExprOrRedirect
translateRedirectToken = \case
  T_FdRedirect _ src redirTok -> RedirectVal <$> translateFdRedirect src redirTok
  _ -> Nothing

translateRedirectTokenM :: Token -> TranslateM (Maybe ([FishStatement], ExprOrRedirect))
translateRedirectTokenM = \case
  T_FdRedirect _ src redirTok -> do
    mRedir <- translateFdRedirectM src redirTok
    pure (fmap (\(pre, redir) -> (pre, RedirectVal redir)) mRedir)
  _ -> pure Nothing

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

translateFdRedirectM :: String -> Token -> TranslateM (Maybe ([FishStatement], Redirect))
translateFdRedirectM src redirTok =
  case redirTok of
    T_IoFile _ op file ->
      case redirectOpFromToken op of
        Just (redirOp, dir) -> do
          (pre, expr) <- translateTokenToExprM file
          let source = sourceFromFd src dir
          pure (Just (pre, Redirect source redirOp (RedirectFile expr)))
        Nothing -> pure Nothing
    T_IoDuplicate _ op target ->
      case redirectOpFromToken op of
        Just (redirOp, dir) ->
          case redirectTargetFromDup target of
            Just targetRef -> do
              let source = sourceFromFd src dir
              pure (Just ([], Redirect source redirOp targetRef))
            Nothing -> pure Nothing
        Nothing -> pure Nothing
    T_HereString _ word -> do
      (pre, expr) <- hereStringExprM [word]
      pure (Just (pre, Redirect (sourceFromFd src InputRedirect) RedirectIn (RedirectFile expr)))
    T_HereDoc _ _ _ _ toks -> do
      (pre, expr) <- hereDocExprM toks
      pure (Just (pre, Redirect (sourceFromFd src InputRedirect) RedirectIn (RedirectFile expr)))
    _ -> pure Nothing

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
      printfStmt =
        Stmt
          ( Command
              "printf"
              [ ExprVal (ExprLiteral fmt),
                ExprVal expr
              ]
          )
   in ExprProcessSubst (printfStmt NE.:| [])

hereDocExprM :: [Token] -> TranslateM ([FishStatement], FishExpr TStr)
hereDocExprM = hereExprM "%s"

hereStringExprM :: [Token] -> TranslateM ([FishStatement], FishExpr TStr)
hereStringExprM = hereExprM "%s\n"

hereExprM :: Text -> [Token] -> TranslateM ([FishStatement], FishExpr TStr)
hereExprM fmt toks = do
  (pre, expr) <- concatHereDocM toks
  let printfStmt =
        Stmt
          ( Command
              "printf"
              [ ExprVal (ExprLiteral fmt),
                ExprVal expr
              ]
          )
  pure (pre, ExprProcessSubst (printfStmt NE.:| []))

concatHereDoc :: [Token] -> FishExpr TStr
concatHereDoc [] = ExprLiteral ""
concatHereDoc [t] = translateTokenToExpr t
concatHereDoc (t : ts) =
  foldl' ExprStringConcat (translateTokenToExpr t) (map translateTokenToExpr ts)

concatHereDocM :: [Token] -> TranslateM ([FishStatement], FishExpr TStr)
concatHereDocM [] = pure ([], ExprLiteral "")
concatHereDocM toks = do
  parts <- mapM translateTokenToExprM toks
  let pre = concatMap fst parts
      exprs = map snd parts
      expr =
        case exprs of
          [] -> ExprLiteral ""
          (x : xs) -> foldl' ExprStringConcat x xs
  pure (pre, expr)

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
