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
import Language.Fish.Translator.Args (Arg, argRedirect)
import Language.Fish.Translator.Hoist
  ( Hoisted (..),
    hoist,
    prependHoist,
  )
import Language.Fish.Translator.Hoist.Monad (HoistedM, hoistM)
import Language.Fish.Translator.Variables
  ( translateTokenToExpr,
    translateTokenToExprM,
    tokenToLiteralText,
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
              let redir = argRedirect (Redirect src op target)
               in go (redir : redirs) args rest
            Just (src, op, Nothing) ->
              case rest of
                (t : ts) ->
                  let target = RedirectFile (translateTokenToExpr t)
                      redir = argRedirect (Redirect src op target)
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
      Hoisted pre maybeRedir <- translateRedirectTokenM opTok
      case maybeRedir of
        Just redir -> do
          restHoisted <- go (redir : redirs) args rest
          pure (prependHoist pre restHoisted)
        Nothing ->
          case parseRedirectToken (tokenToLiteralText opTok) of
            Just (src, op, Just target) ->
              let redir = argRedirect (Redirect src op target)
               in go (redir : redirs) args rest
            Just (src, op, Nothing) ->
              case rest of
                (t : ts) -> do
                  Hoisted preTarget expr <- translateTokenToExprM t
                  let target = RedirectFile expr
                      redir = argRedirect (Redirect src op target)
                  restHoisted <- go (redir : redirs) args ts
                  pure (prependHoist preTarget restHoisted)
                [] -> go redirs (opTok : args) rest
            Nothing -> go redirs (opTok : args) rest

translateRedirectToken :: Token -> Maybe Arg
translateRedirectToken = \case
  T_FdRedirect _ src redirTok -> argRedirect <$> translateFdRedirect src redirTok
  _ -> Nothing

translateRedirectTokenM :: Token -> HoistedM (Maybe Arg)
translateRedirectTokenM = \case
  T_FdRedirect _ src redirTok -> do
    Hoisted pre mRedir <- translateFdRedirectM src redirTok
    hoistM pre (fmap argRedirect mRedir)
  _ -> hoistM [] Nothing

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

translateFdRedirectM :: String -> Token -> HoistedM (Maybe Redirect)
translateFdRedirectM src redirTok =
  case redirTok of
    T_IoFile _ op file ->
      case redirectOpFromToken op of
        Just (redirOp, dir) -> do
          Hoisted pre expr <- translateTokenToExprM file
          let source = sourceFromFd src dir
          hoistM pre (Just (Redirect source redirOp (RedirectFile expr)))
        Nothing -> hoistM [] Nothing
    T_IoDuplicate _ op target ->
      case redirectOpFromToken op of
        Just (redirOp, dir) ->
          case redirectTargetFromDup target of
            Just targetRef -> do
              let source = sourceFromFd src dir
              hoistM [] (Just (Redirect source redirOp targetRef))
            Nothing -> hoistM [] Nothing
        Nothing -> hoistM [] Nothing
    T_HereString _ word -> do
      Hoisted pre expr <- hereStringExprM [word]
      hoistM pre (Just (Redirect (sourceFromFd src InputRedirect) RedirectIn (RedirectFile expr)))
    T_HereDoc _ _ _ _ toks -> do
      Hoisted pre expr <- hereDocExprM toks
      hoistM pre (Just (Redirect (sourceFromFd src InputRedirect) RedirectIn (RedirectFile expr)))
    _ -> hoistM [] Nothing

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
hereStringExpr = hereExpr "%s\\n"

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

hereDocExprM :: [Token] -> HoistedM (FishExpr TStr)
hereDocExprM = hereExprM "%s"

hereStringExprM :: [Token] -> HoistedM (FishExpr TStr)
hereStringExprM = hereExprM "%s\\n"

hereExprM :: Text -> [Token] -> HoistedM (FishExpr TStr)
hereExprM fmt toks = do
  Hoisted pre expr <- concatHereDocM toks
  let printfStmt =
        Stmt
          ( Command
              "printf"
              [ ExprVal (ExprLiteral fmt),
                ExprVal expr
              ]
          )
  hoistM pre (ExprProcessSubst (printfStmt NE.:| []))

concatHereDoc :: [Token] -> FishExpr TStr
concatHereDoc [] = ExprLiteral ""
concatHereDoc [t] = translateTokenToExpr t
concatHereDoc (t : ts) =
  foldl' ExprStringConcat (translateTokenToExpr t) (map translateTokenToExpr ts)

concatHereDocM :: [Token] -> HoistedM (FishExpr TStr)
concatHereDocM [] = pure (hoist [] (ExprLiteral ""))
concatHereDocM toks = do
  parts <- mapM translateTokenToExprM toks
  let Hoisted pre exprs = sequenceA parts
      expr =
        case exprs of
          [] -> ExprLiteral ""
          (x : xs) -> foldl' ExprStringConcat x xs
  hoistM pre expr

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
