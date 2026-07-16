{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Redirections.Core
  ( translateFdRedirectWith,
    translateFdRedirectMWith,
    parseRedirectToken,
  )
where

import Data.Char (isDigit)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Fish.Translator.Hoist (Hoisted (..), hoist)
import Language.Fish.Translator.Hoist.Monad (HoistedM, hoistM)
import Language.Fish.Translator.Monad
  ( WarningCode (HereStringIssue),
    unsupported,
  )
import Language.Fish.Translator.Types
import ShellCheck.AST

translateFdRedirectWith ::
  (Token -> FishExpr TStr) ->
  String ->
  Token ->
  Maybe Redirect
translateFdRedirectWith translateExpr src = \case
  T_IoFile _ op file -> do
    (redirOp, dir) <- redirectOpFromToken op
    let source = sourceFromFd src dir
    pure (MkRedirect source redirOp (RedirectFile (translateExpr file)))
  T_IoDuplicate _ op target -> do
    (redirOp, dir) <- redirectOpFromToken op
    targetRef <- redirectTargetFromDup target
    pure (MkRedirect (sourceFromFd src dir) redirOp targetRef)
  T_HereString _ word ->
    pure (MkRedirect (sourceFromFd src InputRedirect) RedirectIn (RedirectFile (hereStringExpr translateExpr [word])))
  T_HereDoc _ _ _ _ toks ->
    pure (MkRedirect (sourceFromFd src InputRedirect) RedirectIn (RedirectFile (hereDocExpr translateExpr toks)))
  _ -> Nothing

translateFdRedirectMWith ::
  (Token -> HoistedM (FishExpr TStr)) ->
  String ->
  Token ->
  HoistedM (Maybe Redirect)
translateFdRedirectMWith translateExprM src = \case
  T_IoFile _ op file ->
    case redirectOpFromToken op of
      Just (redirOp, dir) -> do
        MkHoisted pre expr <- translateExprM file
        let source = sourceFromFd src dir
        hoistM pre (Just (MkRedirect source redirOp (RedirectFile expr)))
      Nothing -> hoistM [] Nothing
  T_IoDuplicate _ op target ->
    case redirectOpFromToken op of
      Just (redirOp, dir) ->
        hoistM [] (MkRedirect (sourceFromFd src dir) redirOp <$> redirectTargetFromDup target)
      Nothing -> hoistM [] Nothing
  T_HereString _ word -> do
    unsupported HereStringIssue (Just "here-string is approximated with an in-memory printf pipe")
    MkHoisted pre expr <- hereStringExprM translateExprM [word]
    hoistM pre (Just (MkRedirect (sourceFromFd src InputRedirect) RedirectIn (RedirectFile expr)))
  T_HereDoc _ _ _ _ toks -> do
    MkHoisted pre expr <- hereDocExprM translateExprM toks
    hoistM pre (Just (MkRedirect (sourceFromFd src InputRedirect) RedirectIn (RedirectFile expr)))
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

hereDocExpr ::
  (Token -> FishExpr TStr) ->
  [Token] ->
  FishExpr TStr
hereDocExpr = hereExpr "%s"

hereStringExpr ::
  (Token -> FishExpr TStr) ->
  [Token] ->
  FishExpr TStr
hereStringExpr = hereExpr "%s\\n"

hereExpr ::
  Text ->
  (Token -> FishExpr TStr) ->
  [Token] ->
  FishExpr TStr
hereExpr fmt translateExpr toks =
  let expr = concatHereDoc translateExpr toks
      printfStmt =
        Stmt
          ( Command
              "printf"
              [ ExprVal (ExprLiteral fmt),
                ExprVal expr
              ]
          )
   in ExprProcessSubst (printfStmt NE.:| [])

hereDocExprM ::
  (Token -> HoistedM (FishExpr TStr)) ->
  [Token] ->
  HoistedM (FishExpr TStr)
hereDocExprM = hereExprM "%s"

hereStringExprM ::
  (Token -> HoistedM (FishExpr TStr)) ->
  [Token] ->
  HoistedM (FishExpr TStr)
hereStringExprM = hereExprM "%s\\n"

hereExprM ::
  Text ->
  (Token -> HoistedM (FishExpr TStr)) ->
  [Token] ->
  HoistedM (FishExpr TStr)
hereExprM fmt translateExprM toks = do
  MkHoisted pre expr <- concatHereDocM translateExprM toks
  let printfStmt =
        Stmt
          ( Command
              "printf"
              [ ExprVal (ExprLiteral fmt),
                ExprVal expr
              ]
          )
  hoistM pre (ExprProcessSubst (printfStmt NE.:| []))

concatHereDoc ::
  (Token -> FishExpr TStr) ->
  [Token] ->
  FishExpr TStr
concatHereDoc _ [] = ExprLiteral ""
concatHereDoc translateExpr (t : ts) =
  foldl' ExprStringConcat (translateExpr t) (map translateExpr ts)

concatHereDocM ::
  (Token -> HoistedM (FishExpr TStr)) ->
  [Token] ->
  HoistedM (FishExpr TStr)
concatHereDocM _ [] = pure (hoist [] (ExprLiteral ""))
concatHereDocM translateExprM toks = do
  parts <- mapM translateExprM toks
  let MkHoisted pre exprs = sequenceA parts
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
