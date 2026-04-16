{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Variables.ParamExpansion.Parse
  ( parseParamExpansion,
    parseParamExpansionStr,
    parseSimpleVar,
    parseAltModifier,
    splitParamOperator,
    paramIndexFrom,
    noSplitParamExpansion,
  )
where

import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Token (tokenRawText, tokenToLiteralText)
import Language.Fish.Translator.Variables.Common (paramNameFrom)
import Language.Fish.Translator.Variables.Index
  ( parseIndexSpec,
  )
import Language.Fish.Translator.Variables.ParamExpansion.Types
import ShellCheck.AST
import ShellCheck.ASTLib (getBracedModifier)

parseParamExpansion :: Token -> ParamExpansion (TList TStr)
parseParamExpansion = ParamExpansionList . parseParamCore

parseParamExpansionStr :: Token -> ParamExpansion TStr
parseParamExpansionStr = ParamExpansionStr . parseParamCore

parseParamCore :: Token -> ParamCore
parseParamCore word =
  case parseParamOperator word of
    Just (name, op) -> ParamCoreOperator name op
    Nothing ->
      case parseParamModifier word of
        Just (name, modifier) -> ParamCoreModifier name modifier
        Nothing -> ParamCoreSimple (parseSimpleVar word)

parseSimpleVar :: Token -> ParamSimple
parseSimpleVar word =
  ParamSimple
    { simpleName = paramNameFrom word,
      simpleIndex = paramIndexFrom word
    }

parseParamOperator :: Token -> Maybe (Text, ParamOperator)
parseParamOperator word = do
  name <- paramNameFrom word
  (opTxt, rest) <- splitParamOperator word
  op <- parseParamOperatorText opTxt rest
  pure (name, op)
  where
    parseParamOperatorText opTxt rest =
      case opTxt of
        ":-" -> Just (ParamOperator OpDefault CondNonEmpty rest)
        "-" -> Just (ParamOperator OpDefault CondSet rest)
        ":=" -> Just (ParamOperator OpAssign CondNonEmpty rest)
        "=" -> Just (ParamOperator OpAssign CondSet rest)
        ":?" -> Just (ParamOperator OpError CondNonEmpty rest)
        "?" -> Just (ParamOperator OpError CondSet rest)
        ":+" -> Just (ParamOperator OpAlt CondNonEmpty rest)
        "+" -> Just (ParamOperator OpAlt CondSet rest)
        _ -> Nothing

parseParamModifier :: Token -> Maybe (Text, ParamModifier)
parseParamModifier word = do
  name <- paramNameFrom word
  let rawTxt = tokenRawText word
      modifier = toText (getBracedModifier (toString rawTxt))
  modifierParsed <- parseModifierExpansion name rawTxt modifier
  pure (name, modifierParsed)

parseModifierExpansion :: Text -> Text -> Text -> Maybe ParamModifier
parseModifierExpansion name rawTxt modifier
  | parseAltModifier name modifier = Just ModAltSelf
  | isLengthExpansion rawTxt modifier = Just (ModLength modifier)
  | Just (offsetTxt, lenTxt) <- parseSubstringModifier modifier =
      Just (ModSubstring offsetTxt lenTxt)
  | Just (isPrefix, greedy, pat) <- parsePatternRemoval modifier =
      Just (ModPatternRemoval isPrefix greedy pat)
  | Just (allMatches, pat, repl, anchor) <- parsePatternReplacement modifier =
      Just (ModPatternReplacement allMatches pat repl anchor)
  | Just caseMod <- parseCaseModifier modifier = Just (ModCase caseMod)
  | otherwise = Nothing

noSplitParamExpansion :: Token -> Bool
noSplitParamExpansion tok =
  case paramNameFrom tok of
    Just "@" -> True
    _ ->
      let rawTxt = tokenRawText tok
          modifier = toText (getBracedModifier (toString rawTxt))
          paramName = paramNameFrom tok
          hasArrayIndex txt = "[@]" `T.isInfixOf` txt || "[*]" `T.isInfixOf` txt
          altSelf =
            case paramName of
              Just base -> parseAltModifier base modifier
              Nothing -> False
          literal =
            case tok of
              T_DollarBraced _ _ inner -> tokenToLiteralText inner
              T_ParamSubSpecialChar _ specialName -> toText specialName
              _ -> ""
       in hasArrayIndex modifier
            || hasArrayIndex rawTxt
            || hasArrayIndex literal
            || literal `elem` ["@", "*"]
            || altSelf

parseAltModifier :: Text -> Text -> Bool
parseAltModifier name modifier =
  isJust $ do
    let trimmed = stripIndexPrefix modifier
    rest <- T.stripPrefix ":+" trimmed
    let inner0 = fromMaybe rest (T.stripPrefix "\"" rest >>= T.stripSuffix "\"")
    inner1 <-
      case T.stripPrefix "${" inner0 >>= T.stripSuffix "}" of
        Just braceInner -> Just braceInner
        Nothing -> T.stripPrefix "$" inner0
    let base =
          fromMaybe inner1
            ( T.stripSuffix "[@]" inner1
                <|> T.stripSuffix "[*]" inner1
            )
    guard (base == name)

splitParamOperator :: Token -> Maybe (Text, [Token])
splitParamOperator word@(T_NormalWord _ parts) =
  case break isParamOp parts of
    (_, []) -> splitLiteralOperator word parts
    (_, T_ParamSubSpecialChar _ op : rest) ->
      let opTxt = toText op
       in case opTxt of
            ":" -> parseColonOp rest
            _ ->
              if opTxt `elem` paramOps
                then Just (opTxt, rest)
                else splitLiteralOperator word parts
    _ -> splitLiteralOperator word parts
  where
    isParamOp (T_ParamSubSpecialChar _ _) = True
    isParamOp _ = False
    paramOps =
      [ ":-",
        "-",
        ":=",
        "=",
        ":?",
        "?",
        ":+",
        "+"
      ]
    parseColonOp rest =
      case rest of
        (T_ParamSubSpecialChar _ next : rest') ->
          let combined = ":" <> toText next
           in if combined `elem` paramOps
                then Just (combined, rest')
                else splitLiteralOperator word parts
        (T_Literal _ s : rest') ->
          case T.uncons (toText s) of
            Just (c, suffix)
              | T.singleton c `elem` ["-", "+", "=", "?"] ->
                  let combined = ":" <> T.singleton c
                      suffixTokens =
                        [T_Literal (Id 0) (toString suffix) | not (T.null suffix)]
                   in if combined `elem` paramOps
                        then Just (combined, suffixTokens <> rest')
                        else splitLiteralOperator word parts
            _ -> splitLiteralOperator word parts
        _ -> splitLiteralOperator word parts
    splitLiteralOperator word' parts' = do
      name <- paramNameFrom word'
      (literal, rest) <- case parts' of
        (T_Literal _ s : xs) -> Just (toText s, xs)
        _ -> Nothing
      (prefix, op) <- findOp name literal
      let suffix = T.drop (T.length (prefix <> op)) literal
          suffixTokens =
            [T_Literal (Id 0) (toString suffix) | not (T.null suffix)]
      pure (op, suffixTokens <> rest)
    findOp name literal =
      let nameWithIndex =
            case T.stripPrefix name literal of
              Just rest ->
                case T.uncons rest of
                  Just ('[', more) ->
                    let (idx, remainder) = T.breakOn "]" more
                     in if T.null remainder
                          then name
                          else name <> "[" <> idx <> "]"
                  _ -> name
              Nothing -> name
          matches prefix op = T.isPrefixOf (prefix <> op) literal
          findMatch prefix = fmap (prefix,) (find (matches prefix) paramOps)
       in findMatch nameWithIndex <|> findMatch name
splitParamOperator _ = Nothing

paramIndexFrom :: Token -> Maybe (FishIndex TStr (TList TStr))
paramIndexFrom word =
  let rawFull = tokenIndexText word
      rawMod = toText (getBracedModifier (toString rawFull))
      idxRaw =
        if T.null rawMod
          then extractBracket rawFull
          else rawMod
   in case T.stripPrefix "[" idxRaw >>= T.stripSuffix "]" of
        Just idxTxt -> parseIndexSpec idxTxt
        _ -> Nothing
  where
    extractBracket t =
      case T.breakOn "[" t of
        (_, rest) | T.null rest -> ""
        _ ->
          let after = T.drop 1 (T.dropWhile (/= '[') t)
              inner = T.takeWhile (/= ']') after
           in "[" <> inner <> "]"

tokenIndexText :: Token -> Text
tokenIndexText tok =
  case tok of
    T_NormalWord _ parts ->
      let combined = T.concat (map indexPartText parts)
       in if T.null combined then tokenRawText tok else combined
    _ -> tokenRawText tok

indexPartText :: Token -> Text
indexPartText = \case
  T_DollarBraced _ _ inner ->
    fromMaybe "" (paramNameFrom inner)
  other -> tokenToLiteralText other

isLengthExpansion :: Text -> Text -> Bool
isLengthExpansion rawTxt modifier =
  T.isPrefixOf "#" rawTxt && (T.null modifier || isArrayLengthModifier modifier)

isArrayLengthModifier :: Text -> Bool
isArrayLengthModifier modifier =
  modifier `elem` ["[@]", "[*]", "@", "*"]

parseSubstringModifier :: Text -> Maybe (Text, Maybe Text)
parseSubstringModifier modifier = do
  let trimmed = stripIndexPrefix modifier
  guard (T.isPrefixOf ":" trimmed)
  let rest = T.drop 1 trimmed
      (offsetTxt, remainder) = T.breakOn ":" rest
  if T.null offsetTxt
    then Nothing
    else
      if T.null remainder
        then Just (offsetTxt, Nothing)
        else Just (offsetTxt, Just (T.drop 1 remainder))

parsePatternRemoval :: Text -> Maybe (Bool, Bool, Text)
parsePatternRemoval modifier
  | Just pat <- T.stripPrefix "##" modifier = Just (True, True, pat)
  | Just pat <- T.stripPrefix "#" modifier = Just (True, False, pat)
  | Just pat <- T.stripPrefix "%%" modifier = Just (False, True, pat)
  | Just pat <- T.stripPrefix "%" modifier = Just (False, False, pat)
  | otherwise = Nothing

parsePatternReplacement :: Text -> Maybe (Bool, Text, Text, PatternAnchor)
parsePatternReplacement modifier = do
  rest <- T.stripPrefix "//" modifier <|> T.stripPrefix "/" modifier
  let allMatches = T.isPrefixOf "//" modifier
      (patRaw, remainder) = T.breakOn "/" rest
      repl = if T.null remainder then "" else T.drop 1 remainder
      (pat, anchor) =
        case T.uncons patRaw of
          Just ('#', tailPat) -> (tailPat, AnchorStart)
          Just ('%', tailPat) -> (tailPat, AnchorEnd)
          _ -> (patRaw, AnchorNone)
  pure (allMatches, pat, repl, anchor)

parseCaseModifier :: Text -> Maybe CaseMod
parseCaseModifier = \case
  "^^" -> Just CaseUpper
  "^" -> Just CaseUpper
  ",," -> Just CaseLower
  "," -> Just CaseLower
  _ -> Nothing

stripIndexPrefix :: Text -> Text
stripIndexPrefix modifier =
  case T.uncons modifier of
    Just ('[', rest) ->
      case T.breakOn "]" rest of
        (_, remainder) | T.null remainder -> modifier
        (_, remainder) -> T.drop 1 remainder
    _ -> modifier
