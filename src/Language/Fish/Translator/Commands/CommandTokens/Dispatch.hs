{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Commands.CommandTokens.Dispatch
  ( translateCommandTokens,
    translateCommandTokensM,
  )
where

import Data.Text qualified as T
import Language.Fish.AST
import Language.Fish.Translator.Commands.Args (translateArgsM)
import Language.Fish.Translator.Commands.CommandTokens.Core
  ( translateCommandTokensWithoutTime,
    translateCommandTokensWithoutTimeM,
  )
import Language.Fish.Translator.Commands.CommandTokens.Status (translateTimeReserved)
import Language.Fish.Translator.Hoist (Hoisted (..))
import Language.Fish.Translator.Hoist.Monad (HoistedM, hoistM)
import Language.Fish.Translator.Pipeline (applyPipefailIfEnabled)
import Language.Fish.Translator.Redirections (parseRedirectTokens, parseRedirectTokensM)
import Language.Fish.Translator.Token (tokenToLiteralText)
import ShellCheck.AST

--------------------------------------------------------------------------------
-- Command translation (time-aware wrapper)
--------------------------------------------------------------------------------

translateCommandTokens :: [Token] -> Maybe (FishCommand TStatus)
translateCommandTokens cmdTokens =
  case cmdTokens of
    [] -> Nothing
    (c : args) ->
      let name = tokenToLiteralText c
          (_redirs, plainArgs) = parseRedirectTokens args
       in if T.null name
            then Nothing
            else case translateTimeReserved name plainArgs of
              Just timedCmd -> Just timedCmd
              Nothing -> translateCommandTokensWithoutTime cmdTokens

translateCommandTokensM :: [Token] -> HoistedM (Maybe (FishCommand TStatus))
translateCommandTokensM =
  translateCommandTokensHoistedM

translateCommandTokensHoistedM :: [Token] -> HoistedM (Maybe (FishCommand TStatus))
translateCommandTokensHoistedM cmdTokens =
  case cmdTokens of
    [] -> hoistM [] Nothing
    (c : args) -> do
      let name = tokenToLiteralText c
      Hoisted preRedirs (_redirs, plainArgs) <- parseRedirectTokensM args
      if T.null name
        then do
          Hoisted preArgs _ <- translateArgsM plainArgs
          hoistM (preRedirs <> preArgs) Nothing
        else case translateTimeReserved name plainArgs of
          Just timedCmd -> do
            timedCmd' <- applyPipefailIfEnabled timedCmd
            hoistM preRedirs (Just timedCmd')
          Nothing -> do
            Hoisted pre cmd <- translateCommandTokensWithoutTimeM (c : args)
            hoistM (preRedirs <> pre) cmd
