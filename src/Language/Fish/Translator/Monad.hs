{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fish.Translator.Monad
  ( TranslateM
  , TranslateState(..)
  , TranslateConfig(..)
  , defaultConfig
  , TranslateError(..)
  , Warning(..)
  , TranslationContext(..)
  , runTranslate
  , evalTranslate
  , addWarning
  ) where

import Relude
import qualified Data.Map.Strict as M
import Language.Fish.AST (SourceRange(..))

-- | Configuration flags controlling translation behavior
data TranslateConfig = TranslateConfig
  { strictMode       :: Bool        -- ^ Fail on unsupported constructs
  , preserveComments :: Bool        -- ^ Keep original comments, if available
  } deriving (Show, Eq)

defaultConfig :: TranslateConfig
defaultConfig = TranslateConfig
  { strictMode = False
  , preserveComments = True
  }

-- | Context flags describing where we are in the script
data TranslationContext = TranslationContext
  { inFunction :: Bool
  , inLoop     :: Bool
  } deriving (Show, Eq)

-- | Lightweight warning structure for non-fatal issues
data Warning = Warning
  { warnMessage :: Text
  , warnRange   :: Maybe SourceRange
  } deriving (Show, Eq)

-- | Translation errors for unsupported or invalid constructs
data TranslateError
  = Unsupported Text (Maybe SourceRange)
  | InternalError Text
  deriving (Show, Eq)

-- | Mutable translation state
data TranslateState = TranslateState
  { sourceMap :: M.Map SourceRange Text -- ^ map Fish fragments back to source hints
  , warnings  :: [Warning]
  , context   :: TranslationContext
  , config    :: TranslateConfig
  } deriving (Show, Eq)

type TranslateM = StateT TranslateState (Either TranslateError)

runTranslate :: TranslateConfig -> TranslateM a -> Either TranslateError (a, TranslateState)
runTranslate cfg m =
  let initState = TranslateState { sourceMap = mempty
                                 , warnings  = []
                                 , context   = TranslationContext False False
                                 , config    = cfg
                                 }
  in runStateT m initState

evalTranslate :: TranslateConfig -> TranslateM a -> Either TranslateError a
evalTranslate cfg m = fmap fst (runTranslate cfg m)

addWarning :: Text -> TranslateM ()
addWarning msg = modify' (\st -> st { warnings = warnings st <> [Warning msg Nothing] })

