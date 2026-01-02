{-# LANGUAGE OverloadedStrings #-}

module TestSupport
  ( trueCond
  , translateScript
  , translateScriptMaybe
  ) where

import Monk
import Test.Tasty.HUnit as H
import qualified Data.List.NonEmpty as NE

trueCond :: FishJobList
trueCond =
  FishJobList
    ( FishJobConjunction
        Nothing
        (FishJobPipeline False [] (Stmt (Command "true" [])) [] False)
        []
        NE.:| []
    )

translateScript :: Text -> IO Text
translateScript script = do
  result <- parseBashScript "spec.sh" script
  case translateParseResult defaultConfig result of
    Left err -> H.assertFailure ("translateParseResult failed: " <> show err) >> pure ""
    Right (stmt, _) -> pure (renderFish [stmt])

translateScriptMaybe :: Text -> IO (Maybe Text)
translateScriptMaybe script = do
  result <- parseBashScript "spec.sh" script
  case translateParseResult defaultConfig result of
    Left _ -> pure Nothing
    Right (stmt, _) -> pure (Just (renderFish [stmt]))
