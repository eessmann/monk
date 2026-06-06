{-# LANGUAGE OverloadedStrings #-}

module TestSupport
  ( trueCond,
    translateScript,
    translateScriptMaybe,
  )
where

import Monk.AST (JobList, command, condition)
import Monk.Translation
import Test.Tasty.HUnit as H

trueCond :: JobList
trueCond = condition (command "true" [])

translateScript :: Text -> IO Text
translateScript script = do
  result <- parseBashScript "spec.sh" script
  case translateParseResult defaultConfig result of
    Left err -> H.assertFailure ("translateParseResult failed: " <> show err) >> pure ""
    Right translation -> pure (renderTranslation translation)

translateScriptMaybe :: Text -> IO (Maybe Text)
translateScriptMaybe script = do
  result <- parseBashScript "spec.sh" script
  case translateParseResult defaultConfig result of
    Left _ -> pure Nothing
    Right translation -> pure (Just (renderTranslation translation))
