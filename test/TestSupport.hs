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
  result <- translateBashScript defaultConfig "spec.sh" script
  case result of
    Left err -> H.assertFailure ("translateBashScript failed: " <> show err) >> pure ""
    Right translation -> pure (renderTranslation translation)

translateScriptMaybe :: Text -> IO (Maybe Text)
translateScriptMaybe script = do
  result <- translateBashScript defaultConfig "spec.sh" script
  case result of
    Left _ -> pure Nothing
    Right translation -> pure (Just (renderTranslation translation))
