-- | Compact portable evidence without discarding its source identity.
module Monk.Tooling.Summary (compact, summaryReport) where

import Data.Aeson (Value (..), eitherDecodeStrict', object, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Text qualified as T
import Monk.Runtime.Digest (sha256)

compact :: Value -> Value
compact value = case value of
  Object fields -> Object (KM.map compact (KM.filterWithKey keepField fields))
  Array values -> Array (fmap compact values)
  other -> other
  where
    keepField key _ = key /= "observations" && key /= "base64" && not ("_base64" `T.isSuffixOf` K.toText key)

summaryReport :: FilePath -> B.ByteString -> Either String Value
summaryReport sourcePath raw = do
  parsed <- eitherDecodeStrict' raw
  case compact parsed of
    Object fields ->
      Right $ Object $ KM.insert "raw_report" sourceIdentity fields
    _ -> Left "portable evidence report must be a JSON object"
  where
    sourceIdentity =
      object
        [ "path" .= sourcePath,
          "sha256" .= C.unpack (sha256 raw),
          "scope" .= ("Complete raw streams and per-sample observations are retained locally; this summary preserves hashes, outcomes and sample timings." :: Text)
        ]
