module CrossScopeReturnTarget where

import Language.Bash.Plan.Control
import Monk.Translation.Types (EntryMode (Sourceable))

-- The materializer's actual exit consumer requires the current root.
invalid :: Maybe ReturnRole
invalid = withEntryControl Sourceable $ \source -> do
  target <- returnTarget source
  withChildControl source $ \_ child ->
    Just (consumeReturn (rootWitness child) target)
