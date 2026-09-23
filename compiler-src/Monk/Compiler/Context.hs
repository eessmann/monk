{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

-- | One generative compilation owner reifies its finite target, entry and
-- provider contract before normalization. Every later phase retains it.
module Monk.Compiler.Context
  ( Phase (..),
    Target,
    Entry,
    Provider,
    Context,
    withContext,
    contextConfig,
  )
where

import Monk.Translation.Types

-- The kinds enumerate only supported static dimensions; ordinary runtime
-- values (paths, names, requirements) remain finite values in the context.
data Phase = Parsed | Normalized | Draft | Admitted

data Target = Bash53Fish46

data Entry = StandaloneEntry | SourceableEntry

data Provider = SearchPathProvider | ExplicitPathProvider | GenerationProvider

data STarget target where
  SBash53Fish46 :: STarget Bash53Fish46

data SEntry entry where
  SStandalone :: SEntry StandaloneEntry
  SSourceable :: SEntry SourceableEntry

data SProvider provider where
  SSearchPath :: SProvider SearchPathProvider
  SExplicitPath :: SProvider ExplicitPathProvider
  SGeneration :: SProvider GenerationProvider

type role Context nominal nominal nominal nominal

data Context (owner :: Type) target entry provider = Context (STarget target) (SEntry entry) (SProvider provider) TranslateConfig

-- | The owner is generative. The concrete entry/provider indices are reified
-- from this same immutable config before the materialized payload is admitted.
withContext :: TranslateConfig -> (forall owner target entry provider. Context owner target entry provider -> result) -> result
withContext config consume = case (targetProfile config, entryMode config, translationRuntime config) of
  (Bash53Signed64Fish46, Standalone, RuntimeOnPath) -> consume (Context SBash53Fish46 SStandalone SSearchPath config)
  (Bash53Signed64Fish46, Standalone, RuntimePath _) -> consume (Context SBash53Fish46 SStandalone SExplicitPath config)
  (Bash53Signed64Fish46, Standalone, RuntimeGeneration _) -> consume (Context SBash53Fish46 SStandalone SGeneration config)
  (Bash53Signed64Fish46, Sourceable, RuntimeOnPath) -> consume (Context SBash53Fish46 SSourceable SSearchPath config)
  (Bash53Signed64Fish46, Sourceable, RuntimePath _) -> consume (Context SBash53Fish46 SSourceable SExplicitPath config)
  (Bash53Signed64Fish46, Sourceable, RuntimeGeneration _) -> consume (Context SBash53Fish46 SSourceable SGeneration config)

contextConfig :: Context owner target entry provider -> TranslateConfig
contextConfig (Context _ _ _ config) = config
