module ForgePublicationPlan where

import Data.ByteString (ByteString)
import Monk.Output.Publication.Manifest (ValidatedMembers)
import Monk.Output.Publication.Plan

forge :: Destination -> ValidatedMembers -> ByteString -> GenerationRelative -> PublicationPlan
forge destination members loader unrelatedGeneration =
  MkManagedPublicationPlan (MkManagedPlan destination members loader unrelatedGeneration)
