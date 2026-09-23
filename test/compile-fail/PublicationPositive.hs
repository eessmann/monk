module PublicationPositive where

import qualified Data.ByteString.Char8 as Bytes
import Monk.Output.Publication (PublicationFailure)
import Monk.Output.Publication.Plan (PublicationPlan, planManagedPublication)

valid :: Either PublicationFailure PublicationPlan
valid = planManagedPublication "entry.fish" [("main.fish", Bytes.pack "echo hello")] (Bytes.pack "source generation")
