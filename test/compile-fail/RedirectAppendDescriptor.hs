module RedirectAppendDescriptor where

import qualified Language.Fish.DSL as Fish

invalid = Fish.redirect Fish.stdout Fish.append (Fish.fdTarget 2)
