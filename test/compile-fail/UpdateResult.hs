module UpdateResult where

import Monk.AST (script)
import Monk.Translation

forge :: TranslationResult -> TranslationResult
forge value = value {translationScript = script []}
