module Language.Fish.Translator.Builtins
  ( translateLocalCommand,
    translateExportCommand,
    translateDeclareCommand,
    translateReadonlyCommand,
    translateShiftCommand,
    translateUnsetCommand,
    translateTrapCommand,
    translateScopedAssignment,
  )
where

import Language.Fish.Translator.Builtins.Declare
  ( translateDeclareCommand,
    translateReadonlyCommand,
  )
import Language.Fish.Translator.Builtins.Scope
  ( translateExportCommand,
    translateLocalCommand,
    translateScopedAssignment,
  )
import Language.Fish.Translator.Builtins.Shift (translateShiftCommand)
import Language.Fish.Translator.Builtins.Trap (translateTrapCommand)
import Language.Fish.Translator.Builtins.Unset (translateUnsetCommand)
