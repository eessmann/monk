module Language.Fish.Translator.Variables.ParamExpansion
  ( ParamExpansion,
    parseParamExpansion,
    parseParamExpansionStr,
    renderParamExpansion,
    renderParamExpansionWithPrelude,
    noSplitParamExpansion,
    translateSimpleVar,
    translateSimpleVarM,
    splitParamOperator,
    paramIndexFrom,
    translateDefaultExpansionWith,
    translateAssignDefaultExpansionWith,
    translateErrorExpansionWith,
    translateAltExpansionWith,
    varNonEmptyCond,
    varSetCond,
    emitList,
    commandSubst,
  )
where

import Language.Fish.Translator.Variables.ParamExpansion.Parse
  ( noSplitParamExpansion,
    paramIndexFrom,
    parseParamExpansion,
    parseParamExpansionStr,
    splitParamOperator,
  )
import Language.Fish.Translator.Variables.ParamExpansion.Render
  ( commandSubst,
    emitList,
    renderParamExpansion,
    renderParamExpansionWithPrelude,
    translateAltExpansionWith,
    translateAssignDefaultExpansionWith,
    translateDefaultExpansionWith,
    translateErrorExpansionWith,
    translateSimpleVar,
    translateSimpleVarM,
    varNonEmptyCond,
    varSetCond,
  )
import Language.Fish.Translator.Variables.ParamExpansion.Types (ParamExpansion)
