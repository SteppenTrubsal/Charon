module Generator where

import           Desc.Parser

type GeneratorCtx a = ()
type GeneratorError = ()

initGeneratorCtx :: Desc -> ()
initGeneratorCtx _ = ()