{-
Copyright (c) 2015 Nils 'bash0r' Jonsson

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{- |
Module      :  $Header$
Description :  Conversion from PolyDSL modules to JavaScript.
Author	    :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License	    :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

Conversion from PolyDSL modules to JavaScript.
-}
module Language.PolyDSL.Transformation.JavaScript.Modules
(
) where

import           Language.JavaScript
import           Language.Transformation.Protocol
import           Language.Transformation.Semantics

import qualified Language.PolyDSL.DOM as DOM

import           Language.PolyDSL.Transformation.JavaScript.GADTs
import           Language.PolyDSL.Transformation.JavaScript.Internal


moduleRegister = this ... "$module_register$"


filterDecls []                        is ts tas fs = (reverse is, reverse ts, reverse tas, reverse fs)
filterDecls (i@(DOM.Import    {}):ds) is ts tas fs = filterDecls ds (i:is) ts     tas     fs
filterDecls (t@(DOM.GADT      {}):ds) is ts tas fs = filterDecls ds is     (t:ts) tas     fs
filterDecls (t@(DOM.TypeAlias {}):ds) is ts tas fs = filterDecls ds is     ts     (t:tas) fs
filterDecls (f@(DOM.Function  {}):ds) is ts tas fs = filterDecls ds is     ts     tas     (f:fs)
filterDecls (s@(DOM.Signature {}):ds) is ts tas fs = filterDecls ds is     ts     tas     (s:fs)

importDeclaration (DOM.Import i) =
  foreach "v" (moduleRegister ... i)
    (when (call (moduleRegister ... i ... "hasOwnProperty") [ident "v"])
      (expr (call (ident "console.log") [ident "v"])))


instance (Semantics m) => Transformer DOM.Module (m ModL1) where
  transform (DOM.Module mName es ds) = do
      let (is, ts, tas, fs) = filterDecls ds [] [] [] []
      return (ModL1 mName es (Imports is) (GADTs ts) (TypeAliases tas) (Functions fs))

-- TODO: prepend a module existance check to prevent modules
-- overriding other modules during definition
instance (Semantics m) => Transformer ModL1 (m Statement) where
  transform (ModL1 mName es is ts tas fs) = do
    ts' <- transform ts
    let impc  = when (ident "imported") (ret (ident "undefined"))
        imps  = expr (ident "imported" .= val True)
        impb  = impc : imps : map importDeclaration (getImports is)
        impf  = expr (this ... "import" .= function [] impb)
        expb  = object (map (\v -> v .: ident "scope" ... v) es)
        expf  = expr (this ... "export" .= function []
                       [ expr (call (this ... "import") [])
                       , ret expb
                       ])
        scope = var "scope" (Just (object []))
        impg  = var "imported" (Just (val False))
        defs  = map (\(n,t) -> expr (ident "scope" ... n .= t)) ts'
        body  = new (function [] (scope : impg : impf : expf : defs)) []
    return (expr (moduleRegister ... mName .= body))

instance (Semantics m) => Transformer DOM.Module (m Statement) where
  transform p = do
    p' <- transform p
    transform (p' :: ModL1)

instance (Semantics m) => Transformer [DOM.Module] (m [ModL1]) where
  transform ms = do
    ms' <- mapM transform ms
    checkForImportCycles ms'
    return ms'

instance (Semantics m) => Transformer [ModL1] (m Statement) where
  transform ms = do
    let modBlockInit = expr (moduleRegister .= object [])
    ms' <- mapM transform ms
    return (block (modBlockInit : ms'))

instance (Semantics m) => Transformer [DOM.Module] (m Statement) where
  transform ms = do
    ms' <- mapM transform ms
    transform (ms' :: [ModL1])

checkForImportCycles :: (Semantics m) => [ModL1] -> m ()
checkForImportCycles ms = return ()

