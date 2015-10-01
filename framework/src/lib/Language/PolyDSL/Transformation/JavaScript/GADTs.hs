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

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Module      :  $Header$
Description :  Conversion from GADTs to JavaScript functions.
Author	    :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License	    :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

Conversion from GADTs to JavaScript functions.
-}
module Language.PolyDSL.Transformation.JavaScript.GADTs
(
) where

import           Language.JavaScript
import           Language.Transformation.Protocol
import           Language.Transformation.Semantics

import qualified Language.PolyDSL.DOM as DOM

import           Language.PolyDSL.Transformation.JavaScript.Internal


-- TODO: transform signature and keep return type.
transSig (DOM.Type {}         ) = []
transSig (DOM.ListType {}     ) = []
transSig (DOM.TupleType {}    ) = []
transSig (DOM.GenericType {}  ) = []
transSig (DOM.FunctionType p r) = p : transSig r

genAssignment 0 as = as
genAssignment i as = genAssignment n ((show n .: (ident . getId) n) : as)
  where n = i - 1

fun []     i = object (genAssignment i []) -- TODO: implement type label generation
fun (x:xs) i = function [getId i] [ ret (fun xs (i + 1)) ] -- TODO: implement signature check.

getId i = '_' : show i

initCons (n, DOM.TypeSignature _ sig) = (n, fun (transSig sig) 0)

initType (DOM.GADT tn ps cs) = map initCons cs


instance (Semantics m) => Transformer GADTs (m [(String, Expression)]) where
  transform (GADTs ts) = return (concatMap initType ts)

