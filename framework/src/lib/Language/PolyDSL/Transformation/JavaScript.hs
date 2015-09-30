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
{-# LANGUAGE TypeSynonymInstances  #-}

{- |
Module      :  $Header$
Description :  PolyDSL to JavaScript conversion.
Author	    :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License	    :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

PolyDSL to JavaScript conversion.
-}
module Language.PolyDSL.Transformation.JavaScript
(
) where

import Language.Transformation.Protocol

import Language.JavaScript

import qualified Language.PolyDSL.DOM as DOM


instance Transformer DOM.Expression Expression where
  transform (DOM.NumberLiteral    v     ) = num v
  transform (DOM.StringLiteral    v     ) = val v
  transform (DOM.Identifier       i     ) = ident i
  transform (DOM.BinaryExpression op l r) = call (call (ident "operator_table" ... op) [transform l]) [transform r]
  transform (DOM.FunctionCall     f  e  ) = call (transform f) [transform e]


type Exports = [String]

newtype Imports   = Imports   { getImports   :: [DOM.Declaration] }
newtype Functions = Functions { getFunctions :: [DOM.Declaration] }

newtype FunctionSignature = FunctionSignature { getFunctionSignature :: DOM.Declaration }

data FunctionPack
  = FunctionPack String (Maybe FunctionSignature) [([String], Expression)]

newtype GADTs = GADTs { getGADTs :: [DOM.Declaration] }

instance Transformer GADTs [Statement] where
  transform (GADTs ts) = concatMap initType ts
    where
      initType (DOM.GADT tn ps cs) =
        let initCons (n, DOM.TypeSignature _ sig) = var n (Just $ fun (transSig sig) 0)
            transSig (DOM.Type {}         ) = []
            transSig (DOM.ListType {}     ) = []
            transSig (DOM.TupleType {}    ) = []
            transSig (DOM.GenericType {}  ) = []
            transSig (DOM.FunctionType p r) = p : transSig r
            getId i = '_' : show i
            genAssignment 0 as = as
            genAssignment i as = genAssignment (i - 1) ((show (i - 1) .: (ident . getId) (i - 1)) : as)
            fun []     i = object (genAssignment i []) -- TODO: implement type label generation
            fun (x:xs) i = function [getId i] [ ret (fun xs (i + 1)) ] -- TODO: implement signature check.
         in map initCons cs

newtype TypeAliases = TypeAliases { getTypeAliases :: [DOM.Declaration] }


data ModL1
  = ModL1 String Exports Imports GADTs TypeAliases Functions

instance Transformer DOM.Module ModL1 where
  transform (DOM.Module mName es ds) =
      let (is, ts, tas, fs) = filterDecls ds [] [] [] []
       in ModL1 mName es (Imports is) (GADTs ts) (TypeAliases tas) (Functions fs)
    where
      filterDecls []                        is ts tas fs = (reverse is, reverse ts, reverse tas, reverse fs)
      filterDecls (i@(DOM.Import    {}):ds) is ts tas fs = filterDecls ds (i:is) ts     tas     fs
      filterDecls (t@(DOM.GADT      {}):ds) is ts tas fs = filterDecls ds is     (t:ts) tas     fs
      filterDecls (t@(DOM.TypeAlias {}):ds) is ts tas fs = filterDecls ds is     ts     (t:tas) fs
      filterDecls (f@(DOM.Function  {}):ds) is ts tas fs = filterDecls ds is     ts     tas     (f:fs)
      filterDecls (s@(DOM.Signature {}):ds) is ts tas fs = filterDecls ds is     ts     tas     (s:fs)

instance Transformer ModL1 Statement where
  transform (ModL1 mName es is ts tas fs) = expr (this ... "module_register" ... mName .= body)
    where
      body = new (function [] (defs ++ exps)) []
      defs = transform ts
      exps = map (\v -> expr (this ... v .= ident v)) es

instance Transformer DOM.Module Statement where
  transform p = transform (transform p :: ModL1) :: Statement

instance Transformer [DOM.Module] Statement where
  transform ms = let modBlockInit = expr (this ... "module_register" .= object [])
                  in block $ modBlockInit : map transform ms

