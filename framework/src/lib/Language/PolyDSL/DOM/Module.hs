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

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |
Module      :  $Header$
Description :  A module definition in the PolyDSL language.
Author	    :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License	    :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

A module definition in the PolyDSL language.
-}
module Language.PolyDSL.DOM.Module
( ModuleT (..)
, Module
) where

import           Language.Transformation.Semantics

import           Language.PolyDSL.DOM.Declaration


-- | A generic module definition.
data ModuleT a where
  -- | A module definition.
  Module :: (CompilationUnitName a) => a -> [String] -> [DeclarationT a] -> ModuleT a

-- | A module definition.
type Module = ModuleT String

instance CompilationUnitName String

instance CompilationUnit ModuleT where
  unitName (Module n _ _) = n
  unitDependencies = collectImports

collectImports :: (CompilationUnitName a) => ModuleT a -> [a]
collectImports (Module _ _ ds) = ci ds []
  where
    ci []            rs = reverse rs
    ci (Import i:is) rs = ci is (i:rs)
    ci (_:is)        rs = ci is rs

