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
Description :  Conversion from a main module to JavaScript.
Author	    :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License	    :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

Conversion from a main module to JavaScript.
-}
module Language.PolyDSL.Transformation.JavaScript.MainModule
( MainModule (..)
, VirtualResolverT (..)
, VirtualResolver
) where

import           Control.Applicative
import           Control.Monad

import           Language.JavaScript hiding (when)
import           Language.Transformation.Protocol
import           Language.Transformation.Semantics

import           Language.PolyDSL.Lib

import qualified Language.PolyDSL.DOM as DOM

import           Language.PolyDSL.Transformation.JavaScript.Internal


newtype (CompilationUnitResolver a, CompilationUnit b, CompilationUnitName c) => MainModule a b c = MainModule { getMainModule :: (a b c, b c) }

newtype (CompilationUnit a, CompilationUnitName b) => VirtualResolverT a b = VirtualResolver { getVirtualResolver :: [a b] }


-- | A virtual resolver for compilation units.
type VirtualResolver = VirtualResolverT DOM.ModuleT String


instance CompilationUnitResolver VirtualResolverT where
  resolveCompilationUnit (VirtualResolver ms) m = do
    let ms' = filter (\m' -> unitName m' == m) ms
    when (null ms')        (fail ("Module " ++ show m ++ " is not in scope."))
    when (length ms' /= 1) (fail ("Module name " ++ show m ++ " exists multiple times."))
    let (m:_) = ms'
    return m

instance (CompilationUnitResolver a, CompilationUnit b, CompilationUnitName c, Semantics m) => Transformer (MainModule a b c) (m [b c]) where
  transform (MainModule (cur, m)) = cur `resolveDependencies` m

