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
) where

import           Language.JavaScript
import           Language.Transformation.Protocol
import           Language.Transformation.Semantics

import qualified Language.PolyDSL.DOM as DOM

import           Language.PolyDSL.Transformation.JavaScript.Internal


newtype MainModule = MainModule { getMainModule :: DOM.Module }


instance (Semantics m) => Transformer MainModule (m [DOM.Module]) where
  transform (MainModule m) = collectImports m >>= loadDependencies [m]

loadDependencies :: (Semantics m) => [DOM.Module] -> [String] -> m [DOM.Module]
loadDependencies = ld
  where
    ld ms []     = (return . reverse) ms
    ld ms (i:is) = do
      m    <- loadModule i
      is'  <- collectImports m
      let ms' = m:ms
      is'' <- dropLoadedModules ms' (is ++ is')
      ld ms' is''

loadModule :: (Semantics m) => String -> m DOM.Module
loadModule = undefined -- TODO: implement module loading

dropLoadedModules :: (Semantics m) => [DOM.Module] -> [String] -> m [String]
dropLoadedModules ms is = return (dlm (map (\(DOM.Module n _ _) -> n) ms) is [])
  where
    dlm ms []     rs = rs
    dlm ms (i:is) rs = if i `elem` ms then dlm ms is rs else dlm ms is (i:rs)

collectImports :: (Semantics m) => DOM.Module -> m [String]
collectImports (DOM.Module _ _ ds) = return (ci ds [])
  where
    ci :: [DOM.Declaration] -> [String] -> [String]
    ci []                rs = rs
    ci (DOM.Import i:is) rs = ci is (i:rs)
    ci (_:is)            rs = ci is rs

