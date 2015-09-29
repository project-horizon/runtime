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

instance Transformer DOM.Declaration Statement where
  transform (DOM.Function f []     e) = var f (Just (transform e))
  transform (DOM.Function f (p:ps) e) = method f [p] [ ret (trans ps) ]
    where
      trans []     = transform e
      trans (p:ps) = function [p] [ ret (trans ps) ]

instance Transformer DOM.Module Statement where
  transform (DOM.Module mName exps decls) = var mName (Just $ object [])

