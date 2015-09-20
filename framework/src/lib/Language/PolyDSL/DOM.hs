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

{-# LANGUAGE GADTs #-}

{- |
Module      :  $Header$
Description :  The DOM of the PolyDSL language.
Author	    :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License	    :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

The DOM of the PolyDSL language.
-}
module Language.PolyDSL.DOM
( Expression (..)
) where


data Expression where
  -- | Integer literal.
  IntegralLiteral :: (Integral a, Show a, Eq a) => a -> Expression
  -- | Fractional literal.
  FractionalLiteral :: (Fractional a, Show a, Eq a) => a -> Expression

instance Show Expression where
  show (IntegralLiteral   v) = "IntegralLiteral " ++ show v
  show (FractionalLiteral v) = "FractionalLiteral " ++ show v

instance Eq Expression where
  (IntegralLiteral   l) == (IntegralLiteral   r) = l == r
  (FractionalLiteral l) == (FractionalLiteral r) = l == r
  _                     == _                     = False

