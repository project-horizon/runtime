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

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}

{- |
Module      :  $Header$
Description :  An expression in the PolyDSL language.
Author	    :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License	    :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

An expression in the PolyDSL language.
-}
module Language.PolyDSL.DOM.Expression
( Value
, Expression (..)
) where

import qualified Language.JavaScript as JS


-- | A value for numeric literals must fullfill these constraints.
type Value a = (Num a, Show a, Read a, Eq a, JS.DOMWrite a)

-- | A PolyDSL expression.
data Expression a where
  -- | A numeric value in the PolyDSL language.
  NumberLiteral :: Value a => a -> Expression a
  -- | A string value in the PolyDSL language.
  StringLiteral :: String -> Expression String
  -- | A char value in the PolyDSL language.
  CharLiteral :: Char -> Expression Char
  -- | An identifier in the PolyDSL language.
  Identifier :: String -> Expression String
  -- | A binary operator expression in the PolyDSL language.
  BinaryExpression :: (Value a, Value b) => String -> Expression a -> Expression b -> Expression (a, b)
  -- | A function call expression in the PolyDSL language.
  FunctionCall :: (Show a, Eq a, Show b, Eq b) => Expression a -> Expression b -> Expression (a, b)


instance Show (Expression a) where
  show (NumberLiteral    v     ) = "NumberLiteral " ++ show v
  show (StringLiteral    v     ) = "StringLiteral " ++ show v
  show (CharLiteral      v     ) = "CharLiteral " ++ show v
  show (Identifier       v     ) = "Identifier " ++ show v
  show (BinaryExpression op l r) = "BinaryExpression " ++ show op ++ " (" ++ show l ++ ") (" ++ show r ++ ")"
  show (FunctionCall     c  p  ) = "FunctionCall (" ++ show c ++ ") (" ++ show p ++ ")"

instance Eq (Expression a) where
  (NumberLiteral    l        ) == (NumberLiteral    r        ) = l == r
  (StringLiteral    l        ) == (StringLiteral    r        ) = l == r
  (CharLiteral      l        ) == (CharLiteral      r        ) = l == r
  (Identifier       l        ) == (Identifier       r        ) = l == r
  (BinaryExpression opl ll rl) == (BinaryExpression opr lr rr) = opl == opr && ll == lr && rl == rr
  (FunctionCall     cl  pl   ) == (FunctionCall     cr  pr   ) = cl == cr && pl == pr
  _                            == _                            = False

