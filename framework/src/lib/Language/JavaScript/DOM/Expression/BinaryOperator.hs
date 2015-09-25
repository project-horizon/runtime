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

{- |
Module      :  $Header$
Description :  The available binary operators in JavaScript.
Author	    :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License	    :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

The available binary operators in JavaScript.
-}
module Language.JavaScript.DOM.Expression.BinaryOperator
( BinaryOperator (..)
) where

-- | All available binary operators in the JavaScript language.
data BinaryOperator
  -- | The addition operator.
  = Addition
  -- | The subtraction operator.
  | Subtraction
  -- | The multiplication operator.
  | Multiplication
  -- | The division operator.
  | Division
  -- | The equal operator.
  | Equal
  -- | The not equal operator.
  | NotEqual
  -- | The strict equal operator.
  | StrictEqual
  -- | The strict not equal operator.
  | StrictNotEqual
  -- | The less than operator.
  | LessThan
  -- | The greater than operator.
  | GreaterThan
  -- | The less than or equal operator.
  | LessThanEqual
  -- | The greater than or equal operator.
  | GreaterThanEqual
  deriving (Show, Eq)

