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

{-# LANGUAGE TypeSynonymInstances #-}

{- |
Module      :  $Header$
Description :  The expression definitions of the EDSL.
Author	    :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License	    :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

The expression definitions of the EDSL.
-}
module Language.JavaScript.DSL.Operators
-- Member operators
( (.:)
, (...)

-- Comparison operators
, (.==)
, (.!=)
, (.===)
, (.!==)
, (.<)
, (.>)
, (.<=)
, (.>=)

-- Bitwise operators
, (.&)
, (.|)
, (.^)

-- Logical operators
, (.&&)
, (.||)
) where

import           Data.Bits

import qualified Language.JavaScript.DOM as DOM

import           Language.JavaScript.DSL.TypeAliases


-- | Creates a field from a field name and an expression.
infixr 1 .:
(.:) :: FieldName -> Expression -> Field
(.:) k v = (k, v)

-- | Creates an object access expression.
infixl 9 ...
(...) :: Expression -> VariableName -> Expression
(...) = DOM.ObjectAccess


-- | Creates an equal expression.
infixl 4 .==
(.==) :: Expression -> Expression -> Expression
(.==) (DOM.NumberLiteral  l) (DOM.NumberLiteral  r) = DOM.BooleanLiteral (l == r)
(.==) (DOM.StringLiteral  l) (DOM.StringLiteral  r) = DOM.BooleanLiteral (l == r)
(.==) (DOM.BooleanLiteral l) (DOM.BooleanLiteral r) = DOM.BooleanLiteral (l == r)
(.==) (DOM.Object         l) (DOM.Object         r) = DOM.BooleanLiteral (l == r)
(.==) (DOM.Array          l) (DOM.Array          r) = DOM.BooleanLiteral (l == r)
(.==) l                      r                      = DOM.BinaryExpression DOM.Equal l r

-- | Creates not equal expression.
infixl 4 .!=
(.!=) :: Expression -> Expression -> Expression
(.!=) (DOM.NumberLiteral  l) (DOM.NumberLiteral  r) = DOM.BooleanLiteral (l /= r)
(.!=) (DOM.StringLiteral  l) (DOM.StringLiteral  r) = DOM.BooleanLiteral (l /= r)
(.!=) (DOM.BooleanLiteral l) (DOM.BooleanLiteral r) = DOM.BooleanLiteral (l /= r)
(.!=) (DOM.Object         l) (DOM.Object         r) = DOM.BooleanLiteral (l /= r)
(.!=) (DOM.Array          l) (DOM.Array          r) = DOM.BooleanLiteral (l /= r)
(.!=) l                      r                      = DOM.BinaryExpression DOM.NotEqual l r

-- | Creates a strict equal expression.
infixl 4 .===
(.===) :: Expression -> Expression -> Expression
(.===) (DOM.NumberLiteral  l) (DOM.NumberLiteral  r) = DOM.BooleanLiteral (l == r)
(.===) (DOM.StringLiteral  l) (DOM.StringLiteral  r) = DOM.BooleanLiteral (l == r)
(.===) (DOM.BooleanLiteral l) (DOM.BooleanLiteral r) = DOM.BooleanLiteral (l == r)
(.===) (DOM.Object         l) (DOM.Object         r) = DOM.BooleanLiteral (l == r)
(.===) (DOM.Array          l) (DOM.Array          r) = DOM.BooleanLiteral (l == r)
(.===) l                      r                      = DOM.BinaryExpression DOM.StrictEqual l r

-- | Creates a strict not equal expression.
infixl 4 .!==
(.!==) :: Expression -> Expression -> Expression
(.!==) (DOM.NumberLiteral  l) (DOM.NumberLiteral  r) = DOM.BooleanLiteral (l /= r)
(.!==) (DOM.StringLiteral  l) (DOM.StringLiteral  r) = DOM.BooleanLiteral (l /= r)
(.!==) (DOM.BooleanLiteral l) (DOM.BooleanLiteral r) = DOM.BooleanLiteral (l /= r)
(.!==) (DOM.Object         l) (DOM.Object         r) = DOM.BooleanLiteral (l /= r)
(.!==) l                      r                      = DOM.BinaryExpression DOM.StrictNotEqual l r

-- | Creates a less than expression.
infixl 5 .<
(.<) :: Expression -> Expression -> Expression
(.<) (DOM.NumberLiteral l) (DOM.NumberLiteral r) = DOM.BooleanLiteral (l < r)
(.<) l                     r                     = DOM.BinaryExpression DOM.LessThan l r

-- | Creates a greater than expression.
infixl 5 .>
(.>) :: Expression -> Expression -> Expression
(.>) (DOM.NumberLiteral l) (DOM.NumberLiteral r) = DOM.BooleanLiteral (l > r)
(.>) l                     r                     = DOM.BinaryExpression DOM.GreaterThan l r

-- | Creates a less than or equal expression.
infixl 5 .<=
(.<=) :: Expression -> Expression -> Expression
(.<=) (DOM.NumberLiteral l) (DOM.NumberLiteral r) = DOM.BooleanLiteral (l <= r)
(.<=) l                     r                     = DOM.BinaryExpression DOM.LessThanEqual l r

-- | Creates a greater than or equal expression.
infixl 5 .>=
(.>=) :: Expression -> Expression -> Expression
(.>=) (DOM.NumberLiteral l) (DOM.NumberLiteral r) = DOM.BooleanLiteral (l >= r)
(.>=) l                     r                     = DOM.BinaryExpression DOM.GreaterThanEqual l r


-- | Creates a bitwise and expression.
infixl 2 .&
(.&) :: Expression -> Expression -> Expression
(.&) = DOM.BinaryExpression DOM.BitwiseAnd

-- | Creates a bitwise or expression.
infixl 2 .|
(.|) :: Expression -> Expression -> Expression
(.|) = DOM.BinaryExpression DOM.BitwiseOr

-- | Creates a bitwise xor expression.
infixl 2 .^
(.^) :: Expression -> Expression -> Expression
(.^) = DOM.BinaryExpression DOM.BitwiseXOr


-- | Creates a logical and expression.
infixl 1 .&&
(.&&) :: Expression -> Expression -> Expression
(.&&) (DOM.BooleanLiteral l) (DOM.BooleanLiteral r) = DOM.BooleanLiteral (l && r)
(.&&) l                      r                      = DOM.BinaryExpression DOM.LogicalAnd l r

-- | Creates a logical or expression.
infixl 1 .||
(.||) :: Expression -> Expression -> Expression
(.||) (DOM.BooleanLiteral l) (DOM.BooleanLiteral r) = DOM.BooleanLiteral (l || r)
(.||) l                      r                      = DOM.BinaryExpression DOM.LogicalOr l r


instance Num Expression where
  (+) (DOM.NumberLiteral l) (DOM.NumberLiteral r) = DOM.NumberLiteral (l + r)
  (+) l                     r                     = DOM.BinaryExpression DOM.Addition l r

  (-) (DOM.NumberLiteral l) (DOM.NumberLiteral r) = DOM.NumberLiteral (l - r)
  (-) l                     r                     = DOM.BinaryExpression DOM.Subtraction l r

  (*) (DOM.NumberLiteral l) (DOM.NumberLiteral r) = DOM.NumberLiteral (l * r)
  (*) l                     r                     = DOM.BinaryExpression DOM.Multiplication l r

  negate (DOM.NumberLiteral v) = DOM.NumberLiteral (negate v)
  negate v                     = DOM.BinaryExpression DOM.Multiplication v (DOM.NumberLiteral (-1))

  abs    (DOM.NumberLiteral v) = DOM.NumberLiteral (abs v)
  abs    v                     = DOM.FunctionCall function []
    where
      function   = DOM.Function Nothing [] [ DOM.ConditionTree conditions otherwise ]
      conditions = [(condition, action)]
      condition  = DOM.BinaryExpression DOM.LessThan v (DOM.NumberLiteral 0)
      action     = DOM.Return (DOM.BinaryExpression DOM.Multiplication v (DOM.NumberLiteral (-1)))
      otherwise  = DOM.Return v

  signum (DOM.NumberLiteral v) = DOM.NumberLiteral (signum v)
  signum v                     = DOM.FunctionCall function []
    where
      function   = DOM.Function Nothing [] [ DOM.ConditionTree conditions otherwise ]
      conditions = [(condGZero, actGZero), (condLZero, actLZero)]
      condLZero  = DOM.BinaryExpression DOM.LessThan v (DOM.NumberLiteral 0)
      condGZero  = DOM.BinaryExpression DOM.GreaterThan v (DOM.NumberLiteral 0)
      actLZero   = DOM.Return (DOM.NumberLiteral (-1))
      actGZero   = DOM.Return (DOM.NumberLiteral 1)
      otherwise  = DOM.Return (DOM.NumberLiteral 0)

  fromInteger i = DOM.NumberLiteral (fromInteger i)

