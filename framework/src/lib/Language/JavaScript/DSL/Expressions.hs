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

{-# LANGUAGE FlexibleContexts #-}

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
module Language.JavaScript.DSL.Expressions
( num
, true
, false
, ident
, array
, object
, function
, new
, call
, val
, this
) where

import qualified Language.JavaScript.DOM as DOM

import           Language.JavaScript.DSL.TypeAliases

import           Language.Transformation.Protocol


-- | Creates a numeric value expression.
num :: Rational -> Expression
num = DOM.NumberLiteral

-- | Creates a boolean value expression.
true :: Expression
true = DOM.BooleanLiteral True

-- | Creates a boolean value expression.
false :: Expression
false = DOM.BooleanLiteral False

-- | Creates an identifier access.
ident :: VariableName -> Expression
ident = DOM.Identifier

-- | Creates an object from multiple fields.
object :: [Field] -> Expression
object = DOM.Object

-- | Creates an expression from multiple expressions.
array :: [Expression] -> Expression
array = DOM.Array

-- | Creates a function.
function :: [FunctionParameter] -> [Statement] -> Expression
function = DOM.Function Nothing

-- | Creates an expression from a function name and function call
--   arguments.
call :: Expression -> [Argument] -> Expression
call = DOM.FunctionCall

-- | Creates an object creation.
new :: Expression -> [Expression] -> Expression
new = DOM.NewCall

-- | Creates an expression from a Haskell value.
val :: (Transformer a Expression) => a -> Expression
val = transform

-- | Creates a this keyword expression.
this :: Expression
this = ident "this"

