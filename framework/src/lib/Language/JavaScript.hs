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
Description :  The EDSL for creating JavaScript DOM elements.
Author	    :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License	    :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

The EDSL for creating JavaScript DOM elements.
-}
module Language.JavaScript
( Expression
, Statement

, FunctionName
, FunctionParameter
, Argument

, VariableName

, Script (..)

, block

, function
, expr
, call
, array
) where

import qualified Language.JavaScript.DOM as DOM


-- | An expression.
type Expression = DOM.Expression

-- | A statement.
type Statement = DOM.Statement


-- | The name of a function.
type FunctionName = DOM.FunctionName

-- | The name of a parameter in a function.
type FunctionParameter = DOM.FunctionParameter

-- | An argument for a function call.
type Argument = DOM.Argument


-- | The name of a field in an object.
type FieldName = DOM.FieldName

-- | A field of an object.
type Field = DOM.Field


-- | A container for a JavaScript DOM tree.
data Script
  -- | A container for a JavaScript DOM tree.
  = Script [Statement]
  deriving (Show, Eq)

-- | Creates a scope block.
block :: [Statement] -> Statement
block = DOM.StatementBlock


-- | Creates a function.
function :: FunctionName -> [FunctionParameter] -> [Statement] -> Expression
function = DOM.Function

-- | Creates a statement from an expression.
expr :: Expression -> Statement
expr = DOM.ExprAsStmt

-- | Creates an expression from a function name and function call
--   arguments.
call :: FunctionName -> [Argument] -> Expression
call = DOM.FunctionCall

-- | Creates an expression from multiple expressions.
array :: [Expression] -> Expression
array = DOM.Array

-- | Creates a field from a field name and an expression.
infixr 3 =:
(=:) :: FieldName -> Expression -> Field
(=:) k v = (k, v)

-- | Creates an object from multiple fields.
object :: [Field] -> Expression
object = Object

