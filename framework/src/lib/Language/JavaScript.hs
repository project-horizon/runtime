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
, LoopHead

, FunctionName
, FunctionParameter
, OptionalFunctionName
, Argument

, VariableName
, Initializations
, Condition
, Changes
, Container

, FieldName
, Field

, Script (..)

-- Statements
, method
, block
, when
, for
, foreach
, while
, dowhile

-- Expressions
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

-- | A loop head.
type LoopHead = DOM.LoopHead


-- | The name of a function.
type FunctionName = String

-- | The name of a parameter in a function.
type FunctionParameter = String

-- | A function name that is optional in JavaScript syntax.
type OptionalFunctionName = Maybe FunctionName

-- | An argument for a function call.
type Argument = Expression


-- | The name of a variable.
type VariableName = String

-- | The statements to initialize a for loop.
type Initializations = [Statement]

-- | A condition in control flow statements.
type Condition = Expression

-- | The statements to change the state of a for loop.
type Changes = [Statement]

-- | An expression containing the elements to iterate over.
type Container = Expression


-- | The name of a field in an object.
type FieldName = String

-- | A field of an object.
type Field = (FieldName, Expression)


-- | A container for a JavaScript DOM tree.
data Script
  -- | A container for a JavaScript DOM tree.
  = Script [Statement]
  deriving (Show, Eq)


-- | Creates a method definition.
method :: FunctionName -> [FunctionParameter] -> [Statement] -> Statement
method f ps ss = expr (function (Just f) ps ss)

-- | Creates a scope block.
block :: [Statement] -> Statement
block = DOM.StatementBlock

-- | Creates an if statement.
when :: Condition -> [Statement] -> Statement
when c ss = DOM.ConditionTree [(c,block ss)] (block [])




-- | Creates a for loop head.
for :: Initializations -> Condition -> Changes -> [Statement] -> Statement
for i c cs = DOM.Loop (DOM.IterationLoop i c cs)

-- | Creates a for each loop head.
foreach :: VariableName -> Container -> [Statement] -> Statement
foreach v c = DOM.Loop (DOM.ForEachLoop v c)

-- | Creates a while loop head.
while :: Condition -> [Statement] -> Statement
while c = DOM.Loop (DOM.WhileLoop c)

-- | Creates a do while loop head.
dowhile :: Condition -> [Statement] -> Statement
dowhile c = DOM.Loop (DOM.DoWhileLoop c)


-- | Creates a function.
function :: OptionalFunctionName -> [FunctionParameter] -> [Statement] -> Expression
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
object = DOM.Object

