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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
, Argument

, VariableName
, Initializations
, Changes
, Container

, FieldName
, Field

, Condition
, Case

, Script (..)

-- Statements
, for
, foreach
, while
, dowhile
, when
, either
, switch
, expr
, var
, rel
, block
, method

-- Expressions
, int
, dbl
, true
, false
, ident
, array
, object
, function
, call
, val

, (=:)
, (+:)
, (-:)
, (*:)
, (/:)
, (.:)

, module Write
, module Read
) where

import           Prelude hiding (either)

import           Data.Int
import           Data.List

import           Text.Generation

import           Language.JavaScript.Generator
import qualified Language.JavaScript.DOM as DOM
import qualified Language.JavaScript.DOM.Expression.BinaryOperator as BinaryOperator
import qualified Language.JavaScript.DOM.Expression.UnaryOperator as UnaryOperator
import qualified Language.JavaScript.DOM.Conversion.Write as Write
import qualified Language.JavaScript.DOM.Conversion.Read as Read


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

-- | An argument for a function call.
type Argument = Expression


-- | The name of a variable.
type VariableName = String

-- | The statements to initialize a for loop.
type Initializations = [Statement]

-- | The statements to change the state of a for loop.
type Changes = [Statement]

-- | An expression containing the elements to iterate over.
type Container = Expression


-- | The name of a field in an object.
type FieldName = String

-- | A field of an object.
type Field = (FieldName, Expression)


-- | A condition in control flow statements.
type Condition = Expression

-- | A case statement.
type Case = (Condition, Statement)


-- | A container for a JavaScript DOM tree.
data Script
  -- | A container for a JavaScript DOM tree.
  = Script [Statement]
  deriving (Eq)

instance Show Script where
  show (Script stmts) = intercalate ";" (map generate stmts)

-- | Creates a statement from an expression.
expr :: Expression -> Statement
expr = DOM.ExprAsStmt

-- | Creates a for loop head.
for :: Initializations -> Condition -> Changes -> Statement -> Statement
for i c cs = DOM.Loop (DOM.IterationLoop i c cs)

-- | Creates a for each loop head.
foreach :: VariableName -> Container -> Statement -> Statement
foreach v c = DOM.Loop (DOM.ForEachLoop v c)

-- | Creates a while loop head.
while :: Condition -> Statement -> Statement
while c = DOM.Loop (DOM.WhileLoop c)

-- | Creates a do while loop head.
dowhile :: Condition -> Statement -> Statement
dowhile c = DOM.Loop (DOM.DoWhileLoop c)

-- | Creates a simple if statement.
when :: Condition -> Statement -> Statement
when c s = DOM.ConditionTree [(c, s)] (block [])

-- | Creates a if else statement.
either :: Condition -> Statement -> Statement -> Statement
either c s = DOM.ConditionTree [(c, s)]

-- | Creates a complex if statement.
switch :: [Case] -> Statement -> Statement
switch = DOM.ConditionTree

-- | Creates a return statement.
ret :: Expression -> Statement
ret = DOM.Return

-- | Creates a variable definition statement.
var :: VariableName -> Expression -> Statement
var = DOM.Var

-- | Creates a scope block.
block :: [Statement] -> Statement
block = DOM.StatementBlock

-- | Creates a method definition.
method :: FunctionName -> [FunctionParameter] -> [Statement] -> Statement
method f ps ss = expr (DOM.Function (Just f) ps ss)


-- | Creates a numeric value expression.
int :: (Integral a) => a -> Expression
int i = DOM.NumberLiteral (fromIntegral i)

-- | Creates a numeric value expression.
dbl :: (Real a) => a -> Expression
dbl d = DOM.NumberLiteral (realToFrac d)

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
call :: FunctionName -> [Argument] -> Expression
call = DOM.FunctionCall

-- | Creates an expression from a Haskell value.
val :: (Write.C a) => a -> Expression
val = Write.write


-- | Creates a field from a field name and an expression.
infixr 3 =:
(=:) :: FieldName -> Expression -> Field
(=:) k v = (k, v)

-- | Creates a binary addition expression.
infixl 3 +:
(+:) :: Expression -> Expression -> Expression
(+:) = DOM.BinaryExpression BinaryOperator.Addition

-- | Creates a binary subtraction expression.
infixl 3 -:
(-:) :: Expression -> Expression -> Expression
(-:) = DOM.BinaryExpression BinaryOperator.Subtraction

-- | Creates a binary multiplication expression.
infixl 4 *:
(*:) :: Expression -> Expression -> Expression
(*:) = DOM.BinaryExpression BinaryOperator.Multiplication

-- | Creates a binary division expression.
infixl 4 /:
(/:) :: Expression -> Expression -> Expression
(/:) = DOM.BinaryExpression BinaryOperator.Division

-- | Creates an object access expression.
infixl 9 .:
(.:) :: Expression -> VariableName -> Expression
(.:) = DOM.ObjectAccess

instance (Write.C a) => Write.C [a] where
  write vs = array (map val vs)

instance Write.C String where
  write = DOM.StringLiteral

instance Write.C Char where
  write c = DOM.StringLiteral [c]

instance Write.C Int where
  write i = DOM.NumberLiteral (fromIntegral i)

instance Write.C Double where
  write = DOM.NumberLiteral

instance Write.C Float where
  write f = DOM.NumberLiteral (realToFrac f)

instance Write.C Int8 where
  write i = DOM.NumberLiteral (fromIntegral i)

instance Write.C Int16 where
  write i = DOM.NumberLiteral (fromIntegral i)

instance Write.C Int32 where
  write i = DOM.NumberLiteral (fromIntegral i)

instance Write.C Int64 where
  write i = DOM.NumberLiteral (fromIntegral i)

