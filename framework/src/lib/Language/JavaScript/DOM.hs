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
Description :  The domain object model of the JavaScript language.
Author	    :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License	    :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

The domain object model of the JavaScript language.
-}
module Language.JavaScript.DOM
( module Export

, Expression (..)
, Statement (..)
, LoopHead (..)
) where

import           Language.JavaScript.DOM.Expression as Export


-- | A JavaScript exmression.
data Expression
  -- | A JavaScript number expression.
  = NumberLiteral Rational
  -- | A JavaScript string expression.
  | StringLiteral String
  -- | A JavaScript boolean expression.
  | BooleanLiteral Bool
  -- | A JavaScript variable identifier expression.
  | Identifier String
  -- | A JavaScript binary expression.
  | BinaryExpression BinaryOperator Expression Expression
  -- | A JavaScript unary expression.
  | UnaryExpression UnaryOperator Expression
  -- | A JavaScript object expression.
  | Object [(String,Expression)]
  -- | A JavaScript array expression.
  | Array [Expression]
  -- | A JavaScript object access expression.
  | ObjectAccess Expression String
  -- | A JavaScript function expression.
  | Function (Maybe String) [String] [Statement]
  -- | A JavaScript function call expression.
  | FunctionCall Expression [Expression]
  deriving (Show, Eq)

-- | A simple statement representation.
data Statement
  -- | A conditional execution statement.
  = ConditionTree [(Expression,Statement)] Statement
  -- | A loop statement.
  | Loop LoopHead Statement
  -- | An expression used as a statement.
  | ExprAsStmt Expression
  -- | A break statement.
  | Break
  -- | A continue statement.
  | Continue
  -- | A var statement.
  | Var String (Maybe Expression)
  -- | A return statement.
  | Return Expression
  -- | A block of statements.
  | StatementBlock [Statement]
  deriving (Show, Eq)

-- | Represents the head of a loop.
data LoopHead
  -- | A simple iterational loop.
  = IterationLoop [(String,Expression)] (Maybe Expression) [Expression]
  -- | A loop iterating over every element in a collection.
  | ForEachLoop String Expression
  -- | A loop iterating until the condition becomes false.
  | WhileLoop Expression
  -- | A loop iterating until the condition becomes false but evaluating
  --   the condition after the execution of the loop block.
  | DoWhileLoop Expression
  deriving (Show, Eq)

