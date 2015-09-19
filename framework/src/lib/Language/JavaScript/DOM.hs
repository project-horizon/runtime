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
( Expression (..)
, Statement (..)
, LoopHead (..)
) where

import qualified Language.JavaScript.DOM.Expression.BinaryOperator as BinaryOperator
import qualified Language.JavaScript.DOM.Expression.UnaryOperator as UnaryOperator


-- | A JavaScript exmression.
data Expression
  -- | A JavaScript number expression.
  = NumberLiteral Double
  -- | A JavaScript string expression.
  | StringLiteral String
  -- | A JavaScript binary expression.
  | BinaryOperator BinaryOperator.T Expression Expression
  -- | A JavaScript unary expression.
  | UnaryOperator UnaryOperator.T Expression
  -- | A JavaScript object expression.
  | Object [(String,Expression)]
  -- | A JavaScript array expression.
  | Array [Expression]
  -- | A JavaScript function expression.
  | Function (Maybe String) [String] [Statement]
  -- | A JavaScript function call expression.
  | FunctionCall String [Expression]
  deriving (Show, Eq)

-- | A simple statement representation.
data Statement
  -- | A conditional execution statement.
  = ConditionalExecution Condition 
  -- | A loop statement.
  | Loop LoopHead [Statement]
  -- | An expression used as a statement.
  | ExprAsStmt Expression
  -- | A block of statements.
  | StatementBlock [Statement]
  deriving (Show, Eq)

-- | Represents the head of a loop.
data LoopHead
  -- | A simple iterational loop.
  = IterationLoop [Statement] Expression [Statement]
  -- | A loop iterating over every element in a collection.
  | ForEachLoop String Expression
  -- | A loop iterating until the condition becomes false.
  | WhileLoop Expression
  -- | A loop iterating until the condition becomes false but evaluating
  --   the condition after the execution of the loop block.
  | DoWhileLoop Expression

