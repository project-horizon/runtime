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
Description :  The statement definitions of the EDSL.
Author	    :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License	    :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

The statement definitions of the EDSL.
-}
module Language.JavaScript.DSL.Statements
( for
, foreach
, while
, dowhile
, when
, eitherOr
, switch
, expr
, break
, continue
, var
, ret
, block
, method
) where

import           Prelude hiding (break)

import qualified Language.JavaScript.DOM as DOM

import           Language.JavaScript.DSL.TypeAliases


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
eitherOr :: Condition -> Statement -> Statement -> Statement
eitherOr c s = DOM.ConditionTree [(c, s)]

-- | Creates a complex if statement.
switch :: [Case] -> Statement -> Statement
switch = DOM.ConditionTree

-- | Creates a variable definition statement.
var :: VariableName -> Maybe Expression -> Statement
var = DOM.Var

-- | Creates a break statement.
break :: Statement
break = DOM.Break

-- | Creates a continue statement.
continue :: Statement
continue = DOM.Continue

-- | Creates a return statement.
ret :: Expression -> Statement
ret = DOM.Return

-- | Creates a scope block.
block :: [Statement] -> Statement
block = DOM.StatementBlock

-- | Creates a method definition.
method :: FunctionName -> [FunctionParameter] -> [Statement] -> Statement
method f ps ss = var f (Just (DOM.Function Nothing ps ss))

