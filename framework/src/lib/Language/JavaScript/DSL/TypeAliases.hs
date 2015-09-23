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
Description :  Contains type alias definitions.
Author	    :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License	    :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

Contains type alias definitions.
-}
module Language.JavaScript.DSL.TypeAliases
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

-- | An argument for a function call.
type Argument = Expression


-- | The name of a variable.
type VariableName = String

-- | The statements to initialize a for loop.
type Initializations = [(VariableName, Expression)]

-- | The statements to change the state of a for loop.
type Changes = [Expression]

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


