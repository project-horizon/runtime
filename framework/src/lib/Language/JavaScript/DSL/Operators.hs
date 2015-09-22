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
( (=:)
, (+:)
, (-:)
, (*:)
, (/:)
, (.:)
) where

import qualified Language.JavaScript.DOM as DOM

import           Language.JavaScript.DSL.TypeAliases


-- | Creates a field from a field name and an expression.
infixr 3 =:
(=:) :: FieldName -> Expression -> Field
(=:) k v = (k, v)

-- | Creates a binary addition expression.
infixl 3 +:
(+:) :: Expression -> Expression -> Expression
(+:) = DOM.BinaryExpression DOM.Addition

-- | Creates a binary subtraction expression.
infixl 3 -:
(-:) :: Expression -> Expression -> Expression
(-:) = DOM.BinaryExpression DOM.Subtraction

-- | Creates a binary multiplication expression.
infixl 4 *:
(*:) :: Expression -> Expression -> Expression
(*:) = DOM.BinaryExpression DOM.Multiplication

-- | Creates a binary division expression.
infixl 4 /:
(/:) :: Expression -> Expression -> Expression
(/:) = DOM.BinaryExpression DOM.Division

-- | Creates an object access expression.
infixl 9 .:
(.:) :: Expression -> VariableName -> Expression
(.:) = DOM.ObjectAccess

