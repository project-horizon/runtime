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
Description :  Contains definitions for expressions in EDSL.
Author	    :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License	    :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

Contains definitions for expressions in EDSL.
-}
module Language.PolyDSL.DSL.Declarations
( include
, function
, signature
, gadt

-- type signatures
, t
, (-->)
, listT
, tupleT
, genericT
) where

import qualified Language.PolyDSL.DOM as DOM

import           Language.PolyDSL.DSL.TypeAliases


-- | An import declaration.
include :: ModuleName -> Declaration
include = DOM.Import

-- | A function declaration
function :: FunctionName -> [FunctionParameter] -> Expression -> Declaration
function = DOM.Function

-- | A type signature
signature :: FunctionName -> TypeSignature -> Declaration
signature = DOM.Signature

-- | A generalized algebraic data type.
gadt :: TypeName -> [TypeParameter] -> [Constructor] -> Declaration
gadt = DOM.GADT


-- | A simple type.
t :: TypeName -> Type
t = DOM.Type

-- | A function type.
infixr 9 -->
(-->) :: Type -> Type -> Type
(-->) = DOM.FunctionType

-- | A list type.
listT :: Type -> Type
listT = DOM.ListType

-- | A list type.
tupleT :: [Type] -> Type
tupleT = DOM.TupleType

-- | A generic type.
genericT :: TypeName -> [Type] -> Type
genericT = DOM.GenericType

