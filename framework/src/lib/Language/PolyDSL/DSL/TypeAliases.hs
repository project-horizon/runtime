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
Description :  Type aliases for the EDSL.
Author	    :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License	    :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

Type aliases for the EDSL.
-}
module Language.PolyDSL.DSL.TypeAliases
( Expression
, Declaration
, Module

-- Types
, Type
, TypeSignature

-- Expressions
, Operator

-- Declarations
, Constructor
, FunctionName
, FunctionParameter
, TypeName
, TypeParameter

-- Modules
, ModuleName
, SymbolName
) where

import qualified Language.PolyDSL.DOM as DOM


-- | An expression in the PolyDSL DOM.
type Expression = DOM.Expression

-- | A declaration in the PolyDSL DOM.
type Declaration = DOM.Declaration

-- | A module declaration in the PolyDSL DOM.
type Module = DOM.Module


-- | A type label.
type Type = DOM.Type

-- | A type signature.
type TypeSignature = DOM.TypeSignature


-- | The name of an operator.
type Operator = String


-- | A constructor definition.
type Constructor = (String, TypeSignature)

-- | The name of a function
type FunctionName = String

-- | The name of a function parameter.
type FunctionParameter = String

-- | The name of a type.
type TypeName = String

-- | The name of a type parameter.
type TypeParameter = String


-- | The name of a module.
type ModuleName = String

-- | The name of a symbol.
type SymbolName = String

