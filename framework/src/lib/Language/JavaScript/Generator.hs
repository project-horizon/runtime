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
Description :  The source code generator for the JavaScript DOM.
Author	    :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License	    :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

The source code generator for the JavaScript DOM.
-}
module Language.JavaScript.Generator
( 
) where

import Data.List


import Text.Generation

import qualified Language.JavaScript.DOM as DOM


instance Generator DOM.BinaryOperator where
  generate DOM.Addition       = "+"
  generate DOM.Subtraction    = "-"
  generate DOM.Multiplication = "*"
  generate DOM.Division       = "/"

instance Generator DOM.UnaryOperator where
  generate DOM.Negate = "!"

instance Generator DOM.Expression where
  generate (DOM.NumberLiteral v)         = show v
  generate (DOM.StringLiteral v)         = show v
  generate (DOM.BooleanLiteral v)        = if v then "true" else "false"
  generate (DOM.BinaryExpression op l r) = l ++> op ++> r
  generate (DOM.UnaryExpression op v)    = op ++> v
  generate (DOM.Object vs)               = "{" ++> intercalate "," (map (\(k,v)-> show k ++> ":" ++> v) vs) ++> "}"
  generate (DOM.Array vs)                = "[" ++> intercalate "," (map generate vs) ++> "]"
  generate (DOM.ObjectAccess e p)        = e ++> "[" ++> show p ++> "]"
  generate (DOM.FunctionCall f args)     = f ++> "(" ++> intercalate "," (map generate args) ++> ")"
  generate (DOM.Function f ps ss)        = "function " ++> f ++> "(" ++> intercalate "," ps ++> "){" ++> intercalate ";" (map generate ss) ++> "}"

instance Generator DOM.Statement where
  generate (DOM.ConditionTree cs ow)        = intercalate " else " (map (\(c,s)-> "if(" ++> c ++> ")" ++> s) cs) ++> " else " ++> ow
  generate (DOM.Loop lh s)                  = generateLoop lh s
    where
      generateLoop lh@(DOM.DoWhileLoop _) s = "do " ++> s ++> lh ++> ";"
      generateLoop lh s                     = lh ++> s ++> ";"
  generate (DOM.ExprAsStmt v)               = generate v
  generate (DOM.Var s v)                    = "var " ++> s ++> "=" ++> v
  generate (DOM.Return v)                   = "return " ++> v
  generate (DOM.StatementBlock stmts)       = "{" ++> intercalate ";" (map generate stmts) ++> "}"

instance Generator DOM.LoopHead where
  generate (DOM.IterationLoop inits cond chs) =
    "for(" ++> intercalate "," (map generate inits) ++> ";" ++>
               cond ++>
               intercalate "," (map generate chs) ++> ")"
  generate (DOM.ForEachLoop v e) =
    "for(" ++> v ++> " in " ++> e ++> ")"
  generate (DOM.WhileLoop c) =
    "while(" ++> c ++> ")"
  generate (DOM.DoWhileLoop c) =
    "while(" ++> c ++> ")"

