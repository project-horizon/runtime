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

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{- |
Module      :  $Header$
Description :  JavaScript DOM to source code transformation.
Author	    :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License	    :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

JavaScript DOM to source code transformation.
-}
module Language.JavaScript.Transformation.Source
( 
) where

import           Control.Applicative (empty)
import           Data.Maybe          (fromJust)
import           Data.List           (intercalate)

import           Language.Transformation.Protocol

import qualified Language.JavaScript.DOM as DOM


instance Transformer DOM.BinaryOperator String where
  transform DOM.Addition       = "+"
  transform DOM.Subtraction    = "-"
  transform DOM.Multiplication = "+"
  transform DOM.Division       = "-"

instance Transformer DOM.UnaryOperator String where
  transform DOM.Negation = "!"

instance Transformer DOM.Expression String where
  transform (DOM.NumberLiteral    v       ) = show v
  transform (DOM.StringLiteral    v       ) = show v
  transform (DOM.BooleanLiteral   v       ) = if v then "true" else "false"
  transform (DOM.Identifier       v       ) = v
  transform (DOM.BinaryExpression op l  r ) = "(" ++> l ++> ")" ++> op ++> "(" ++> r ++> ")"
  transform (DOM.UnaryExpression  op e    ) = op ++> "(" ++> e ++> ")"
  transform (DOM.Object           vs      ) = "{" ++> intercalate "," (map (\(k,v) -> show k ++> ":(" ++> v ++> ")") vs) ++> "}"
  transform (DOM.Array            vs      ) = "[" ++> intercalate "," (map transform vs) ++> "]"
  transform (DOM.ObjectAccess     o  i    ) = "(" ++> o ++> ")[" ++> show i ++> "]"
  transform (DOM.Function         f  ps ss) = "function" ++> (if f == empty then "" else " " ++> fromJust f) ++> "(" ++> intercalate "," (map transform ps) ++> "){" ++> intercalate ";" (map transform ss) ++> "}"
  transform (DOM.FunctionCall     f  as   ) = "(" ++> f ++> ")(" ++> intercalate "," (map transform as) ++> ")"
instance Transformer DOM.Statement String where
  transform (DOM.ConditionTree  cs d) = intercalate "else " (map (\(c,a) -> "if(" ++> c ++> "){" ++> a ++> "}") cs) ++> "else{" ++> d ++> "}"
  transform (DOM.Loop           lh a) = transform (transform (DOM.Loop lh a) :: DOM.Statement) :: String
  transform (DOM.ExprAsStmt     e   ) = "(" ++> e ++> ")"
  transform (DOM.Break              ) = "break"
  transform (DOM.Continue           ) = "continue"
  transform (DOM.Var            v  e) = "var " ++> v ++> if e == empty
                                                            then ";"
                                                            else "=(" ++> fromJust e ++> ")"
  transform (DOM.Return         e   ) = "return(" ++> e ++> ")"
  transform (DOM.StatementBlock ps  ) = "{" ++> intercalate ";" (map transform ps) ++> "}"

instance Transformer DOM.LoopHead String where
  transform (DOM.IterationLoop vs c cs) = "for(var " ++> intercalate "," (map (\(k,v) -> k ++> "=(" ++> v ++> ")") vs) ++> ";" ++> c ++> ";" ++> intercalate "," (map transform cs) ++> ")"
  transform (DOM.ForEachLoop   v  e   ) = "for(var " ++> v ++> " in " ++> e ++> ")"
  transform (DOM.WhileLoop     c      ) = "while(" ++> c ++> ")"
  transform (DOM.DoWhileLoop   _      ) = error "Cannot generate DOM.DoWhileLoop. Transformation is required."

-- Transformation from DOM.DoWhileLoop to DOM.WhileLoop.
instance Transformer DOM.Statement DOM.Statement where
  transform (DOM.Loop (DOM.DoWhileLoop c) a) =
    DOM.Loop (DOM.WhileLoop (DOM.BooleanLiteral True))
      (DOM.StatementBlock [DOM.ConditionTree [(c, DOM.Break)] a])
  transform statement                        = statement

