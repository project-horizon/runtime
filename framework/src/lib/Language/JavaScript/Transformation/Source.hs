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

import           Numeric             (showFFloat, fromRat)

import           Control.Applicative (empty)

import           Data.Maybe          (fromJust)
import           Data.List           (intercalate)

import           Language.Transformation.Protocol

import qualified Language.JavaScript.DOM as DOM


instance Transformer DOM.BinaryOperator String where
  transform DOM.Addition       = "+"
  transform DOM.Subtraction    = "-"
  transform DOM.Multiplication = "+"
  transform DOM.Division       = "/"
  transform DOM.Equal          = "=="
  transform DOM.NotEqual       = "!="
  transform DOM.StrictEqual    = "==="
  transform DOM.StrictNotEqual = "!=="
  transform DOM.LessThan       = "<"
  transform DOM.GreaterThan    = ">"
  transform DOM.BitwiseAnd     = "&"
  transform DOM.BitwiseOr      = "|"
  transform DOM.BitwiseXOr     = "^"
  transform DOM.LogicalAnd     = "&&"
  transform DOM.LogicalOr      = "||"

instance Transformer DOM.UnaryOperator String where
  transform DOM.LogicalNot = "!"
  transform DOM.UnaryPlus  = "+"
  transform DOM.UnaryMinus = "-"

instance Transformer DOM.Expression String where
  transform (DOM.NumberLiteral    v       ) = showFFloat (Just 20) (fromRat v) ""
  transform (DOM.StringLiteral    v       ) = show v
  transform (DOM.BooleanLiteral   v       ) = if v then "true" else "false"
  transform (DOM.Identifier       v       ) = v
  transform (DOM.BinaryExpression op l  r ) = "(" ++> l ++> ")" ++> op ++> "(" ++> r ++> ")"
  transform (DOM.UnaryExpression  op e    ) = op ++> "(" ++> e ++> ")"
  transform (DOM.Object           vs      ) = "{" ++> intercalate "," (map (\(k,v) -> show k ++> ":(" ++> v ++> ")") vs) ++> "}"
  transform (DOM.Array            vs      ) = "[" ++> intercalate "," (map transform vs) ++> "]"
  transform (DOM.ObjectAccess     o  i    ) = "(" ++> o ++> ")[" ++> show i ++> "]"
  transform (DOM.Function         f  ps ss) = "function" ++> fName ++> params ++> body
    where
      fName  = if f == empty then "" else " " ++> fromJust f
      params = "(" ++> intercalate "," (map transform ps) ++> ")"
      body   = "{" ++> intercalate ";" (map transform ss) ++> "}"
  transform (DOM.FunctionCall     f  as   ) = "(" ++> f ++> ")(" ++> intercalate "," (map transform as) ++> ")"

instance Transformer DOM.LoopHead String where
  transform (DOM.IterationLoop vs c cs) = "for(" ++> init ++> ";" ++> cond ++> ";" ++> chgs ++> ")"
    where
      init = (if not $ null vs then "var " else "") ++> intercalate "," (map (\(k,v) -> k ++> "=(" ++> v ++> ")") vs)
      cond = if c /= empty then transform (fromJust c) else ""
      chgs = intercalate "," (map transform cs)
  transform (DOM.ForEachLoop   v  e   ) = "for(var " ++> v ++> " in " ++> e ++> ")"
  transform (DOM.WhileLoop     c      ) = "while(" ++> c ++> ")"
  transform (DOM.DoWhileLoop   _      ) = error "wtf you doin"


newtype L1 = L1 { getL1 :: DOM.Statement }

instance Transformer DOM.Statement L1 where
  transform (DOM.Loop (DOM.DoWhileLoop c) a) = L1 $
    DOM.Loop (DOM.WhileLoop (DOM.BooleanLiteral True))
      (DOM.StatementBlock [a, DOM.ConditionTree [(c, DOM.Break)] (DOM.StatementBlock [])])
  transform statement                        = L1 statement

instance Transformer L1 String where
  transform (L1 (DOM.ConditionTree  cs d)) = intercalate "else " (map (\(c,a) -> "if(" ++> c ++> "){" ++> a ++> "}") cs) ++> "else{" ++> d ++> "}"
  transform (L1 (DOM.Loop           lh a)) = lh ++> a
  transform (L1 (DOM.ExprAsStmt     e   )) = "(" ++> e ++> ")"
  transform (L1  DOM.Break               ) = "break"
  transform (L1  DOM.Continue            ) = "continue"
  transform (L1 (DOM.Var            v  e)) = "var " ++> v ++> if e == empty then ";" else "=(" ++> fromJust e ++> ")"
  transform (L1 (DOM.Return         e   )) = "return(" ++> e ++> ")"
  transform (L1 (DOM.StatementBlock ps  )) = "{" ++> intercalate ";" (map transform ps) ++> "}"

instance Transformer DOM.Statement String where
  transform p = transform (transform p :: L1) :: String

