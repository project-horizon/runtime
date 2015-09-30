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
Description :  A semantic result monad.
Author	    :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License	    :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

A semantic result monad.
-}
module Language.Transformation.Semantics.SemanticResult
( SemanticResult (..)
) where

import           Control.Applicative
import           Control.Monad

import           Language.Transformation.Semantics.Class


data SemanticResult a
  = Result a
  | Error String

instance Functor SemanticResult where
  fmap f (Result a) = (Result . f) a
  fmap _ (Error  m) = Error m

instance Applicative SemanticResult where
  pure = Result

  (Result f) <*> r = fmap f r
  (Error  m) <*> _ = Error m

instance Alternative SemanticResult where
  empty = Error ""

  l@(Result {}) <|> _             = l
  (Error _    ) <|> r@(Result {}) = r
  (Error l    ) <|> (Error r    ) = Error (l ++ "\nor " ++ r)

instance Monad SemanticResult where
  return = pure

  fail = Error

  (Result a) >>= f = f a
  (Error  m) >>= _ = Error m

instance Semantics SemanticResult where
  report = fail

