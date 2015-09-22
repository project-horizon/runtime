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

{-# LANGUAGE GADTs #-}

{- |
Module      :  $Header$
Description :  A declaration in the PolyDSL language.
Author	    :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License	    :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

A declaration in the PolyDSL language.
-}
module Language.PolyDSL.DOM.Declaration
( module Export

, Declaration (..)
) where

import           Language.PolyDSL.DOM.Expression
import           Language.PolyDSL.DOM.Declaration.Type as Export


-- | A PolyDSL declaration.
data Declaration a where
  -- | Import declaration.
  Import :: String -> Declaration String -- | TODO: decide if Declaration String is a proper type.
  -- | Function declaration.
  Function :: String -> [String] -> Expression a -> Declaration a
  -- | Signature declaration.
  Signature :: String -> [Type] -> Declaration ()


instance Show (Declaration a) where
  show (Import    p     ) = "Import " ++ show p
  show (Function  f ps e) = "Function " ++ show f ++ " " ++ show ps ++ " (" ++ show e ++ ")"
  show (Signature s ps  ) = "Signature " ++ show s ++ " " ++ show ps

instance Eq (Declaration a) where
  (Import    pl       ) == (Import    pr       ) = pl == pr
  (Function  fl psl el) == (Function  fr psr er) = fl == fr && psl == psr && el == er
  (Signature sl psl   ) == (Signature sr psr   ) = sl == sr && psl == psr
  _                     == _                     = False

