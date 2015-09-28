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

{-# LANGUAGE FlexibleInstances #-}

{- |
Module      :  $Header$
Description :  Generalized semantic analysis.
Author	    :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License	    :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

Generalized semantic analysis.
-}
module Language.Transformation.Semantics
( Semantics (..)
, Scope (..)
, FullyQualifiedName (..)
, HierarchicalScope (..)
) where

import           Control.Applicative
import           Control.Monad

import           Language.Transformation.Protocol

import           Data.List (intercalate)


-- | A special monad for handling semantic constraints in language
--   analysis.
class (Monad m) => Semantics m where
  -- | Report a semantic error during analysis.
  report :: String -> m a


instance Semantics (Either String) where
  report = Left


-- | Functionality for handling fully qualified names.
class (Eq a) => FullyQualifiedName a where
  -- | The fully qualified name.
  fullyQualifiedName :: a -> a
  -- | The namespace of the identifier.
  namespace          :: a -> a
  -- | The name of the value.
  name               :: a -> a

split :: String -> [String]
split s = split' s []
  where
    split' :: String -> String -> [String]
    split' []       p = [reverse p]
    split' ('.':xs) p = reverse p : split' xs []
    split' (x  :xs) p = split' xs (x : p)

instance FullyQualifiedName String where
  fullyQualifiedName s = s
  namespace          s = intercalate "." (reverse (drop 1 (reverse (split s))))
  name               s = let vs = split s; [v] = drop (length vs - 1) vs; in v

-- | A type class representing the required functionality of a scope.
class Scope a where
  -- | Checks if the given identifier is contained within a scope.
  isIn :: (FullyQualifiedName b) => b -> a b -> Bool

data (FullyQualifiedName a) => HierarchicalScope a
  = HScope [a] (HierarchicalScope a)
  | IScope [a]
  deriving (Show, Eq)

instance Scope HierarchicalScope where
  isIn fqn (HScope ids  p) = fullyQualifiedName fqn `elem` ids || fqn `isIn` p
  isIn fqn (IScope ids   ) = fullyQualifiedName fqn `elem` ids

