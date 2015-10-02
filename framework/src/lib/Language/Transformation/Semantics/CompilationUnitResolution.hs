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
Description :  Algorithms for compilation unit resolution.
Author	    :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License	    :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

Algorithms for compilation unit resolution.
-}
module Language.Transformation.Semantics.CompilationUnitResolution
( CompilationUnitName (..)
, CompilationUnit (..)
, CompilationUnitResolver (..)
, resolveDependencies
) where

import           Language.Transformation.Semantics.Class


-- | The name of a compilation unit.
class (Eq a, Show a) => CompilationUnitName a

-- | A compilation unit for handling imports.
class CompilationUnit a where
  -- | The name of the compilation unit.
  unitName         :: (CompilationUnitName b) => a b -> b
  -- | The dependencies of the compilation unit.
  unitDependencies :: (CompilationUnitName b) => a b -> [b]

-- | A compilation unit resolver for handling import of compilation units.
class CompilationUnitResolver a where
  -- | Resolve the name of a compilation unit.
  resolveCompilationUnit :: (CompilationUnit b, CompilationUnitName c, Semantics m) => a b c -> c -> m (b c)


-- | Resolves the dependencies of a compilation unit.
resolveDependencies :: (CompilationUnitResolver a, CompilationUnit b, CompilationUnitName c, Semantics m) => a b c -> b c -> m [b c]
resolveDependencies cur cu = ld (unitDependencies cu) [cu]
  where
    ld []     ms = return (reverse ms)
    ld (i:is) ms = do
      m <- cur `resolveCompilationUnit` i
      ld is (m:ms)

