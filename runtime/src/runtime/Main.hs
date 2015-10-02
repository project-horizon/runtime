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
Description :  The application entry point.
Author	    :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License	    :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

The application entry point.
-}
module Main
( main
) where

import           Control.Monad

import           Data.List      (intercalate)

import           System.Exit    (exitWith, ExitCode(ExitFailure))
import           System.IO      (hPutStrLn, stderr)
import           System.Process (readCreateProcess, shell)

import           Language.PolyDSL
import           Language.PolyDSL.Lib
import           Language.Transformation.Protocol
import           Language.Transformation.Semantics

import qualified Language.JavaScript as JS


moduleMain = defModule "Main" []
  [ include "Data.Bool"
  ]

stdLibResolver = VirtualResolver stdLib

main :: IO ()
main = case transform (MainModule (stdLibResolver, moduleMain)) of
  Result a -> putStrLn a
  Error  m -> do
    hPutStrLn stderr m
    exitWith (ExitFailure (-1))

