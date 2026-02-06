-- MIT License
--
-- Copyright (c) 2026 Luca Padovani
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

-- |Automatic instrumentation
module Instrumenter where

import Atoms
import Common
import Type
import Process

instrument :: [ProcessDefS] -> [ProcessDefS]
instrument = map goD
  where
    goD :: ProcessDefS -> ProcessDefS
    goD (pname, xts, p) = (pname, mapSnd goE xts, goP p)

    goP :: ProcessS -> ProcessS
    goP (Wait x p) = Wait x (goP p)
    goP (Fork x y p q) = Fork x y (goP p) (goP q)
    goP (Join x y p) = Join x y (goP p)
    goP (Select x tag p) = Select x tag (PutGas x (goP p))
    goP (Case x bs) = Case x (mapSnd (GetGas x . goP) bs)
    goP (Cut x t p q) = Cut x (goE t) (goP p) (goP q)
    goP (PutGas x p) = PutGas x (goP p)
    goP (GetGas x p) = GetGas x (goP p)
    goP p = p

    goE :: TypeE -> TypeE
    goE (Copy t) = Copy (goT t)
    goE (Dual t) = Dual (goT t)

    goT :: TypeS -> TypeS
    goT (Inv tname) = Inv tname
    goT (Rec tname t) = Rec tname (goT t)
    goT (Seq t s) = Seq (goT t) (goT s)
    goT (Par t s) = Par (goT t) (goT s)
    goT (Mul t s) = Mul (goT t) (goT s)
    goT (With bs) = With (mapSnd (Seq (Get ()) . goT) bs)
    goT (Plus bs) = Plus (mapSnd (Seq (Put ()) . goT) bs)
    goT (Put m)   = Put m
    goT (Get m)   = Get m
    goT t = t
