-- MIT License
-- 
-- Copyright (c) 2025 Luca Padovani
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

-- |Expansion of session types into closed recursive terms.
module Resolver (resolve) where

import Common
import Atoms
import Exceptions
import Measure
import Type
import Process
import Control.Exception (throw)
import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as State

import qualified Data.Set as Set

-- |Given a list of type definitions and a possibly open type,
-- create a closed type.
resolveT :: [TypeDef] -> TypeS -> TypeS
resolveT tdefs = aux []
  where
    aux :: [TypeName] -> TypeS -> TypeS
    aux tnames One  = One
    aux tnames Bot  = Bot
    aux tnames Skip = Skip
    aux tnames (Seq t s) = Seq (aux tnames t) (aux tnames s)
    aux tnames (Var d tname) = Var d tname
    aux tnames (Rec tname t) = Rec tname (aux (tname : tnames) t)
    aux tnames (Inv tname) | tname `elem` tnames = Inv tname
    aux tnames (Inv tname) =
      case lookup tname tdefs of
        Nothing -> throw (ErrorUnknownIdentifier "type" (showWithPos tname))
        Just t  ->
          let s = aux (tname : tnames) t in
            if Set.member (RecVar tname) (tvars s)
            then Rec tname s
            else s
    aux tnames (Par t s) = Par (aux tnames t) (aux tnames s)
    aux tnames (Mul t s) = Mul (aux tnames t) (aux tnames s)
    aux tnames (Plus bs) = Plus (mapSnd (aux tnames) bs)
    aux tnames (With bs) = With (mapSnd (aux tnames) bs)
    aux tnames (Put m)   = Put m
    aux tnames (Get m)   = Get m

resolveE :: [TypeDef] -> TypeE -> TypeE
resolveE tdefs (Copy t) = Copy (resolveT tdefs t)
resolveE tdefs (Dual t) = Dual (resolveT tdefs t)

-- |Given a list of type definitions and a process, close all types
-- occurring in the process.
resolveP :: [TypeDef] -> ProcessS -> ProcessS
resolveP tdefs = aux
  where
    aux (Link x y)       = Link x y
    aux (Call pname xs)  = Call pname xs
    aux (Wait x p)       = Wait x (aux p)
    aux (Close x)        = Close x
    aux (Fork x y p q)   = Fork x y (aux p) (aux q)
    aux (Join x y p)     = Join x y (aux p)
    aux (Select x tag p) = Select x tag (aux p)
    aux (Case x bs)      = Case x (mapSnd aux bs)
    aux (Cut x t p q)    = Cut x (resolveE tdefs t) (aux p) (aux q)
    aux (PutGas x p)     = PutGas x (aux p)
    aux (GetGas x p)     = GetGas x (aux p)

-- |Given a list of type definitions and a list of process
-- definitions, close all process definitions.
resolve :: [TypeDef] -> [ProcessDefS] -> [ProcessDefS]
resolve tdefs = map auxD
  where
    auxD :: ProcessDefS -> ProcessDefS
    auxD (pname, xts, p) = (pname, map (uncurry auxE) xts, resolveP tdefs p)

    auxE :: ChannelName -> TypeE -> (ChannelName, TypeE)
    auxE x t = (x, resolveE tdefs t)
