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

-- |This module defines the external representation of __session types__
-- (Section 3.1).
module Type where

import Common
import Atoms
import Measure
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort)

-- |Session type representation. In addition to the forms described
-- in the paper, we also provide a 'Rec' constructor to represent
-- recursive session types explicitly in a closed form that is
-- easier to convert into regular trees.
data Type m
  = One
  | Bot
  -- |Session type variable.
  | Var TypeName
  -- |Recursive session type.
  | Rec TypeName (Type m)
  -- |Input/output of a channel.
  | Par (Type m) (Type m)
  | Mul (Type m) (Type m)
  | With [(Label, Type m)]
  | Plus [(Label, Type m)]
  | Put m (Type m)
  | Get m (Type m)
  deriving (Eq, Ord)

type TypeS = Type (Maybe Int)
data TypeE = Type TypeS | Dual TypeS
type TypeM = Type Measure

type TypeDef = (TypeName, TypeS)

-- DEFINITIONS

-- |A type definition is a pair consisting of a type name and a
-- session type.
-- type TypeDef = (TypeName, Type ())

ftv :: Type m -> Set TypeName
ftv (Var tname) = Set.singleton tname
ftv (Rec tname t) = Set.delete tname (ftv t)
ftv (Par t s) = Set.union (ftv t) (ftv s)
ftv (Mul t s) = Set.union (ftv t) (ftv s)
ftv (With bs) = Set.unions (map (ftv . snd) bs)
ftv (Plus bs) = Set.unions (map (ftv . snd) bs)
ftv (Put _ t) = ftv t
ftv (Get _ t) = ftv t
ftv _ = Set.empty

substT :: TypeName -> Type m -> Type m -> Type m
substT tname t = aux
  where
    aux (Par t1 t2) = Par (aux t1) (aux t2)
    aux (Mul t1 t2) = Mul (aux t1) (aux t2)
    aux (Plus bs)   = Plus (mapSnd aux bs)
    aux (With bs)   = With (mapSnd aux bs)
    aux (Put m t)   = Put m (aux t)
    aux (Get m t)   = Get m (aux t)
    aux (Var sname) | tname == sname = t
    aux (Rec sname s) | tname /= sname = Rec sname (aux s)
    aux s = s

unfold :: Type m -> Type m
unfold t@(Rec tname s) = substT tname t s
unfold t = t

dual :: Type m -> Type m
dual = aux
  where
    aux (Var tname) = Var tname
    aux (Rec tname t) = Rec tname (aux t)
    aux One = Bot
    aux Bot = One
    aux (Par t s) = Mul (aux t) (aux s)
    aux (Mul t s) = Par (aux t) (aux s)
    aux (With bs) = Plus (mapSnd aux bs)
    aux (Plus bs) = With (mapSnd aux bs)
    aux (Put m t) = Get m (aux t)
    aux (Get m t) = Put m (aux t)

instance Functor Type where
  fmap f (Var tname) = Var tname
  fmap f (Rec tname t) = Rec tname (fmap f t)
  fmap f One = One
  fmap f Bot = Bot
  fmap f (Par t s) = Par (fmap f t) (fmap f s)
  fmap f (Mul t s) = Mul (fmap f t) (fmap f s)
  fmap f (With bs) = With (mapSnd (fmap f) bs)
  fmap f (Plus bs) = Plus (mapSnd (fmap f) bs)
  fmap f (Put m t) = Put (f m) (fmap f t)
  fmap f (Get m t) = Get (f m) (fmap f t)

instance MeasureVariables t => MeasureVariables (Type t) where
  mv (Var tname) = Set.empty
  mv (Rec tname t) = mv t
  mv One = Set.empty
  mv Bot = Set.empty
  mv (Par t s) = Set.union (mv t) (mv s)
  mv (Mul t s) = Set.union (mv t) (mv s)
  mv (With bs) = Set.unions (map (mv . snd) bs)
  mv (Plus bs) = Set.unions (map (mv . snd) bs)
  mv (Put m t) = Set.union (mv m) (mv t)
  mv (Get m t) = Set.union (mv m) (mv t)
  subst = fmap . subst
