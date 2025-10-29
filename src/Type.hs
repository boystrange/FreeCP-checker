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

-- |This module defines the internal representation of __types__ (Section 3).
module Type where

import Common
import Atoms
import Measure
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort)
import Control.Monad (forM_)

data TypeVariable = PolyVar TypeName | RecVar TypeName
  deriving (Eq, Ord)

-- |Session type representation. In addition to the forms described in the
-- paper, we also provide a 'Rec' constructor to represent recursive session
-- types explicitly in a closed form that is easier to convert into regular
-- trees.
data Type m
  -- |Constants
  = One
  | Bot
  -- |Polymorphism
  | Poly Bool TypeName
  -- |Sequential composition
  | Skip
  | Seq (Type m) (Type m)
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

-- DEFINITIONS

-- |A type definition is a pair consisting of a type name and a session type.
type TypeDef = (TypeName, TypeS)

wf :: Type m -> Bool
wf t = case aux [] t of
          Nothing -> False
          Just _  -> True
  where
    aux us (Poly _ _) = return 1
    aux us (Var tname) | tname `elem` us = Nothing
                       | otherwise = return 1
    aux us (Rec tname t) = do
      n <- aux (tname : us) t
      if n == 0
        then Nothing
        else return 1
    aux us Skip = return 0
    aux us (Seq t s) = do
      m <- aux us t
      n <- aux (if m == 0 then us else []) s
      return (if m == 1 then 1 else n)
    aux us One = return 1
    aux us Bot = return 1
    aux us (Par t s) = do
      _ <- aux [] t
      _ <- aux [] s
      return 1
    aux us (Mul t s) = do
      _ <- aux [] t
      _ <- aux [] s
      return 1
    aux us (With bs) = do
      forM_ bs (aux us . snd)
      return 1
    aux us (Plus bs) = do
      forM_ bs (aux us . snd)
      return 1
    aux us (Put _ t) = aux us t
    aux us (Get _ t) = aux us t

-- |Compute the sets of free type variables of a type. The first component is
-- the set of variables used for recursion. The second component is the set of
-- polymorphic type variables.
fv :: Type m -> Set TypeVariable
fv (Poly _ tname) = Set.singleton (PolyVar tname)
fv (Var tname)    = Set.singleton (RecVar tname)
fv (Rec tname t)  = Set.delete (RecVar tname) (fv t)
fv (Par t s)      = Set.union (fv t) (fv s)
fv (Mul t s)      = Set.union (fv t) (fv s)
fv (With bs)      = Set.unions (map (fv . snd) bs)
fv (Plus bs)      = Set.unions (map (fv . snd) bs)
fv (Put _ t)      = fv t
fv (Get _ t)      = fv t
fv (Seq t s)      = Set.union (fv t) (fv s)
fv _              = Set.empty

substT :: TypeVariable -> Type m -> Type m -> Type m
substT tname t = aux
  where
    aux (Seq t1 t2)    = Seq (aux t1) (aux t2)
    aux (Par t1 t2)    = Par (aux t1) (aux t2)
    aux (Mul t1 t2)    = Mul (aux t1) (aux t2)
    aux (Plus bs)      = Plus (mapSnd aux bs)
    aux (With bs)      = With (mapSnd aux bs)
    aux (Put m t)      = Put m (aux t)
    aux (Get m t)      = Get m (aux t)
    aux (Poly d sname) | tname == PolyVar sname = (if d then dual else id) t
    aux (Var sname)    | tname == RecVar sname  = t
    aux (Rec sname s)  | tname /= RecVar sname  = Rec sname (aux s)
    aux s = s

unfold :: Type m -> Type m
unfold t@(Rec tname s) = substT (RecVar tname) t s
unfold t = t

hnf :: Type m -> Type m
hnf t =
  case unfold t of
    Seq t1 t2 ->
      case hnf t1 of
        Skip -> hnf t2
        Seq (Poly d tname) s2 -> Seq (Poly d tname) (Seq s2 t2)
        One -> One
        Bot -> Bot
        Mul s1 s2 -> Mul s1 (Seq s2 t2)
        Par s1 s2 -> Par s1 (Seq s2 t2)
        Plus bs -> Plus (map (\(l, s) -> (l, Seq s t2)) bs)
        With bs -> With (map (\(l, s) -> (l, Seq s t2)) bs)
        Put m t -> Put m (Seq t t2)
        Get m t -> Get m (Seq t t2)
        _ -> error "this should be impossible"
    Poly d tname -> Seq (Poly d tname) Skip
    Rec _ _ -> error "this should be impossible"
    Var _ -> error "this should be impossible"
    s -> s

dual :: Type m -> Type m
dual = aux
  where
    aux Skip = Skip
    aux (Seq t s) = Seq (aux t) (aux s)
    aux (Poly d tname) = Poly (not d) tname
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
  fmap f Skip = Skip
  fmap f (Seq t s) = Seq (fmap f t) (fmap f s)
  fmap f (Poly d tname) = Poly d tname
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
  mv Skip = Set.empty
  mv (Seq t s) = Set.union (mv t) (mv s)
  mv (Poly _ tname) = Set.empty
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
