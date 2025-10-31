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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sort)
import Control.Monad (forM_)

data TypeVariable = PolyVar TypeName | RecVar TypeName
  deriving (Eq, Ord)

type TMap m = Map TypeVariable (Type m)

class Ord a => TypeVariables a where
  tvars  :: a -> Set TypeVariable

data TVar = TVar Int
  deriving (Eq, Ord)

instance Enum TVar where
  toEnum = TVar
  fromEnum (TVar n) = n

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

instance Ord m => TypeVariables (Type m) where
  tvars (Poly _ tname) = Set.singleton (PolyVar tname)
  tvars (Var tname)    = Set.singleton (RecVar tname)
  tvars (Rec tname t)  = Set.delete (RecVar tname) (tvars t)
  tvars (Par t s)      = Set.union (tvars t) (tvars s)
  tvars (Mul t s)      = Set.union (tvars t) (tvars s)
  tvars (With bs)      = Set.unions (map (tvars . snd) bs)
  tvars (Plus bs)      = Set.unions (map (tvars . snd) bs)
  tvars (Put _ t)      = tvars t
  tvars (Get _ t)      = tvars t
  tvars (Seq t s)      = Set.union (tvars t) (tvars s)
  tvars _              = Set.empty

tsubst :: TypeVariable -> Type m -> Type m -> Type m
tsubst tname t = tsubsts (Map.singleton tname t)

tsubsts :: TMap m -> Type m -> Type m
tsubsts tmap (Seq t1 t2)    = Seq (tsubsts tmap t1) (tsubsts tmap t2)
tsubsts tmap (Par t1 t2)    = Par (tsubsts tmap t1) (tsubsts tmap t2)
tsubsts tmap (Mul t1 t2)    = Mul (tsubsts tmap t1) (tsubsts tmap t2)
tsubsts tmap (Plus bs)      = Plus (mapSnd (tsubsts tmap) bs)
tsubsts tmap (With bs)      = With (mapSnd (tsubsts tmap) bs)
tsubsts tmap (Put m t)      = Put m (tsubsts tmap t)
tsubsts tmap (Get m t)      = Get m (tsubsts tmap t)
tsubsts tmap (Poly d sname) | Just t <- Map.lookup (PolyVar sname) tmap = if d then dual t else t
tsubsts tmap (Var sname)    | Just t <- Map.lookup (RecVar sname) tmap = t
tsubsts tmap (Rec sname s)  = Rec sname (tsubsts (Map.delete (RecVar sname) tmap) s)
tsubsts tmap s = s

instance TypeVariables a => TypeVariables [a] where
  tvars = Set.unions . map tvars

unfold :: Ord m => Type m -> Type m
unfold t@(Rec tname s) = tsubst (RecVar tname) t s
unfold t = t

hnf :: Ord m => Type m -> Type m
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
  mvars Skip = Set.empty
  mvars (Seq t s) = Set.union (mvars t) (mvars s)
  mvars (Poly _ tname) = Set.empty
  mvars (Var tname) = Set.empty
  mvars (Rec tname t) = mvars t
  mvars One = Set.empty
  mvars Bot = Set.empty
  mvars (Par t s) = Set.union (mvars t) (mvars s)
  mvars (Mul t s) = Set.union (mvars t) (mvars s)
  mvars (With bs) = Set.unions (map (mvars . snd) bs)
  mvars (Plus bs) = Set.unions (map (mvars . snd) bs)
  mvars (Put m t) = Set.union (mvars m) (mvars t)
  mvars (Get m t) = Set.union (mvars m) (mvars t)
  msubst = fmap . msubst
