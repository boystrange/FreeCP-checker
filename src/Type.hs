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
  | Skip
  | Poly Bool TypeName
  | Var TypeName (Type m)
  | Rec TypeName (Type m)
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
    aux us (Var tname _) | tname `elem` us = Nothing
                         | otherwise = return 1
    aux us (Rec tname t) = do
      n <- aux (tname : us) t
      if n == 0
        then Nothing
        else return 1
    aux us Skip = return 0
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

qes :: Type m -> Type m -> Type m
qes _ One            = One
qes _ Bot            = Bot
qes k Skip           = k
qes _ (Poly d tname) = Poly d tname
qes k (Var tname t)  = Var tname (qes k t)
qes k (Rec tname t)  = Rec tname (qes k t)
qes k (Par t s)      = Par t (qes k s)
qes k (Mul t s)      = Mul t (qes k s)
qes k (With bs)      = With (mapSnd (qes k) bs)
qes k (Plus bs)      = Plus (mapSnd (qes k) bs)
qes k (Put m t)      = Put m (qes k t)
qes k (Get m t)      = Get m (qes k t)

instance Ord m => TypeVariables (Type m) where
  tvars (Poly _ tname) = Set.singleton (PolyVar tname)
  tvars (Var tname t)  = Set.insert (RecVar tname) (tvars t)
  tvars (Rec tname t)  = Set.delete (RecVar tname) (tvars t)
  tvars (Par t s)      = Set.union (tvars t) (tvars s)
  tvars (Mul t s)      = Set.union (tvars t) (tvars s)
  tvars (With bs)      = Set.unions (map (tvars . snd) bs)
  tvars (Plus bs)      = Set.unions (map (tvars . snd) bs)
  tvars (Put _ t)      = tvars t
  tvars (Get _ t)      = tvars t
  tvars _              = Set.empty

tsubst :: TypeVariable -> Type m -> Type m -> Type m
tsubst tname t = tsubsts (Map.singleton tname t)

tsubsts :: TMap m -> Type m -> Type m
tsubsts tmap (Par t s)      = Par (tsubsts tmap t) (tsubsts tmap s)
tsubsts tmap (Mul t s)      = Mul (tsubsts tmap t) (tsubsts tmap s)
tsubsts tmap (Plus bs)      = Plus (mapSnd (tsubsts tmap) bs)
tsubsts tmap (With bs)      = With (mapSnd (tsubsts tmap) bs)
tsubsts tmap (Put m t)      = Put m (tsubsts tmap t)
tsubsts tmap (Get m t)      = Get m (tsubsts tmap t)
tsubsts tmap (Poly d sname) | Just t <- Map.lookup (PolyVar sname) tmap = if d then dual t else t
tsubsts tmap (Var sname s)  | Just t <- Map.lookup (RecVar sname) tmap = qes s t
tsubsts tmap (Rec sname s)  = Rec sname (tsubsts (Map.delete (RecVar sname) tmap) s)
tsubsts tmap s              = s

instance TypeVariables a => TypeVariables [a] where
  tvars = Set.unions . map tvars

unfold :: Ord m => Type m -> Type m
unfold t@(Rec tname s) = tsubst (RecVar tname) t s
unfold t = t

-- hnf :: Ord m => Type m -> Type m
-- hnf t =
--   case unfold t of
--     Seq t1 t2 ->
--       case hnf t1 of
--         Skip -> hnf t2
--         Seq (Poly d tname) s2 -> Seq (Poly d tname) (Seq s2 t2)
--         One -> One
--         Bot -> Bot
--         Mul s1 s2 -> Mul s1 (Seq s2 t2)
--         Par s1 s2 -> Par s1 (Seq s2 t2)
--         Plus bs -> Plus (map (\(l, s) -> (l, Seq s t2)) bs)
--         With bs -> With (map (\(l, s) -> (l, Seq s t2)) bs)
--         Put m s -> Put m (Seq s t2)
--         Get m s -> Get m (Seq s t2)
--         _ -> error "this should be impossible"
--     Poly d tname -> Seq (Poly d tname) Skip
--     Rec _ _ -> error "this should be impossible"
--     Var _ -> error "this should be impossible"
--     s -> s

dual :: Type m -> Type m
dual One            = Bot
dual Bot            = One
dual Skip           = Skip
dual (Poly d tname) = Poly (not d) tname
dual (Var tname t)  = Var tname (dual t)
dual (Rec tname t)  = Rec tname (dual t)
dual (Par t s)      = Mul (dual t) (dual s)
dual (Mul t s)      = Par (dual t) (dual s)
dual (With bs)      = Plus (mapSnd dual bs)
dual (Plus bs)      = With (mapSnd dual bs)
dual (Put m t)      = Get m (dual t)
dual (Get m t)      = Put m (dual t)

instance Functor Type where
  fmap f One            = One
  fmap f Bot            = Bot
  fmap f Skip           = Skip
  fmap f (Poly d tname) = Poly d tname
  fmap f (Var tname t)  = Var tname (fmap f t)
  fmap f (Rec tname t)  = Rec tname (fmap f t)
  fmap f (Par t s)      = Par (fmap f t) (fmap f s)
  fmap f (Mul t s)      = Mul (fmap f t) (fmap f s)
  fmap f (With bs)      = With (mapSnd (fmap f) bs)
  fmap f (Plus bs)      = Plus (mapSnd (fmap f) bs)
  fmap f (Put m t)      = Put (f m) (fmap f t)
  fmap f (Get m t)      = Get (f m) (fmap f t)

instance MeasureVariables t => MeasureVariables (Type t) where
  mvars One              = Set.empty
  mvars Bot              = Set.empty
  mvars Skip             = Set.empty
  mvars (Poly _ tname)   = Set.empty
  mvars (Var tname t)    = mvars t
  mvars (Rec tname t)    = mvars t
  mvars (Par t s)        = Set.union (mvars t) (mvars s)
  mvars (Mul t s)        = Set.union (mvars t) (mvars s)
  mvars (With bs)        = Set.unions (map (mvars . snd) bs)
  mvars (Plus bs)        = Set.unions (map (mvars . snd) bs)
  mvars (Put m t)        = Set.union (mvars m) (mvars t)
  mvars (Get m t)        = Set.union (mvars m) (mvars t)
  msubst = fmap . msubst
