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

class TypeVariables a where
  tvars :: a -> Set TypeVariable
  rvars :: a -> Set TypeName
  rvars x = Set.fromList [ tname | RecVar tname <- Set.toList (tvars x) ]

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
  = One
  | Bot
  | Skip
  | Seq (Type m) (Type m)
  | Poly Bool TypeName
  | Var TypeName
  | Rec TypeName (Type m)
  | Par (Type m) (Type m)
  | Mul (Type m) (Type m)
  | With [(Label, Type m)]
  | Plus [(Label, Type m)]
  | Put m -- internal use only
  | Get m -- internal use only
  deriving (Eq, Ord)

type TypeS = Type ()
type TypeM = Type Measure

-- DEFINITIONS

-- |A type definition is a pair consisting of a type name and a session type.
type TypeDef = (TypeName, TypeS)

instance TypeVariables (Type m) where
  tvars (Seq t s)      = Set.union (tvars t) (tvars s)
  tvars (Poly _ tname) = Set.singleton (PolyVar tname)
  tvars (Var tname)    = Set.singleton (RecVar tname)
  tvars (Rec tname t)  = Set.delete (RecVar tname) (tvars t)
  tvars (Par t s)      = Set.union (tvars t) (tvars s)
  tvars (Mul t s)      = Set.union (tvars t) (tvars s)
  tvars (With bs)      = Set.unions (map (tvars . snd) bs)
  tvars (Plus bs)      = Set.unions (map (tvars . snd) bs)
  tvars _              = Set.empty

tsubst :: TypeVariable -> Type m -> Type m -> Type m
tsubst tname t = tsubsts (Map.singleton tname t)

tsubsts :: TMap m -> Type m -> Type m
tsubsts tmap (Seq t s)      = Seq (tsubsts tmap t) (tsubsts tmap s)
tsubsts tmap (Par t s)      = Par (tsubsts tmap t) (tsubsts tmap s)
tsubsts tmap (Mul t s)      = Mul (tsubsts tmap t) (tsubsts tmap s)
tsubsts tmap (Plus bs)      = Plus (mapSnd (tsubsts tmap) bs)
tsubsts tmap (With bs)      = With (mapSnd (tsubsts tmap) bs)
tsubsts tmap (Put m)        = Put m
tsubsts tmap (Get m)        = Get m
tsubsts tmap (Poly d sname) | Just t <- Map.lookup (PolyVar sname) tmap = if d then dual t else t
tsubsts tmap (Var sname)    | Just t <- Map.lookup (RecVar sname) tmap = t
tsubsts tmap (Rec sname t)  = Rec sname (tsubsts (Map.delete (RecVar sname) tmap) t)
tsubsts tmap s              = s

instance TypeVariables a => TypeVariables [a] where
  tvars = Set.unions . map tvars

unfold :: Type m -> Type m
unfold (Seq t s) = unfold (unfold t |> s)
unfold t@(Rec tname s) = unfold (tsubst (RecVar tname) t s)
unfold t = t

dual :: Type m -> Type m
dual One            = Bot
dual Bot            = One
dual Skip           = Skip
dual (Seq t s)      = Seq (dual t) (dual s)
dual (Poly d tname) = Poly (not d) tname
dual (Var tname)    = Var tname
dual (Rec tname t)  = Rec tname (dual t)
dual (Par t s)      = Mul (dual t) (dual s)
dual (Mul t s)      = Par (dual t) (dual s)
dual (With bs)      = Plus (mapSnd (dual) bs)
dual (Plus bs)      = With (mapSnd (dual) bs)
dual (Put m)        = Get m
dual (Get m)        = Put m

-- if s is a (possibly folded) normal form then t |> s is a (possibly folded) normal form
(|>) :: Type m -> Type m -> Type m
(|>) One            _ = One
(|>) Bot            _ = Bot
(|>) Skip           k = k
(|>) (Seq t s)      k = t |> (s |> k)
(|>) (Poly d tname) k = Poly d tname >>> k -- CHECK THIS, MAY OR MAY NOT REQUIRE >>> k
(|>) (Var tname)    k = Var tname >>> k
(|>) (Rec tname t)  k = Rec tname t >>> k
(|>) (Par t s)      k = Par t (s |> k)
(|>) (Mul t s)      k = Mul t (s |> k)
(|>) (With bs)      k = With (mapSnd (|> k) bs)
(|>) (Plus bs)      k = Plus (mapSnd (|> k) bs)
(|>) (Put m)        k = Put m >>> k
(|>) (Get m)        k = Get m >>> k

(>>>) :: Type m -> Type m -> Type m
(>>>) t Skip = t
(>>>) t s    = Seq t s

normalize :: Type m -> Type m
normalize t = t |> Skip

expose :: Type m -> Type m
expose t = aux (normalize t)
  where
    aux t@(Rec tname s) = aux (tsubst (RecVar tname) t s)
    aux (Seq t s)       = aux t |> s
    aux t               = t

-- expose (Seq t s) = expose t |> expose s
-- expose t@(Rec tname s) = expose (tsubst (RecVar tname) t s)
-- expose t = t |> Skip

data Kind = Nullable | Unguarded TypeName | Guarded
  deriving (Eq, Ord)

kind :: Type m -> Maybe Kind
kind = go
  where
    go :: Type m -> Maybe Kind
    go One = return Guarded
    go Bot = return Guarded
    go Skip = return Nullable
    go (Seq t s) = do
      k <- go t
      case k of
        Unguarded tname -> return (Unguarded tname)
        Nullable -> go s
        Guarded -> do
          _ <- go s
          return Guarded
    go (Poly _ _) = return Nullable
    go (Var tname) = return (Unguarded tname)
    go (Rec tname t) = do
      k <- go t
      if k == Unguarded tname
        then Nothing
        else return k
    go (Par t s) = do
      _ <- go t
      _ <- go s
      return Guarded
    go (Mul t s) = do
      _ <- go t
      _ <- go s
      return Guarded
    go (With bs) = do
      forM_ bs (go . snd)
      return Guarded
    go (Plus bs) = do
      forM_ bs (go . snd)
      return Guarded
    go (Get _) = return Guarded
    go (Put _) = return Guarded

instance MeasureVariables t => MeasureVariables (Type t) where
  mvars One            = Set.empty
  mvars Bot            = Set.empty
  mvars Skip           = Set.empty
  mvars (Seq t s)      = Set.union (mvars t) (mvars s)
  mvars (Poly _ tname) = Set.empty
  mvars (Var tname)    = Set.empty
  mvars (Rec tname t)  = mvars t
  mvars (Par t s)      = Set.union (mvars t) (mvars s)
  mvars (Mul t s)      = Set.union (mvars t) (mvars s)
  mvars (With bs)      = Set.unions (map (mvars . snd) bs)
  mvars (Plus bs)      = Set.unions (map (mvars . snd) bs)
  mvars (Put m)        = mvars m
  mvars (Get m)        = mvars m
