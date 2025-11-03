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
  | Put m (Type m)
  | Get m (Type m)
  | Dual (Type m)
  deriving (Eq, Ord)

type TypeS = Type ()
type TypeM = Type Measure

-- DEFINITIONS

-- |A type definition is a pair consisting of a type name and a session type.
type TypeDef = (TypeName, TypeS)

-- wf :: Type m -> Bool
-- wf t = case aux [] t of
--           Nothing -> False
--           Just _  -> True
--   where
--     aux us (Poly _ _) = return 1
--     aux us (Var tname _) | tname `elem` us = Nothing
--                          | otherwise = return 1
--     aux us (Rec tname t) = do
--       n <- aux (tname : us) t
--       if n == 0
--         then Nothing
--         else return 1
--     aux us Skip = return 0
--     aux us One = return 1
--     aux us Bot = return 1
--     aux us (Par t s) = do
--       _ <- aux [] t
--       _ <- aux [] s
--       return 1
--     aux us (Mul t s) = do
--       _ <- aux [] t
--       _ <- aux [] s
--       return 1
--     aux us (With bs) = do
--       forM_ bs (aux us . snd)
--       return 1
--     aux us (Plus bs) = do
--       forM_ bs (aux us . snd)
--       return 1
--     aux us (Put _ t) = aux us t
--     aux us (Get _ t) = aux us t

instance TypeVariables (Type m) where
  tvars (Seq t s)      = Set.union (tvars t) (tvars s)
  tvars (Poly _ tname) = Set.singleton (PolyVar tname)
  tvars (Var tname)    = Set.singleton (RecVar tname)
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
tsubsts tmap (Seq t s)      = Seq (tsubsts tmap t) (tsubsts tmap s)
tsubsts tmap (Par t s)      = Par (tsubsts tmap t) (tsubsts tmap s)
tsubsts tmap (Mul t s)      = Mul (tsubsts tmap t) (tsubsts tmap s)
tsubsts tmap (Plus bs)      = Plus (mapSnd (tsubsts tmap) bs)
tsubsts tmap (With bs)      = With (mapSnd (tsubsts tmap) bs)
tsubsts tmap (Put m t)      = Put m (tsubsts tmap t)
tsubsts tmap (Get m t)      = Get m (tsubsts tmap t)
tsubsts tmap (Poly d sname) | Just t <- Map.lookup (PolyVar sname) tmap = if d then dual t else t
tsubsts tmap (Var sname)    | Just t <- Map.lookup (RecVar sname) tmap = t
tsubsts tmap (Rec sname t)  = Rec sname (tsubsts (Map.delete (RecVar sname) tmap) t)
tsubsts tmap s              = s

instance TypeVariables a => TypeVariables [a] where
  tvars = Set.unions . map tvars

(|>) :: Type m -> Type m -> Type m
(|>) One            _ = One
(|>) Bot            _ = Bot
(|>) Skip           k = k
(|>) (Seq t s)      k = t |> (s |> k)
(|>) (Poly d tname) _ = Poly d tname
(|>) (Var tname)    k = Seq (Var tname) k
(|>) (Rec tname t)  k = Seq (Rec tname t) k
(|>) (Par t s)      k = Par t (s |> k)
(|>) (Mul t s)      k = Mul t (s |> k)
(|>) (With bs)      k = With (mapSnd (|> k) bs)
(|>) (Plus bs)      k = Plus (mapSnd (|> k) bs)
(|>) (Put m t)      k = Put m (t |> k)
(|>) (Get m t)      k = Get m (t |> k)

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
dual (Put m t)      = Get m (dual t)
dual (Get m t)      = Put m (dual t)

normalize :: Type m -> Type m
normalize = go True []
  where
    go :: Bool -> [Type m] -> Type m -> Type m
    go _    _        One             = One
    go _    _        Bot             = Bot
    go _    []       Skip            = Skip
    go unf  (t : ts) Skip            = go unf ts t
    go unf  ts       (Seq t s)       = go unf (s : ts) t
    go _    _        (Poly d tname)  = Poly d tname
    go _    ts       (Mul t s)       = Mul t (go False ts s)
    go _    ts       (Par t s)       = Par t (go False ts s)
    go _    ts       (Plus bs)       = Plus (mapSnd (go False ts) bs)
    go _    ts       (With bs)       = With (mapSnd (go False ts) bs)
    go _    ts       (Put m t)       = Put m (go False ts t)
    go _    ts       (Get m t)       = Get m (go False ts t)
    go True ts       t@(Rec tname s) = go True ts (tsubst (RecVar tname) t s)
    go _    ts       t@(Rec _ _)     = Seq t (go False ts Skip)

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
  mvars (Put m t)      = Set.union (mvars m) (mvars t)
  mvars (Get m t)      = Set.union (mvars m) (mvars t)
