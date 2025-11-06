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
module SourceType where

import Common
import Atoms
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sort)
import Control.Monad (forM_)

-- data TypeVariable = PolyVar TypeName | RecVar TypeName
--   deriving (Eq, Ord)

-- type TMap m = Map TypeVariable (Type m)

-- class TypeVariables a where
--   tvars :: a -> Set TypeVariable
--   rvars :: a -> Set TypeName
--   rvars x = Set.fromList [ tname | RecVar tname <- Set.toList (tvars x) ]

-- data TVar = TVar Int
--   deriving (Eq, Ord)

-- instance Enum TVar where
--   toEnum = TVar
--   fromEnum (TVar n) = n

-- |Session type representation. In addition to the forms described in the
-- paper, we also provide a 'Rec' constructor to represent recursive session
-- types explicitly in a closed form that is easier to convert into regular
-- trees.
data Type
  = One
  | Bot
  | Skip
  | Seq Type Type
  | Poly Bool TypeName
  | Ref TypeName [TypeName]
  | Par Type Type
  | Mul Type Type
  | With [(Label, Type)]
  | Plus [(Label, Type)]
  | Dual Type
  deriving (Eq, Ord)

-- |A type definition is a pair consisting of a type name and a session type.
type TypeDef = (TypeName, ([TypeName], Type))

-- instance TypeVariables SourceType where
--   tvars (Seq t s)      = Set.union (tvars t) (tvars s)
--   tvars (Poly _ tname) = Set.singleton (PolyVar tname)
--   tvars (Var tname)    = Set.singleton (RecVar tname)
--   tvars (Rec tname t)  = Set.delete (RecVar tname) (tvars t)
--   tvars (Par t s)      = Set.union (tvars t) (tvars s)
--   tvars (Mul t s)      = Set.union (tvars t) (tvars s)
--   tvars (With bs)      = Set.unions (map (tvars . snd . snd) bs)
--   tvars (Plus bs)      = Set.unions (map (tvars . snd . snd) bs)
--   tvars _              = Set.empty

-- tsubst :: TypeVariable -> Type m -> Type m -> Type m
-- tsubst tname t = tsubsts (Map.singleton tname t)

-- tsubsts :: TMap m -> Type m -> Type m
-- tsubsts tmap (Seq t s)      = Seq (tsubsts tmap t) (tsubsts tmap s)
-- tsubsts tmap (Par t s)      = Par (tsubsts tmap t) (tsubsts tmap s)
-- tsubsts tmap (Mul t s)      = Mul (tsubsts tmap t) (tsubsts tmap s)
-- tsubsts tmap (Plus bs)      = Plus (mapSnd (fmap (tsubsts tmap)) bs)
-- tsubsts tmap (With bs)      = With (mapSnd (fmap (tsubsts tmap)) bs)
-- tsubsts tmap (Poly d sname) | Just t <- Map.lookup (PolyVar sname) tmap = if d then dual t else t
-- tsubsts tmap (Var sname)    | Just t <- Map.lookup (RecVar sname) tmap = t
-- tsubsts tmap (Rec sname t)  = Rec sname (tsubsts (Map.delete (RecVar sname) tmap) t)
-- tsubsts tmap s              = s

-- instance TypeVariables a => TypeVariables [a] where
--   tvars = Set.unions . map tvars

-- -- unfold :: Type m -> Type m
-- -- unfold (Seq t s) = unfold (unfold t |> s)
-- -- unfold t@(Rec tname s) = unfold (tsubst (RecVar tname) t s)
-- -- unfold t = t

-- dual :: Type m -> Type m
-- dual One            = Bot
-- dual Bot            = One
-- dual Skip           = Skip
-- dual (Seq t s)      = Seq (dual t) (dual s)
-- dual (Poly d tname) = Poly (not d) tname
-- dual (Var tname)    = Var tname
-- dual (Rec tname t)  = Rec tname (dual t)
-- dual (Par t s)      = Mul (dual t) (dual s)
-- dual (Mul t s)      = Par (dual t) (dual s)
-- dual (With bs)      = Plus (mapSnd (fmap dual) bs)
-- dual (Plus bs)      = With (mapSnd (fmap dual) bs)

-- normalize :: Type m -> Type m
-- normalize = go True []
--   where
--     go :: Bool -> [Type m] -> Type m -> Type m
--     go _    _        One             = One
--     go _    _        Bot             = Bot
--     go _    []       Skip            = Skip
--     go unf  (t : ts) Skip            = go unf ts t
--     go unf  ts       (Seq t s)       = go unf (s : ts) t
--     go _    _        (Poly d tname)  = Poly d tname
--     go _    ts       (Mul t s)       = Mul t (go False ts s)
--     go _    ts       (Par t s)       = Par t (go False ts s)
--     go _    ts       (Plus bs)       = Plus (mapSnd (fmap $ go False ts) bs)
--     go _    ts       (With bs)       = With (mapSnd (fmap $ go False ts) bs)
--     go True ts       t@(Rec tname s) = go True ts (tsubst (RecVar tname) t s)
--     go _    ts       t@(Rec _ _)     = Seq t (go False ts Skip)

-- instance MeasureVariables t => MeasureVariables (Type t) where
--   mvars One            = Set.empty
--   mvars Bot            = Set.empty
--   mvars Skip           = Set.empty
--   mvars (Seq t s)      = Set.union (mvars t) (mvars s)
--   mvars (Poly _ tname) = Set.empty
--   mvars (Var tname)    = Set.empty
--   mvars (Rec tname t)  = mvars t
--   mvars (Par t s)      = Set.union (mvars t) (mvars s)
--   mvars (Mul t s)      = Set.union (mvars t) (mvars s)
--   mvars (With bs)      = Set.unions [ Set.union (mvars m) (mvars t) | (_, (m, t)) <- bs ]
--   mvars (Plus bs)      = Set.unions [ Set.union (mvars m) (mvars t) | (_, (m, t)) <- bs ]
  