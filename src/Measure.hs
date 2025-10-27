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

module Measure where

import Common
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Partition (Partition)
import qualified Data.Partition as Partition
import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (sort)

data MVar = MVar Int
  deriving (Eq, Ord)

type MSubst = Partition MVar

instance Enum MVar where
  toEnum = MVar
  fromEnum (MVar n) = n

data Measure
  = MCon Int
  | MRef MVar
  | MAdd Measure Measure
  | MSub Measure Measure
  | MMul Double Measure
  deriving (Eq, Ord)

mzero :: Measure
mzero = MCon 0

mone :: Measure
mone = MCon 1

madd :: Measure -> Measure -> Measure
madd = MAdd

msucc :: Measure -> Measure
msucc = madd mone

data Constraint
  = CEq Measure Measure
  | CLe Measure Measure
  deriving (Eq, Ord)

class Ord a => MeasureVariables a where
  mv :: a -> Set MVar
  subst :: MSubst -> a -> a

instance MeasureVariables Measure where
  mv (MCon _) = Set.empty
  mv (MRef x) = Set.singleton x
  mv (MAdd m n) = Set.union (mv m) (mv n)
  mv (MSub m n) = Set.union (mv m) (mv n)
  mv (MMul w m) = mv m

  subst σ (MCon n) = MCon n
  subst σ (MRef x) = MRef (Partition.rep σ x)
  subst σ (MAdd m n) = MAdd (subst σ m) (subst σ n)
  subst σ (MSub m n) = MSub (subst σ m) (subst σ n)
  subst σ (MMul w m) = MMul w (subst σ m)

instance MeasureVariables Constraint where
  mv (CEq m n) = Set.union (mv m) (mv n)
  mv (CLe m n) = Set.union (mv m) (mv n)
  subst σ (CEq m n) = CEq (subst σ m) (subst σ n)
  subst σ (CLe m n) = CLe (subst σ m) (subst σ n)

instance (MeasureVariables a, MeasureVariables b) => MeasureVariables (a, b) where
  mv (x, y) = Set.union (mv x) (mv y)
  subst σ (x, y) = (subst σ x, subst σ y)

instance MeasureVariables a => MeasureVariables [a] where
  mv = Set.unions . map mv
  subst = map . subst

instance MeasureVariables a => MeasureVariables (Set a) where
  mv = Set.unions . Set.elems . Set.map mv
  subst = Set.map . subst

type MPartition = Partition MVar

gatherSubstitutions :: [Constraint] -> (MSubst, [Constraint])
gatherSubstitutions = foldl aux (Partition.empty, [])
  where
    aux (σ, cs) (CEq (MRef μ) (MRef ν)) = (Partition.joinElems μ ν σ, cs)
    aux (σ, cs) c = (σ, c : cs)
