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

data MeasureConstraint
  = CEq Measure Measure
  | CLe Measure Measure
  deriving (Eq, Ord)

class Ord a => MeasureVariables a where
  mvars :: a -> Set MVar

instance MeasureVariables Measure where
  mvars (MCon _) = Set.empty
  mvars (MRef x) = Set.singleton x
  mvars (MAdd m n) = Set.union (mvars m) (mvars n)
  mvars (MSub m n) = Set.union (mvars m) (mvars n)
  mvars (MMul w m) = mvars m

instance MeasureVariables MeasureConstraint where
  mvars (CEq m n) = Set.union (mvars m) (mvars n)
  mvars (CLe m n) = Set.union (mvars m) (mvars n)

instance (MeasureVariables a, MeasureVariables b) => MeasureVariables (a, b) where
  mvars (x, y) = Set.union (mvars x) (mvars y)

instance MeasureVariables a => MeasureVariables [a] where
  mvars = Set.unions . map mvars

instance MeasureVariables a => MeasureVariables (Set a) where
  mvars = Set.unions . Set.elems . Set.map mvars

type MPartition = Partition MVar
