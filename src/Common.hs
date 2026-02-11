-- MIT License
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

-- |This module contains general-purpose functions that are not found in
-- Haskell's standard library.
module Common where

import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Merge
import Data.Char (chr, ord, isDigit)

-- |Representation of binary relations on u as sets of pairs (u, u)
type Rel u = Set (u, u)

-- |Convert an integer into a subscript
subscript :: Int -> String
subscript = map convert . show
  where
    convert :: Char -> Char
    convert ch | isDigit ch = chr (ord ch - ord '0' + ord '₀')
               | ch == '-' = '₋'
               | otherwise = error "impossible"

-- |Map the second component of a pair
mapSnd :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnd f = map (\(x, y) -> (x, f y))

-- |Compute the least fixed point of a given function
limit :: Eq a => (a -> a) -> a -> a
limit f x | x == y = x
          | otherwise = limit f y
  where
    y = f x

-- |Compute the zip of two maps, dropping those elements found in
-- one map but not the other
zipMap :: Ord k => Map k a -> Map k b -> Map k (a, b)
zipMap = Merge.merge Merge.dropMissing Merge.dropMissing (Merge.zipWithMatched (const (,)))

-- |'mergeMap' merges two maps, using different functions to transform those
-- elements found in one map but not the other and those elements found in both
-- maps.
mergeMap :: Ord k => (a -> c) -> (b -> c) -> (a -> b -> c) -> Map k a -> Map k b -> Map k c
mergeMap f g h = Merge.merge (Merge.mapMissing (const f)) (Merge.mapMissing (const g)) (Merge.zipWithMatched (const h))

-- |Pretty-print a set using the standard notation
showSet :: Show a => Set a -> String
showSet eset | null es = "∅"
             | otherwise = "{" ++ List.intercalate ", " (map show es) ++ "}"
  where
    es = Set.elems eset

allSet :: (a -> Bool) -> Set a -> Bool
allSet p set = all p (Set.elems set)

-- |Compute the transitive of a relation
closure :: Ord u => Rel u -> Rel u
closure = limit aux
  where
    aux rel = Set.union rel (Set.fromList [ (x, z) | let rels = Set.elems rel
                                                   , (x, y) <- rels
                                                   , (y', z) <- rels
                                                   , y == y' ])
