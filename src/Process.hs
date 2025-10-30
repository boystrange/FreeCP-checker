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

-- |Representation of processes (Section 4).
module Process where

import Common
import Atoms
import Measure
import Type
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- |Representation of processes.
data Process t
  -- |Process invocation.
  = Call ProcessName [ChannelName]
  -- |Link.
  | Link ChannelName ChannelName
  -- |Receive session termination signal.
  | Wait ChannelName (Process t)
  -- |Send session termination signal.
  | Close ChannelName
  -- |Fork/output of channel.
  | Fork ChannelName ChannelName (Process t) (Process t)
  -- |Join/input of channel.
  | Join ChannelName ChannelName (Process t)
  -- |Output of label.
  | Select ChannelName Label (Process t)
  -- |Input of label.
  | Case ChannelName [(Label, Process t)]
  -- |Cut.
  | Cut ChannelName t (Process t) (Process t)
  -- |Put gas
  | PutGas ChannelName (Process t)
  -- |Get gas
  | GetGas ChannelName (Process t)
  deriving (Eq, Ord)

-- |Set of channel names occurring free in a process.
fn :: Process t -> Set ChannelName
fn (Call _ xs) = Set.fromList xs
fn (Link x y) = Set.fromList [x, y]
fn (Wait x p) = Set.insert x (fn p)
fn (Close x) = Set.singleton x
fn (Fork x y p q) = Set.insert x (Set.union (Set.delete y (fn p)) (fn q))
fn (Join x y p) = Set.insert x (Set.delete y (fn p))
fn (Select x l p) = Set.insert x (fn p)
fn (Case x gs) = Set.insert x (Set.unions (map (fn . snd) gs))
fn (Cut x _ p q) = Set.delete x (Set.union (fn p) (fn q))
fn (PutGas x p) = Set.insert x (fn p)
fn (GetGas x p) = Set.insert x (fn p)

-- | A __process definition__ is a triple made of a process name, a
-- list of name declarations and an optional process body. When the
-- body is 'Nothing' the process is declared and assumed to be well
-- typed but is left unspecified.
type ProcessE = Process TypeE
type ProcessS = Process TypeS
type ProcessM = Process TypeM

type ProcessDefE = (ProcessName, [(ChannelName, TypeE)], ProcessE)
type ProcessDefS = (ProcessName, [(ChannelName, TypeS)], ProcessS)
type ProcessDef = (ProcessName, Measure, [(ChannelName, TypeM)], ProcessM)

isThread :: ChannelName -> Process t -> Bool
isThread x (Link y z) = x == y || x == z
isThread x (Close y) = x == y
isThread x (Wait y _) = x == y
isThread x (Fork y _ _ _) = x == y
isThread x (Join y _ _) = x == y
isThread x (Select y _ _) = x == y
isThread x (Case y _) = x == y
isThread x (PutGas y _) = x == y
isThread x (GetGas y _) = x == y
isThread _ _ = False

instance Functor Process where
  fmap f (Call pname xs) = Call pname xs
  fmap f (Link x y) = Link x y
  fmap f (Wait x p) = Wait x (fmap f p)
  fmap f (Close x) = Close x
  fmap f (Fork x y p q) = Fork x y (fmap f p) (fmap f q)
  fmap f (Join x y p) = Join x y (fmap f p)
  fmap f (Select x l p) = Select x l (fmap f p)
  fmap f (Case x bs) = Case x (mapSnd (fmap f) bs)
  fmap f (Cut x t p q) = Cut x (f t) (fmap f p) (fmap f q)
  fmap f (PutGas x p) = PutGas x (fmap f p)
  fmap f (GetGas x p) = GetGas x (fmap f p)

substProcessDef :: MSubst -> ProcessDef -> ProcessDef
substProcessDef σ (pname, μ, xts, p) = (pname, msubst σ μ, mapSnd (msubst σ) xts, fmap (msubst σ) p)
