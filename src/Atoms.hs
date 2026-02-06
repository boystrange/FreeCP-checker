-- MIT License
--
-- Copyright (c) 2026 Luca Padovani
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

-- |This module defines the representation of __identifiers__ and
-- __polarities__.
module Atoms where

-- |A position refers to line and column within a script. The constructor
-- 'Somewhere' builds an unknown position.
data Pos = Somewhere
         | At (Int, Int)

instance Show Pos where
  show Somewhere = ""
  show (At (l, c)) = " [line " ++ show l ++ "]"

-- |The kind of labels.
data LabelI

-- |The kind of type names.
data TypeI

-- |The kind of channel names.
data ChannelI

-- |The kind of process names.
data ProcessI

-- |The 'Identifier' data type represents the occurrence of an identifier within
-- a script. It is a phantom type whose type parameter indicates the kind of the
-- identifier.
data Identifier k = Identifier { identifierPos :: Pos
                               , identifierText :: String }
instance Show (Identifier k) where
  show = identifierText

-- |Show an identifier along with its position in the source code, if known.
showWithPos :: Identifier k -> String
showWithPos u = identifierText u ++ show (identifierPos u)

-- |Two identifiers are the same regardless of the position in which they occur.
instance Eq (Identifier k) where
  (==) u v = identifierText u == identifierText v

-- |Two identifiers are ordered regardless of the position in which they occur.
instance Ord (Identifier k) where
  compare u v = compare (identifierText u) (identifierText v)

-- |The type of labels.
type Label       = Identifier LabelI

-- |The type of channel names.
type ChannelName = Identifier ChannelI

-- |The type of type names.
type TypeName    = Identifier TypeI

-- |The type of process names.
type ProcessName = Identifier ProcessI
