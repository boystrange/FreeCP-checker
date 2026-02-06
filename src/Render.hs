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

-- |Pretty printer for session types and error messages.
module Render
  ( printTitle
  , printWarning
  , printOK
  , printNO
  , printType
  , printContext
  , printSolution
  , printProcess
  , printProcessDec )
where

import Atoms
import Measure
import Type
import Process
import Prelude hiding ((<>))
import Prettyprinter
import qualified Prettyprinter.Render.String as PR
import qualified Prettyprinter.Render.Terminal as PT
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (chr, ord)
import Control.Monad (forM_)

-- PRETTY PRINTER COMPATIBILITY

type Document = Doc PT.AnsiStyle

sub :: Int -> String
sub n = map aux (show n)
  where
    aux '-' = '₋'
    aux c | c >= '0' && c <= '9' = chr (ord c - ord '0' + ord '₀')

sup :: Int -> String
sup n = map aux (show n)
  where
    aux '-' = '⁻'
    aux c | c >= '0' && c <= '9' = chr (ord c - ord '0' + ord '⁰')

keyword :: String -> Document
keyword = annotate (PT.color PT.Blue) . pretty

identifier :: String -> Document
identifier = pretty

constant :: String -> Document
constant = annotate (PT.color PT.Magenta) . pretty

operator :: String -> Document
operator = annotate PT.bold . pretty

emark :: Document
emark = operator "!"

qmark :: Document
qmark = operator "?"

dot :: Document
dot = operator "."

bar :: Document
bar = operator "|"

ampersand :: Document
ampersand = operator "&"

-- UTILITIES

embrace :: Document -> Document -> Document -> [Document] -> Document
embrace open close sep ds = align (encloseSep (open <> space) (space <> close) (sep <> space) ds)

sepembrace :: Document -> Document -> Document -> [Document] -> Document
sepembrace open close sep ds = embrace open close sep (map (<> space) (init ds) ++ [last ds])

-- MEASURES

instance Show MVar where
  show (MVar n) | major == 0 = [letters!!minor]
                | otherwise  = letters!!minor : sub major
    where
      letters = ['κ', 'λ', 'μ', 'ν', 'ι']
      max = length letters
      major = n `div` max
      minor = n `mod` max

atom :: Measure -> Bool
atom (MCon _) = True
atom (MRef _) = True
atom _ = False

instance Show Measure where
  show (MCon n) = show n
  show (MRef u) = show u
  show (MAdd m n) = show m ++ " + " ++ show n
  show (MSub m n) = show m ++ " - " ++ if atom n then show n else "(" ++ show n ++ ")"

instance Show MeasureConstraint where
  show (CEq m n) = show m ++ " = " ++ show n
  show (CLe m n) = show m ++ " ≤ " ++ show n

prettyMeasure :: Measure -> Document
prettyMeasure = annotate (PT.color PT.Green) . pretty . show

-- LABELS

prettyLabel :: Label -> Document
prettyLabel = identifier . show

-- TYPE VARIABLES

instance Show TVar where
  show (TVar n) | major == 0 = ['_', letters!!minor]
                | otherwise  = '_' : letters!!minor : sub major
    where
      letters = ['α'..'ε']
      max = length letters
      major = n `div` max
      minor = n `mod` max

-- TYPES

prettyType :: (m -> Document) -> Type m -> Document
prettyType prettyMeasure = annotate (PT.colorDull PT.Cyan) . aux
  where
    aux Void = keyword "void"
    aux One  = keyword "1"
    aux Bot  = keyword "⊥"
    aux Skip = keyword "skip"
    aux (Seq t s) = parens (aux t <> operator ";" <+> aux s)
    aux (Var d tname) = identifier (show tname) <> (if d then operator "^" else emptyDoc)
    aux (Inv tname) = identifier (show tname)
    aux (Rec tname t) = keyword "rec" <+> identifier (show tname) <> Render.dot <> aux t
    aux (Par t s) = parens (aux t <+> operator "⅋" <+> aux s)
    aux (Mul t s) = parens (aux t <+> operator "⊗" <+> aux s)
    aux (Plus bs) = operator "⊕" <> embrace lbrace rbrace comma (map auxB bs)
    aux (With bs) = operator "&" <> embrace lbrace rbrace comma (map auxB bs)
    aux (Put m)   = operator "put" <+> prettyMeasure m
    aux (Get m)   = operator "get" <+> prettyMeasure m

    auxB (tag, t) = constant (show tag) <+> colon <+> aux t

prettyContext :: (m -> Document) -> Map ChannelName (Type m)  -> Document
prettyContext prettyMeasure ctx = embrace lbrace rbrace comma (map prettyBind (Map.toList ctx))
  where
    prettyBind (x, t) = identifier (show x) <+> operator ":" <+> prettyType prettyMeasure t

instance Show m => Show (Type m) where
  show = PR.renderString . layoutPretty defaultLayoutOptions . prettyType unprettyMeasure
    where
      unprettyMeasure = const (pretty "…")

-- |Print a type.
printType :: TypeM -> IO ()
printType = PT.putDoc . prettyType prettyMeasure

printContext :: Map ChannelName TypeM -> IO ()
printContext = PT.putDoc . prettyContext prettyMeasure

-- PROCESSES

prettyProcess :: ProcessM -> Document
prettyProcess = go
  where
    go (Call pname xs) = identifier (show pname) <> encloseSep langle rangle comma (map (identifier . show) xs)
    go (Link x y) = identifier (show x) <+> operator "↔" <+> identifier (show y)
    go (Close x) = identifier (show x) <> brackets emptyDoc
    go (Wait x p) = identifier (show x) <> parens emptyDoc <> Render.dot <> go p
    go (Fork x y p q) = identifier (show x) <> parens (identifier (show y)) <+> go p <+> keyword "in" <+> go q
    go (Join x y p) = identifier (show x) <> parens (identifier (show y)) <> Render.dot <> go p
    go (Select x tag p) = identifier (show x) <> operator "◃" <> identifier (show tag) <> Render.dot <> go p
    go (Case x bs) = identifier (show x) <> operator "▹" <> embrace lbrace rbrace comma (map goCase bs)
    go (Cut x t p q) = parens (identifier (show x) <+> colon <+> prettyType prettyMeasure t) <>
                       encloseSep lparen rparen (space <> bar <> space) [go p, go q]
    go (PutGas x p) = identifier (show x) <+> keyword "put" <> Render.dot <> go p
    go (GetGas x p) = identifier (show x) <+> keyword "get" <> Render.dot <> go p

    goCase (tag, p) = identifier (show tag) <+> colon <+> go p

printProcess :: ProcessM -> IO ()
printProcess = PT.putDoc . prettyProcess

-- AUXILIARY PRINTING OPERATIONS

-- |Print a newline.
printNewLine :: IO ()
printNewLine = putStrLn ""

-- |Print a string with style annotations.
printAnnotatedString :: [PT.AnsiStyle] -> String -> IO ()
printAnnotatedString anns msg = PT.putDoc (foldr annotate (pretty msg) anns)

-- |Print a string as a title.
printTitle :: String -> IO ()
printTitle msg = printAnnotatedString [PT.bold, PT.underlined] msg >> printNewLine

-- |Print a warning message.
printWarning :: String -> IO ()
printWarning msg = printAnnotatedString [PT.color PT.Red] msg >> printNewLine

-- |Print an error message.
printNO :: String -> IO ()
printNO msg = do
  printAnnotatedString [PT.color PT.Red] "NO:"
  putStrLn $ " " ++ msg

-- |Print a success message.
printOK :: Maybe String -> IO ()
printOK msg = do
  printAnnotatedString [PT.bold, PT.color PT.Green] "OK"
  case msg of
    Nothing -> printNewLine
    Just m -> putStrLn $ " (" ++ m ++ ")"

printSolution :: Show a => Map.Map MVar a -> IO ()
printSolution = PT.putDoc . fillSep . punctuate comma . map aux . Map.toList
  where
    aux (μ, n) = prettyMeasure (MRef μ) <+> operator "=" <+> pretty (show n)

printProcessDec :: ProcessDef -> IO ()
printProcessDec (pname, μ, xts, p) = do
  PT.putDoc (identifier (show pname) <> brackets (prettyMeasure μ))
  printNewLine
  forM_ xts (\(x, t) -> do
               PT.putDoc (space <+> identifier (show x) <+> colon <+> prettyType prettyMeasure t)
               printNewLine
            )
  PT.putDoc (operator "=" <+> prettyProcess p)
  printNewLine
