{
{-# OPTIONS -w #-}
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

-- |This module implements the parser for FairCheck scripts.
module Parser (parseProcess) where

import Lexer
import Atoms
import Type
import Process
import Render

import Data.Either (partitionEithers)
import Control.Exception
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
%error { happyError }

%token
  TYPE      { Token _ TokenType }
  WAIT      { Token _ TokenWait }
  CLOSE     { Token _ TokenClose }
  CASE      { Token _ TokenCase }
  SKIP      { Token _ TokenSkip }
  NEW       { Token _ TokenNew }
  IN        { Token _ TokenIn }
  CID       { $$@(Token _ (TokenCID _)) }
  LID       { $$@(Token _ (TokenLID _)) }
  INT       { $$@(Token _ (TokenINT _)) }
  FLOAT     { $$@(Token _ (TokenFLOAT _)) }
  '='       { Token _ TokenEQ }
  '.'       { Token _ TokenDot }
  ':'       { Token _ TokenColon }
  ';'       { Token _ TokenSemiColon }
  ','       { Token _ TokenComma }
  '('       { Token _ TokenLParen }
  ')'       { Token _ TokenRParen }
  '{'       { Token _ TokenLBrace }
  '}'       { Token _ TokenRBrace }
  '['       { Token _ TokenLBrack }
  ']'       { Token _ TokenRBrack }
  '<'       { Token _ TokenLAngle }
  '>'       { Token _ TokenRAngle }
  '&'       { Token _ TokenAmp }
  '|'       { Token _ TokenPar }
  '⊥'       { Token _ TokenBot }
  '*'       { Token _ TokenTimes }
  '+'       { Token _ TokenPlus }
  '++'      { Token _ TokenPut }
  '--'      { Token _ TokenGet }
  '?'       { Token _ TokenQMark }
  '!'       { Token _ TokenEMark }
  '^'       { Token _ TokenDual }

%nonassoc '}' ']' IN

%right ','
%left '+'
%right ';'
%left '*' '|'
%nonassoc '++' '--'
%nonassoc '?' '!'

%%

-- PROGRAMS

Program
  : TypeDefList ProcessDefList { ($1, $2) }

TypeDefList
  : { [] }
  | TypeDef TypeDefList { $1 : $2 }

TypeDef
  : TYPE TypeName '=' Type { ($2, $4) }

ProcessDefList
  : { [] }
  | ProcessDef ProcessDefList { $1 : $2 }

ProcessDef
  : ProcessName Parameters '=' Process { ($1, $2, $4) }

Parameters
  : { [] }
  | '(' ParameterList ')' { $2 }

ParameterList
  : { [] }
  | ParameterNeList { $1 }

ParameterNeList
  : Parameter { [$1] }
  | ParameterNeList ',' ParameterNeList { $1 ++ $3 }

Parameter
  : ChannelName ':' TypeExpr { ($1, $3) }

-- PROCESSES

Process
  : '(' Process ')' { $2 }
  | ChannelName '=' ChannelName { Link $1 $3 }
  | CLOSE ChannelName { Close $2 }
  | WAIT ChannelName '.' Process { Wait $2 $4 }
  | ChannelName '(' ChannelName ')' Process IN Process { Fork $1 $3 $5 $7 }
  | ChannelName '<' ChannelName '>' '.' Process
    { let tmp = Identifier (At $ getPos $2) "_tmp_" in
      Fork $1 tmp (Link $3 tmp) $6 }
  | ChannelName '(' ChannelName ')' '.' Process { Join $1 $3 $6 }
  | ChannelName '[' Label ']' '.' Process { Select $1 $3 $6 }
  | CASE ChannelName Cases { Case $2 $3 }
  | NEW '(' ChannelName ':' TypeExpr ')' Process IN Process { Cut $3 $5 $7 $9 }
  | ProcessName Names { Call $1 $2 }

Names
  : { [] }
  | '(' ')' { [] }
  | '(' NameNeList ')' { $2 }

NameNeList
  : ChannelName { [$1] }
  | ChannelName ',' NameNeList { $1 : $3 }

Choices
  : '{' ChoiceNeList '}' { $2 }

ChoiceNeList
  : Choice { [$1] }
  | Choice ',' ChoiceNeList { $1 : $3 }

Choice
  : WeightOpt Process { ($1, $2) }

WeightOpt
  :         { 1 }
  | Num ':' { $1 }

Cases
  : '{' CaseList '}' { $2 }

CaseList
  : { [] }
  | CaseNeList { $1 }

CaseNeList
  : Case { [$1] }
  | Case ',' CaseNeList { $1 : $3 }

Case
  : Label ':' Process { ($1, $3) }

-- IDENTIFIERS

ChannelName
  : LID { Identifier (At $ getPos $1) (getId $1) :: ChannelName }

TypeName
  : CID { Identifier (At $ getPos $1) (getId $1) :: TypeName }

PolyName
  : LID { Identifier (At $ getPos $1) (getId $1) :: TypeName }

ProcessName
  : CID { Identifier (At $ getPos $1) (getId $1) :: ProcessName }

Label
  : LID { Identifier (At $ getPos $1) (getId $1) :: Label }

-- TYPES

TypeExpr
  : Type { Type $1 }
  | '^' Type { Dual $2 }

Type
  : Num  { if $1 == 1 then One
           else error $ (show $1) ++ " is not a type" }
  | '⊥'  { Bot }
  | TypeName { Var $1 }
  | PolyName { Poly False $1 }
  | '^' PolyName { Poly True $2 }
  | '(' Type ')' { $2 }
  | Type '*' Type { Mul $1 $3 }
  | Type '|' Type { Par $1 $3 }
  | '!' Type { Mul $2 Skip }
  | '?' Type { Par $2 Skip }
  | SKIP { Skip }
  | Type ';' Type { Type.qes $3 $1 }
  | '&' Branches { With $2 }
  | '+' Branches { Plus $2 }
  | '++' MeasureOpt Type { Put $2 $3 }
  | '--' MeasureOpt Type { Get $2 $3 }

MeasureOpt
  : { Nothing }
  | '[' Int ']' { Just $2 }

Num : Int { fromIntegral $1 }
  | Float { $1 }

Int : INT { getInt $1 }

Float : FLOAT { getFloat $1 }

Branches
  : '{' BranchList '}' { $2 }

BranchList
  : { [] }
  | BranchNeList { $1 }

BranchNeList
  : Branch { [$1] }
  | Branch ',' BranchNeList { $1 : $3 }

Branch
  : Label ':' Type { ($1, $3) }

{
-- external :: Type -> Type -> Type
-- external (Type.Label In bs1) (Type.Label In bs2) = Type.Label In (bs1 ++ bs2)
-- external t s = error $ "cannot combine external choice " ++ show t ++ " and " ++ show s

-- internal :: Type -> Type -> Type
-- internal (Type.Label Out bs1) (Type.Label Out bs2) = Type.Label Out (bs1 ++ bs2)
-- internal t s = error $ "cannot combine internal choice " ++ show t ++ " and " ++ show s

getId :: Token -> String
getId (Token _ (TokenLID x)) = x
getId (Token _ (TokenCID x)) = x

getInt :: Token -> Int
getInt (Token _ (TokenINT n)) = n

getFloat :: Token -> Double
getFloat (Token _ (TokenFLOAT n)) = n

getPos :: Token -> (Int, Int)
getPos (Token (AlexPn _ line col) _) = (line, col)

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) = alexError' p ("parse error at token '" ++ show t ++ "'")

parseProcess :: FilePath -> String -> Either String ([TypeDef], [ProcessDefE])
parseProcess = runAlex' parse
}
