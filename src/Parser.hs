{-# OPTIONS_GHC -w #-}
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
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30
	| HappyAbsSyn31 t31
	| HappyAbsSyn32 t32
	| HappyAbsSyn33 t33
	| HappyAbsSyn34 t34

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,288) ([0,0,4,0,0,0,128,0,0,0,0,1,0,0,0,2,0,0,0,1024,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,64,0,0,0,0,0,0,0,2048,0,0,0,8192,0,0,0,0,0,0,0,0,33736,64128,0,0,32768,0,0,0,0,0,0,0,0,4096,33408,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,58368,16449,125,0,0,8192,0,0,0,0,0,0,0,0,128,0,0,0,16384,0,0,0,0,8,0,0,30976,20496,31,0,8192,527,1002,0,0,32768,0,0,0,0,4,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,53248,128,0,0,0,0,0,0,0,0,162,56,0,0,64,0,0,0,2048,0,0,0,416,1,0,0,61952,40992,62,0,0,4,0,0,0,0,0,0,0,0,0,32,0,0,0,1024,0,0,16868,32064,0,0,4096,0,0,0,36864,263,501,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,9216,8352,0,0,3872,59906,3,0,58368,16449,125,0,32768,2108,4008,0,0,0,0,0,0,61952,40992,62,0,0,256,2088,0,0,0,0,1,0,0,0,32,0,0,128,1044,0,0,0,0,0,0,0,1,0,0,0,2048,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,8192,0,0,0,64,0,0,0,0,128,0,0,1024,0,0,0,0,0,0,0,0,16392,65,0,0,2048,0,0,0,128,0,0,0,0,0,0,0,0,1026,0,0,0,32832,0,0,0,0,256,0,0,0,1,0,0,0,32800,0,0,0,1024,0,0,0,32768,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,128,0,0,0,256,0,0,0,0,4096,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1024,0,0,0,8192,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,3872,59906,3,0,0,32784,130,0,0,0,0,0,0,1936,62721,1,0,8192,0,0,0,0,0,0,0,0,32976,0,0,0,6656,17,0,0,0,32,0,0,0,0,2,0,0,0,0,0,0,0,128,0,0,0,1024,0,0,0,1664,4,0,0,53248,128,0,0,0,0,0,0,0,0,0,0,0,26624,64,0,0,0,8,0,0,0,0,0,0,0,13312,32,0,0,0,1,0,0,0,32976,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,3328,8,0,0,0,0,0,0,0,8244,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,2061,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Program","TypeDefList","TypeDef","ProcessDefList","ProcessDef","Parameters","ParameterList","ParameterNeList","Parameter","Process","Names","NameNeList","Cases","CaseList","CaseNeList","Case","ChannelName","TypeName","PolyName","ProcessName","Label","TypeExpr","Type","MeasureOpt","Num","Int","Float","Branches","BranchList","BranchNeList","Branch","TYPE","SKIP","NEW","IN","CID","LID","INT","FLOAT","'='","'.'","':'","';'","','","'('","')'","'{'","'}'","'['","']'","'<'","'>'","'&'","'|'","'\8869'","'*'","'+'","'++'","'--'","'?'","'!'","'^'","'\9657'","'\9667'","'\8596'","%eof"]
        bit_start = st Prelude.* 69
        bit_end = (st Prelude.+ 1) Prelude.* 69
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..68]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (35) = happyShift action_4
action_0 (4) = happyGoto action_5
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 _ = happyReduce_2

action_1 (35) = happyShift action_4
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (39) = happyShift action_12
action_2 (7) = happyGoto action_9
action_2 (8) = happyGoto action_10
action_2 (23) = happyGoto action_11
action_2 _ = happyReduce_5

action_3 (35) = happyShift action_4
action_3 (5) = happyGoto action_8
action_3 (6) = happyGoto action_3
action_3 _ = happyReduce_2

action_4 (39) = happyShift action_7
action_4 (21) = happyGoto action_6
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (69) = happyAccept
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (43) = happyShift action_16
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_39

action_8 _ = happyReduce_3

action_9 _ = happyReduce_1

action_10 (39) = happyShift action_12
action_10 (7) = happyGoto action_15
action_10 (8) = happyGoto action_10
action_10 (23) = happyGoto action_11
action_10 _ = happyReduce_5

action_11 (48) = happyShift action_14
action_11 (9) = happyGoto action_13
action_11 _ = happyReduce_8

action_12 _ = happyReduce_41

action_13 (43) = happyShift action_40
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (40) = happyShift action_39
action_14 (10) = happyGoto action_35
action_14 (11) = happyGoto action_36
action_14 (12) = happyGoto action_37
action_14 (20) = happyGoto action_38
action_14 _ = happyReduce_10

action_15 _ = happyReduce_6

action_16 (36) = happyShift action_23
action_16 (39) = happyShift action_7
action_16 (40) = happyShift action_24
action_16 (41) = happyShift action_25
action_16 (42) = happyShift action_26
action_16 (48) = happyShift action_27
action_16 (56) = happyShift action_28
action_16 (58) = happyShift action_29
action_16 (60) = happyShift action_30
action_16 (61) = happyShift action_31
action_16 (62) = happyShift action_32
action_16 (63) = happyShift action_33
action_16 (64) = happyShift action_34
action_16 (21) = happyGoto action_17
action_16 (22) = happyGoto action_18
action_16 (26) = happyGoto action_19
action_16 (28) = happyGoto action_20
action_16 (29) = happyGoto action_21
action_16 (30) = happyGoto action_22
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (43) = happyShift action_62
action_17 _ = happyReduce_46

action_18 _ = happyReduce_48

action_19 (46) = happyShift action_58
action_19 (57) = happyShift action_59
action_19 (59) = happyShift action_60
action_19 (65) = happyShift action_61
action_19 _ = happyReduce_4

action_20 _ = happyReduce_44

action_21 _ = happyReduce_63

action_22 _ = happyReduce_64

action_23 _ = happyReduce_55

action_24 _ = happyReduce_40

action_25 _ = happyReduce_65

action_26 _ = happyReduce_66

action_27 (36) = happyShift action_23
action_27 (39) = happyShift action_7
action_27 (40) = happyShift action_24
action_27 (41) = happyShift action_25
action_27 (42) = happyShift action_26
action_27 (48) = happyShift action_27
action_27 (56) = happyShift action_28
action_27 (58) = happyShift action_29
action_27 (60) = happyShift action_30
action_27 (61) = happyShift action_31
action_27 (62) = happyShift action_32
action_27 (63) = happyShift action_33
action_27 (64) = happyShift action_34
action_27 (21) = happyGoto action_17
action_27 (22) = happyGoto action_18
action_27 (26) = happyGoto action_57
action_27 (28) = happyGoto action_20
action_27 (29) = happyGoto action_21
action_27 (30) = happyGoto action_22
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (50) = happyShift action_55
action_28 (31) = happyGoto action_56
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_45

action_30 (50) = happyShift action_55
action_30 (31) = happyGoto action_54
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (52) = happyShift action_52
action_31 (27) = happyGoto action_53
action_31 _ = happyReduce_61

action_32 (52) = happyShift action_52
action_32 (27) = happyGoto action_51
action_32 _ = happyReduce_61

action_33 (36) = happyShift action_23
action_33 (39) = happyShift action_7
action_33 (40) = happyShift action_24
action_33 (41) = happyShift action_25
action_33 (42) = happyShift action_26
action_33 (48) = happyShift action_27
action_33 (56) = happyShift action_28
action_33 (58) = happyShift action_29
action_33 (60) = happyShift action_30
action_33 (61) = happyShift action_31
action_33 (62) = happyShift action_32
action_33 (63) = happyShift action_33
action_33 (64) = happyShift action_34
action_33 (21) = happyGoto action_17
action_33 (22) = happyGoto action_18
action_33 (26) = happyGoto action_50
action_33 (28) = happyGoto action_20
action_33 (29) = happyGoto action_21
action_33 (30) = happyGoto action_22
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (36) = happyShift action_23
action_34 (39) = happyShift action_7
action_34 (40) = happyShift action_24
action_34 (41) = happyShift action_25
action_34 (42) = happyShift action_26
action_34 (48) = happyShift action_27
action_34 (56) = happyShift action_28
action_34 (58) = happyShift action_29
action_34 (60) = happyShift action_30
action_34 (61) = happyShift action_31
action_34 (62) = happyShift action_32
action_34 (63) = happyShift action_33
action_34 (64) = happyShift action_34
action_34 (21) = happyGoto action_17
action_34 (22) = happyGoto action_18
action_34 (26) = happyGoto action_49
action_34 (28) = happyGoto action_20
action_34 (29) = happyGoto action_21
action_34 (30) = happyGoto action_22
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (49) = happyShift action_48
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (47) = happyShift action_47
action_36 _ = happyReduce_11

action_37 _ = happyReduce_12

action_38 (45) = happyShift action_46
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_38

action_40 (37) = happyShift action_44
action_40 (39) = happyShift action_12
action_40 (40) = happyShift action_39
action_40 (48) = happyShift action_45
action_40 (13) = happyGoto action_41
action_40 (20) = happyGoto action_42
action_40 (23) = happyGoto action_43
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_7

action_42 (48) = happyShift action_83
action_42 (52) = happyShift action_84
action_42 (54) = happyShift action_85
action_42 (66) = happyShift action_86
action_42 (67) = happyShift action_87
action_42 (68) = happyShift action_88
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (48) = happyShift action_82
action_43 (14) = happyGoto action_81
action_43 _ = happyReduce_27

action_44 (48) = happyShift action_80
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (37) = happyShift action_44
action_45 (39) = happyShift action_12
action_45 (40) = happyShift action_39
action_45 (48) = happyShift action_45
action_45 (13) = happyGoto action_79
action_45 (20) = happyGoto action_42
action_45 (23) = happyGoto action_43
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (36) = happyShift action_23
action_46 (39) = happyShift action_7
action_46 (40) = happyShift action_24
action_46 (41) = happyShift action_25
action_46 (42) = happyShift action_26
action_46 (48) = happyShift action_27
action_46 (56) = happyShift action_28
action_46 (58) = happyShift action_29
action_46 (60) = happyShift action_30
action_46 (61) = happyShift action_31
action_46 (62) = happyShift action_32
action_46 (63) = happyShift action_33
action_46 (64) = happyShift action_34
action_46 (21) = happyGoto action_17
action_46 (22) = happyGoto action_18
action_46 (25) = happyGoto action_77
action_46 (26) = happyGoto action_78
action_46 (28) = happyGoto action_20
action_46 (29) = happyGoto action_21
action_46 (30) = happyGoto action_22
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (40) = happyShift action_39
action_47 (11) = happyGoto action_76
action_47 (12) = happyGoto action_37
action_47 (20) = happyGoto action_38
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_9

action_49 (65) = happyShift action_61
action_49 _ = happyReduce_53

action_50 (65) = happyShift action_61
action_50 _ = happyReduce_54

action_51 (36) = happyShift action_23
action_51 (39) = happyShift action_7
action_51 (40) = happyShift action_24
action_51 (41) = happyShift action_25
action_51 (42) = happyShift action_26
action_51 (48) = happyShift action_27
action_51 (56) = happyShift action_28
action_51 (58) = happyShift action_29
action_51 (60) = happyShift action_30
action_51 (61) = happyShift action_31
action_51 (62) = happyShift action_32
action_51 (63) = happyShift action_33
action_51 (64) = happyShift action_34
action_51 (21) = happyGoto action_17
action_51 (22) = happyGoto action_18
action_51 (26) = happyGoto action_75
action_51 (28) = happyGoto action_20
action_51 (29) = happyGoto action_21
action_51 (30) = happyGoto action_22
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (41) = happyShift action_25
action_52 (29) = happyGoto action_74
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (36) = happyShift action_23
action_53 (39) = happyShift action_7
action_53 (40) = happyShift action_24
action_53 (41) = happyShift action_25
action_53 (42) = happyShift action_26
action_53 (48) = happyShift action_27
action_53 (56) = happyShift action_28
action_53 (58) = happyShift action_29
action_53 (60) = happyShift action_30
action_53 (61) = happyShift action_31
action_53 (62) = happyShift action_32
action_53 (63) = happyShift action_33
action_53 (64) = happyShift action_34
action_53 (21) = happyGoto action_17
action_53 (22) = happyGoto action_18
action_53 (26) = happyGoto action_73
action_53 (28) = happyGoto action_20
action_53 (29) = happyGoto action_21
action_53 (30) = happyGoto action_22
action_53 _ = happyFail (happyExpListPerState 53)

action_54 _ = happyReduce_58

action_55 (40) = happyShift action_72
action_55 (24) = happyGoto action_68
action_55 (32) = happyGoto action_69
action_55 (33) = happyGoto action_70
action_55 (34) = happyGoto action_71
action_55 _ = happyReduce_68

action_56 _ = happyReduce_57

action_57 (46) = happyShift action_58
action_57 (49) = happyShift action_67
action_57 (57) = happyShift action_59
action_57 (59) = happyShift action_60
action_57 (65) = happyShift action_61
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (36) = happyShift action_23
action_58 (39) = happyShift action_7
action_58 (40) = happyShift action_24
action_58 (41) = happyShift action_25
action_58 (42) = happyShift action_26
action_58 (48) = happyShift action_27
action_58 (56) = happyShift action_28
action_58 (58) = happyShift action_29
action_58 (60) = happyShift action_30
action_58 (61) = happyShift action_31
action_58 (62) = happyShift action_32
action_58 (63) = happyShift action_33
action_58 (64) = happyShift action_34
action_58 (21) = happyGoto action_17
action_58 (22) = happyGoto action_18
action_58 (26) = happyGoto action_66
action_58 (28) = happyGoto action_20
action_58 (29) = happyGoto action_21
action_58 (30) = happyGoto action_22
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (36) = happyShift action_23
action_59 (39) = happyShift action_7
action_59 (40) = happyShift action_24
action_59 (41) = happyShift action_25
action_59 (42) = happyShift action_26
action_59 (48) = happyShift action_27
action_59 (56) = happyShift action_28
action_59 (58) = happyShift action_29
action_59 (60) = happyShift action_30
action_59 (61) = happyShift action_31
action_59 (62) = happyShift action_32
action_59 (63) = happyShift action_33
action_59 (64) = happyShift action_34
action_59 (21) = happyGoto action_17
action_59 (22) = happyGoto action_18
action_59 (26) = happyGoto action_65
action_59 (28) = happyGoto action_20
action_59 (29) = happyGoto action_21
action_59 (30) = happyGoto action_22
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (36) = happyShift action_23
action_60 (39) = happyShift action_7
action_60 (40) = happyShift action_24
action_60 (41) = happyShift action_25
action_60 (42) = happyShift action_26
action_60 (48) = happyShift action_27
action_60 (56) = happyShift action_28
action_60 (58) = happyShift action_29
action_60 (60) = happyShift action_30
action_60 (61) = happyShift action_31
action_60 (62) = happyShift action_32
action_60 (63) = happyShift action_33
action_60 (64) = happyShift action_34
action_60 (21) = happyGoto action_17
action_60 (22) = happyGoto action_18
action_60 (26) = happyGoto action_64
action_60 (28) = happyGoto action_20
action_60 (29) = happyGoto action_21
action_60 (30) = happyGoto action_22
action_60 _ = happyFail (happyExpListPerState 60)

action_61 _ = happyReduce_49

action_62 (36) = happyShift action_23
action_62 (39) = happyShift action_7
action_62 (40) = happyShift action_24
action_62 (41) = happyShift action_25
action_62 (42) = happyShift action_26
action_62 (48) = happyShift action_27
action_62 (56) = happyShift action_28
action_62 (58) = happyShift action_29
action_62 (60) = happyShift action_30
action_62 (61) = happyShift action_31
action_62 (62) = happyShift action_32
action_62 (63) = happyShift action_33
action_62 (64) = happyShift action_34
action_62 (21) = happyGoto action_17
action_62 (22) = happyGoto action_18
action_62 (26) = happyGoto action_63
action_62 (28) = happyGoto action_20
action_62 (29) = happyGoto action_21
action_62 (30) = happyGoto action_22
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (46) = happyShift action_58
action_63 (57) = happyShift action_59
action_63 (59) = happyShift action_60
action_63 (65) = happyShift action_61
action_63 _ = happyReduce_47

action_64 (65) = happyShift action_61
action_64 _ = happyReduce_51

action_65 (65) = happyShift action_61
action_65 _ = happyReduce_52

action_66 (46) = happyShift action_58
action_66 (57) = happyShift action_59
action_66 (59) = happyShift action_60
action_66 (65) = happyShift action_61
action_66 _ = happyReduce_56

action_67 _ = happyReduce_50

action_68 (45) = happyShift action_106
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (51) = happyShift action_105
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_69

action_71 (47) = happyShift action_104
action_71 _ = happyReduce_70

action_72 _ = happyReduce_42

action_73 (65) = happyShift action_61
action_73 _ = happyReduce_59

action_74 (53) = happyShift action_103
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (65) = happyShift action_61
action_75 _ = happyReduce_60

action_76 (47) = happyShift action_47
action_76 _ = happyReduce_13

action_77 _ = happyReduce_14

action_78 (46) = happyShift action_58
action_78 (57) = happyShift action_59
action_78 (59) = happyShift action_60
action_78 (65) = happyShift action_61
action_78 _ = happyReduce_43

action_79 (49) = happyShift action_102
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (40) = happyShift action_39
action_80 (20) = happyGoto action_101
action_80 _ = happyFail (happyExpListPerState 80)

action_81 _ = happyReduce_26

action_82 (40) = happyShift action_39
action_82 (49) = happyShift action_100
action_82 (15) = happyGoto action_98
action_82 (20) = happyGoto action_99
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (40) = happyShift action_39
action_83 (49) = happyShift action_97
action_83 (20) = happyGoto action_96
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (53) = happyShift action_95
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (40) = happyShift action_39
action_85 (20) = happyGoto action_94
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (40) = happyShift action_72
action_86 (50) = happyShift action_93
action_86 (16) = happyGoto action_91
action_86 (24) = happyGoto action_92
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (40) = happyShift action_72
action_87 (24) = happyGoto action_90
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (40) = happyShift action_39
action_88 (20) = happyGoto action_89
action_88 _ = happyFail (happyExpListPerState 88)

action_89 _ = happyReduce_16

action_90 (44) = happyShift action_120
action_90 _ = happyFail (happyExpListPerState 90)

action_91 _ = happyReduce_23

action_92 (44) = happyShift action_119
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (40) = happyShift action_72
action_93 (17) = happyGoto action_115
action_93 (18) = happyGoto action_116
action_93 (19) = happyGoto action_117
action_93 (24) = happyGoto action_118
action_93 _ = happyReduce_33

action_94 (55) = happyShift action_114
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_17

action_96 (49) = happyShift action_113
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (44) = happyShift action_112
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (49) = happyShift action_111
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (47) = happyShift action_110
action_99 _ = happyReduce_30

action_100 _ = happyReduce_28

action_101 (45) = happyShift action_109
action_101 _ = happyFail (happyExpListPerState 101)

action_102 _ = happyReduce_15

action_103 _ = happyReduce_62

action_104 (40) = happyShift action_72
action_104 (24) = happyGoto action_68
action_104 (33) = happyGoto action_108
action_104 (34) = happyGoto action_71
action_104 _ = happyFail (happyExpListPerState 104)

action_105 _ = happyReduce_67

action_106 (36) = happyShift action_23
action_106 (39) = happyShift action_7
action_106 (40) = happyShift action_24
action_106 (41) = happyShift action_25
action_106 (42) = happyShift action_26
action_106 (48) = happyShift action_27
action_106 (56) = happyShift action_28
action_106 (58) = happyShift action_29
action_106 (60) = happyShift action_30
action_106 (61) = happyShift action_31
action_106 (62) = happyShift action_32
action_106 (63) = happyShift action_33
action_106 (64) = happyShift action_34
action_106 (21) = happyGoto action_17
action_106 (22) = happyGoto action_18
action_106 (26) = happyGoto action_107
action_106 (28) = happyGoto action_20
action_106 (29) = happyGoto action_21
action_106 (30) = happyGoto action_22
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (46) = happyShift action_58
action_107 (57) = happyShift action_59
action_107 (59) = happyShift action_60
action_107 (65) = happyShift action_61
action_107 _ = happyReduce_72

action_108 _ = happyReduce_71

action_109 (36) = happyShift action_23
action_109 (39) = happyShift action_7
action_109 (40) = happyShift action_24
action_109 (41) = happyShift action_25
action_109 (42) = happyShift action_26
action_109 (48) = happyShift action_27
action_109 (56) = happyShift action_28
action_109 (58) = happyShift action_29
action_109 (60) = happyShift action_30
action_109 (61) = happyShift action_31
action_109 (62) = happyShift action_32
action_109 (63) = happyShift action_33
action_109 (64) = happyShift action_34
action_109 (21) = happyGoto action_17
action_109 (22) = happyGoto action_18
action_109 (25) = happyGoto action_131
action_109 (26) = happyGoto action_78
action_109 (28) = happyGoto action_20
action_109 (29) = happyGoto action_21
action_109 (30) = happyGoto action_22
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (40) = happyShift action_39
action_110 (15) = happyGoto action_130
action_110 (20) = happyGoto action_99
action_110 _ = happyFail (happyExpListPerState 110)

action_111 _ = happyReduce_29

action_112 (37) = happyShift action_44
action_112 (39) = happyShift action_12
action_112 (40) = happyShift action_39
action_112 (48) = happyShift action_45
action_112 (13) = happyGoto action_129
action_112 (20) = happyGoto action_42
action_112 (23) = happyGoto action_43
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (37) = happyShift action_44
action_113 (39) = happyShift action_12
action_113 (40) = happyShift action_39
action_113 (44) = happyShift action_128
action_113 (48) = happyShift action_45
action_113 (13) = happyGoto action_127
action_113 (20) = happyGoto action_42
action_113 (23) = happyGoto action_43
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (44) = happyShift action_126
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (51) = happyShift action_125
action_115 _ = happyFail (happyExpListPerState 115)

action_116 _ = happyReduce_34

action_117 (47) = happyShift action_124
action_117 _ = happyReduce_35

action_118 (45) = happyShift action_123
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (37) = happyShift action_44
action_119 (39) = happyShift action_12
action_119 (40) = happyShift action_39
action_119 (48) = happyShift action_45
action_119 (13) = happyGoto action_122
action_119 (20) = happyGoto action_42
action_119 (23) = happyGoto action_43
action_119 _ = happyFail (happyExpListPerState 119)

action_120 (37) = happyShift action_44
action_120 (39) = happyShift action_12
action_120 (40) = happyShift action_39
action_120 (48) = happyShift action_45
action_120 (13) = happyGoto action_121
action_120 (20) = happyGoto action_42
action_120 (23) = happyGoto action_43
action_120 _ = happyFail (happyExpListPerState 120)

action_121 _ = happyReduce_22

action_122 _ = happyReduce_24

action_123 (37) = happyShift action_44
action_123 (39) = happyShift action_12
action_123 (40) = happyShift action_39
action_123 (48) = happyShift action_45
action_123 (13) = happyGoto action_137
action_123 (20) = happyGoto action_42
action_123 (23) = happyGoto action_43
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (40) = happyShift action_72
action_124 (18) = happyGoto action_136
action_124 (19) = happyGoto action_117
action_124 (24) = happyGoto action_118
action_124 _ = happyFail (happyExpListPerState 124)

action_125 _ = happyReduce_32

action_126 (37) = happyShift action_44
action_126 (39) = happyShift action_12
action_126 (40) = happyShift action_39
action_126 (48) = happyShift action_45
action_126 (13) = happyGoto action_135
action_126 (20) = happyGoto action_42
action_126 (23) = happyGoto action_43
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (38) = happyShift action_134
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (37) = happyShift action_44
action_128 (39) = happyShift action_12
action_128 (40) = happyShift action_39
action_128 (48) = happyShift action_45
action_128 (13) = happyGoto action_133
action_128 (20) = happyGoto action_42
action_128 (23) = happyGoto action_43
action_128 _ = happyFail (happyExpListPerState 128)

action_129 _ = happyReduce_18

action_130 _ = happyReduce_31

action_131 (49) = happyShift action_132
action_131 _ = happyFail (happyExpListPerState 131)

action_132 (37) = happyShift action_44
action_132 (39) = happyShift action_12
action_132 (40) = happyShift action_39
action_132 (48) = happyShift action_45
action_132 (13) = happyGoto action_139
action_132 (20) = happyGoto action_42
action_132 (23) = happyGoto action_43
action_132 _ = happyFail (happyExpListPerState 132)

action_133 _ = happyReduce_21

action_134 (37) = happyShift action_44
action_134 (39) = happyShift action_12
action_134 (40) = happyShift action_39
action_134 (48) = happyShift action_45
action_134 (13) = happyGoto action_138
action_134 (20) = happyGoto action_42
action_134 (23) = happyGoto action_43
action_134 _ = happyFail (happyExpListPerState 134)

action_135 _ = happyReduce_20

action_136 _ = happyReduce_36

action_137 _ = happyReduce_37

action_138 _ = happyReduce_19

action_139 (38) = happyShift action_140
action_139 _ = happyFail (happyExpListPerState 139)

action_140 (37) = happyShift action_44
action_140 (39) = happyShift action_12
action_140 (40) = happyShift action_39
action_140 (48) = happyShift action_45
action_140 (13) = happyGoto action_141
action_140 (20) = happyGoto action_42
action_140 (23) = happyGoto action_43
action_140 _ = happyFail (happyExpListPerState 140)

action_141 _ = happyReduce_25

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ((happy_var_1, happy_var_2)
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 4 6 happyReduction_4
happyReduction_4 ((HappyAbsSyn26  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_0  7 happyReduction_5
happyReduction_5  =  HappyAbsSyn7
		 ([]
	)

happyReduce_6 = happySpecReduce_2  7 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happyReduce 4 8 happyReduction_7
happyReduction_7 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	(HappyAbsSyn23  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ((happy_var_1, happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_0  9 happyReduction_8
happyReduction_8  =  HappyAbsSyn9
		 ([]
	)

happyReduce_9 = happySpecReduce_3  9 happyReduction_9
happyReduction_9 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_0  10 happyReduction_10
happyReduction_10  =  HappyAbsSyn10
		 ([]
	)

happyReduce_11 = happySpecReduce_1  10 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  11 happyReduction_12
happyReduction_12 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  11 happyReduction_13
happyReduction_13 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 ++ happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  12 happyReduction_14
happyReduction_14 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn12
		 ((happy_var_1, happy_var_3)
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  13 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  13 happyReduction_16
happyReduction_16 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn13
		 (Link happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  13 happyReduction_17
happyReduction_17 _
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn13
		 (Close happy_var_1
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 5 13 happyReduction_18
happyReduction_18 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Wait happy_var_1 happy_var_5
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 7 13 happyReduction_19
happyReduction_19 ((HappyAbsSyn13  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Fork happy_var_1 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 6 13 happyReduction_20
happyReduction_20 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (let tmp = Identifier (At $ getPos happy_var_2) "_tmp_" in
      Fork happy_var_1 tmp (Link happy_var_3 tmp) happy_var_6
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 6 13 happyReduction_21
happyReduction_21 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Join happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 5 13 happyReduction_22
happyReduction_22 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Select happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_3  13 happyReduction_23
happyReduction_23 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn13
		 (Case happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 5 13 happyReduction_24
happyReduction_24 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Case happy_var_1 [(happy_var_3, happy_var_5)]
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 9 13 happyReduction_25
happyReduction_25 ((HappyAbsSyn13  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Cut happy_var_3 happy_var_5 happy_var_7 happy_var_9
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_2  13 happyReduction_26
happyReduction_26 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn13
		 (Call happy_var_1 happy_var_2
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_0  14 happyReduction_27
happyReduction_27  =  HappyAbsSyn14
		 ([]
	)

happyReduce_28 = happySpecReduce_2  14 happyReduction_28
happyReduction_28 _
	_
	 =  HappyAbsSyn14
		 ([]
	)

happyReduce_29 = happySpecReduce_3  14 happyReduction_29
happyReduction_29 _
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  15 happyReduction_30
happyReduction_30 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  15 happyReduction_31
happyReduction_31 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 : happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  16 happyReduction_32
happyReduction_32 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_0  17 happyReduction_33
happyReduction_33  =  HappyAbsSyn17
		 ([]
	)

happyReduce_34 = happySpecReduce_1  17 happyReduction_34
happyReduction_34 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  18 happyReduction_35
happyReduction_35 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 ([happy_var_1]
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  18 happyReduction_36
happyReduction_36 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 : happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  19 happyReduction_37
happyReduction_37 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn19
		 ((happy_var_1, happy_var_3)
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  20 happyReduction_38
happyReduction_38 (HappyTerminal (happy_var_1@(Token _ (TokenLID _))))
	 =  HappyAbsSyn20
		 (Identifier (At $ getPos happy_var_1) (getId happy_var_1) :: ChannelName
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  21 happyReduction_39
happyReduction_39 (HappyTerminal (happy_var_1@(Token _ (TokenCID _))))
	 =  HappyAbsSyn21
		 (Identifier (At $ getPos happy_var_1) (getId happy_var_1) :: TypeName
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  22 happyReduction_40
happyReduction_40 (HappyTerminal (happy_var_1@(Token _ (TokenLID _))))
	 =  HappyAbsSyn22
		 (Identifier (At $ getPos happy_var_1) (getId happy_var_1) :: TypeName
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  23 happyReduction_41
happyReduction_41 (HappyTerminal (happy_var_1@(Token _ (TokenCID _))))
	 =  HappyAbsSyn23
		 (Identifier (At $ getPos happy_var_1) (getId happy_var_1) :: ProcessName
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  24 happyReduction_42
happyReduction_42 (HappyTerminal (happy_var_1@(Token _ (TokenLID _))))
	 =  HappyAbsSyn24
		 (Identifier (At $ getPos happy_var_1) (getId happy_var_1) :: Label
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  25 happyReduction_43
happyReduction_43 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  26 happyReduction_44
happyReduction_44 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn26
		 (if happy_var_1 == 1 then One
           else error $ (show happy_var_1) ++ " is not a type"
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  26 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn26
		 (Bot
	)

happyReduce_46 = happySpecReduce_1  26 happyReduction_46
happyReduction_46 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn26
		 (Var happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  26 happyReduction_47
happyReduction_47 (HappyAbsSyn26  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn26
		 (Rec happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  26 happyReduction_48
happyReduction_48 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn26
		 (Poly False happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_2  26 happyReduction_49
happyReduction_49 _
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (Dual happy_var_1
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  26 happyReduction_50
happyReduction_50 _
	(HappyAbsSyn26  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (happy_var_2
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  26 happyReduction_51
happyReduction_51 (HappyAbsSyn26  happy_var_3)
	_
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  26 happyReduction_52
happyReduction_52 (HappyAbsSyn26  happy_var_3)
	_
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (Par happy_var_1 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_2  26 happyReduction_53
happyReduction_53 (HappyAbsSyn26  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (Mul happy_var_2 Skip
	)
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_2  26 happyReduction_54
happyReduction_54 (HappyAbsSyn26  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (Par happy_var_2 Skip
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  26 happyReduction_55
happyReduction_55 _
	 =  HappyAbsSyn26
		 (Skip
	)

happyReduce_56 = happySpecReduce_3  26 happyReduction_56
happyReduction_56 (HappyAbsSyn26  happy_var_3)
	_
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (Seq happy_var_1 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_2  26 happyReduction_57
happyReduction_57 (HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (With happy_var_2
	)
happyReduction_57 _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_2  26 happyReduction_58
happyReduction_58 (HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (Plus happy_var_2
	)
happyReduction_58 _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_3  26 happyReduction_59
happyReduction_59 (HappyAbsSyn26  happy_var_3)
	(HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (Put happy_var_2 happy_var_3
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  26 happyReduction_60
happyReduction_60 (HappyAbsSyn26  happy_var_3)
	(HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (Get happy_var_2 happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_0  27 happyReduction_61
happyReduction_61  =  HappyAbsSyn27
		 (Nothing
	)

happyReduce_62 = happySpecReduce_3  27 happyReduction_62
happyReduction_62 _
	(HappyAbsSyn29  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (Just happy_var_2
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  28 happyReduction_63
happyReduction_63 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn28
		 (fromIntegral happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  28 happyReduction_64
happyReduction_64 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn28
		 (happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  29 happyReduction_65
happyReduction_65 (HappyTerminal (happy_var_1@(Token _ (TokenINT _))))
	 =  HappyAbsSyn29
		 (getInt happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  30 happyReduction_66
happyReduction_66 (HappyTerminal (happy_var_1@(Token _ (TokenFLOAT _))))
	 =  HappyAbsSyn30
		 (getFloat happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  31 happyReduction_67
happyReduction_67 _
	(HappyAbsSyn32  happy_var_2)
	_
	 =  HappyAbsSyn31
		 (happy_var_2
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_0  32 happyReduction_68
happyReduction_68  =  HappyAbsSyn32
		 ([]
	)

happyReduce_69 = happySpecReduce_1  32 happyReduction_69
happyReduction_69 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  33 happyReduction_70
happyReduction_70 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn33
		 ([happy_var_1]
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  33 happyReduction_71
happyReduction_71 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1 : happy_var_3
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  34 happyReduction_72
happyReduction_72 (HappyAbsSyn26  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn34
		 ((happy_var_1, happy_var_3)
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexwrap(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Token _ TokenEOF -> action 69 69 tk (HappyState action) sts stk;
	Token _ TokenType -> cont 35;
	Token _ TokenSkip -> cont 36;
	Token _ TokenNew -> cont 37;
	Token _ TokenIn -> cont 38;
	happy_dollar_dollar@(Token _ (TokenCID _)) -> cont 39;
	happy_dollar_dollar@(Token _ (TokenLID _)) -> cont 40;
	happy_dollar_dollar@(Token _ (TokenINT _)) -> cont 41;
	happy_dollar_dollar@(Token _ (TokenFLOAT _)) -> cont 42;
	Token _ TokenEQ -> cont 43;
	Token _ TokenDot -> cont 44;
	Token _ TokenColon -> cont 45;
	Token _ TokenSemiColon -> cont 46;
	Token _ TokenComma -> cont 47;
	Token _ TokenLParen -> cont 48;
	Token _ TokenRParen -> cont 49;
	Token _ TokenLBrace -> cont 50;
	Token _ TokenRBrace -> cont 51;
	Token _ TokenLBrack -> cont 52;
	Token _ TokenRBrack -> cont 53;
	Token _ TokenLAngle -> cont 54;
	Token _ TokenRAngle -> cont 55;
	Token _ TokenAmp -> cont 56;
	Token _ TokenPar -> cont 57;
	Token _ TokenBot -> cont 58;
	Token _ TokenTimes -> cont 59;
	Token _ TokenPlus -> cont 60;
	Token _ TokenPut -> cont 61;
	Token _ TokenGet -> cont 62;
	Token _ TokenQMark -> cont 63;
	Token _ TokenEMark -> cont 64;
	Token _ TokenDual -> cont 65;
	Token _ TokenRTriangle -> cont 66;
	Token _ TokenLTriangle -> cont 67;
	Token _ TokenLRArrow -> cont 68;
	_ -> happyError' (tk, [])
	})

happyError_ explist 69 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> Alex a
happyReturn = (Prelude.return)
happyThen1 :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen1 = happyThen
happyReturn1 :: () => a -> Alex a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [Prelude.String]) -> Alex a
happyError' tk = (\(tokens, _) -> happyError tokens) tk
parse = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
