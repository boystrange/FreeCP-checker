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

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35 t36 t37 t38
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
	| HappyAbsSyn35 t35
	| HappyAbsSyn36 t36
	| HappyAbsSyn37 t37
	| HappyAbsSyn38 t38

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,331) ([0,0,64,0,0,0,32768,0,0,0,0,32768,0,0,0,0,2,0,0,0,0,2,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,512,0,0,0,0,0,0,0,0,64,0,0,0,4096,0,0,0,0,0,0,0,0,58368,16449,253,0,0,1024,0,0,0,0,0,0,0,0,32768,5120,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,527,2026,0,0,0,16,0,0,0,0,0,0,0,0,64,0,0,0,0,2,0,0,0,1024,0,0,0,33736,64128,1,0,36864,263,1013,0,0,512,0,0,0,0,2048,0,0,0,0,4,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,55040,128,0,0,0,0,0,0,0,0,41488,0,0,0,0,4,0,0,0,8,0,0,0,4096,0,0,0,0,32,0,0,0,0,64,0,0,0,32983,0,0,0,36864,263,1013,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16868,64832,0,0,0,1,0,0,0,1936,62721,3,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,40996,0,0,0,8434,32416,0,0,58368,16449,253,0,0,33736,64128,1,0,36864,263,1013,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,160,0,0,0,0,0,0,0,2048,0,0,0,0,1024,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,64,10,0,0,3872,59906,7,0,0,2048,0,0,0,2048,0,0,0,0,16384,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,256,2,0,0,0,2,0,0,0,1024,0,0,0,0,8,0,0,0,4096,0,0,0,0,0,16,0,0,0,2048,0,0,0,0,1,0,0,0,0,0,0,0,0,4,0,0,0,512,0,0,0,0,0,0,0,57344,4122,0,0,0,0,0,0,0,0,64,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,10241,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,58368,16449,253,0,0,8192,1280,0,0,0,0,0,0,0,3872,59906,7,0,0,8192,0,0,0,0,0,0,0,0,2048,0,0,0,0,4,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,23552,547,0,0,0,16384,0,0,0,0,128,0,0,0,6880,16,0,0,49152,8245,0,0,0,4096,0,0,0,0,32983,0,0,0,0,0,0,0,0,860,2,0,0,0,4,0,0,0,0,0,0,0,0,8192,0,0,0,13760,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,23552,515,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,27520,64,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Program","TypeDefList","TypeDef","ProcessDefList","ProcessDef","Parameters","ParameterList","ParameterNeList","Parameter","Process","Names","NameNeList","Choices","ChoiceNeList","Choice","WeightOpt","Cases","CaseList","CaseNeList","Case","ChannelName","TypeName","PolyName","ProcessName","Label","TypeExpr","Type","MeasureOpt","Num","Int","Float","Branches","BranchList","BranchNeList","Branch","TYPE","WAIT","CLOSE","CASE","SKIP","NEW","IN","CID","LID","INT","FLOAT","'='","'.'","':'","';'","','","'('","')'","'{'","'}'","'['","']'","'<'","'>'","'&'","'|'","'\8869'","'*'","'+'","'++'","'--'","'?'","'!'","'^'","%eof"]
        bit_start = st Prelude.* 73
        bit_end = (st Prelude.+ 1) Prelude.* 73
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..72]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (39) = happyShift action_4
action_0 (4) = happyGoto action_5
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 _ = happyReduce_2

action_1 (39) = happyShift action_4
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (46) = happyShift action_12
action_2 (7) = happyGoto action_9
action_2 (8) = happyGoto action_10
action_2 (27) = happyGoto action_11
action_2 _ = happyReduce_5

action_3 (39) = happyShift action_4
action_3 (5) = happyGoto action_8
action_3 (6) = happyGoto action_3
action_3 _ = happyReduce_2

action_4 (46) = happyShift action_7
action_4 (25) = happyGoto action_6
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (73) = happyAccept
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (50) = happyShift action_16
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_44

action_8 _ = happyReduce_3

action_9 _ = happyReduce_1

action_10 (46) = happyShift action_12
action_10 (7) = happyGoto action_15
action_10 (8) = happyGoto action_10
action_10 (27) = happyGoto action_11
action_10 _ = happyReduce_5

action_11 (55) = happyShift action_14
action_11 (9) = happyGoto action_13
action_11 _ = happyReduce_8

action_12 _ = happyReduce_46

action_13 (50) = happyShift action_41
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (47) = happyShift action_40
action_14 (10) = happyGoto action_36
action_14 (11) = happyGoto action_37
action_14 (12) = happyGoto action_38
action_14 (24) = happyGoto action_39
action_14 _ = happyReduce_10

action_15 _ = happyReduce_6

action_16 (43) = happyShift action_23
action_16 (46) = happyShift action_7
action_16 (47) = happyShift action_24
action_16 (48) = happyShift action_25
action_16 (49) = happyShift action_26
action_16 (55) = happyShift action_27
action_16 (63) = happyShift action_28
action_16 (65) = happyShift action_29
action_16 (67) = happyShift action_30
action_16 (68) = happyShift action_31
action_16 (69) = happyShift action_32
action_16 (70) = happyShift action_33
action_16 (71) = happyShift action_34
action_16 (72) = happyShift action_35
action_16 (25) = happyGoto action_17
action_16 (26) = happyGoto action_18
action_16 (30) = happyGoto action_19
action_16 (32) = happyGoto action_20
action_16 (33) = happyGoto action_21
action_16 (34) = happyGoto action_22
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (50) = happyShift action_66
action_17 _ = happyReduce_52

action_18 _ = happyReduce_54

action_19 (53) = happyShift action_63
action_19 (64) = happyShift action_64
action_19 (66) = happyShift action_65
action_19 _ = happyReduce_4

action_20 _ = happyReduce_50

action_21 _ = happyReduce_69

action_22 _ = happyReduce_70

action_23 _ = happyReduce_61

action_24 _ = happyReduce_45

action_25 _ = happyReduce_71

action_26 _ = happyReduce_72

action_27 (43) = happyShift action_23
action_27 (46) = happyShift action_7
action_27 (47) = happyShift action_24
action_27 (48) = happyShift action_25
action_27 (49) = happyShift action_26
action_27 (55) = happyShift action_27
action_27 (63) = happyShift action_28
action_27 (65) = happyShift action_29
action_27 (67) = happyShift action_30
action_27 (68) = happyShift action_31
action_27 (69) = happyShift action_32
action_27 (70) = happyShift action_33
action_27 (71) = happyShift action_34
action_27 (72) = happyShift action_35
action_27 (25) = happyGoto action_17
action_27 (26) = happyGoto action_18
action_27 (30) = happyGoto action_62
action_27 (32) = happyGoto action_20
action_27 (33) = happyGoto action_21
action_27 (34) = happyGoto action_22
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (57) = happyShift action_60
action_28 (35) = happyGoto action_61
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_51

action_30 (57) = happyShift action_60
action_30 (35) = happyGoto action_59
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (59) = happyShift action_57
action_31 (31) = happyGoto action_58
action_31 _ = happyReduce_67

action_32 (59) = happyShift action_57
action_32 (31) = happyGoto action_56
action_32 _ = happyReduce_67

action_33 (43) = happyShift action_23
action_33 (46) = happyShift action_7
action_33 (47) = happyShift action_24
action_33 (48) = happyShift action_25
action_33 (49) = happyShift action_26
action_33 (55) = happyShift action_27
action_33 (63) = happyShift action_28
action_33 (65) = happyShift action_29
action_33 (67) = happyShift action_30
action_33 (68) = happyShift action_31
action_33 (69) = happyShift action_32
action_33 (70) = happyShift action_33
action_33 (71) = happyShift action_34
action_33 (72) = happyShift action_35
action_33 (25) = happyGoto action_17
action_33 (26) = happyGoto action_18
action_33 (30) = happyGoto action_55
action_33 (32) = happyGoto action_20
action_33 (33) = happyGoto action_21
action_33 (34) = happyGoto action_22
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (43) = happyShift action_23
action_34 (46) = happyShift action_7
action_34 (47) = happyShift action_24
action_34 (48) = happyShift action_25
action_34 (49) = happyShift action_26
action_34 (55) = happyShift action_27
action_34 (63) = happyShift action_28
action_34 (65) = happyShift action_29
action_34 (67) = happyShift action_30
action_34 (68) = happyShift action_31
action_34 (69) = happyShift action_32
action_34 (70) = happyShift action_33
action_34 (71) = happyShift action_34
action_34 (72) = happyShift action_35
action_34 (25) = happyGoto action_17
action_34 (26) = happyGoto action_18
action_34 (30) = happyGoto action_54
action_34 (32) = happyGoto action_20
action_34 (33) = happyGoto action_21
action_34 (34) = happyGoto action_22
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (47) = happyShift action_24
action_35 (26) = happyGoto action_53
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (56) = happyShift action_52
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (54) = happyShift action_51
action_37 _ = happyReduce_11

action_38 _ = happyReduce_12

action_39 (52) = happyShift action_50
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_43

action_41 (40) = happyShift action_45
action_41 (41) = happyShift action_46
action_41 (42) = happyShift action_47
action_41 (44) = happyShift action_48
action_41 (46) = happyShift action_12
action_41 (47) = happyShift action_40
action_41 (55) = happyShift action_49
action_41 (13) = happyGoto action_42
action_41 (24) = happyGoto action_43
action_41 (27) = happyGoto action_44
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_7

action_43 (50) = happyShift action_91
action_43 (55) = happyShift action_92
action_43 (59) = happyShift action_93
action_43 (61) = happyShift action_94
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (55) = happyShift action_90
action_44 (14) = happyGoto action_89
action_44 _ = happyReduce_26

action_45 (47) = happyShift action_40
action_45 (24) = happyGoto action_88
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (47) = happyShift action_40
action_46 (24) = happyGoto action_87
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (47) = happyShift action_40
action_47 (24) = happyGoto action_86
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (55) = happyShift action_85
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (40) = happyShift action_45
action_49 (41) = happyShift action_46
action_49 (42) = happyShift action_47
action_49 (44) = happyShift action_48
action_49 (46) = happyShift action_12
action_49 (47) = happyShift action_40
action_49 (55) = happyShift action_49
action_49 (13) = happyGoto action_84
action_49 (24) = happyGoto action_43
action_49 (27) = happyGoto action_44
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (43) = happyShift action_23
action_50 (46) = happyShift action_7
action_50 (47) = happyShift action_24
action_50 (48) = happyShift action_25
action_50 (49) = happyShift action_26
action_50 (55) = happyShift action_27
action_50 (63) = happyShift action_28
action_50 (65) = happyShift action_29
action_50 (67) = happyShift action_30
action_50 (68) = happyShift action_31
action_50 (69) = happyShift action_32
action_50 (70) = happyShift action_33
action_50 (71) = happyShift action_34
action_50 (72) = happyShift action_83
action_50 (25) = happyGoto action_17
action_50 (26) = happyGoto action_18
action_50 (29) = happyGoto action_81
action_50 (30) = happyGoto action_82
action_50 (32) = happyGoto action_20
action_50 (33) = happyGoto action_21
action_50 (34) = happyGoto action_22
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (47) = happyShift action_40
action_51 (11) = happyGoto action_80
action_51 (12) = happyGoto action_38
action_51 (24) = happyGoto action_39
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_9

action_53 _ = happyReduce_55

action_54 _ = happyReduce_59

action_55 _ = happyReduce_60

action_56 (43) = happyShift action_23
action_56 (46) = happyShift action_7
action_56 (47) = happyShift action_24
action_56 (48) = happyShift action_25
action_56 (49) = happyShift action_26
action_56 (55) = happyShift action_27
action_56 (63) = happyShift action_28
action_56 (65) = happyShift action_29
action_56 (67) = happyShift action_30
action_56 (68) = happyShift action_31
action_56 (69) = happyShift action_32
action_56 (70) = happyShift action_33
action_56 (71) = happyShift action_34
action_56 (72) = happyShift action_35
action_56 (25) = happyGoto action_17
action_56 (26) = happyGoto action_18
action_56 (30) = happyGoto action_79
action_56 (32) = happyGoto action_20
action_56 (33) = happyGoto action_21
action_56 (34) = happyGoto action_22
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (48) = happyShift action_25
action_57 (33) = happyGoto action_78
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (43) = happyShift action_23
action_58 (46) = happyShift action_7
action_58 (47) = happyShift action_24
action_58 (48) = happyShift action_25
action_58 (49) = happyShift action_26
action_58 (55) = happyShift action_27
action_58 (63) = happyShift action_28
action_58 (65) = happyShift action_29
action_58 (67) = happyShift action_30
action_58 (68) = happyShift action_31
action_58 (69) = happyShift action_32
action_58 (70) = happyShift action_33
action_58 (71) = happyShift action_34
action_58 (72) = happyShift action_35
action_58 (25) = happyGoto action_17
action_58 (26) = happyGoto action_18
action_58 (30) = happyGoto action_77
action_58 (32) = happyGoto action_20
action_58 (33) = happyGoto action_21
action_58 (34) = happyGoto action_22
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_64

action_60 (47) = happyShift action_76
action_60 (28) = happyGoto action_72
action_60 (36) = happyGoto action_73
action_60 (37) = happyGoto action_74
action_60 (38) = happyGoto action_75
action_60 _ = happyReduce_74

action_61 _ = happyReduce_63

action_62 (53) = happyShift action_63
action_62 (56) = happyShift action_71
action_62 (64) = happyShift action_64
action_62 (66) = happyShift action_65
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (43) = happyShift action_23
action_63 (46) = happyShift action_7
action_63 (47) = happyShift action_24
action_63 (48) = happyShift action_25
action_63 (49) = happyShift action_26
action_63 (55) = happyShift action_27
action_63 (63) = happyShift action_28
action_63 (65) = happyShift action_29
action_63 (67) = happyShift action_30
action_63 (68) = happyShift action_31
action_63 (69) = happyShift action_32
action_63 (70) = happyShift action_33
action_63 (71) = happyShift action_34
action_63 (72) = happyShift action_35
action_63 (25) = happyGoto action_17
action_63 (26) = happyGoto action_18
action_63 (30) = happyGoto action_70
action_63 (32) = happyGoto action_20
action_63 (33) = happyGoto action_21
action_63 (34) = happyGoto action_22
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (43) = happyShift action_23
action_64 (46) = happyShift action_7
action_64 (47) = happyShift action_24
action_64 (48) = happyShift action_25
action_64 (49) = happyShift action_26
action_64 (55) = happyShift action_27
action_64 (63) = happyShift action_28
action_64 (65) = happyShift action_29
action_64 (67) = happyShift action_30
action_64 (68) = happyShift action_31
action_64 (69) = happyShift action_32
action_64 (70) = happyShift action_33
action_64 (71) = happyShift action_34
action_64 (72) = happyShift action_35
action_64 (25) = happyGoto action_17
action_64 (26) = happyGoto action_18
action_64 (30) = happyGoto action_69
action_64 (32) = happyGoto action_20
action_64 (33) = happyGoto action_21
action_64 (34) = happyGoto action_22
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (43) = happyShift action_23
action_65 (46) = happyShift action_7
action_65 (47) = happyShift action_24
action_65 (48) = happyShift action_25
action_65 (49) = happyShift action_26
action_65 (55) = happyShift action_27
action_65 (63) = happyShift action_28
action_65 (65) = happyShift action_29
action_65 (67) = happyShift action_30
action_65 (68) = happyShift action_31
action_65 (69) = happyShift action_32
action_65 (70) = happyShift action_33
action_65 (71) = happyShift action_34
action_65 (72) = happyShift action_35
action_65 (25) = happyGoto action_17
action_65 (26) = happyGoto action_18
action_65 (30) = happyGoto action_68
action_65 (32) = happyGoto action_20
action_65 (33) = happyGoto action_21
action_65 (34) = happyGoto action_22
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (43) = happyShift action_23
action_66 (46) = happyShift action_7
action_66 (47) = happyShift action_24
action_66 (48) = happyShift action_25
action_66 (49) = happyShift action_26
action_66 (55) = happyShift action_27
action_66 (63) = happyShift action_28
action_66 (65) = happyShift action_29
action_66 (67) = happyShift action_30
action_66 (68) = happyShift action_31
action_66 (69) = happyShift action_32
action_66 (70) = happyShift action_33
action_66 (71) = happyShift action_34
action_66 (72) = happyShift action_35
action_66 (25) = happyGoto action_17
action_66 (26) = happyGoto action_18
action_66 (30) = happyGoto action_67
action_66 (32) = happyGoto action_20
action_66 (33) = happyGoto action_21
action_66 (34) = happyGoto action_22
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (53) = happyShift action_63
action_67 (64) = happyShift action_64
action_67 (66) = happyShift action_65
action_67 _ = happyReduce_53

action_68 _ = happyReduce_57

action_69 _ = happyReduce_58

action_70 (53) = happyShift action_63
action_70 (64) = happyShift action_64
action_70 (66) = happyShift action_65
action_70 _ = happyReduce_62

action_71 _ = happyReduce_56

action_72 (52) = happyShift action_112
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (58) = happyShift action_111
action_73 _ = happyFail (happyExpListPerState 73)

action_74 _ = happyReduce_75

action_75 (54) = happyShift action_110
action_75 _ = happyReduce_76

action_76 _ = happyReduce_47

action_77 _ = happyReduce_65

action_78 (60) = happyShift action_109
action_78 _ = happyFail (happyExpListPerState 78)

action_79 _ = happyReduce_66

action_80 (54) = happyShift action_51
action_80 _ = happyReduce_13

action_81 _ = happyReduce_14

action_82 (53) = happyShift action_63
action_82 (64) = happyShift action_64
action_82 (66) = happyShift action_65
action_82 _ = happyReduce_48

action_83 (43) = happyShift action_23
action_83 (46) = happyShift action_7
action_83 (47) = happyShift action_24
action_83 (48) = happyShift action_25
action_83 (49) = happyShift action_26
action_83 (55) = happyShift action_27
action_83 (63) = happyShift action_28
action_83 (65) = happyShift action_29
action_83 (67) = happyShift action_30
action_83 (68) = happyShift action_31
action_83 (69) = happyShift action_32
action_83 (70) = happyShift action_33
action_83 (71) = happyShift action_34
action_83 (72) = happyShift action_35
action_83 (25) = happyGoto action_17
action_83 (26) = happyGoto action_107
action_83 (30) = happyGoto action_108
action_83 (32) = happyGoto action_20
action_83 (33) = happyGoto action_21
action_83 (34) = happyGoto action_22
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (56) = happyShift action_106
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (47) = happyShift action_40
action_85 (24) = happyGoto action_105
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (57) = happyShift action_104
action_86 (20) = happyGoto action_103
action_86 _ = happyFail (happyExpListPerState 86)

action_87 _ = happyReduce_17

action_88 (51) = happyShift action_102
action_88 _ = happyFail (happyExpListPerState 88)

action_89 _ = happyReduce_25

action_90 (47) = happyShift action_40
action_90 (56) = happyShift action_101
action_90 (15) = happyGoto action_99
action_90 (24) = happyGoto action_100
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (47) = happyShift action_40
action_91 (24) = happyGoto action_98
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (47) = happyShift action_40
action_92 (24) = happyGoto action_97
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (47) = happyShift action_76
action_93 (28) = happyGoto action_96
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (47) = happyShift action_40
action_94 (24) = happyGoto action_95
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (62) = happyShift action_125
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (60) = happyShift action_124
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (56) = happyShift action_123
action_97 _ = happyFail (happyExpListPerState 97)

action_98 _ = happyReduce_16

action_99 (56) = happyShift action_122
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (54) = happyShift action_121
action_100 _ = happyReduce_29

action_101 _ = happyReduce_27

action_102 (40) = happyShift action_45
action_102 (41) = happyShift action_46
action_102 (42) = happyShift action_47
action_102 (44) = happyShift action_48
action_102 (46) = happyShift action_12
action_102 (47) = happyShift action_40
action_102 (55) = happyShift action_49
action_102 (13) = happyGoto action_120
action_102 (24) = happyGoto action_43
action_102 (27) = happyGoto action_44
action_102 _ = happyFail (happyExpListPerState 102)

action_103 _ = happyReduce_23

action_104 (47) = happyShift action_76
action_104 (21) = happyGoto action_116
action_104 (22) = happyGoto action_117
action_104 (23) = happyGoto action_118
action_104 (28) = happyGoto action_119
action_104 _ = happyReduce_38

action_105 (52) = happyShift action_115
action_105 _ = happyFail (happyExpListPerState 105)

action_106 _ = happyReduce_15

action_107 (53) = happyReduce_55
action_107 (54) = happyReduce_55
action_107 (56) = happyReduce_55
action_107 (64) = happyReduce_55
action_107 (66) = happyReduce_55
action_107 _ = happyReduce_55

action_108 (53) = happyShift action_63
action_108 (64) = happyShift action_64
action_108 (66) = happyShift action_65
action_108 _ = happyReduce_49

action_109 _ = happyReduce_68

action_110 (47) = happyShift action_76
action_110 (28) = happyGoto action_72
action_110 (37) = happyGoto action_114
action_110 (38) = happyGoto action_75
action_110 _ = happyFail (happyExpListPerState 110)

action_111 _ = happyReduce_73

action_112 (43) = happyShift action_23
action_112 (46) = happyShift action_7
action_112 (47) = happyShift action_24
action_112 (48) = happyShift action_25
action_112 (49) = happyShift action_26
action_112 (55) = happyShift action_27
action_112 (63) = happyShift action_28
action_112 (65) = happyShift action_29
action_112 (67) = happyShift action_30
action_112 (68) = happyShift action_31
action_112 (69) = happyShift action_32
action_112 (70) = happyShift action_33
action_112 (71) = happyShift action_34
action_112 (72) = happyShift action_35
action_112 (25) = happyGoto action_17
action_112 (26) = happyGoto action_18
action_112 (30) = happyGoto action_113
action_112 (32) = happyGoto action_20
action_112 (33) = happyGoto action_21
action_112 (34) = happyGoto action_22
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (53) = happyShift action_63
action_113 (64) = happyShift action_64
action_113 (66) = happyShift action_65
action_113 _ = happyReduce_78

action_114 _ = happyReduce_77

action_115 (43) = happyShift action_23
action_115 (46) = happyShift action_7
action_115 (47) = happyShift action_24
action_115 (48) = happyShift action_25
action_115 (49) = happyShift action_26
action_115 (55) = happyShift action_27
action_115 (63) = happyShift action_28
action_115 (65) = happyShift action_29
action_115 (67) = happyShift action_30
action_115 (68) = happyShift action_31
action_115 (69) = happyShift action_32
action_115 (70) = happyShift action_33
action_115 (71) = happyShift action_34
action_115 (72) = happyShift action_83
action_115 (25) = happyGoto action_17
action_115 (26) = happyGoto action_18
action_115 (29) = happyGoto action_134
action_115 (30) = happyGoto action_82
action_115 (32) = happyGoto action_20
action_115 (33) = happyGoto action_21
action_115 (34) = happyGoto action_22
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (58) = happyShift action_133
action_116 _ = happyFail (happyExpListPerState 116)

action_117 _ = happyReduce_39

action_118 (54) = happyShift action_132
action_118 _ = happyReduce_40

action_119 (52) = happyShift action_131
action_119 _ = happyFail (happyExpListPerState 119)

action_120 _ = happyReduce_18

action_121 (47) = happyShift action_40
action_121 (15) = happyGoto action_130
action_121 (24) = happyGoto action_100
action_121 _ = happyFail (happyExpListPerState 121)

action_122 _ = happyReduce_28

action_123 (40) = happyShift action_45
action_123 (41) = happyShift action_46
action_123 (42) = happyShift action_47
action_123 (44) = happyShift action_48
action_123 (46) = happyShift action_12
action_123 (47) = happyShift action_40
action_123 (51) = happyShift action_129
action_123 (55) = happyShift action_49
action_123 (13) = happyGoto action_128
action_123 (24) = happyGoto action_43
action_123 (27) = happyGoto action_44
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (51) = happyShift action_127
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (51) = happyShift action_126
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (40) = happyShift action_45
action_126 (41) = happyShift action_46
action_126 (42) = happyShift action_47
action_126 (44) = happyShift action_48
action_126 (46) = happyShift action_12
action_126 (47) = happyShift action_40
action_126 (55) = happyShift action_49
action_126 (13) = happyGoto action_141
action_126 (24) = happyGoto action_43
action_126 (27) = happyGoto action_44
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (40) = happyShift action_45
action_127 (41) = happyShift action_46
action_127 (42) = happyShift action_47
action_127 (44) = happyShift action_48
action_127 (46) = happyShift action_12
action_127 (47) = happyShift action_40
action_127 (55) = happyShift action_49
action_127 (13) = happyGoto action_140
action_127 (24) = happyGoto action_43
action_127 (27) = happyGoto action_44
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (45) = happyShift action_139
action_128 _ = happyFail (happyExpListPerState 128)

action_129 (40) = happyShift action_45
action_129 (41) = happyShift action_46
action_129 (42) = happyShift action_47
action_129 (44) = happyShift action_48
action_129 (46) = happyShift action_12
action_129 (47) = happyShift action_40
action_129 (55) = happyShift action_49
action_129 (13) = happyGoto action_138
action_129 (24) = happyGoto action_43
action_129 (27) = happyGoto action_44
action_129 _ = happyFail (happyExpListPerState 129)

action_130 _ = happyReduce_30

action_131 (40) = happyShift action_45
action_131 (41) = happyShift action_46
action_131 (42) = happyShift action_47
action_131 (44) = happyShift action_48
action_131 (46) = happyShift action_12
action_131 (47) = happyShift action_40
action_131 (55) = happyShift action_49
action_131 (13) = happyGoto action_137
action_131 (24) = happyGoto action_43
action_131 (27) = happyGoto action_44
action_131 _ = happyFail (happyExpListPerState 131)

action_132 (47) = happyShift action_76
action_132 (22) = happyGoto action_136
action_132 (23) = happyGoto action_118
action_132 (28) = happyGoto action_119
action_132 _ = happyFail (happyExpListPerState 132)

action_133 _ = happyReduce_37

action_134 (56) = happyShift action_135
action_134 _ = happyFail (happyExpListPerState 134)

action_135 (40) = happyShift action_45
action_135 (41) = happyShift action_46
action_135 (42) = happyShift action_47
action_135 (44) = happyShift action_48
action_135 (46) = happyShift action_12
action_135 (47) = happyShift action_40
action_135 (55) = happyShift action_49
action_135 (13) = happyGoto action_143
action_135 (24) = happyGoto action_43
action_135 (27) = happyGoto action_44
action_135 _ = happyFail (happyExpListPerState 135)

action_136 _ = happyReduce_41

action_137 _ = happyReduce_42

action_138 _ = happyReduce_21

action_139 (40) = happyShift action_45
action_139 (41) = happyShift action_46
action_139 (42) = happyShift action_47
action_139 (44) = happyShift action_48
action_139 (46) = happyShift action_12
action_139 (47) = happyShift action_40
action_139 (55) = happyShift action_49
action_139 (13) = happyGoto action_142
action_139 (24) = happyGoto action_43
action_139 (27) = happyGoto action_44
action_139 _ = happyFail (happyExpListPerState 139)

action_140 _ = happyReduce_22

action_141 _ = happyReduce_20

action_142 _ = happyReduce_19

action_143 (45) = happyShift action_144
action_143 _ = happyFail (happyExpListPerState 143)

action_144 (40) = happyShift action_45
action_144 (41) = happyShift action_46
action_144 (42) = happyShift action_47
action_144 (44) = happyShift action_48
action_144 (46) = happyShift action_12
action_144 (47) = happyShift action_40
action_144 (55) = happyShift action_49
action_144 (13) = happyGoto action_145
action_144 (24) = happyGoto action_43
action_144 (27) = happyGoto action_44
action_144 _ = happyFail (happyExpListPerState 144)

action_145 _ = happyReduce_24

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
happyReduction_4 ((HappyAbsSyn30  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
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
	(HappyAbsSyn27  happy_var_1) `HappyStk`
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
happyReduction_14 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
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
happyReduction_16 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn13
		 (Link happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  13 happyReduction_17
happyReduction_17 (HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Close happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 13 happyReduction_18
happyReduction_18 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Wait happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 7 13 happyReduction_19
happyReduction_19 ((HappyAbsSyn13  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Fork happy_var_1 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 6 13 happyReduction_20
happyReduction_20 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn24  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (let tmp = Identifier (At $ getPos happy_var_2) "_tmp_" in
      Fork happy_var_1 tmp (Link happy_var_3 tmp) happy_var_6
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 6 13 happyReduction_21
happyReduction_21 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Join happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 6 13 happyReduction_22
happyReduction_22 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Select happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_3  13 happyReduction_23
happyReduction_23 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Case happy_var_2 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 9 13 happyReduction_24
happyReduction_24 ((HappyAbsSyn13  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Cut happy_var_3 happy_var_5 happy_var_7 happy_var_9
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_2  13 happyReduction_25
happyReduction_25 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn13
		 (Call happy_var_1 happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_0  14 happyReduction_26
happyReduction_26  =  HappyAbsSyn14
		 ([]
	)

happyReduce_27 = happySpecReduce_2  14 happyReduction_27
happyReduction_27 _
	_
	 =  HappyAbsSyn14
		 ([]
	)

happyReduce_28 = happySpecReduce_3  14 happyReduction_28
happyReduction_28 _
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  15 happyReduction_29
happyReduction_29 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  15 happyReduction_30
happyReduction_30 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 : happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  16 happyReduction_31
happyReduction_31 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  17 happyReduction_32
happyReduction_32 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 ([happy_var_1]
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  17 happyReduction_33
happyReduction_33 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 : happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_2  18 happyReduction_34
happyReduction_34 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 ((happy_var_1, happy_var_2)
	)
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_0  19 happyReduction_35
happyReduction_35  =  HappyAbsSyn19
		 (1
	)

happyReduce_36 = happySpecReduce_2  19 happyReduction_36
happyReduction_36 _
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  20 happyReduction_37
happyReduction_37 _
	(HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_0  21 happyReduction_38
happyReduction_38  =  HappyAbsSyn21
		 ([]
	)

happyReduce_39 = happySpecReduce_1  21 happyReduction_39
happyReduction_39 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  22 happyReduction_40
happyReduction_40 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 ([happy_var_1]
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  22 happyReduction_41
happyReduction_41 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1 : happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  23 happyReduction_42
happyReduction_42 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn23
		 ((happy_var_1, happy_var_3)
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  24 happyReduction_43
happyReduction_43 (HappyTerminal (happy_var_1@(Token _ (TokenLID _))))
	 =  HappyAbsSyn24
		 (Identifier (At $ getPos happy_var_1) (getId happy_var_1) :: ChannelName
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  25 happyReduction_44
happyReduction_44 (HappyTerminal (happy_var_1@(Token _ (TokenCID _))))
	 =  HappyAbsSyn25
		 (Identifier (At $ getPos happy_var_1) (getId happy_var_1) :: TypeName
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  26 happyReduction_45
happyReduction_45 (HappyTerminal (happy_var_1@(Token _ (TokenLID _))))
	 =  HappyAbsSyn26
		 (Identifier (At $ getPos happy_var_1) (getId happy_var_1) :: TypeName
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  27 happyReduction_46
happyReduction_46 (HappyTerminal (happy_var_1@(Token _ (TokenCID _))))
	 =  HappyAbsSyn27
		 (Identifier (At $ getPos happy_var_1) (getId happy_var_1) :: ProcessName
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  28 happyReduction_47
happyReduction_47 (HappyTerminal (happy_var_1@(Token _ (TokenLID _))))
	 =  HappyAbsSyn28
		 (Identifier (At $ getPos happy_var_1) (getId happy_var_1) :: Label
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  29 happyReduction_48
happyReduction_48 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn29
		 (Type happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_2  29 happyReduction_49
happyReduction_49 (HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn29
		 (Dual happy_var_2
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  30 happyReduction_50
happyReduction_50 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn30
		 (if happy_var_1 == 1 then One
           else error $ (show happy_var_1) ++ " is not a type"
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  30 happyReduction_51
happyReduction_51 _
	 =  HappyAbsSyn30
		 (Bot
	)

happyReduce_52 = happySpecReduce_1  30 happyReduction_52
happyReduction_52 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn30
		 (Var happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  30 happyReduction_53
happyReduction_53 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn30
		 (Rec happy_var_1 happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  30 happyReduction_54
happyReduction_54 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn30
		 (Poly False happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_2  30 happyReduction_55
happyReduction_55 (HappyAbsSyn26  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (Poly True happy_var_2
	)
happyReduction_55 _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  30 happyReduction_56
happyReduction_56 _
	(HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (happy_var_2
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  30 happyReduction_57
happyReduction_57 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  30 happyReduction_58
happyReduction_58 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (Par happy_var_1 happy_var_3
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_2  30 happyReduction_59
happyReduction_59 (HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (Mul happy_var_2 Skip
	)
happyReduction_59 _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_2  30 happyReduction_60
happyReduction_60 (HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (Par happy_var_2 Skip
	)
happyReduction_60 _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  30 happyReduction_61
happyReduction_61 _
	 =  HappyAbsSyn30
		 (Skip
	)

happyReduce_62 = happySpecReduce_3  30 happyReduction_62
happyReduction_62 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (Seq happy_var_1 happy_var_3
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_2  30 happyReduction_63
happyReduction_63 (HappyAbsSyn35  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (With happy_var_2
	)
happyReduction_63 _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_2  30 happyReduction_64
happyReduction_64 (HappyAbsSyn35  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (Plus happy_var_2
	)
happyReduction_64 _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  30 happyReduction_65
happyReduction_65 (HappyAbsSyn30  happy_var_3)
	(HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (Put happy_var_2 happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  30 happyReduction_66
happyReduction_66 (HappyAbsSyn30  happy_var_3)
	(HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (Get happy_var_2 happy_var_3
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_0  31 happyReduction_67
happyReduction_67  =  HappyAbsSyn31
		 (Nothing
	)

happyReduce_68 = happySpecReduce_3  31 happyReduction_68
happyReduction_68 _
	(HappyAbsSyn33  happy_var_2)
	_
	 =  HappyAbsSyn31
		 (Just happy_var_2
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  32 happyReduction_69
happyReduction_69 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn32
		 (fromIntegral happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  32 happyReduction_70
happyReduction_70 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  33 happyReduction_71
happyReduction_71 (HappyTerminal (happy_var_1@(Token _ (TokenINT _))))
	 =  HappyAbsSyn33
		 (getInt happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  34 happyReduction_72
happyReduction_72 (HappyTerminal (happy_var_1@(Token _ (TokenFLOAT _))))
	 =  HappyAbsSyn34
		 (getFloat happy_var_1
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  35 happyReduction_73
happyReduction_73 _
	(HappyAbsSyn36  happy_var_2)
	_
	 =  HappyAbsSyn35
		 (happy_var_2
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_0  36 happyReduction_74
happyReduction_74  =  HappyAbsSyn36
		 ([]
	)

happyReduce_75 = happySpecReduce_1  36 happyReduction_75
happyReduction_75 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn36
		 (happy_var_1
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  37 happyReduction_76
happyReduction_76 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn37
		 ([happy_var_1]
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  37 happyReduction_77
happyReduction_77 (HappyAbsSyn37  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1 : happy_var_3
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_3  38 happyReduction_78
happyReduction_78 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn38
		 ((happy_var_1, happy_var_3)
	)
happyReduction_78 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexwrap(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Token _ TokenEOF -> action 73 73 tk (HappyState action) sts stk;
	Token _ TokenType -> cont 39;
	Token _ TokenWait -> cont 40;
	Token _ TokenClose -> cont 41;
	Token _ TokenCase -> cont 42;
	Token _ TokenSkip -> cont 43;
	Token _ TokenNew -> cont 44;
	Token _ TokenIn -> cont 45;
	happy_dollar_dollar@(Token _ (TokenCID _)) -> cont 46;
	happy_dollar_dollar@(Token _ (TokenLID _)) -> cont 47;
	happy_dollar_dollar@(Token _ (TokenINT _)) -> cont 48;
	happy_dollar_dollar@(Token _ (TokenFLOAT _)) -> cont 49;
	Token _ TokenEQ -> cont 50;
	Token _ TokenDot -> cont 51;
	Token _ TokenColon -> cont 52;
	Token _ TokenSemiColon -> cont 53;
	Token _ TokenComma -> cont 54;
	Token _ TokenLParen -> cont 55;
	Token _ TokenRParen -> cont 56;
	Token _ TokenLBrace -> cont 57;
	Token _ TokenRBrace -> cont 58;
	Token _ TokenLBrack -> cont 59;
	Token _ TokenRBrack -> cont 60;
	Token _ TokenLAngle -> cont 61;
	Token _ TokenRAngle -> cont 62;
	Token _ TokenAmp -> cont 63;
	Token _ TokenPar -> cont 64;
	Token _ TokenBot -> cont 65;
	Token _ TokenTimes -> cont 66;
	Token _ TokenPlus -> cont 67;
	Token _ TokenPut -> cont 68;
	Token _ TokenGet -> cont 69;
	Token _ TokenQMark -> cont 70;
	Token _ TokenEMark -> cont 71;
	Token _ TokenDual -> cont 72;
	_ -> happyError' (tk, [])
	})

happyError_ explist 73 tk = happyError' (tk, explist)
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
