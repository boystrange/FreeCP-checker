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
happyExpList = Happy_Data_Array.listArray (0,295) ([0,0,64,0,0,0,8192,0,0,0,0,2048,0,0,0,2048,0,0,0,0,512,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,8,0,0,0,0,0,0,0,4096,0,0,0,0,1,0,0,0,0,0,0,0,57344,20545,39,0,0,0,0,0,0,0,0,0,0,0,0,20,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4216,2516,0,0,0,32,0,0,0,0,0,0,0,0,8,0,0,0,4096,0,0,0,0,8,0,0,16384,0,0,0,0,16384,0,0,0,0,8,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,430,1,0,0,0,0,0,0,0,17920,1536,0,0,0,32,0,0,0,16,0,0,0,2048,0,0,0,0,4,0,0,0,0,2,0,0,44544,257,0,0,0,33728,20128,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,2108,1258,0,0,2048,0,0,0,0,33295,314,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,20544,0,0,0,4216,2516,0,0,15360,59912,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,512,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,20480,0,0,0,4216,2516,0,0,0,16,0,0,0,4,0,0,0,0,8,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,16416,0,0,0,4096,0,0,0,0,8,0,0,0,1024,0,0,0,0,128,0,0,0,16384,0,0,0,55040,128,0,0,32768,16491,0,0,0,0,64,0,0,0,1024,0,0,0,0,0,0,0,0,2048,0,0,0,0,1,0,0,0,0,0,0,0,32983,0,0,0,0,0,0,0,0,32,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,61440,43040,19,0,0,0,40,0,0,0,0,0,0,0,1054,629,0,0,0,16,0,0,0,0,0,0,0,16384,0,0,0,0,8,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,1720,4,0,0,23552,643,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,13760,32,0,0,0,0,0,0,0,0,0,0,0,47104,1030,0,0,0,512,0,0,0,0,0,0,0,0,0,1,0,0,27520,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1720,4,0,0,0,0,0,0,0,64,0,0,0,55040,128,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Program","TypeDefList","TypeDef","ProcessDefList","ProcessDef","Parameters","ParameterList","ParameterNeList","Parameter","Process","Names","NameNeList","Choices","ChoiceNeList","Choice","WeightOpt","Cases","CaseList","CaseNeList","Case","ChannelName","TypeName","PolyName","ProcessName","Label","TypeExpr","Type","MeasureOpt","Num","Int","Float","Branches","BranchList","BranchNeList","Branch","TYPE","WAIT","CLOSE","CASE","FLIP","NEW","IN","CID","LID","INT","FLOAT","'='","'.'","':'","';'","','","'('","')'","'{'","'}'","'['","']'","'&'","'|'","'\8869'","'*'","'+'","'++'","'--'","'?'","'!'","'^'","%eof"]
        bit_start = st Prelude.* 71
        bit_end = (st Prelude.+ 1) Prelude.* 71
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..70]
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

action_5 (71) = happyAccept
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (50) = happyShift action_16
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_45

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

action_12 _ = happyReduce_47

action_13 (50) = happyShift action_38
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (47) = happyShift action_37
action_14 (10) = happyGoto action_33
action_14 (11) = happyGoto action_34
action_14 (12) = happyGoto action_35
action_14 (24) = happyGoto action_36
action_14 _ = happyReduce_10

action_15 _ = happyReduce_6

action_16 (46) = happyShift action_7
action_16 (47) = happyShift action_23
action_16 (48) = happyShift action_24
action_16 (49) = happyShift action_25
action_16 (55) = happyShift action_26
action_16 (61) = happyShift action_27
action_16 (63) = happyShift action_28
action_16 (65) = happyShift action_29
action_16 (66) = happyShift action_30
action_16 (67) = happyShift action_31
action_16 (70) = happyShift action_32
action_16 (25) = happyGoto action_17
action_16 (26) = happyGoto action_18
action_16 (30) = happyGoto action_19
action_16 (32) = happyGoto action_20
action_16 (33) = happyGoto action_21
action_16 (34) = happyGoto action_22
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_53

action_18 _ = happyReduce_54

action_19 (62) = happyShift action_58
action_19 (64) = happyShift action_59
action_19 _ = happyReduce_4

action_20 _ = happyReduce_51

action_21 _ = happyReduce_65

action_22 _ = happyReduce_66

action_23 _ = happyReduce_46

action_24 _ = happyReduce_67

action_25 _ = happyReduce_68

action_26 (46) = happyShift action_7
action_26 (47) = happyShift action_23
action_26 (48) = happyShift action_24
action_26 (49) = happyShift action_25
action_26 (55) = happyShift action_26
action_26 (61) = happyShift action_27
action_26 (63) = happyShift action_28
action_26 (65) = happyShift action_29
action_26 (66) = happyShift action_30
action_26 (67) = happyShift action_31
action_26 (70) = happyShift action_32
action_26 (25) = happyGoto action_17
action_26 (26) = happyGoto action_18
action_26 (30) = happyGoto action_57
action_26 (32) = happyGoto action_20
action_26 (33) = happyGoto action_21
action_26 (34) = happyGoto action_22
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (57) = happyShift action_55
action_27 (35) = happyGoto action_56
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_52

action_29 (57) = happyShift action_55
action_29 (35) = happyGoto action_54
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (59) = happyShift action_52
action_30 (31) = happyGoto action_53
action_30 _ = happyReduce_63

action_31 (59) = happyShift action_52
action_31 (31) = happyGoto action_51
action_31 _ = happyReduce_63

action_32 (47) = happyShift action_23
action_32 (26) = happyGoto action_50
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (56) = happyShift action_49
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (54) = happyShift action_48
action_34 _ = happyReduce_11

action_35 _ = happyReduce_12

action_36 (52) = happyShift action_47
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_44

action_38 (40) = happyShift action_42
action_38 (41) = happyShift action_43
action_38 (42) = happyShift action_44
action_38 (44) = happyShift action_45
action_38 (46) = happyShift action_12
action_38 (47) = happyShift action_37
action_38 (55) = happyShift action_46
action_38 (13) = happyGoto action_39
action_38 (24) = happyGoto action_40
action_38 (27) = happyGoto action_41
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_7

action_40 (50) = happyShift action_82
action_40 (51) = happyShift action_83
action_40 (55) = happyShift action_84
action_40 (66) = happyShift action_85
action_40 (67) = happyShift action_86
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (55) = happyShift action_81
action_41 (14) = happyGoto action_80
action_41 _ = happyReduce_27

action_42 (47) = happyShift action_37
action_42 (24) = happyGoto action_79
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (47) = happyShift action_37
action_43 (24) = happyGoto action_78
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (47) = happyShift action_37
action_44 (24) = happyGoto action_77
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (55) = happyShift action_76
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (40) = happyShift action_42
action_46 (41) = happyShift action_43
action_46 (42) = happyShift action_44
action_46 (44) = happyShift action_45
action_46 (46) = happyShift action_12
action_46 (47) = happyShift action_37
action_46 (55) = happyShift action_46
action_46 (13) = happyGoto action_75
action_46 (24) = happyGoto action_40
action_46 (27) = happyGoto action_41
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (46) = happyShift action_7
action_47 (47) = happyShift action_23
action_47 (48) = happyShift action_24
action_47 (49) = happyShift action_25
action_47 (55) = happyShift action_26
action_47 (61) = happyShift action_27
action_47 (63) = happyShift action_28
action_47 (65) = happyShift action_29
action_47 (66) = happyShift action_30
action_47 (67) = happyShift action_31
action_47 (70) = happyShift action_74
action_47 (25) = happyGoto action_17
action_47 (26) = happyGoto action_18
action_47 (29) = happyGoto action_72
action_47 (30) = happyGoto action_73
action_47 (32) = happyGoto action_20
action_47 (33) = happyGoto action_21
action_47 (34) = happyGoto action_22
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (47) = happyShift action_37
action_48 (11) = happyGoto action_71
action_48 (12) = happyGoto action_35
action_48 (24) = happyGoto action_36
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_9

action_50 _ = happyReduce_55

action_51 (46) = happyShift action_7
action_51 (47) = happyShift action_23
action_51 (48) = happyShift action_24
action_51 (49) = happyShift action_25
action_51 (55) = happyShift action_26
action_51 (61) = happyShift action_27
action_51 (63) = happyShift action_28
action_51 (65) = happyShift action_29
action_51 (66) = happyShift action_30
action_51 (67) = happyShift action_31
action_51 (70) = happyShift action_32
action_51 (25) = happyGoto action_17
action_51 (26) = happyGoto action_18
action_51 (30) = happyGoto action_70
action_51 (32) = happyGoto action_20
action_51 (33) = happyGoto action_21
action_51 (34) = happyGoto action_22
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (48) = happyShift action_24
action_52 (33) = happyGoto action_69
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (46) = happyShift action_7
action_53 (47) = happyShift action_23
action_53 (48) = happyShift action_24
action_53 (49) = happyShift action_25
action_53 (55) = happyShift action_26
action_53 (61) = happyShift action_27
action_53 (63) = happyShift action_28
action_53 (65) = happyShift action_29
action_53 (66) = happyShift action_30
action_53 (67) = happyShift action_31
action_53 (70) = happyShift action_32
action_53 (25) = happyGoto action_17
action_53 (26) = happyGoto action_18
action_53 (30) = happyGoto action_68
action_53 (32) = happyGoto action_20
action_53 (33) = happyGoto action_21
action_53 (34) = happyGoto action_22
action_53 _ = happyFail (happyExpListPerState 53)

action_54 _ = happyReduce_60

action_55 (47) = happyShift action_67
action_55 (28) = happyGoto action_63
action_55 (36) = happyGoto action_64
action_55 (37) = happyGoto action_65
action_55 (38) = happyGoto action_66
action_55 _ = happyReduce_70

action_56 _ = happyReduce_59

action_57 (56) = happyShift action_62
action_57 (62) = happyShift action_58
action_57 (64) = happyShift action_59
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (46) = happyShift action_7
action_58 (47) = happyShift action_23
action_58 (48) = happyShift action_24
action_58 (49) = happyShift action_25
action_58 (55) = happyShift action_26
action_58 (61) = happyShift action_27
action_58 (63) = happyShift action_28
action_58 (65) = happyShift action_29
action_58 (66) = happyShift action_30
action_58 (67) = happyShift action_31
action_58 (70) = happyShift action_32
action_58 (25) = happyGoto action_17
action_58 (26) = happyGoto action_18
action_58 (30) = happyGoto action_61
action_58 (32) = happyGoto action_20
action_58 (33) = happyGoto action_21
action_58 (34) = happyGoto action_22
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (46) = happyShift action_7
action_59 (47) = happyShift action_23
action_59 (48) = happyShift action_24
action_59 (49) = happyShift action_25
action_59 (55) = happyShift action_26
action_59 (61) = happyShift action_27
action_59 (63) = happyShift action_28
action_59 (65) = happyShift action_29
action_59 (66) = happyShift action_30
action_59 (67) = happyShift action_31
action_59 (70) = happyShift action_32
action_59 (25) = happyGoto action_17
action_59 (26) = happyGoto action_18
action_59 (30) = happyGoto action_60
action_59 (32) = happyGoto action_20
action_59 (33) = happyGoto action_21
action_59 (34) = happyGoto action_22
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_57

action_61 _ = happyReduce_58

action_62 _ = happyReduce_56

action_63 (52) = happyShift action_105
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (58) = happyShift action_104
action_64 _ = happyFail (happyExpListPerState 64)

action_65 _ = happyReduce_71

action_66 (54) = happyShift action_103
action_66 _ = happyReduce_72

action_67 _ = happyReduce_48

action_68 _ = happyReduce_61

action_69 (60) = happyShift action_102
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_62

action_71 (54) = happyShift action_48
action_71 _ = happyReduce_13

action_72 _ = happyReduce_14

action_73 (62) = happyShift action_58
action_73 (64) = happyShift action_59
action_73 _ = happyReduce_49

action_74 (46) = happyShift action_7
action_74 (47) = happyShift action_23
action_74 (48) = happyShift action_24
action_74 (49) = happyShift action_25
action_74 (55) = happyShift action_26
action_74 (61) = happyShift action_27
action_74 (63) = happyShift action_28
action_74 (65) = happyShift action_29
action_74 (66) = happyShift action_30
action_74 (67) = happyShift action_31
action_74 (70) = happyShift action_32
action_74 (25) = happyGoto action_17
action_74 (26) = happyGoto action_100
action_74 (30) = happyGoto action_101
action_74 (32) = happyGoto action_20
action_74 (33) = happyGoto action_21
action_74 (34) = happyGoto action_22
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (56) = happyShift action_99
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (47) = happyShift action_37
action_76 (24) = happyGoto action_98
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (57) = happyShift action_97
action_77 (20) = happyGoto action_96
action_77 _ = happyFail (happyExpListPerState 77)

action_78 _ = happyReduce_17

action_79 (53) = happyShift action_95
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_26

action_81 (47) = happyShift action_37
action_81 (56) = happyShift action_94
action_81 (15) = happyGoto action_92
action_81 (24) = happyGoto action_93
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (47) = happyShift action_37
action_82 (24) = happyGoto action_91
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (47) = happyShift action_67
action_83 (28) = happyGoto action_90
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (47) = happyShift action_37
action_84 (24) = happyGoto action_89
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (53) = happyShift action_88
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (53) = happyShift action_87
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (40) = happyShift action_42
action_87 (41) = happyShift action_43
action_87 (42) = happyShift action_44
action_87 (44) = happyShift action_45
action_87 (46) = happyShift action_12
action_87 (47) = happyShift action_37
action_87 (55) = happyShift action_46
action_87 (13) = happyGoto action_119
action_87 (24) = happyGoto action_40
action_87 (27) = happyGoto action_41
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (40) = happyShift action_42
action_88 (41) = happyShift action_43
action_88 (42) = happyShift action_44
action_88 (44) = happyShift action_45
action_88 (46) = happyShift action_12
action_88 (47) = happyShift action_37
action_88 (55) = happyShift action_46
action_88 (13) = happyGoto action_118
action_88 (24) = happyGoto action_40
action_88 (27) = happyGoto action_41
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (56) = happyShift action_117
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (53) = happyShift action_116
action_90 _ = happyFail (happyExpListPerState 90)

action_91 _ = happyReduce_16

action_92 (56) = happyShift action_115
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (54) = happyShift action_114
action_93 _ = happyReduce_30

action_94 _ = happyReduce_28

action_95 (40) = happyShift action_42
action_95 (41) = happyShift action_43
action_95 (42) = happyShift action_44
action_95 (44) = happyShift action_45
action_95 (46) = happyShift action_12
action_95 (47) = happyShift action_37
action_95 (55) = happyShift action_46
action_95 (13) = happyGoto action_113
action_95 (24) = happyGoto action_40
action_95 (27) = happyGoto action_41
action_95 _ = happyFail (happyExpListPerState 95)

action_96 _ = happyReduce_22

action_97 (47) = happyShift action_67
action_97 (21) = happyGoto action_109
action_97 (22) = happyGoto action_110
action_97 (23) = happyGoto action_111
action_97 (28) = happyGoto action_112
action_97 _ = happyReduce_39

action_98 (52) = happyShift action_108
action_98 _ = happyFail (happyExpListPerState 98)

action_99 _ = happyReduce_15

action_100 (54) = happyReduce_55
action_100 (56) = happyReduce_55
action_100 (62) = happyReduce_55
action_100 (64) = happyReduce_55
action_100 _ = happyReduce_55

action_101 (62) = happyShift action_58
action_101 (64) = happyShift action_59
action_101 _ = happyReduce_50

action_102 _ = happyReduce_64

action_103 (47) = happyShift action_67
action_103 (28) = happyGoto action_63
action_103 (37) = happyGoto action_107
action_103 (38) = happyGoto action_66
action_103 _ = happyFail (happyExpListPerState 103)

action_104 _ = happyReduce_69

action_105 (46) = happyShift action_7
action_105 (47) = happyShift action_23
action_105 (48) = happyShift action_24
action_105 (49) = happyShift action_25
action_105 (55) = happyShift action_26
action_105 (61) = happyShift action_27
action_105 (63) = happyShift action_28
action_105 (65) = happyShift action_29
action_105 (66) = happyShift action_30
action_105 (67) = happyShift action_31
action_105 (70) = happyShift action_32
action_105 (25) = happyGoto action_17
action_105 (26) = happyGoto action_18
action_105 (30) = happyGoto action_106
action_105 (32) = happyGoto action_20
action_105 (33) = happyGoto action_21
action_105 (34) = happyGoto action_22
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (62) = happyShift action_58
action_106 (64) = happyShift action_59
action_106 _ = happyReduce_74

action_107 _ = happyReduce_73

action_108 (46) = happyShift action_7
action_108 (47) = happyShift action_23
action_108 (48) = happyShift action_24
action_108 (49) = happyShift action_25
action_108 (55) = happyShift action_26
action_108 (61) = happyShift action_27
action_108 (63) = happyShift action_28
action_108 (65) = happyShift action_29
action_108 (66) = happyShift action_30
action_108 (67) = happyShift action_31
action_108 (70) = happyShift action_74
action_108 (25) = happyGoto action_17
action_108 (26) = happyGoto action_18
action_108 (29) = happyGoto action_127
action_108 (30) = happyGoto action_73
action_108 (32) = happyGoto action_20
action_108 (33) = happyGoto action_21
action_108 (34) = happyGoto action_22
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (58) = happyShift action_126
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_40

action_111 (54) = happyShift action_125
action_111 _ = happyReduce_41

action_112 (52) = happyShift action_124
action_112 _ = happyFail (happyExpListPerState 112)

action_113 _ = happyReduce_18

action_114 (47) = happyShift action_37
action_114 (15) = happyGoto action_123
action_114 (24) = happyGoto action_93
action_114 _ = happyFail (happyExpListPerState 114)

action_115 _ = happyReduce_29

action_116 (40) = happyShift action_42
action_116 (41) = happyShift action_43
action_116 (42) = happyShift action_44
action_116 (44) = happyShift action_45
action_116 (46) = happyShift action_12
action_116 (47) = happyShift action_37
action_116 (55) = happyShift action_46
action_116 (13) = happyGoto action_122
action_116 (24) = happyGoto action_40
action_116 (27) = happyGoto action_41
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (40) = happyShift action_42
action_117 (41) = happyShift action_43
action_117 (42) = happyShift action_44
action_117 (44) = happyShift action_45
action_117 (46) = happyShift action_12
action_117 (47) = happyShift action_37
action_117 (53) = happyShift action_121
action_117 (55) = happyShift action_46
action_117 (13) = happyGoto action_120
action_117 (24) = happyGoto action_40
action_117 (27) = happyGoto action_41
action_117 _ = happyFail (happyExpListPerState 117)

action_118 _ = happyReduce_24

action_119 _ = happyReduce_25

action_120 (45) = happyShift action_132
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (40) = happyShift action_42
action_121 (41) = happyShift action_43
action_121 (42) = happyShift action_44
action_121 (44) = happyShift action_45
action_121 (46) = happyShift action_12
action_121 (47) = happyShift action_37
action_121 (55) = happyShift action_46
action_121 (13) = happyGoto action_131
action_121 (24) = happyGoto action_40
action_121 (27) = happyGoto action_41
action_121 _ = happyFail (happyExpListPerState 121)

action_122 _ = happyReduce_21

action_123 _ = happyReduce_31

action_124 (40) = happyShift action_42
action_124 (41) = happyShift action_43
action_124 (42) = happyShift action_44
action_124 (44) = happyShift action_45
action_124 (46) = happyShift action_12
action_124 (47) = happyShift action_37
action_124 (55) = happyShift action_46
action_124 (13) = happyGoto action_130
action_124 (24) = happyGoto action_40
action_124 (27) = happyGoto action_41
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (47) = happyShift action_67
action_125 (22) = happyGoto action_129
action_125 (23) = happyGoto action_111
action_125 (28) = happyGoto action_112
action_125 _ = happyFail (happyExpListPerState 125)

action_126 _ = happyReduce_38

action_127 (56) = happyShift action_128
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (40) = happyShift action_42
action_128 (41) = happyShift action_43
action_128 (42) = happyShift action_44
action_128 (44) = happyShift action_45
action_128 (46) = happyShift action_12
action_128 (47) = happyShift action_37
action_128 (55) = happyShift action_46
action_128 (13) = happyGoto action_134
action_128 (24) = happyGoto action_40
action_128 (27) = happyGoto action_41
action_128 _ = happyFail (happyExpListPerState 128)

action_129 _ = happyReduce_42

action_130 _ = happyReduce_43

action_131 _ = happyReduce_20

action_132 (40) = happyShift action_42
action_132 (41) = happyShift action_43
action_132 (42) = happyShift action_44
action_132 (44) = happyShift action_45
action_132 (46) = happyShift action_12
action_132 (47) = happyShift action_37
action_132 (55) = happyShift action_46
action_132 (13) = happyGoto action_133
action_132 (24) = happyGoto action_40
action_132 (27) = happyGoto action_41
action_132 _ = happyFail (happyExpListPerState 132)

action_133 _ = happyReduce_19

action_134 (45) = happyShift action_135
action_134 _ = happyFail (happyExpListPerState 134)

action_135 (40) = happyShift action_42
action_135 (41) = happyShift action_43
action_135 (42) = happyShift action_44
action_135 (44) = happyShift action_45
action_135 (46) = happyShift action_12
action_135 (47) = happyShift action_37
action_135 (55) = happyShift action_46
action_135 (13) = happyGoto action_136
action_135 (24) = happyGoto action_40
action_135 (27) = happyGoto action_41
action_135 _ = happyFail (happyExpListPerState 135)

action_136 _ = happyReduce_23

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
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Join happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 5 13 happyReduction_21
happyReduction_21 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Select happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_3  13 happyReduction_22
happyReduction_22 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Case happy_var_2 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happyReduce 9 13 happyReduction_23
happyReduction_23 ((HappyAbsSyn13  happy_var_9) `HappyStk`
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

happyReduce_24 = happyReduce 4 13 happyReduction_24
happyReduction_24 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (PutGas happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 4 13 happyReduction_25
happyReduction_25 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (GetGas happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_2  13 happyReduction_26
happyReduction_26 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn27  happy_var_1)
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
happyReduction_30 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  15 happyReduction_31
happyReduction_31 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
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

happyReduce_33 = happySpecReduce_1  17 happyReduction_33
happyReduction_33 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 ([happy_var_1]
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  17 happyReduction_34
happyReduction_34 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 : happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_2  18 happyReduction_35
happyReduction_35 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 ((happy_var_1, happy_var_2)
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_0  19 happyReduction_36
happyReduction_36  =  HappyAbsSyn19
		 (1
	)

happyReduce_37 = happySpecReduce_2  19 happyReduction_37
happyReduction_37 _
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  20 happyReduction_38
happyReduction_38 _
	(HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_0  21 happyReduction_39
happyReduction_39  =  HappyAbsSyn21
		 ([]
	)

happyReduce_40 = happySpecReduce_1  21 happyReduction_40
happyReduction_40 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  22 happyReduction_41
happyReduction_41 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 ([happy_var_1]
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  22 happyReduction_42
happyReduction_42 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1 : happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  23 happyReduction_43
happyReduction_43 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn23
		 ((happy_var_1, happy_var_3)
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  24 happyReduction_44
happyReduction_44 (HappyTerminal (happy_var_1@(Token _ (TokenLID _))))
	 =  HappyAbsSyn24
		 (Identifier (At $ getPos happy_var_1) (getId happy_var_1) :: ChannelName
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  25 happyReduction_45
happyReduction_45 (HappyTerminal (happy_var_1@(Token _ (TokenCID _))))
	 =  HappyAbsSyn25
		 (Identifier (At $ getPos happy_var_1) (getId happy_var_1) :: TypeName
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  26 happyReduction_46
happyReduction_46 (HappyTerminal (happy_var_1@(Token _ (TokenLID _))))
	 =  HappyAbsSyn26
		 (Identifier (At $ getPos happy_var_1) (getId happy_var_1) :: TypeName
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  27 happyReduction_47
happyReduction_47 (HappyTerminal (happy_var_1@(Token _ (TokenCID _))))
	 =  HappyAbsSyn27
		 (Identifier (At $ getPos happy_var_1) (getId happy_var_1) :: ProcessName
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  28 happyReduction_48
happyReduction_48 (HappyTerminal (happy_var_1@(Token _ (TokenLID _))))
	 =  HappyAbsSyn28
		 (Identifier (At $ getPos happy_var_1) (getId happy_var_1) :: Label
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  29 happyReduction_49
happyReduction_49 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn29
		 (Type happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_2  29 happyReduction_50
happyReduction_50 (HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn29
		 (Dual happy_var_2
	)
happyReduction_50 _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  30 happyReduction_51
happyReduction_51 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn30
		 (if happy_var_1 == 1 then One
           else error $ (show happy_var_1) ++ " is not a type"
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  30 happyReduction_52
happyReduction_52 _
	 =  HappyAbsSyn30
		 (Bot
	)

happyReduce_53 = happySpecReduce_1  30 happyReduction_53
happyReduction_53 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn30
		 (Var happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

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
happyReduction_59 (HappyAbsSyn35  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (With happy_var_2
	)
happyReduction_59 _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_2  30 happyReduction_60
happyReduction_60 (HappyAbsSyn35  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (Plus happy_var_2
	)
happyReduction_60 _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  30 happyReduction_61
happyReduction_61 (HappyAbsSyn30  happy_var_3)
	(HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (Put happy_var_2 happy_var_3
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  30 happyReduction_62
happyReduction_62 (HappyAbsSyn30  happy_var_3)
	(HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (Get happy_var_2 happy_var_3
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_0  31 happyReduction_63
happyReduction_63  =  HappyAbsSyn31
		 (Nothing
	)

happyReduce_64 = happySpecReduce_3  31 happyReduction_64
happyReduction_64 _
	(HappyAbsSyn33  happy_var_2)
	_
	 =  HappyAbsSyn31
		 (Just happy_var_2
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  32 happyReduction_65
happyReduction_65 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn32
		 (fromIntegral happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  32 happyReduction_66
happyReduction_66 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  33 happyReduction_67
happyReduction_67 (HappyTerminal (happy_var_1@(Token _ (TokenINT _))))
	 =  HappyAbsSyn33
		 (getInt happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  34 happyReduction_68
happyReduction_68 (HappyTerminal (happy_var_1@(Token _ (TokenFLOAT _))))
	 =  HappyAbsSyn34
		 (getFloat happy_var_1
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  35 happyReduction_69
happyReduction_69 _
	(HappyAbsSyn36  happy_var_2)
	_
	 =  HappyAbsSyn35
		 (happy_var_2
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_0  36 happyReduction_70
happyReduction_70  =  HappyAbsSyn36
		 ([]
	)

happyReduce_71 = happySpecReduce_1  36 happyReduction_71
happyReduction_71 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn36
		 (happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  37 happyReduction_72
happyReduction_72 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn37
		 ([happy_var_1]
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  37 happyReduction_73
happyReduction_73 (HappyAbsSyn37  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1 : happy_var_3
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_3  38 happyReduction_74
happyReduction_74 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn38
		 ((happy_var_1, happy_var_3)
	)
happyReduction_74 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexwrap(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Token _ TokenEOF -> action 71 71 tk (HappyState action) sts stk;
	Token _ TokenType -> cont 39;
	Token _ TokenWait -> cont 40;
	Token _ TokenClose -> cont 41;
	Token _ TokenCase -> cont 42;
	Token _ TokenFlip -> cont 43;
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
	Token _ TokenAmp -> cont 61;
	Token _ TokenPar -> cont 62;
	Token _ TokenBot -> cont 63;
	Token _ TokenTimes -> cont 64;
	Token _ TokenPlus -> cont 65;
	Token _ TokenPut -> cont 66;
	Token _ TokenGet -> cont 67;
	Token _ TokenQMark -> cont 68;
	Token _ TokenEMark -> cont 69;
	Token _ TokenDual -> cont 70;
	_ -> happyError' (tk, [])
	})

happyError_ explist 71 tk = happyError' (tk, explist)
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
