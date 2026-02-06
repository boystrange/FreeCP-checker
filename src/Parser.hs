{-# OPTIONS_GHC -w #-}
{-# OPTIONS -w #-}
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

-- |This module implements the parser for FreeCP scripts.
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

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32
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

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,256) ([0,0,1,0,0,32768,0,0,0,0,1,0,0,8192,0,0,0,16384,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,32768,0,0,0,0,0,0,0,256,0,0,0,32,0,0,0,0,0,0,0,1054,936,0,0,0,0,0,0,0,0,0,0,32,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3840,54274,1,0,0,4,0,0,0,0,0,0,0,1,0,0,8432,7488,0,0,4216,3744,0,0,4096,0,0,0,512,0,0,0,0,0,0,0,32,0,0,0,0,0,0,49152,64,0,0,0,0,0,0,0,1296,224,0,0,512,0,0,3072,4,0,0,3840,54274,1,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,1152,40,0,32768,263,234,0,49152,131,117,0,57344,32833,58,0,0,0,0,0,0,16384,1,0,0,40960,0,0,0,20481,0,0,0,0,0,0,8192,0,0,0,0,4,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,1024,0,0,0,256,1104,0,0,0,0,0,0,512,0,0,0,10384,1792,0,0,0,0,0,16384,4096,0,0,8192,32,0,0,4096,256,0,0,2048,0,0,0,1024,8,0,0,512,0,0,0,256,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,128,0,0,0,8,0,0,0,0,1,0,0,8192,0,0,0,0,0,0,0,128,0,0,0,2,0,0,0,2048,0,0,0,4,0,0,0,0,0,0,3840,54274,1,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,4216,3744,0,0,512,160,0,0,0,0,0,0,1024,0,0,0,2,0,0,0,0,0,0,49152,64,0,0,0,2,0,0,0,16,0,0,32768,0,0,0,0,32,0,0,0,0,0,0,32768,0,0,0,4096,0,0,0,16576,0,0,0,8288,0,0,0,0,0,0,0,0,0,0,0,1036,0,0,0,4,0,0,0,0,0,0,32768,129,0,0,49152,64,0,0,24576,32,0,0,0,0,0,0,0,0,0,0,0,4,0,0,1536,2,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2072,0,0,0,0,4,0,0,518,0,0,0,512,0,0,0,0,0,0,0,128,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Program","TypeDefList","TypeDef","ProcessDefList","ProcessDef","Parameters","ParameterList","ParameterNeList","Parameter","Process","Names","NameNeList","Cases","CaseList","CaseNeList","Case","ChannelName","TypeName","PolyName","ProcessName","Label","Type","TypeExpr","Num","Int","Branches","BranchList","BranchNeList","Branch","TYPE","SKIP","CID","LID","INT","'='","'.'","':'","';'","','","'('","')'","'{'","'}'","'['","']'","'\10216'","'\10217'","'|'","'&'","'\8523'","'\8869'","'\8855'","'\8853'","'?'","'!'","'^'","'\9657'","'\9667'","'\8596'","%eof"]
        bit_start = st Prelude.* 63
        bit_end = (st Prelude.+ 1) Prelude.* 63
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..62]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (33) = happyShift action_4
action_0 (4) = happyGoto action_5
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 _ = happyReduce_2

action_1 (33) = happyShift action_4
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (35) = happyShift action_12
action_2 (7) = happyGoto action_9
action_2 (8) = happyGoto action_10
action_2 (23) = happyGoto action_11
action_2 _ = happyReduce_5

action_3 (33) = happyShift action_4
action_3 (5) = happyGoto action_8
action_3 (6) = happyGoto action_3
action_3 _ = happyReduce_2

action_4 (35) = happyShift action_7
action_4 (21) = happyGoto action_6
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (63) = happyAccept
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (38) = happyShift action_16
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_39

action_8 _ = happyReduce_3

action_9 _ = happyReduce_1

action_10 (35) = happyShift action_12
action_10 (7) = happyGoto action_15
action_10 (8) = happyGoto action_10
action_10 (23) = happyGoto action_11
action_10 _ = happyReduce_5

action_11 (43) = happyShift action_14
action_11 (9) = happyGoto action_13
action_11 _ = happyReduce_8

action_12 _ = happyReduce_41

action_13 (38) = happyShift action_36
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (36) = happyShift action_35
action_14 (10) = happyGoto action_31
action_14 (11) = happyGoto action_32
action_14 (12) = happyGoto action_33
action_14 (20) = happyGoto action_34
action_14 _ = happyReduce_10

action_15 _ = happyReduce_6

action_16 (34) = happyShift action_22
action_16 (35) = happyShift action_7
action_16 (36) = happyShift action_23
action_16 (37) = happyShift action_24
action_16 (43) = happyShift action_25
action_16 (52) = happyShift action_26
action_16 (54) = happyShift action_27
action_16 (56) = happyShift action_28
action_16 (57) = happyShift action_29
action_16 (58) = happyShift action_30
action_16 (21) = happyGoto action_17
action_16 (22) = happyGoto action_18
action_16 (25) = happyGoto action_19
action_16 (27) = happyGoto action_20
action_16 (28) = happyGoto action_21
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_45

action_18 (59) = happyShift action_53
action_18 _ = happyReduce_46

action_19 (41) = happyShift action_50
action_19 (53) = happyShift action_51
action_19 (55) = happyShift action_52
action_19 _ = happyReduce_4

action_20 _ = happyReduce_43

action_21 _ = happyReduce_59

action_22 _ = happyReduce_53

action_23 _ = happyReduce_40

action_24 _ = happyReduce_60

action_25 (34) = happyShift action_22
action_25 (35) = happyShift action_7
action_25 (36) = happyShift action_23
action_25 (37) = happyShift action_24
action_25 (43) = happyShift action_25
action_25 (52) = happyShift action_26
action_25 (54) = happyShift action_27
action_25 (56) = happyShift action_28
action_25 (57) = happyShift action_29
action_25 (58) = happyShift action_30
action_25 (21) = happyGoto action_17
action_25 (22) = happyGoto action_18
action_25 (25) = happyGoto action_49
action_25 (27) = happyGoto action_20
action_25 (28) = happyGoto action_21
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (45) = happyShift action_47
action_26 (29) = happyGoto action_48
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_44

action_28 (45) = happyShift action_47
action_28 (29) = happyGoto action_46
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (34) = happyShift action_22
action_29 (35) = happyShift action_7
action_29 (36) = happyShift action_23
action_29 (37) = happyShift action_24
action_29 (43) = happyShift action_25
action_29 (52) = happyShift action_26
action_29 (54) = happyShift action_27
action_29 (56) = happyShift action_28
action_29 (57) = happyShift action_29
action_29 (58) = happyShift action_30
action_29 (21) = happyGoto action_17
action_29 (22) = happyGoto action_18
action_29 (25) = happyGoto action_45
action_29 (27) = happyGoto action_20
action_29 (28) = happyGoto action_21
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (34) = happyShift action_22
action_30 (35) = happyShift action_7
action_30 (36) = happyShift action_23
action_30 (37) = happyShift action_24
action_30 (43) = happyShift action_25
action_30 (52) = happyShift action_26
action_30 (54) = happyShift action_27
action_30 (56) = happyShift action_28
action_30 (57) = happyShift action_29
action_30 (58) = happyShift action_30
action_30 (21) = happyGoto action_17
action_30 (22) = happyGoto action_18
action_30 (25) = happyGoto action_44
action_30 (27) = happyGoto action_20
action_30 (28) = happyGoto action_21
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (44) = happyShift action_43
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (42) = happyShift action_42
action_32 _ = happyReduce_11

action_33 _ = happyReduce_12

action_34 (40) = happyShift action_41
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_38

action_36 (35) = happyShift action_12
action_36 (36) = happyShift action_35
action_36 (43) = happyShift action_40
action_36 (13) = happyGoto action_37
action_36 (20) = happyGoto action_38
action_36 (23) = happyGoto action_39
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_7

action_38 (43) = happyShift action_70
action_38 (47) = happyShift action_71
action_38 (49) = happyShift action_72
action_38 (60) = happyShift action_73
action_38 (61) = happyShift action_74
action_38 (62) = happyShift action_75
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (49) = happyShift action_69
action_39 (14) = happyGoto action_68
action_39 _ = happyReduce_27

action_40 (35) = happyShift action_12
action_40 (36) = happyShift action_35
action_40 (43) = happyShift action_40
action_40 (13) = happyGoto action_66
action_40 (20) = happyGoto action_67
action_40 (23) = happyGoto action_39
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (34) = happyShift action_22
action_41 (35) = happyShift action_7
action_41 (36) = happyShift action_23
action_41 (37) = happyShift action_24
action_41 (43) = happyShift action_25
action_41 (52) = happyShift action_26
action_41 (54) = happyShift action_27
action_41 (56) = happyShift action_28
action_41 (57) = happyShift action_29
action_41 (58) = happyShift action_30
action_41 (21) = happyGoto action_17
action_41 (22) = happyGoto action_18
action_41 (25) = happyGoto action_64
action_41 (26) = happyGoto action_65
action_41 (27) = happyGoto action_20
action_41 (28) = happyGoto action_21
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (36) = happyShift action_35
action_42 (11) = happyGoto action_63
action_42 (12) = happyGoto action_33
action_42 (20) = happyGoto action_34
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_9

action_44 _ = happyReduce_51

action_45 _ = happyReduce_52

action_46 _ = happyReduce_56

action_47 (36) = happyShift action_62
action_47 (24) = happyGoto action_58
action_47 (30) = happyGoto action_59
action_47 (31) = happyGoto action_60
action_47 (32) = happyGoto action_61
action_47 _ = happyReduce_62

action_48 _ = happyReduce_55

action_49 (41) = happyShift action_50
action_49 (44) = happyShift action_57
action_49 (53) = happyShift action_51
action_49 (55) = happyShift action_52
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (34) = happyShift action_22
action_50 (35) = happyShift action_7
action_50 (36) = happyShift action_23
action_50 (37) = happyShift action_24
action_50 (43) = happyShift action_25
action_50 (52) = happyShift action_26
action_50 (54) = happyShift action_27
action_50 (56) = happyShift action_28
action_50 (57) = happyShift action_29
action_50 (58) = happyShift action_30
action_50 (21) = happyGoto action_17
action_50 (22) = happyGoto action_18
action_50 (25) = happyGoto action_56
action_50 (27) = happyGoto action_20
action_50 (28) = happyGoto action_21
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (34) = happyShift action_22
action_51 (35) = happyShift action_7
action_51 (36) = happyShift action_23
action_51 (37) = happyShift action_24
action_51 (43) = happyShift action_25
action_51 (52) = happyShift action_26
action_51 (54) = happyShift action_27
action_51 (56) = happyShift action_28
action_51 (57) = happyShift action_29
action_51 (58) = happyShift action_30
action_51 (21) = happyGoto action_17
action_51 (22) = happyGoto action_18
action_51 (25) = happyGoto action_55
action_51 (27) = happyGoto action_20
action_51 (28) = happyGoto action_21
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (34) = happyShift action_22
action_52 (35) = happyShift action_7
action_52 (36) = happyShift action_23
action_52 (37) = happyShift action_24
action_52 (43) = happyShift action_25
action_52 (52) = happyShift action_26
action_52 (54) = happyShift action_27
action_52 (56) = happyShift action_28
action_52 (57) = happyShift action_29
action_52 (58) = happyShift action_30
action_52 (21) = happyGoto action_17
action_52 (22) = happyGoto action_18
action_52 (25) = happyGoto action_54
action_52 (27) = happyGoto action_20
action_52 (28) = happyGoto action_21
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_47

action_54 (53) = happyShift action_51
action_54 (55) = happyShift action_52
action_54 _ = happyReduce_49

action_55 (53) = happyShift action_51
action_55 (55) = happyShift action_52
action_55 _ = happyReduce_50

action_56 (41) = happyShift action_50
action_56 (53) = happyShift action_51
action_56 (55) = happyShift action_52
action_56 _ = happyReduce_54

action_57 _ = happyReduce_48

action_58 (40) = happyShift action_94
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (46) = happyShift action_93
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_63

action_61 (42) = happyShift action_92
action_61 _ = happyReduce_64

action_62 _ = happyReduce_42

action_63 (42) = happyShift action_42
action_63 _ = happyReduce_13

action_64 (41) = happyShift action_50
action_64 (53) = happyShift action_51
action_64 (55) = happyShift action_52
action_64 (59) = happyShift action_91
action_64 _ = happyReduce_57

action_65 _ = happyReduce_14

action_66 (44) = happyShift action_90
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (40) = happyShift action_89
action_67 (43) = happyShift action_70
action_67 (47) = happyShift action_71
action_67 (49) = happyShift action_72
action_67 (60) = happyShift action_73
action_67 (61) = happyShift action_74
action_67 (62) = happyShift action_75
action_67 _ = happyFail (happyExpListPerState 67)

action_68 _ = happyReduce_26

action_69 (36) = happyShift action_35
action_69 (50) = happyShift action_88
action_69 (15) = happyGoto action_86
action_69 (20) = happyGoto action_87
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (36) = happyShift action_35
action_70 (44) = happyShift action_85
action_70 (20) = happyGoto action_84
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (36) = happyShift action_35
action_71 (48) = happyShift action_83
action_71 (20) = happyGoto action_82
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (36) = happyShift action_35
action_72 (20) = happyGoto action_81
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (36) = happyShift action_62
action_73 (45) = happyShift action_80
action_73 (16) = happyGoto action_78
action_73 (24) = happyGoto action_79
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (36) = happyShift action_62
action_74 (24) = happyGoto action_77
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (36) = happyShift action_35
action_75 (20) = happyGoto action_76
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_16

action_77 (39) = happyShift action_109
action_77 _ = happyFail (happyExpListPerState 77)

action_78 _ = happyReduce_23

action_79 (39) = happyShift action_108
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (36) = happyShift action_62
action_80 (17) = happyGoto action_104
action_80 (18) = happyGoto action_105
action_80 (19) = happyGoto action_106
action_80 (24) = happyGoto action_107
action_80 _ = happyReduce_33

action_81 (50) = happyShift action_103
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (48) = happyShift action_102
action_82 _ = happyFail (happyExpListPerState 82)

action_83 _ = happyReduce_17

action_84 (44) = happyShift action_101
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (39) = happyShift action_100
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (50) = happyShift action_99
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (42) = happyShift action_98
action_87 _ = happyReduce_30

action_88 _ = happyReduce_28

action_89 (34) = happyShift action_22
action_89 (35) = happyShift action_7
action_89 (36) = happyShift action_23
action_89 (37) = happyShift action_24
action_89 (43) = happyShift action_25
action_89 (52) = happyShift action_26
action_89 (54) = happyShift action_27
action_89 (56) = happyShift action_28
action_89 (57) = happyShift action_29
action_89 (58) = happyShift action_30
action_89 (21) = happyGoto action_17
action_89 (22) = happyGoto action_18
action_89 (25) = happyGoto action_64
action_89 (26) = happyGoto action_97
action_89 (27) = happyGoto action_20
action_89 (28) = happyGoto action_21
action_89 _ = happyFail (happyExpListPerState 89)

action_90 _ = happyReduce_15

action_91 _ = happyReduce_58

action_92 (36) = happyShift action_62
action_92 (24) = happyGoto action_58
action_92 (31) = happyGoto action_96
action_92 (32) = happyGoto action_61
action_92 _ = happyFail (happyExpListPerState 92)

action_93 _ = happyReduce_61

action_94 (34) = happyShift action_22
action_94 (35) = happyShift action_7
action_94 (36) = happyShift action_23
action_94 (37) = happyShift action_24
action_94 (43) = happyShift action_25
action_94 (52) = happyShift action_26
action_94 (54) = happyShift action_27
action_94 (56) = happyShift action_28
action_94 (57) = happyShift action_29
action_94 (58) = happyShift action_30
action_94 (21) = happyGoto action_17
action_94 (22) = happyGoto action_18
action_94 (25) = happyGoto action_95
action_94 (27) = happyGoto action_20
action_94 (28) = happyGoto action_21
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (41) = happyShift action_50
action_95 (53) = happyShift action_51
action_95 (55) = happyShift action_52
action_95 _ = happyReduce_66

action_96 _ = happyReduce_65

action_97 (44) = happyShift action_120
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (36) = happyShift action_35
action_98 (15) = happyGoto action_119
action_98 (20) = happyGoto action_87
action_98 _ = happyFail (happyExpListPerState 98)

action_99 _ = happyReduce_29

action_100 (35) = happyShift action_12
action_100 (36) = happyShift action_35
action_100 (43) = happyShift action_40
action_100 (13) = happyGoto action_118
action_100 (20) = happyGoto action_38
action_100 (23) = happyGoto action_39
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (39) = happyShift action_117
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (43) = happyShift action_116
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (39) = happyShift action_115
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (46) = happyShift action_114
action_104 _ = happyFail (happyExpListPerState 104)

action_105 _ = happyReduce_34

action_106 (42) = happyShift action_113
action_106 _ = happyReduce_35

action_107 (40) = happyShift action_112
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (35) = happyShift action_12
action_108 (36) = happyShift action_35
action_108 (43) = happyShift action_40
action_108 (13) = happyGoto action_111
action_108 (20) = happyGoto action_38
action_108 (23) = happyGoto action_39
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (35) = happyShift action_12
action_109 (36) = happyShift action_35
action_109 (43) = happyShift action_40
action_109 (13) = happyGoto action_110
action_109 (20) = happyGoto action_38
action_109 (23) = happyGoto action_39
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_22

action_111 _ = happyReduce_24

action_112 (35) = happyShift action_12
action_112 (36) = happyShift action_35
action_112 (43) = happyShift action_40
action_112 (13) = happyGoto action_126
action_112 (20) = happyGoto action_38
action_112 (23) = happyGoto action_39
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (36) = happyShift action_62
action_113 (18) = happyGoto action_125
action_113 (19) = happyGoto action_106
action_113 (24) = happyGoto action_107
action_113 _ = happyFail (happyExpListPerState 113)

action_114 _ = happyReduce_32

action_115 (35) = happyShift action_12
action_115 (36) = happyShift action_35
action_115 (43) = happyShift action_40
action_115 (13) = happyGoto action_124
action_115 (20) = happyGoto action_38
action_115 (23) = happyGoto action_39
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (35) = happyShift action_12
action_116 (36) = happyShift action_35
action_116 (43) = happyShift action_40
action_116 (13) = happyGoto action_123
action_116 (20) = happyGoto action_38
action_116 (23) = happyGoto action_39
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (35) = happyShift action_12
action_117 (36) = happyShift action_35
action_117 (43) = happyShift action_40
action_117 (13) = happyGoto action_122
action_117 (20) = happyGoto action_38
action_117 (23) = happyGoto action_39
action_117 _ = happyFail (happyExpListPerState 117)

action_118 _ = happyReduce_18

action_119 _ = happyReduce_31

action_120 (43) = happyShift action_121
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (35) = happyShift action_12
action_121 (36) = happyShift action_35
action_121 (43) = happyShift action_40
action_121 (13) = happyGoto action_128
action_121 (20) = happyGoto action_38
action_121 (23) = happyGoto action_39
action_121 _ = happyFail (happyExpListPerState 121)

action_122 _ = happyReduce_21

action_123 (51) = happyShift action_127
action_123 _ = happyFail (happyExpListPerState 123)

action_124 _ = happyReduce_20

action_125 _ = happyReduce_36

action_126 _ = happyReduce_37

action_127 (35) = happyShift action_12
action_127 (36) = happyShift action_35
action_127 (43) = happyShift action_40
action_127 (13) = happyGoto action_130
action_127 (20) = happyGoto action_38
action_127 (23) = happyGoto action_39
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (51) = happyShift action_129
action_128 _ = happyFail (happyExpListPerState 128)

action_129 (35) = happyShift action_12
action_129 (36) = happyShift action_35
action_129 (43) = happyShift action_40
action_129 (13) = happyGoto action_132
action_129 (20) = happyGoto action_38
action_129 (23) = happyGoto action_39
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (44) = happyShift action_131
action_130 _ = happyFail (happyExpListPerState 130)

action_131 _ = happyReduce_19

action_132 (44) = happyShift action_133
action_132 _ = happyFail (happyExpListPerState 132)

action_133 _ = happyReduce_25

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
happyReduction_4 ((HappyAbsSyn25  happy_var_4) `HappyStk`
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
happyReduction_14 (HappyAbsSyn26  happy_var_3)
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

happyReduce_19 = happyReduce 9 13 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Fork happy_var_1 happy_var_3 happy_var_6 happy_var_8
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

happyReduce_25 = happyReduce 10 13 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Cut happy_var_2 happy_var_4 happy_var_7 happy_var_9
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
happyReduction_43 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn25
		 (if happy_var_1 == 1 then One
           else error $ (show happy_var_1) ++ " is not a type"
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  25 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn25
		 (Bot
	)

happyReduce_45 = happySpecReduce_1  25 happyReduction_45
happyReduction_45 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn25
		 (Inv happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  25 happyReduction_46
happyReduction_46 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn25
		 (Var False happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_2  25 happyReduction_47
happyReduction_47 _
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn25
		 (Var True happy_var_1
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  25 happyReduction_48
happyReduction_48 _
	(HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (happy_var_2
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  25 happyReduction_49
happyReduction_49 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn25
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  25 happyReduction_50
happyReduction_50 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn25
		 (Par happy_var_1 happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_2  25 happyReduction_51
happyReduction_51 (HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (Mul happy_var_2 Skip
	)
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_2  25 happyReduction_52
happyReduction_52 (HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (Par happy_var_2 Skip
	)
happyReduction_52 _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  25 happyReduction_53
happyReduction_53 _
	 =  HappyAbsSyn25
		 (Skip
	)

happyReduce_54 = happySpecReduce_3  25 happyReduction_54
happyReduction_54 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn25
		 (Seq happy_var_1 happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_2  25 happyReduction_55
happyReduction_55 (HappyAbsSyn29  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (With happy_var_2
	)
happyReduction_55 _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_2  25 happyReduction_56
happyReduction_56 (HappyAbsSyn29  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (Plus happy_var_2
	)
happyReduction_56 _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  26 happyReduction_57
happyReduction_57 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (Copy happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_2  26 happyReduction_58
happyReduction_58 _
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn26
		 (Dual happy_var_1
	)
happyReduction_58 _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  27 happyReduction_59
happyReduction_59 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn27
		 (fromIntegral happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  28 happyReduction_60
happyReduction_60 (HappyTerminal (happy_var_1@(Token _ (TokenINT _))))
	 =  HappyAbsSyn28
		 (getInt happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  29 happyReduction_61
happyReduction_61 _
	(HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn29
		 (happy_var_2
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_0  30 happyReduction_62
happyReduction_62  =  HappyAbsSyn30
		 ([]
	)

happyReduce_63 = happySpecReduce_1  30 happyReduction_63
happyReduction_63 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  31 happyReduction_64
happyReduction_64 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn31
		 ([happy_var_1]
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  31 happyReduction_65
happyReduction_65 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1 : happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  32 happyReduction_66
happyReduction_66 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn32
		 ((happy_var_1, happy_var_3)
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexwrap(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Token _ TokenEOF -> action 63 63 tk (HappyState action) sts stk;
	Token _ TokenType -> cont 33;
	Token _ TokenSkip -> cont 34;
	happy_dollar_dollar@(Token _ (TokenCID _)) -> cont 35;
	happy_dollar_dollar@(Token _ (TokenLID _)) -> cont 36;
	happy_dollar_dollar@(Token _ (TokenINT _)) -> cont 37;
	Token _ TokenEQ -> cont 38;
	Token _ TokenDot -> cont 39;
	Token _ TokenColon -> cont 40;
	Token _ TokenSemiColon -> cont 41;
	Token _ TokenComma -> cont 42;
	Token _ TokenLParen -> cont 43;
	Token _ TokenRParen -> cont 44;
	Token _ TokenLBrace -> cont 45;
	Token _ TokenRBrace -> cont 46;
	Token _ TokenLBrack -> cont 47;
	Token _ TokenRBrack -> cont 48;
	Token _ TokenLAngle -> cont 49;
	Token _ TokenRAngle -> cont 50;
	Token _ TokenBar -> cont 51;
	Token _ TokenAmp -> cont 52;
	Token _ TokenPar -> cont 53;
	Token _ TokenBot -> cont 54;
	Token _ TokenTimes -> cont 55;
	Token _ TokenPlus -> cont 56;
	Token _ TokenQMark -> cont 57;
	Token _ TokenEMark -> cont 58;
	Token _ TokenDual -> cont 59;
	Token _ TokenRTriangle -> cont 60;
	Token _ TokenLTriangle -> cont 61;
	Token _ TokenLRArrow -> cont 62;
	_ -> happyError' (tk, [])
	})

happyError_ explist 63 tk = happyError' (tk, explist)
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

parseProcess :: FilePath -> String -> Either String ([TypeDef], [ProcessDefS])
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
