--------------------------------------------------------------------------------
--
-- Copyright (C) 2006 
--
-- This program is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the Free
-- Software Foundation; either version 2 of the License, or (at your option)
-- any later version. This program is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- See the GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License along
-- with this program; if not, write to the Free Software Foundation, Inc.,
-- 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
--
--------------------------------------------------------------------------------

-- parser produced by Happy Version 1.13

module CCHRParser where
import Char
import List
import CCHRLexer

data HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21
	= HappyTerminal Token
	| HappyErrorToken Int
	| HappyAbsSyn4 ([Expr])
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

action_0 (22) = happyShift action_3
action_0 (23) = happyShift action_4
action_0 (24) = happyShift action_5
action_0 (25) = happyShift action_6
action_0 (37) = happyShift action_7
action_0 (4) = happyGoto action_8
action_0 (5) = happyGoto action_9
action_0 _ = happyFail

action_1 (22) = happyShift action_3
action_1 (23) = happyShift action_4
action_1 (24) = happyShift action_5
action_1 (25) = happyShift action_6
action_1 (37) = happyShift action_7
action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 (26) = happyShift action_24
action_2 _ = happyFail

action_3 (27) = happyShift action_20
action_3 (28) = happyShift action_21
action_3 (29) = happyShift action_22
action_3 (41) = happyShift action_23
action_3 (10) = happyGoto action_18
action_3 (11) = happyGoto action_19
action_3 _ = happyFail

action_4 (28) = happyShift action_17
action_4 (14) = happyGoto action_15
action_4 (15) = happyGoto action_16
action_4 _ = happyFail

action_5 (27) = happyShift action_13
action_5 (28) = happyShift action_14
action_5 _ = happyFail

action_6 (28) = happyShift action_12
action_6 _ = happyFail

action_7 (31) = happyShift action_11
action_7 _ = happyFail

action_8 (51) = happyAccept
action_8 _ = happyFail

action_9 (26) = happyShift action_10
action_9 _ = happyFail

action_10 (22) = happyShift action_3
action_10 (23) = happyShift action_4
action_10 (24) = happyShift action_5
action_10 (25) = happyShift action_6
action_10 (37) = happyShift action_7
action_10 (4) = happyGoto action_37
action_10 (5) = happyGoto action_9
action_10 _ = happyReduce_1

action_11 (38) = happyShift action_36
action_11 _ = happyFail

action_12 (33) = happyShift action_35
action_12 _ = happyFail

action_13 _ = happyReduce_3

action_14 _ = happyReduce_4

action_15 (35) = happyShift action_33
action_15 (36) = happyShift action_34
action_15 _ = happyFail

action_16 (32) = happyShift action_32
action_16 _ = happyReduce_33

action_17 (43) = happyShift action_31
action_17 _ = happyReduce_35

action_18 (33) = happyShift action_29
action_18 (48) = happyShift action_30
action_18 _ = happyFail

action_19 (27) = happyShift action_20
action_19 (28) = happyShift action_21
action_19 (29) = happyShift action_22
action_19 (41) = happyShift action_23
action_19 (10) = happyGoto action_28
action_19 (11) = happyGoto action_19
action_19 _ = happyReduce_26

action_20 _ = happyReduce_22

action_21 _ = happyReduce_28

action_22 (27) = happyShift action_20
action_22 (28) = happyShift action_21
action_22 (29) = happyShift action_22
action_22 (41) = happyShift action_23
action_22 (10) = happyGoto action_26
action_22 (11) = happyGoto action_19
action_22 (12) = happyGoto action_27
action_22 _ = happyFail

action_23 (27) = happyShift action_20
action_23 (28) = happyShift action_21
action_23 (29) = happyShift action_22
action_23 (41) = happyShift action_23
action_23 (10) = happyGoto action_25
action_23 (11) = happyGoto action_19
action_23 _ = happyFail

action_24 _ = happyFail

action_25 (42) = happyShift action_62
action_25 (48) = happyShift action_30
action_25 _ = happyFail

action_26 (30) = happyShift action_60
action_26 (32) = happyShift action_61
action_26 (48) = happyShift action_30
action_26 _ = happyFail

action_27 (30) = happyShift action_59
action_27 _ = happyFail

action_28 (48) = happyShift action_30
action_28 _ = happyReduce_29

action_29 (28) = happyShift action_58
action_29 (6) = happyGoto action_57
action_29 _ = happyFail

action_30 (27) = happyShift action_20
action_30 (28) = happyShift action_21
action_30 (29) = happyShift action_22
action_30 (41) = happyShift action_23
action_30 (10) = happyGoto action_56
action_30 (11) = happyGoto action_19
action_30 _ = happyFail

action_31 (27) = happyShift action_50
action_31 (28) = happyShift action_51
action_31 (29) = happyShift action_52
action_31 (41) = happyShift action_53
action_31 (44) = happyShift action_54
action_31 (49) = happyShift action_55
action_31 (16) = happyGoto action_47
action_31 (17) = happyGoto action_48
action_31 (20) = happyGoto action_49
action_31 _ = happyFail

action_32 (28) = happyShift action_17
action_32 (14) = happyGoto action_46
action_32 (15) = happyGoto action_16
action_32 _ = happyFail

action_33 (28) = happyShift action_45
action_33 (39) = happyShift action_42
action_33 (40) = happyShift action_43
action_33 (21) = happyGoto action_44
action_33 _ = happyFail

action_34 (28) = happyShift action_41
action_34 (39) = happyShift action_42
action_34 (40) = happyShift action_43
action_34 (21) = happyGoto action_40
action_34 _ = happyFail

action_35 (28) = happyShift action_39
action_35 (8) = happyGoto action_38
action_35 _ = happyFail

action_36 _ = happyReduce_5

action_37 _ = happyReduce_2

action_38 _ = happyReduce_6

action_39 (34) = happyShift action_79
action_39 (43) = happyShift action_80
action_39 _ = happyReduce_16

action_40 _ = happyReduce_11

action_41 (34) = happyShift action_78
action_41 _ = happyReduce_55

action_42 _ = happyReduce_56

action_43 _ = happyReduce_57

action_44 _ = happyReduce_9

action_45 (34) = happyShift action_77
action_45 _ = happyReduce_55

action_46 _ = happyReduce_34

action_47 (44) = happyShift action_76
action_47 _ = happyFail

action_48 (45) = happyShift action_74
action_48 (46) = happyShift action_75
action_48 _ = happyReduce_38

action_49 (27) = happyShift action_50
action_49 (28) = happyShift action_51
action_49 (29) = happyShift action_52
action_49 (41) = happyShift action_53
action_49 (49) = happyShift action_55
action_49 (17) = happyGoto action_73
action_49 (20) = happyGoto action_49
action_49 _ = happyReduce_46

action_50 (47) = happyShift action_72
action_50 _ = happyReduce_40

action_51 _ = happyReduce_53

action_52 (27) = happyShift action_50
action_52 (28) = happyShift action_51
action_52 (29) = happyShift action_52
action_52 (41) = happyShift action_53
action_52 (49) = happyShift action_55
action_52 (17) = happyGoto action_70
action_52 (18) = happyGoto action_71
action_52 (20) = happyGoto action_49
action_52 _ = happyFail

action_53 (27) = happyShift action_50
action_53 (28) = happyShift action_51
action_53 (29) = happyShift action_52
action_53 (41) = happyShift action_53
action_53 (42) = happyShift action_69
action_53 (49) = happyShift action_55
action_53 (17) = happyGoto action_67
action_53 (18) = happyGoto action_68
action_53 (20) = happyGoto action_49
action_53 _ = happyFail

action_54 _ = happyReduce_36

action_55 _ = happyReduce_49

action_56 (48) = happyShift action_30
action_56 _ = happyReduce_27

action_57 _ = happyReduce_7

action_58 (27) = happyShift action_20
action_58 (28) = happyShift action_21
action_58 (29) = happyShift action_22
action_58 (41) = happyShift action_23
action_58 (7) = happyGoto action_65
action_58 (10) = happyGoto action_66
action_58 (11) = happyGoto action_19
action_58 _ = happyReduce_14

action_59 _ = happyReduce_24

action_60 _ = happyReduce_25

action_61 (27) = happyShift action_20
action_61 (28) = happyShift action_21
action_61 (29) = happyShift action_22
action_61 (41) = happyShift action_23
action_61 (10) = happyGoto action_63
action_61 (11) = happyGoto action_19
action_61 (13) = happyGoto action_64
action_61 _ = happyFail

action_62 _ = happyReduce_23

action_63 (32) = happyShift action_96
action_63 (48) = happyShift action_30
action_63 _ = happyReduce_31

action_64 _ = happyReduce_30

action_65 (34) = happyShift action_95
action_65 _ = happyReduce_12

action_66 (48) = happyShift action_30
action_66 _ = happyReduce_15

action_67 (32) = happyShift action_92
action_67 (42) = happyShift action_94
action_67 (46) = happyShift action_75
action_67 _ = happyFail

action_68 (42) = happyShift action_93
action_68 _ = happyFail

action_69 _ = happyReduce_44

action_70 (30) = happyShift action_91
action_70 (32) = happyShift action_92
action_70 (46) = happyShift action_75
action_70 _ = happyFail

action_71 (30) = happyShift action_90
action_71 _ = happyFail

action_72 (27) = happyShift action_50
action_72 (28) = happyShift action_51
action_72 (29) = happyShift action_52
action_72 (41) = happyShift action_53
action_72 (49) = happyShift action_55
action_72 (17) = happyGoto action_89
action_72 (20) = happyGoto action_49
action_72 _ = happyFail

action_73 (46) = happyShift action_75
action_73 _ = happyReduce_54

action_74 (27) = happyShift action_50
action_74 (28) = happyShift action_51
action_74 (29) = happyShift action_52
action_74 (41) = happyShift action_53
action_74 (49) = happyShift action_55
action_74 (16) = happyGoto action_88
action_74 (17) = happyGoto action_48
action_74 (20) = happyGoto action_49
action_74 _ = happyFail

action_75 (27) = happyShift action_50
action_75 (28) = happyShift action_51
action_75 (29) = happyShift action_52
action_75 (41) = happyShift action_53
action_75 (49) = happyShift action_55
action_75 (17) = happyGoto action_87
action_75 (20) = happyGoto action_49
action_75 _ = happyFail

action_76 _ = happyReduce_37

action_77 (28) = happyShift action_85
action_77 (39) = happyShift action_42
action_77 (40) = happyShift action_43
action_77 (21) = happyGoto action_86
action_77 _ = happyFail

action_78 (28) = happyShift action_85
action_78 (39) = happyShift action_42
action_78 (40) = happyShift action_43
action_78 (21) = happyGoto action_84
action_78 _ = happyFail

action_79 (28) = happyShift action_39
action_79 (8) = happyGoto action_83
action_79 _ = happyFail

action_80 (27) = happyShift action_20
action_80 (28) = happyShift action_21
action_80 (29) = happyShift action_22
action_80 (41) = happyShift action_23
action_80 (9) = happyGoto action_81
action_80 (10) = happyGoto action_82
action_80 (11) = happyGoto action_19
action_80 _ = happyFail

action_81 (44) = happyShift action_102
action_81 _ = happyFail

action_82 (45) = happyShift action_101
action_82 (48) = happyShift action_30
action_82 _ = happyReduce_20

action_83 _ = happyReduce_18

action_84 _ = happyReduce_10

action_85 _ = happyReduce_55

action_86 _ = happyReduce_8

action_87 (46) = happyShift action_75
action_87 _ = happyReduce_41

action_88 _ = happyReduce_39

action_89 (46) = happyShift action_75
action_89 _ = happyReduce_47

action_90 _ = happyReduce_45

action_91 _ = happyReduce_48

action_92 (27) = happyShift action_50
action_92 (28) = happyShift action_51
action_92 (29) = happyShift action_52
action_92 (41) = happyShift action_53
action_92 (49) = happyShift action_55
action_92 (17) = happyGoto action_99
action_92 (19) = happyGoto action_100
action_92 (20) = happyGoto action_49
action_92 _ = happyFail

action_93 _ = happyReduce_42

action_94 _ = happyReduce_43

action_95 (28) = happyShift action_58
action_95 (6) = happyGoto action_98
action_95 _ = happyFail

action_96 (27) = happyShift action_20
action_96 (28) = happyShift action_21
action_96 (29) = happyShift action_22
action_96 (41) = happyShift action_23
action_96 (10) = happyGoto action_63
action_96 (11) = happyGoto action_19
action_96 (13) = happyGoto action_97
action_96 _ = happyFail

action_97 _ = happyReduce_32

action_98 _ = happyReduce_13

action_99 (32) = happyShift action_105
action_99 (46) = happyShift action_75
action_99 _ = happyReduce_51

action_100 _ = happyReduce_50

action_101 (27) = happyShift action_20
action_101 (28) = happyShift action_21
action_101 (29) = happyShift action_22
action_101 (41) = happyShift action_23
action_101 (9) = happyGoto action_104
action_101 (10) = happyGoto action_82
action_101 (11) = happyGoto action_19
action_101 _ = happyFail

action_102 (34) = happyShift action_103
action_102 _ = happyReduce_17

action_103 (28) = happyShift action_39
action_103 (8) = happyGoto action_107
action_103 _ = happyFail

action_104 _ = happyReduce_21

action_105 (27) = happyShift action_50
action_105 (28) = happyShift action_51
action_105 (29) = happyShift action_52
action_105 (41) = happyShift action_53
action_105 (49) = happyShift action_55
action_105 (17) = happyGoto action_99
action_105 (19) = happyGoto action_106
action_105 (20) = happyGoto action_49
action_105 _ = happyFail

action_106 _ = happyReduce_52

action_107 _ = happyReduce_19

happyReduce_1 = happySpecReduce_2 4 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3 4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1:happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2 5 happyReduction_3
happyReduction_3 (HappyTerminal (TokenVar happy_var_2))
	_
	 =  HappyAbsSyn5
		 (Name happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2 5 happyReduction_4
happyReduction_4 (HappyTerminal (TokenConst happy_var_2))
	_
	 =  HappyAbsSyn5
		 (Name happy_var_2
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3 5 happyReduction_5
happyReduction_5 _
	(HappyTerminal (TokenHaskell happy_var_2))
	_
	 =  HappyAbsSyn5
		 (Code happy_var_2
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happyReduce 4 5 happyReduction_6
happyReduction_6 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenConst happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Cons happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 4 5 happyReduction_7
happyReduction_7 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Data (parseType happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 6 5 happyReduction_8
happyReduction_8 ((HappyAbsSyn21  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenConst happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (CHR (SimpRule happy_var_2 (Guard happy_var_4) happy_var_6)
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 4 5 happyReduction_9
happyReduction_9 ((HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (CHR (SimpRule happy_var_2 NoGuard happy_var_4)
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 6 5 happyReduction_10
happyReduction_10 ((HappyAbsSyn21  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenConst happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (CHR (PropRule happy_var_2 (Guard happy_var_4) happy_var_6)
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 4 5 happyReduction_11
happyReduction_11 ((HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (CHR (PropRule happy_var_2 NoGuard happy_var_4)
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_2 6 happyReduction_12
happyReduction_12 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal (TokenConst happy_var_1))
	 =  HappyAbsSyn6
		 ([TypeSig happy_var_1 happy_var_2]
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 4 6 happyReduction_13
happyReduction_13 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	(HappyTerminal (TokenConst happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 ((TypeSig happy_var_1 happy_var_2):happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_0 7 happyReduction_14
happyReduction_14  =  HappyAbsSyn7
		 ([]
	)

happyReduce_15 = happySpecReduce_1 7 happyReduction_15
happyReduction_15 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1 8 happyReduction_16
happyReduction_16 (HappyTerminal (TokenConst happy_var_1))
	 =  HappyAbsSyn8
		 ([TypeSig happy_var_1 []]
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happyReduce 4 8 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenConst happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ([TypeSig happy_var_1 happy_var_3]
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_3 8 happyReduction_18
happyReduction_18 (HappyAbsSyn8  happy_var_3)
	_
	(HappyTerminal (TokenConst happy_var_1))
	 =  HappyAbsSyn8
		 ((TypeSig happy_var_1 []):happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happyReduce 6 8 happyReduction_19
happyReduction_19 ((HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenConst happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ((TypeSig happy_var_1 happy_var_3):happy_var_6
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_1 9 happyReduction_20
happyReduction_20 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ([(parseType happy_var_1)]
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3 9 happyReduction_21
happyReduction_21 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ((parseType happy_var_1):happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1 10 happyReduction_22
happyReduction_22 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn10
		 ([VarType happy_var_1]
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3 10 happyReduction_23
happyReduction_23 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 ([ListType (parseType happy_var_2)]
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3 10 happyReduction_24
happyReduction_24 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn10
		 ([TupleType happy_var_2]
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3 10 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 ([(parseType happy_var_2)]
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1 10 happyReduction_26
happyReduction_26 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3 10 happyReduction_27
happyReduction_27 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 ([FuncType (parseType happy_var_1) (parseType happy_var_3)]
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1 11 happyReduction_28
happyReduction_28 (HappyTerminal (TokenConst happy_var_1))
	 =  HappyAbsSyn11
		 ([ConstType happy_var_1]
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2 11 happyReduction_29
happyReduction_29 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 ++ happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3 12 happyReduction_30
happyReduction_30 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn12
		 ((parseType happy_var_1):happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1 13 happyReduction_31
happyReduction_31 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn13
		 ([(parseType happy_var_1)]
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3 13 happyReduction_32
happyReduction_32 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn13
		 ((parseType happy_var_1):happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1 14 happyReduction_33
happyReduction_33 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3 14 happyReduction_34
happyReduction_34 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1:happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1 15 happyReduction_35
happyReduction_35 (HappyTerminal (TokenConst happy_var_1))
	 =  HappyAbsSyn15
		 (Pattern happy_var_1 []
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3 15 happyReduction_36
happyReduction_36 _
	_
	(HappyTerminal (TokenConst happy_var_1))
	 =  HappyAbsSyn15
		 (Pattern happy_var_1 []
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happyReduce 4 15 happyReduction_37
happyReduction_37 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenConst happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Pattern happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_38 = happySpecReduce_1 16 happyReduction_38
happyReduction_38 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 ([(parsePat happy_var_1)]
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3 16 happyReduction_39
happyReduction_39 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 ((parsePat happy_var_1):happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1 17 happyReduction_40
happyReduction_40 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn17
		 ([Var happy_var_1]
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3 17 happyReduction_41
happyReduction_41 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 ([ListPat1 (parsePat happy_var_1) (parsePat happy_var_3)]
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3 17 happyReduction_42
happyReduction_42 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn17
		 ([ListPat2 happy_var_2]
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3 17 happyReduction_43
happyReduction_43 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 ([ListPat2 [(parsePat happy_var_2)]]
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_2 17 happyReduction_44
happyReduction_44 _
	_
	 =  HappyAbsSyn17
		 ([ListPat2 []]
	)

happyReduce_45 = happySpecReduce_3 17 happyReduction_45
happyReduction_45 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn17
		 ([TuplePat happy_var_2]
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1 17 happyReduction_46
happyReduction_46 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3 17 happyReduction_47
happyReduction_47 (HappyAbsSyn17  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn17
		 ([AtPat happy_var_1 (parsePat happy_var_3)]
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3 17 happyReduction_48
happyReduction_48 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 ([BrackPat (parsePat happy_var_2)]
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1 17 happyReduction_49
happyReduction_49 _
	 =  HappyAbsSyn17
		 ([WildCardPat]
	)

happyReduce_50 = happySpecReduce_3 18 happyReduction_50
happyReduction_50 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 ((parsePat happy_var_1):happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1 19 happyReduction_51
happyReduction_51 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 ([(parsePat happy_var_1)]
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3 19 happyReduction_52
happyReduction_52 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn19
		 ((parsePat happy_var_1):happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1 20 happyReduction_53
happyReduction_53 (HappyTerminal (TokenConst happy_var_1))
	 =  HappyAbsSyn20
		 ([Const happy_var_1]
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_2 20 happyReduction_54
happyReduction_54 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 ++ happy_var_2
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1 21 happyReduction_55
happyReduction_55 (HappyTerminal (TokenConst happy_var_1))
	 =  HappyAbsSyn21
		 (Body happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1 21 happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn21
		 (NoBody
	)

happyReduce_57 = happySpecReduce_1 21 happyReduction_57
happyReduction_57 _
	 =  HappyAbsSyn21
		 (FalseBody
	)

happyNewToken action sts stk [] =
	action 51 51 (error "reading EOF!") (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenData -> cont 22;
	TokenRule -> cont 23;
	TokenName -> cont 24;
	TokenCons -> cont 25;
	TokenSemi -> cont 26;
	TokenVar happy_dollar_dollar -> cont 27;
	TokenConst happy_dollar_dollar -> cont 28;
	TokenLBrack -> cont 29;
	TokenRBrack -> cont 30;
	TokenHaskell happy_dollar_dollar -> cont 31;
	TokenComma -> cont 32;
	TokenEq -> cont 33;
	TokenBar -> cont 34;
	TokenSimp -> cont 35;
	TokenProp -> cont 36;
	TokenMLBrack -> cont 37;
	TokenMRBrack -> cont 38;
	TokenTrue -> cont 39;
	TokenFalse -> cont 40;
	TokenLList -> cont 41;
	TokenRList -> cont 42;
	TokenSLBrack -> cont 43;
	TokenSRBrack -> cont 44;
	TokenSComma -> cont 45;
	TokenCollen -> cont 46;
	TokenAt -> cont 47;
	TokenArrow -> cont 48;
	TokenWild -> cont 49;
	TokenError -> cont 50;
	_ -> happyError tks
	}

happyThen = \m k -> k m
happyReturn = \a -> a
happyThen1 = happyThen
happyReturn1 = \a tks -> a

hCHRParser tks = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq

happyError :: [Token] -> a
happyError _ = error ("Parse error\n")
type HSyn  = String
type VarId = String
data TypeSig = TypeSig String [Type]
data Type = ListType Type | TupleType [Type] | ConstrType String [Type] | ConstType String | VarType String
          | FuncType Type Type | UnknownType
data TermPat = Var HSyn | Const HSyn | ListPat1 TermPat TermPat | ListPat2 [TermPat] | TuplePat [TermPat] 
             | ConstrPat String [TermPat] | AtPat String TermPat | BrackPat TermPat | WildCardPat
data Pattern = Pattern HSyn [TermPat] 
data CHRRule = SimpRule [Pattern] Guard Body | PropRule [Pattern] Guard Body
data Guard   = Guard HSyn | NoGuard
data Body    = Body HSyn | NoBody | FalseBody
data Expr    = Name String | CHR CHRRule | Code HSyn | Cons String [TypeSig] | Data Type [TypeSig]
parseType :: [Type] -> Type
parseType [ConstType c] = ConstType c
parseType ((ConstType c):ts) = ConstrType c ts
parseType [t] = t
parseType _ = error "Error in parseType"                 
parsePat :: [TermPat] -> TermPat
parsePat [Const s] = Const s
parsePat ((Const s):ps) = ConstrPat s ps
parsePat [p] = p
parsePat _ = error "Error in parsePat"
instance Eq Type where
   (ListType t1) == (ListType t2)   = t1 == t2
   (TupleType t1) == (TupleType t2) = t1 == t2
   (ConstrType s1 t1) == (ConstrType s2 t2) = (s1 == s2) && (t1 == t2)
   (ConstType s1) == (ConstType s2) = s1 == s2
   (VarType s1) == (VarType s2) = s1 == s2
   (FuncType t1 t1') == (FuncType t2 t2') = (t1 == t2) && (t1' == t2')
   _ == _ = False
instance Eq TypeSig where
   (TypeSig s1 t1) == (TypeSig s2 t2) = (s1 == s2) && (t1 == t2)
instance Show TypeSig where
   show (TypeSig s ts) = "TypeSig(" ++ s ++ "," ++ (show ts) ++ ")"
instance Show Type where
   show (ListType t)   = "List(" ++ (show t) ++ ")"
   show (TupleType ts) = "Tuple(" ++ (show ts) ++ ")"
   show (ConstrType s ts) = "Constr(" ++ s ++ "," ++ (show ts) ++ ")"
   show (ConstType s)  = "Const(" ++ s ++ ")"
   show (VarType s) = "Var(" ++ s ++ ")"
   show (FuncType t1 t2) = "Arrow(" ++ (show t1) ++ "," ++ (show t2) ++ ")"
   show UnknownType = "Unknown()"
instance Show TermPat where
   show (Var x)   = "Var(" ++ x ++ ")"
   show (Const x) = "Const(" ++ x ++ ")"
   show (ListPat1 t1 t2) = "List1(" ++ (show t1) ++ "," ++ (show t2) ++ ")"
   show (ListPat2 ts) = "List2(" ++ (show ts) ++ ")"
   show (TuplePat ts) = "Tuple(" ++ (show ts) ++ ")"
   show (ConstrPat s ts) = "Constr(" ++ s ++ "," ++ (show ts) ++ ")"
   show (AtPat s t) = "At(" ++ s ++ "," ++ (show t) ++ ")"
   show (BrackPat t) = "Brack(" ++ (show t) ++ ")"
   show WildCardPat = "WildCard"
instance Show Pattern where
   show (Pattern s ts) = "Pattern(" ++ s ++ ","++ (show ts) ++ ")" 
instance Show CHRRule where
   show (SimpRule cons g b) = "SimpRule(" ++ (show cons) ++ "," ++ (show g) ++ "," ++ (show b) ++ ")"
   show (PropRule cons g b) = "PropRule(" ++ (show cons) ++ "," ++ (show g) ++ "," ++ (show b) ++ ")"
instance Show Guard where
   show (Guard g) = "Guard(" ++ (show g) ++ ")"
   show (NoGuard) = "Guard(True)"
instance Show Body where
   show (Body b)  = "Body(" ++ (show b) ++ ")"
   show NoBody    = "Body(True)"
   show FalseBody = "Body(False)"
instance Show Expr where
   show (Name s) = "Name(" ++ s ++ ")"
   show (CHR c)  = "CHR(" ++ (show c) ++ ")"
   show (Code s) = "Code(" ++ (show s) ++ ")"
   show (Cons s cs) = "Cons(" ++ s ++ "," ++ (show cs) ++ ")"
   show (Data s cs) = "Data(" ++ (show s) ++ "," ++ (show cs) ++ ")"
runParser :: String -> IO [Expr] 
runParser s = do let ts = lexerTop s
                 return (hCHRParser ts)
{-# LINE 1 "GenericTemplate.hs" #-}
-- $Id: DCHRParser.hs,v 1.1 2006/10/25 17:13:14 lamsoonl Exp $

{-# LINE 15 "GenericTemplate.hs" #-}






















































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

happyAccept j tk st sts (HappyStk ans _) = 

					   (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 150 "GenericTemplate.hs" #-}


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
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
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError


{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
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
