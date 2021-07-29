/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1988                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   * Copyright (c) 1972 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   *********************************************************** */




/* HISTORY COMMENTS:
  1) change(88-09-30,WAAnderson), approve(88-09-30,MCR7952),
     audit(88-09-30,JRGray), install(88-10-24,MR12.2-1184):
     Modified as part of C-Probe Support.
                                                   END HISTORY COMMENTS */


probe_scan_data_: proc;				/* CDS source */

/* Modified June 83 JMAthane to add PASCAL */
/* Added PASCAL_RANGE and PASCAL_ASSIGN, and associated actions 07/26/83 S. Herbst */
/* Fixed to allow dots in entry names when running FORTRAN 02/15/84 S. Herbst */
/* Modified April 88 Hinatsu to add C operators */

	dcl     create_data_segment_	 entry (ptr, fixed bin (35));

	dcl     1 cds		 aligned like cds_args;

	dcl     code		 fixed bin (35);
	dcl     (addr, null, size)	 builtin;
	dcl     com_err_		 entry options (variable);

%include probe_scan_data;


	cds.sections (1).p = addr (probe_scan_data);
	cds.sections (1).len = size (probe_scan_data);
	cds.sections (1).struct_name = "probe_scan_data";
	cds.sections (2).p = null ();
	cds.sections (2).len = 0;
	cds.sections (2).struct_name = "";
	cds.seg_name = "probe_scan_data_";
	cds.num_exclude_names = 0;
	cds.exclude_array_ptr = null ();
	cds.switches = "0"b;
	cds.switches.have_text = "1"b;


	call build_data;

	call create_data_segment_ (addr (cds), code);
	if code ^= 0 then call com_err_ (code);
	return;

build_data: proc;

	dcl     rank		 builtin;


	doubles (1).pair = "^="; doubles (1).type = op_coder (NOT_EQUALS);
	doubles (2).pair = "^<"; doubles (2).type = op_coder (NOT_LESS_THAN);
	doubles (3).pair = "^>"; doubles (3).type = op_coder (NOT_GREATER_THAN);
	doubles (4).pair = ">="; doubles (4).type = op_coder (NOT_LESS_THAN);
	doubles (5).pair = "<="; doubles (5).type = op_coder (NOT_GREATER_THAN);
	doubles (6).pair = "->"; doubles (6).type = op_coder (ARROW);
	doubles (7).pair = "<>"; doubles (7).type = op_coder (NOT_EQUALS);
	doubles (8).pair = ".."; doubles (8).type = op_coder (PASCAL_RANGE);
	doubles (9).pair = ":="; doubles (9).type = op_coder (PASCAL_ASSIGN);
/* the special cases for C doubles */
	doubles (10).pair = "=="; doubles (10).type = op_coder (C_EQUAL);
	doubles (11).pair = "!="; doubles (11).type = op_coder (C_NOT_EQUAL);
	doubles (12).pair = "<<"; doubles (12).type = op_coder (C_LEFT_SHIFT);
	doubles (13).pair = ">>"; doubles (13).type = op_coder (C_RIGHT_SHIFT);

	fort_ops (1).name = "not "; fort_ops (1).op_code = op_coder (NOT_SIGN);
	fort_ops (2).name = "and "; fort_ops (2).op_code = op_coder (AMPERSAND);
	fort_ops (3).name = "or  "; fort_ops (3).op_code = op_coder (OR_BAR);
	fort_ops (4).name = "ge  "; fort_ops (4).op_code = op_coder (NOT_LESS_THAN);
	fort_ops (5).name = "le  "; fort_ops (5).op_code = op_coder (NOT_GREATER_THAN);
	fort_ops (6).name = "gt  "; fort_ops (6).op_code = op_coder (GREATER_THAN);
	fort_ops (7).name = "lt  "; fort_ops (7).op_code = op_coder (LESS_THAN);
	fort_ops (8).name = "eq  "; fort_ops (8).op_code = op_coder (EQUALS);
	fort_ops (9).name = "ne  "; fort_ops (9).op_code = op_coder (NOT_EQUALS);

	pasc_ops (1).name = "not"; pasc_ops (1).op_code = op_coder (NOT_SIGN);
	pasc_ops (2).name = "and"; pasc_ops (2).op_code = op_coder (AMPERSAND);
	pasc_ops (3).name = "or"; pasc_ops (3).op_code = op_coder (OR_BAR);


	call set_char_types;

	probe_scan_data.cobol_char_type,
	     probe_scan_data.fortran_char_type,
	     probe_scan_data.pascal_char_type,
	     probe_scan_data.c_char_type = probe_scan_data.pl1_char_type;

/* now for the exceptions to the rule */

	probe_scan_data.fortran_char_type (rank ("'")) = QUOTE_CHAR_TYPE;

	probe_scan_data.pascal_char_type (rank ("'")) = QUOTE_CHAR_TYPE;
	probe_scan_data.pascal_char_type (rank ("#")) = OTHER_OPS_CHAR_TYPE;
	probe_scan_data.pascal_char_type (rank ("@")) = PASCAL_ARROW_CHAR_TYPE;
	probe_scan_data.pascal_char_type (rank ("^")) = PASCAL_ARROW_CHAR_TYPE;
	probe_scan_data.c_char_type (rank ("!")) = NOT_CHAR_TYPE;
	probe_scan_data.c_char_type (rank ("'")) = QUOTE_CHAR_TYPE;
	probe_scan_data.c_char_type (rank ("&")) = C_ADDRESS_CHAR_TYPE;
	probe_scan_data.c_char_type (rank ("%")) = C_MOD_CHAR_TYPE;
 

	probe_scan_data.cobol_action_table,
	     probe_scan_data.pascal_action_table,
	     probe_scan_data.fortran_action_table,
	     probe_scan_data.c_action_table = probe_scan_data.pl1_action_table;

/* now for the exceptions */

	probe_scan_data.cobol_action_table (NAME_STATE, MINUS_CHAR_TYPE) = 36;

	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 0) = 99;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 1) = 59;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 2) = 59;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 3) = 58;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 4) = 59;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 5) = 59;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 6) = 59;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 7) = 59;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 8) = 59;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 9) = 59;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 10) = 59;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 11) = 59;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 12) = 59;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 13) = 59;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 14) = 59;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 15) = 57;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 16) = 57;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 17) = 57;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 18) = 57;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 19) = 57;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 20) = 59;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 21) = 59;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 22) = 57;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 23) = 59;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 24) = 59;
	probe_scan_data.fortran_action_table (IN_DOT_OP_STATE, 25) = 59;

	probe_scan_data.fortran_action_table (DOT_SEEN_STATE, LETTER_B_CHAR_TYPE) = 56;
	probe_scan_data.fortran_action_table (DOT_SEEN_STATE, LETTER_E_CHAR_TYPE) = 56;
	probe_scan_data.fortran_action_table (DOT_SEEN_STATE, LETTER_I_CHAR_TYPE) = 56;
	probe_scan_data.fortran_action_table (DOT_SEEN_STATE, LETTER_O_CHAR_TYPE) = 56;
	probe_scan_data.fortran_action_table (DOT_SEEN_STATE, LETTER_F_CHAR_TYPE) = 56;
	probe_scan_data.fortran_action_table (DOT_SEEN_STATE, OTHER_LETTER_CHAR_TYPE) = 56;

	probe_scan_data.pascal_action_table (NULL_STATE, PASCAL_ARROW_CHAR_TYPE) = 16;
	probe_scan_data.pascal_action_table (POSSIBLE_DOUBLE_STATE, PASCAL_ARROW_CHAR_TYPE) = 71;
	probe_scan_data.pascal_action_table (NAME_STATE, PASCAL_ARROW_CHAR_TYPE) = 32;
	probe_scan_data.pascal_action_table (STRING_STATE, PASCAL_ARROW_CHAR_TYPE) = 41;
	probe_scan_data.pascal_action_table (QUOTE_SEEN_STATE, PASCAL_ARROW_CHAR_TYPE) = 45;
	probe_scan_data.pascal_action_table (BIT_STR_END_STATE, PASCAL_ARROW_CHAR_TYPE) = 51;
	probe_scan_data.pascal_action_table (DOT_SEEN_STATE, PASCAL_ARROW_CHAR_TYPE) = 55;
	probe_scan_data.pascal_action_table (NUMBER_PRE_DOT_STATE, PASCAL_ARROW_CHAR_TYPE) = 23;
	probe_scan_data.pascal_action_table (NUMBER_POST_DOT_STATE, PASCAL_ARROW_CHAR_TYPE) = 23;
	probe_scan_data.pascal_action_table (NUMBER_EXP1_STATE, PASCAL_ARROW_CHAR_TYPE) = 23;
	probe_scan_data.pascal_action_table (NUMBER_EXP2_STATE, PASCAL_ARROW_CHAR_TYPE) = 23;
	probe_scan_data.pascal_action_table (IN_DOT_OP_STATE, PASCAL_ARROW_CHAR_TYPE) = 0; /* undefined */
	probe_scan_data.pascal_action_table (PTR_WORD_STATE, PASCAL_ARROW_CHAR_TYPE) = 64;
	probe_scan_data.pascal_action_table (PTR_BIT_STATE, PASCAL_ARROW_CHAR_TYPE) = 95;
	probe_scan_data.pascal_action_table (DOT_SEEN_STATE, DOT_CHAR_TYPE) = 71;
	probe_scan_data.pascal_action_table (NULL_STATE, COLON_CHAR_TYPE) = 15;
	probe_scan_data.pascal_action_table (POSSIBLE_DOUBLE_STATE, EQUAL_CHAR_TYPE) = 71;


	probe_scan_data.c_action_table (NULL_STATE, C_ADDRESS_CHAR_TYPE) = 13;
	probe_scan_data.c_action_table (POSSIBLE_DOUBLE_STATE, C_ADDRESS_CHAR_TYPE) = 70;
	probe_scan_data.c_action_table (NAME_STATE, C_ADDRESS_CHAR_TYPE) = 32;
	probe_scan_data.c_action_table (STRING_STATE, C_ADDRESS_CHAR_TYPE) = 41;
	probe_scan_data.c_action_table (QUOTE_SEEN_STATE, C_ADDRESS_CHAR_TYPE) = 45;
	probe_scan_data.c_action_table (BIT_STR_END_STATE, C_ADDRESS_CHAR_TYPE) = 51;
	probe_scan_data.c_action_table (DOT_SEEN_STATE, C_ADDRESS_CHAR_TYPE) = 55;
	probe_scan_data.c_action_table (NUMBER_PRE_DOT_STATE, C_ADDRESS_CHAR_TYPE) = 23;
	probe_scan_data.c_action_table (NUMBER_POST_DOT_STATE, C_ADDRESS_CHAR_TYPE) = 23;
	probe_scan_data.c_action_table (NUMBER_EXP1_STATE, C_ADDRESS_CHAR_TYPE) = 25;
	probe_scan_data.c_action_table (NUMBER_EXP2_STATE, C_ADDRESS_CHAR_TYPE) = 22;
	probe_scan_data.c_action_table (PTR_WORD_STATE, C_ADDRESS_CHAR_TYPE) = 64;
	probe_scan_data.c_action_table (PTR_BIT_STATE, C_ADDRESS_CHAR_TYPE) = 95;

	probe_scan_data.c_action_table (NULL_STATE, C_MOD_CHAR_TYPE) = 13;
	probe_scan_data.c_action_table (POSSIBLE_DOUBLE_STATE, C_MOD_CHAR_TYPE) = 70;
	probe_scan_data.c_action_table (NAME_STATE, C_MOD_CHAR_TYPE) = 32;
	probe_scan_data.c_action_table (STRING_STATE, C_MOD_CHAR_TYPE) = 41;
	probe_scan_data.c_action_table (QUOTE_SEEN_STATE, C_MOD_CHAR_TYPE) = 45;
	probe_scan_data.c_action_table (BIT_STR_END_STATE, C_MOD_CHAR_TYPE) = 51;
	probe_scan_data.c_action_table (DOT_SEEN_STATE, C_MOD_CHAR_TYPE) = 55;
	probe_scan_data.c_action_table (NUMBER_PRE_DOT_STATE, C_MOD_CHAR_TYPE) = 23;
	probe_scan_data.c_action_table (NUMBER_POST_DOT_STATE, C_MOD_CHAR_TYPE) = 23;
	probe_scan_data.c_action_table (NUMBER_EXP1_STATE, C_MOD_CHAR_TYPE) = 25;
	probe_scan_data.c_action_table (NUMBER_EXP2_STATE, C_MOD_CHAR_TYPE) = 22;
	probe_scan_data.c_action_table (PTR_WORD_STATE, C_MOD_CHAR_TYPE) = 64;
	probe_scan_data.c_action_table (PTR_BIT_STATE, C_MOD_CHAR_TYPE) = 95;


op_coder: proc (opt) returns (fixed bin);
	dcl     opt		 bit (18) aligned parameter;

	dcl     (bin, substr)	 builtin;

	return (bin (substr (opt, 13)));
     end op_coder;
     end build_data;

set_char_types: proc;


/* this proc is set off from build data because it is so boring */

	probe_scan_data.pl1_char_type (0) = ILLEGAL_CHAR_TYPE; /* \000 */
	probe_scan_data.pl1_char_type (1) = ILLEGAL_CHAR_TYPE; /* \001 */
	probe_scan_data.pl1_char_type (2) = ILLEGAL_CHAR_TYPE; /* \002 */
	probe_scan_data.pl1_char_type (3) = ILLEGAL_CHAR_TYPE; /* \003 */
	probe_scan_data.pl1_char_type (4) = ILLEGAL_CHAR_TYPE; /* \004 */
	probe_scan_data.pl1_char_type (5) = ILLEGAL_CHAR_TYPE; /* \005 */
	probe_scan_data.pl1_char_type (6) = ILLEGAL_CHAR_TYPE; /* \006 */
	probe_scan_data.pl1_char_type (7) = ILLEGAL_CHAR_TYPE; /* \007 */
	probe_scan_data.pl1_char_type (8) = ILLEGAL_CHAR_TYPE; /* \010 */
	probe_scan_data.pl1_char_type (9) = WHITE_SPACE_CHAR_TYPE; /* \011 */
	probe_scan_data.pl1_char_type (10) = OTHER_OPS_CHAR_TYPE; /* \012 */
	probe_scan_data.pl1_char_type (11) = ILLEGAL_CHAR_TYPE; /* \013 */
	probe_scan_data.pl1_char_type (12) = WHITE_SPACE_CHAR_TYPE; /* \014 */
	probe_scan_data.pl1_char_type (13) = OTHER_OPS_CHAR_TYPE; /* \015 */
	probe_scan_data.pl1_char_type (14) = ILLEGAL_CHAR_TYPE; /* \016 */
	probe_scan_data.pl1_char_type (15) = ILLEGAL_CHAR_TYPE; /* \017 */
	probe_scan_data.pl1_char_type (16) = ILLEGAL_CHAR_TYPE; /* \020 */
	probe_scan_data.pl1_char_type (17) = ILLEGAL_CHAR_TYPE; /* \021 */
	probe_scan_data.pl1_char_type (18) = ILLEGAL_CHAR_TYPE; /* \022 */
	probe_scan_data.pl1_char_type (19) = ILLEGAL_CHAR_TYPE; /* \023 */
	probe_scan_data.pl1_char_type (20) = ILLEGAL_CHAR_TYPE; /* \024 */
	probe_scan_data.pl1_char_type (21) = ILLEGAL_CHAR_TYPE; /* \025 */
	probe_scan_data.pl1_char_type (22) = ILLEGAL_CHAR_TYPE; /* \026 */
	probe_scan_data.pl1_char_type (23) = ILLEGAL_CHAR_TYPE; /* \027 */
	probe_scan_data.pl1_char_type (24) = ILLEGAL_CHAR_TYPE; /* \030 */
	probe_scan_data.pl1_char_type (25) = ILLEGAL_CHAR_TYPE; /* \031 */
	probe_scan_data.pl1_char_type (26) = ILLEGAL_CHAR_TYPE; /* \032 */
	probe_scan_data.pl1_char_type (27) = ILLEGAL_CHAR_TYPE; /* \033 */
	probe_scan_data.pl1_char_type (28) = ILLEGAL_CHAR_TYPE; /* \034 */
	probe_scan_data.pl1_char_type (29) = ILLEGAL_CHAR_TYPE; /* \035 */
	probe_scan_data.pl1_char_type (30) = ILLEGAL_CHAR_TYPE; /* \036 */
	probe_scan_data.pl1_char_type (31) = ILLEGAL_CHAR_TYPE; /* \037 */
	probe_scan_data.pl1_char_type (32) = WHITE_SPACE_CHAR_TYPE; /* \040  <SPACE> */
	probe_scan_data.pl1_char_type (33) = ILLEGAL_CHAR_TYPE; /* \041  ! */
	probe_scan_data.pl1_char_type (34) = QUOTE_CHAR_TYPE; /* \042  " */
	probe_scan_data.pl1_char_type (35) = ILLEGAL_CHAR_TYPE; /* \043  # */
	probe_scan_data.pl1_char_type (36) = OTHER_LETTER_CHAR_TYPE; /* \044   $ */
	probe_scan_data.pl1_char_type (37) = OTHER_LETTER_CHAR_TYPE; /* \045  % */
	probe_scan_data.pl1_char_type (38) = OTHER_OPS_CHAR_TYPE; /* \046  & */
	probe_scan_data.pl1_char_type (39) = ILLEGAL_CHAR_TYPE; /* \047 */
	probe_scan_data.pl1_char_type (40) = LEFT_PAREN_CHAR_TYPE; /* \050 */
	probe_scan_data.pl1_char_type (41) = RIGHT_PAREN_CHAR_TYPE; /* \051 */
	probe_scan_data.pl1_char_type (42) = OTHER_OPS_CHAR_TYPE; /* \052 */
	probe_scan_data.pl1_char_type (43) = PLUS_CHAR_TYPE; /* \053 */
	probe_scan_data.pl1_char_type (44) = OTHER_OPS_CHAR_TYPE; /* \054 , */
	probe_scan_data.pl1_char_type (45) = MINUS_CHAR_TYPE; /* \055 */
	probe_scan_data.pl1_char_type (46) = DOT_CHAR_TYPE; /* \056 */
	probe_scan_data.pl1_char_type (47) = OTHER_OPS_CHAR_TYPE; /* \057 / */
	probe_scan_data.pl1_char_type (48) = ZERO_CHAR_TYPE; /* \060  0 */
	probe_scan_data.pl1_char_type (49) = RADIX_DIGITS_CHAR_TYPE; /* \061 */
	probe_scan_data.pl1_char_type (50) = RADIX_DIGITS_CHAR_TYPE; /* \062 */
	probe_scan_data.pl1_char_type (51) = RADIX_DIGITS_CHAR_TYPE; /* \063 */
	probe_scan_data.pl1_char_type (52) = RADIX_DIGITS_CHAR_TYPE; /* \064 */
	probe_scan_data.pl1_char_type (53) = OCTAL_DIGITS_CHAR_TYPE; /* \065 */
	probe_scan_data.pl1_char_type (54) = OCTAL_DIGITS_CHAR_TYPE; /* \066 */
	probe_scan_data.pl1_char_type (55) = OCTAL_DIGITS_CHAR_TYPE; /* \067 */
	probe_scan_data.pl1_char_type (56) = OTHER_DIGITS_CHAR_TYPE; /* \070 */
	probe_scan_data.pl1_char_type (57) = OTHER_DIGITS_CHAR_TYPE; /* \071 */
	probe_scan_data.pl1_char_type (58) = COLON_CHAR_TYPE; /* \072 */
	probe_scan_data.pl1_char_type (59) = OTHER_OPS_CHAR_TYPE; /* \073 */
	probe_scan_data.pl1_char_type (60) = LESS_CHAR_TYPE; /* \074 */
	probe_scan_data.pl1_char_type (61) = EQUAL_CHAR_TYPE; /* \075 */
	probe_scan_data.pl1_char_type (62) = GREATER_CHAR_TYPE; /* \076 */
	probe_scan_data.pl1_char_type (63) = OTHER_OPS_CHAR_TYPE; /* \077 */
	probe_scan_data.pl1_char_type (64) = ILLEGAL_CHAR_TYPE; /* \100 */
	probe_scan_data.pl1_char_type (65) = OTHER_LETTER_CHAR_TYPE; /* \101  A */
	probe_scan_data.pl1_char_type (66) = OTHER_LETTER_CHAR_TYPE; /* \102  B */
	probe_scan_data.pl1_char_type (67) = OTHER_LETTER_CHAR_TYPE; /* \103  C */
	probe_scan_data.pl1_char_type (68) = OTHER_LETTER_CHAR_TYPE; /* \104 */
	probe_scan_data.pl1_char_type (69) = OTHER_LETTER_CHAR_TYPE; /* \105 */
	probe_scan_data.pl1_char_type (70) = OTHER_LETTER_CHAR_TYPE; /* \106 */
	probe_scan_data.pl1_char_type (71) = OTHER_LETTER_CHAR_TYPE; /* \107 */
	probe_scan_data.pl1_char_type (72) = OTHER_LETTER_CHAR_TYPE; /* \110 */
	probe_scan_data.pl1_char_type (73) = OTHER_LETTER_CHAR_TYPE; /* \111 */
	probe_scan_data.pl1_char_type (74) = OTHER_LETTER_CHAR_TYPE; /* \112 */
	probe_scan_data.pl1_char_type (75) = OTHER_LETTER_CHAR_TYPE; /* \113 */
	probe_scan_data.pl1_char_type (76) = OTHER_LETTER_CHAR_TYPE; /* \114 */
	probe_scan_data.pl1_char_type (77) = OTHER_LETTER_CHAR_TYPE; /* \115 */
	probe_scan_data.pl1_char_type (78) = OTHER_LETTER_CHAR_TYPE; /* \116 */
	probe_scan_data.pl1_char_type (79) = OTHER_LETTER_CHAR_TYPE; /* \117 */
	probe_scan_data.pl1_char_type (80) = OTHER_LETTER_CHAR_TYPE; /* \120 */
	probe_scan_data.pl1_char_type (81) = OTHER_LETTER_CHAR_TYPE; /* \121 */
	probe_scan_data.pl1_char_type (82) = OTHER_LETTER_CHAR_TYPE; /* \122 */
	probe_scan_data.pl1_char_type (83) = OTHER_LETTER_CHAR_TYPE; /* \123 */
	probe_scan_data.pl1_char_type (84) = OTHER_LETTER_CHAR_TYPE; /* \124 */
	probe_scan_data.pl1_char_type (85) = OTHER_LETTER_CHAR_TYPE; /* \125 */
	probe_scan_data.pl1_char_type (86) = OTHER_LETTER_CHAR_TYPE; /* \126 */
	probe_scan_data.pl1_char_type (87) = OTHER_LETTER_CHAR_TYPE; /* \127 */
	probe_scan_data.pl1_char_type (88) = OTHER_LETTER_CHAR_TYPE; /* \130 */
	probe_scan_data.pl1_char_type (89) = OTHER_LETTER_CHAR_TYPE; /* \131 */
	probe_scan_data.pl1_char_type (90) = OTHER_LETTER_CHAR_TYPE; /* \132 */
	probe_scan_data.pl1_char_type (91) = OTHER_OPS_CHAR_TYPE; /* \133 */
	probe_scan_data.pl1_char_type (92) = ILLEGAL_CHAR_TYPE; /* \134 */
	probe_scan_data.pl1_char_type (93) = OTHER_OPS_CHAR_TYPE; /* \135 */
	probe_scan_data.pl1_char_type (94) = NOT_CHAR_TYPE; /* \136 */
	probe_scan_data.pl1_char_type (95) = OTHER_LETTER_CHAR_TYPE; /* \137 */
	probe_scan_data.pl1_char_type (96) = ILLEGAL_CHAR_TYPE; /* \140 */
	probe_scan_data.pl1_char_type (97) = OTHER_LETTER_CHAR_TYPE; /* \141  a */
	probe_scan_data.pl1_char_type (98) = LETTER_B_CHAR_TYPE; /* \142 */
	probe_scan_data.pl1_char_type (99) = OTHER_LETTER_CHAR_TYPE; /* \143 */
	probe_scan_data.pl1_char_type (100) = OTHER_LETTER_CHAR_TYPE; /* \144 */
	probe_scan_data.pl1_char_type (101) = LETTER_E_CHAR_TYPE; /* \145 */
	probe_scan_data.pl1_char_type (102) = LETTER_F_CHAR_TYPE; /* \146 */
	probe_scan_data.pl1_char_type (103) = OTHER_LETTER_CHAR_TYPE; /* \147 */
	probe_scan_data.pl1_char_type (104) = OTHER_LETTER_CHAR_TYPE; /* \150 */
	probe_scan_data.pl1_char_type (105) = LETTER_I_CHAR_TYPE; /* \151 */
	probe_scan_data.pl1_char_type (106) = OTHER_LETTER_CHAR_TYPE; /* \152 */
	probe_scan_data.pl1_char_type (107) = OTHER_LETTER_CHAR_TYPE; /* \153 */
	probe_scan_data.pl1_char_type (108) = OTHER_LETTER_CHAR_TYPE; /* \154 */
	probe_scan_data.pl1_char_type (109) = OTHER_LETTER_CHAR_TYPE; /* \155 */
	probe_scan_data.pl1_char_type (110) = OTHER_LETTER_CHAR_TYPE; /* \156 */
	probe_scan_data.pl1_char_type (111) = LETTER_O_CHAR_TYPE; /* \157 */
	probe_scan_data.pl1_char_type (112) = OTHER_LETTER_CHAR_TYPE; /* \160 */
	probe_scan_data.pl1_char_type (113) = OTHER_LETTER_CHAR_TYPE; /* \161 */
	probe_scan_data.pl1_char_type (114) = OTHER_LETTER_CHAR_TYPE; /* \162 */
	probe_scan_data.pl1_char_type (115) = OTHER_LETTER_CHAR_TYPE; /* \163 */
	probe_scan_data.pl1_char_type (116) = OTHER_LETTER_CHAR_TYPE; /* \164 */
	probe_scan_data.pl1_char_type (117) = OTHER_LETTER_CHAR_TYPE; /* \165 */
	probe_scan_data.pl1_char_type (118) = OTHER_LETTER_CHAR_TYPE; /* \166 */
	probe_scan_data.pl1_char_type (119) = OTHER_LETTER_CHAR_TYPE; /* \167 */
	probe_scan_data.pl1_char_type (120) = OTHER_LETTER_CHAR_TYPE; /* \170 */
	probe_scan_data.pl1_char_type (121) = OTHER_LETTER_CHAR_TYPE; /* \171 */
	probe_scan_data.pl1_char_type (122) = OTHER_LETTER_CHAR_TYPE; /* \172 */
	probe_scan_data.pl1_char_type (123) = ILLEGAL_CHAR_TYPE; /* \173 */
	probe_scan_data.pl1_char_type (124) = VERTICAL_BAR_CHAR_TYPE; /* \174 */
	probe_scan_data.pl1_char_type (125) = ILLEGAL_CHAR_TYPE; /* \175 */
	probe_scan_data.pl1_char_type (126) = ILLEGAL_CHAR_TYPE; /* \176 */
	probe_scan_data.pl1_char_type (127) = ILLEGAL_CHAR_TYPE; /* \177 */


     end set_char_types;


%include cds_args;


/* ;;;;;;; */

%include probe_operators;

%include probe_scan_dcls;

     end probe_scan_data_;
