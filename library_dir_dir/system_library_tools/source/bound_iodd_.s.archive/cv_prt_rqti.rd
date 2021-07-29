/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1984 *
   *                                                         *
   * Copyright (c) 1972 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   *********************************************************** */

/* format: style4,delnl,insnl,^ifthendo */

/* format: off */

/* Program to convert an ASCII file into a request info segment */

/* Created:  May 1977 by E. Donner */
/* Modified: June 1979 by C. Hornig to add banner_bars and banner_type: brief; */
/* Modified: December 1981 by G. Palter to add "prt_control: force_ctl_char;" and to fix entry number 0001 (phx06682) from
      the io_daemon error list: cv_prt_rqti prints a misleading error message when improper syntax appears in the
      "message" statement (and probably other statements also) */
/* Modified: November 1983 by C. Marker to add force_nsep */

/* format: on */

/*++

BEGIN
	/ driver_attributes :	/ LEX(2)					/ driver_attr\

	/ driver_wait_time : <legal_wait_time> ;
				/ [prt_rqti.driver_wait_time = wait_time] LEX(4)	/ BEGIN\
	/ driver_wait_time : <any-token> ;
				/ NEXT_STMT				/ BEGIN\

	/ banner_type : standard ;	/ [is_printer = "1"b; prt_rqti.banner_type = 1] LEX(4)
									/ BEGIN\
	/ banner_type : brief ;	/ [is_printer = "1"b; prt_rqti.banner_type = 2] LEX(4)
									/ BEGIN\
	/ banner_type : none ;	/ [is_printer = "1"b; prt_rqti.banner_type = 0] LEX(4)
									/ BEGIN\
	/ banner_type : <any-token> ;	/ LEX(2) ERROR(18) NEXT_STMT			/ BEGIN\
	/ banner_type : <any-token>	/ ERROR(4) NEXT_STMT			/ BEGIN\

	/ banner_bars : double ;	/ [is_printer = "1"b; prt_rqti.banner_bars = 0] LEX(4)
									/ BEGIN\
	/ banner_bars : single ;	/ [is_printer = "1"b; prt_rqti.banner_bars = 1] LEX(4)
									/ BEGIN\
	/ banner_bars : none ;	/ [is_printer = "1"b; prt_rqti.banner_bars = 2] LEX(4)
									/ BEGIN\
	/ banner_bars : <any-token> ;	/ LEX(2) ERROR(14) NEXT_STMT			/ BEGIN\
	/ banner_bars : <any-token>	/ ERROR(4) NEXT_STMT			/ BEGIN\

	/ prt_control :		/ [is_printer = "1"b] LEX(2)			/ process_control_flags\

	/ message : <quoted-string> ;
				/ LEX(2) [prt_rqti.opr_msg = token_value; is_printer = "1"b] LEX(2)
									/ BEGIN\
	/ message : <any-token> 	/ ERROR(4) NEXT_STMT			/ BEGIN\

	/ paper_length : <legal_paper_len> ;
				/ [prt_rqti.paper_length = paper_len; is_printer = "1"b] LEX(4)
									/ BEGIN\
	/ paper_length : <any-token> ;
				/ NEXT_STMT				/ BEGIN\

	/ paper_width : <legal_paper_wdth> ;
				/ [prt_rqti.paper_width = paper_wdth; is_printer = "1"b] LEX(4)
								          / BEGIN\
	/ paper_width : <any-token> ;	/ NEXT_STMT				/ BEGIN\

	/ lines_per_inch : <legal_lpi> ;
				/ [prt_rqti.lines_per_inch = lpi; is_printer = "1"b] LEX(4)
									/ BEGIN\
	/ lines_per_inch : <any-token> ;
				/ NEXT_STMT				/ BEGIN\

	/ line ( <legal_line_no> ) :	/ [is_printer = "1"b] LEX(5)			/ process_nos\
	/ line ( <any-token> ) :	/ NEXT_STMT				/ BEGIN\

	/ end ;			/					/ end\

	/ <any-token> :		/ ERROR(1) NEXT_STMT			/ BEGIN\
	/ <any-token>		/ ERROR(4) NEXT_STMT			/ BEGIN\
	/ <no-token>		/ ERROR(2)				/ end\

process_nos
	/ <legal_chn_no>		/ [substr(prt_rqti.channel_stops(line_no),chn_no,1)="1"b] LEX(1)
									/ get_punct\
	/ <any-token>		/ NEXT_STMT				/ BEGIN\
	/ <no-token>		/ ERROR(2)				/ end\

get_punct
	/ ,			/ LEX					/ process_nos\
	/ ;			/ LEX					/ BEGIN\
	/ <any-token>		/ ERROR(4) NEXT_STMT			/ BEGIN\
	/ <no-token>		/ ERROR(2)				/ end\

end	/			/					/ RETURN\

driver_attr
	/			/ [ind = 1; string (switches(0)) =""b; string (switches (1)) = ""b]
									/ \

driver_loop
	/ ;			/ LEX					/ assign_switches\
	/ ^			/ [ind = 1 - ind] LEX			/ \
	/ meter			/ [switches (ind).meter = "1"b] LEX		/ attpunct\
	/ auto_go			/ [switches (ind).auto_go = "1"b] LEX		/ attpunct\

	/ <any-token>		/ ERROR(15) NEXT_STMT			/ BEGIN\
	/ <no-token>		/ ERROR(2)				/ end\

attpunct
	/ ,			/ [ind = 1] LEX				/ driver_loop\
	/ ;			/ LEX					/ assign_switches\

	/ <any-token>		/ ERROR(4) NEXT_STMT			/ BEGIN\
	/ <no-token>		/ ERROR(2)				/ end\

assign_switches
	/			/ [string(prt_rqti.rqti_switches) = string(switches(1)) & ^string(switches(0))]
									/ BEGIN\

process_control_flags
	/			/ [ind = 1; string(flags(0)) = ""b; string(flags(1)) = ""b]
									/ \

flag_loop
	/ ;			/ LEX					/ assign_flags\

	/ ^			/ [ind = 1 - ind] LEX			/ \
	/ auto_print		/ [flags(ind).no_auto_print = "1"b] LEX		/ flagpunct\
	/ force_nep		/ [flags(ind).force_nep = "1"b] LEX		/ flagpunct\
	/ force_esc		/ [flags(ind).force_esc = "1"b] LEX		/ flagpunct\
	/ force_nsep		/ [flags(ind).force_nsep = "1"b] LEX		/ flagpunct\
	/ force_ctl_char		/ [flags(ind).force_ctl_char = "1"b] LEX	/ flagpunct\

	/ <any-token>		/ ERROR(16) NEXT_STMT			/ BEGIN\
	/ <no-token>		/ ERROR(2)				/ end\

flagpunct
	/ ,			/ [ind = 1] LEX				/ flag_loop\
	/ ;			/ LEX					/ assign_flags\

	/ <any-token>		/ ERROR(4) NEXT_STMT			/ BEGIN\
	/ <no-token>		/ ERROR(2)				/ end\

assign_flags
	/			/ [string(prt_rqti.prt_flags) = string(flags(1)) & ^string(flags(0))]
									/ BEGIN\

++*/

/**/

cv_prt_rqti:
     procedure () options (variable);

/* AUTOMATIC VARIABLES */

dcl  APstmt ptr;
dcl  APtoken ptr;
dcl  area_ptr ptr;
dcl  arg_length fixed bin;
dcl  arg_ptr ptr;
dcl  bitcount fixed bin (24);
dcl  chn_no fixed bin;
dcl  code fixed bin (35);
dcl  dname char (168);
dcl  ename char (32);
dcl  ind fixed bin;
dcl  is_printer bit (1) aligned;
dcl  len_ent fixed bin;
dcl  len_rqti fixed bin (18);
dcl  line_no fixed bin;
dcl  lpi fixed bin;
dcl  max_line_no fixed bin;
dcl  n_chars fixed bin (21);
dcl  paper_len fixed bin;
dcl  paper_wdth fixed bin;
dcl  rqti_name char (32);
dcl  sourcep ptr;
dcl  wait_time fixed bin;

dcl  1 flags (0:1) aligned like prt_rqti.prt_flags;
dcl  1 switches (0:1) aligned like rqti_header.rqti_switches;

/* BASED VARIABLES */

dcl  arg char (arg_length) based (arg_ptr);

/* BUILTINS */

dcl  (clock, collate, dimension, divide, length, null, rtrim, size, string, substr, unspec) builtin;

/* CONDITIONS */

dcl  cleanup condition;

/* EXTERNAL ENTRIES */

dcl  com_err_ entry options (variable);
dcl  cu_$arg_ptr entry (fixed bin, ptr, fixed bin, fixed bin (35));
dcl  cv_dec_check_ entry (char (*), fixed bin (35)) returns (fixed bin);
dcl  expand_pathname_$add_suffix entry (char (*), char (*), char (*), char (*), fixed bin (35));
dcl  hcs_$delentry_seg entry (ptr, fixed bin (35));
dcl  hcs_$initiate_count entry (char (*), char (*), char (*), fixed bin (24), fixed bin (2), ptr, fixed bin (35));
dcl  hcs_$make_seg entry (char (*), char (*), char (*), fixed bin (5), ptr, fixed bin (35));
dcl  hcs_$set_bc_seg entry (ptr, fixed bin (24), fixed bin (35));
dcl  hcs_$terminate_noname entry (ptr, fixed bin (35));
dcl  hcs_$truncate_seg entry (ptr, fixed bin (18), fixed bin (35));
dcl  get_wdir_ entry returns (char (168) aligned);
dcl  lex_error_ entry options (variable);
dcl  lex_string_$init_lex_delims
	entry (char (*), char (*), char (*), char (*), char (*), bit (*), char (*) var, char (*) var, char (*) var,
	char (*) var);
dcl  lex_string_$lex
	entry (ptr, fixed bin (21), fixed bin, ptr, bit (*), char (*), char (*), char (*), char (*), char (*),
	char (*) var, char (*) var, char (*) var, char (*) var, ptr, ptr, fixed bin (35));
dcl  translator_temp_$get_segment entry (char (*), ptr, fixed bin (35));
dcl  translator_temp_$release_all_segments entry (ptr, fixed bin (35));

/* INTERNAL STATIC VARIABLES */

dcl  BREAKS char (128) varying;
dcl  IGBREAKS char (128) varying;
dcl  LEXCTL char (128) varying;
dcl  LEXDLM char (128) varying;
dcl  first_time bit (1) aligned init ("1"b);

/* CONSTANTS */

dcl  me char (11) static options (constant) init ("cv_prt_rqti");

/* EXTERNAL STATIC VARIABLES */

dcl  error_table_$badopt ext fixed bin (35);
dcl  error_table_$entlong ext fixed bin (35);
dcl  error_table_$translation_failed ext fixed bin (35);

/**/

%include prt_rqti;

/**/

/* PROGRAM */

	call cu_$arg_ptr (1, arg_ptr, arg_length, code);
	if code ^= 0
	then do;
	     call com_err_ (code, me, "Usage: cv_prt_rqti pathname (-brief|-bf|-long|-lg");
	     return;
	end;

	call expand_pathname_$add_suffix (arg, "rqti", dname, ename, code);
	if code ^= 0
	then do;
error1:
	     call com_err_ (code, me, arg);
	     return;
	end;

	call cu_$arg_ptr (2, arg_ptr, arg_length, code);
	if code = 0
	then if (arg = "-bf") | (arg = "-brief")
	     then SERROR_CONTROL = "01"b;
	     else if (arg = "-lg") | (arg = "-long")
	     then SERROR_CONTROL = "10"b;
	     else do;
		code = error_table_$badopt;
		go to error1;
	     end;

	len_ent = length (rtrim (ename));

	sourcep = null;
	prt_rqtip = null;
	area_ptr = null;

	on cleanup call clean;

	call hcs_$initiate_count (dname, ename, "", bitcount, 1b, sourcep, code);
	if sourcep = null
	then do;
error2:
	     call com_err_ (code, me, "^a>^a", dname, ename);
	     return;
	end;

	n_chars = divide (bitcount + 8, 9, 22, 0);

	dname = get_wdir_ ();

	rqti_name = substr (ename, 1, len_ent - length (".rqti"));

	call hcs_$make_seg (dname, rqti_name, "", 1011b, prt_rqtip, code);
	if prt_rqtip = null
	then do;
error3:
	     call com_err_ (code, me, "^a>^a", dname, rqti_name);
	     call clean;
	     return;
	end;

	call hcs_$truncate_seg (prt_rqtip, 0, code);
	if code ^= 0
	then go to error3;

	unspec (prt_rqti) = "0"b;

	prt_rqti.time_created = clock ();
	prt_rqti.header_version = rqti_header_version_1;
	prt_rqti.driver_wait_time = 30;

	prt_rqti.version = prt_rqti_version_1;
	prt_rqti.opr_msg = "";
	prt_rqti.banner_type = 1;
	prt_rqti.banner_bars = 0;
	prt_rqti.banner_line = 1;
	prt_rqti.paper_length = 66;
	prt_rqti.paper_width = 136;
	prt_rqti.lines_per_inch = 6;
	prt_rqti.no_auto_print = "1"b;

	max_line_no = 0;

	if first_time				/* only needs to be done once/process */
	then do;
	     BREAKS = substr (collate (), 1, 8) || substr (collate (), 10, 24) || ":,()^";
	     IGBREAKS = substr (collate (), 1, 8) || substr (collate (), 10, 24);
	     call lex_string_$init_lex_delims ("""", """", "/*", "*/", ";", "10"b, BREAKS, IGBREAKS, LEXDLM, LEXCTL);
	     first_time = "0"b;
	end;

	call translator_temp_$get_segment (me, area_ptr, code);
	if area_ptr = null
	then do;
	     call com_err_ (code, me, "Making temporary segment in process directory.");
	     call clean;
	     return;
	end;

	call lex_string_$lex (sourcep, n_chars, 0, area_ptr, "1000"b, """", """", "/*", "*/", ";", BREAKS, IGBREAKS,
	     LEXDLM, LEXCTL, APstmt, APtoken, code);
	if code ^= 0
	then do;
	     call com_err_ (code, me, ename);
	     call clean;
	     return;
	end;

	Pthis_token = APtoken;
	call SEMANTIC_ANALYSIS ();

	if is_printer = "1"b
	then do;

	     prt_rqti.type_code = 1;
	     prt_rqti.no_auto_print = ^prt_rqti.no_auto_print;

	     if (SERROR_PRINTED (10) = "0"b) & (max_line_no > prt_rqti.paper_length)
	     then call semant_error (9, max_line_no, prt_rqti.paper_length);

	     if (SERROR_PRINTED (18) = "0"b) & (prt_rqti.banner_type = 1)
	     then do;

		if (SERROR_PRINTED (11) = "0"b) & (prt_rqti.paper_width < 10)
		then call semant_error (5, 0, 0);

		if (SERROR_PRINTED (10) = "0"b) & (SERROR_PRINTED (12) = "0"b)
		     & (prt_rqti.paper_length < (24 + prt_rqti.lines_per_inch))
		then call semant_error (6, (24 + prt_rqti.lines_per_inch), prt_rqti.lines_per_inch);

	     end;
	end;

	if MERROR_SEVERITY > 1
	then do;
	     call com_err_ (error_table_$translation_failed, me, ename);
	     call hcs_$delentry_seg (prt_rqtip, code);
	     prt_rqtip = null;
	end;
	else do;
	     if prt_rqti.type_code = 0
	     then do;
		len_rqti = size (rqti_header);
		call hcs_$truncate_seg (prt_rqtip, len_rqti, code);
	     end;
	     else len_rqti = size (prt_rqti);
	     bitcount = 36 * len_rqti;
	     call hcs_$set_bc_seg (prt_rqtip, bitcount, code);
	     if code ^= 0
	     then call com_err_ (code, me, "Unable to set bitcount on ^a>^a to ^d", dname, rqti_name, bitcount);
	end;

	call clean;				/* terminate input & output segments */
	return;

/* clean up procedure */

clean:
     procedure;

	if sourcep ^= null
	then call hcs_$terminate_noname (sourcep, code);

	if prt_rqtip ^= null
	then call hcs_$terminate_noname (prt_rqtip, code);

	if area_ptr ^= null
	then call translator_temp_$release_all_segments (area_ptr, code);

     end clean;

/**/
legal_paper_len:
     proc returns (bit (1) aligned);

	paper_len = cv_dec_check_ (token_value, code);
	if code ^= 0
	then do;
	     call statement_error (10, token_value);
	     return ("0"b);
	end;
	if (paper_len < 10) | (paper_len > 127)
	then do;
	     call statement_error (10, token_value);
	     return ("0"b);
	end;
	return ("1"b);

     end legal_paper_len;



legal_paper_wdth:
     proc returns (bit (1) aligned);

	paper_wdth = cv_dec_check_ (token_value, code);
	if code ^= 0
	then do;
	     call statement_error (11, token_value);
	     return ("0"b);
	end;
	if (paper_wdth < 1) | (paper_wdth > 200)
	then do;
	     call statement_error (11, token_value);
	     return ("0"b);
	end;

	if (paper_wdth > 136)
	then call statement_error (7, token_value);
	return ("1"b);

     end legal_paper_wdth;



legal_lpi:
     proc returns (bit (1) aligned);

	lpi = cv_dec_check_ (token_value, code);
	if code ^= 0
	then do;
	     call statement_error (12, token_value);
	     return ("0"b);
	end;

	if (lpi = 6) | (lpi = 8)
	then return ("1"b);

	call statement_error (12, token_value);
	return ("0"b);

     end legal_lpi;

legal_line_no:
     proc returns (bit (1) aligned);

	line_no = cv_dec_check_ (token_value, code);
	if code ^= 0
	then do;
	     call statement_error (13, token_value);
	     return ("0"b);
	end;
	if (line_no < 1) | (line_no > 127)
	then do;
	     call statement_error (13, token_value);
	     return ("0"b);
	end;
	if line_no > max_line_no
	then max_line_no = line_no;

	return ("1"b);

     end legal_line_no;



legal_chn_no:
     proc returns (bit (1) aligned);

	chn_no = cv_dec_check_ (token_value, code);
	if code ^= 0
	then do;
	     call statement_error (3, token_value);
	     return ("0"b);
	end;
	if (chn_no < 1) | (chn_no > 16)
	then do;
	     call statement_error (3, token_value);
	     return ("0"b);
	end;
	return ("1"b);

     end legal_chn_no;

legal_wait_time:
     proc returns (bit (1) aligned);

	wait_time = cv_dec_check_ (token_value, code);
	if code ^= 0
	then do;
	     call statement_error (17, token_value);
	     return ("0"b);
	end;
	if (wait_time < 30) | (wait_time > 300)
	then do;
	     call statement_error (17, token_value);
	     return ("0"b);
	end;
	return ("1"b);

     end legal_wait_time;

semant_error:
     proc (error_num, parm1, parm2);

dcl  error_num fixed bin;
dcl  parm1 fixed bin;
dcl  parm2 fixed bin;

	call lex_error_ (error_num, SERROR_PRINTED (error_num), (error_control_table.severity (error_num)),
	     MERROR_SEVERITY, null, null, SERROR_CONTROL, (error_control_table.message (error_num)),
	     (error_control_table.brief_message (error_num)), parm1, parm2);

     end semant_error;



statement_error:
     proc (error_num, string1);

dcl  error_num fixed bin;
dcl  string1 char (*);
dcl  stmt_ptr ptr;
dcl  token_ptr ptr;

	stmt_ptr = token.Pstmt;
	token_ptr = Pthis_token;

	call lex_error_ (error_num, SERROR_PRINTED (error_num), (error_control_table.severity (error_num)),
	     MERROR_SEVERITY, stmt_ptr, token_ptr, SERROR_CONTROL, (error_control_table.message (error_num)),
	     (error_control_table.brief_message (error_num)), string1);

     end statement_error;				/*						*/

/* ERROR MESSAGES */

dcl  1 error_control_table (18) aligned static options (constant),
       2 severity fixed bin unal init (/* 1 - 4 */ (4) 3,	/* 5 - 7 */
	  (3) 1,					/* 8 - 18 */
	  (11) 3),
       2 Soutput_stmt bit (1) unal init (/* 1 */ "1"b,	/* 2 */
	  "0"b,					/* 3 - 4 */
	  (2) (1)"1"b,				/* 5 - 6 */
	  (2) (1)"0"b,				/* 7 - 18 */
	  (12) (1)"1"b),
       2 message char (100) var init (/* 1 */ "Illegal keyword ""^a"".",
						/* 2 */
	  "Premature end of source segment encountered. Check for missing end statement.",
						/* 3 */
	  "Illegal channel number ""^a"".",		/* 4 */
	  "Syntax error in ""^a"" statement.",		/* 5 */
	  "Paper width should be at least 10 when using standard banners.",
						/* 6 */
	  "Paper length must be at least ""^d"" at ""^d"" lines per inch when using standard banners.",
						/* 7 */
	  "Paper width ""^a"" greater than standard printer platen of 136.",
						/* 8 */
	  "Unused",				/* 9 */
	  "Channel stop specified for line ""^d"" which is greater than paper length ""^d"".",
						/* 10 */
	  "Illegal paper length ""^a"".",		/* 11 */
	  "Illegal paper width ""^a"".",		/* 12 */
	  "Illegal lines per inch ""^a"".",		/* 13 */
	  "Illegal line number ""^a"".",		/* 14 */
	  "Illegal banner bars specifier ""^a"".",	/* 15 */
	  "Illegal driver attribute ""^a"".",		/* 16 */
	  "Illegal control attribute ""^a"".",		/* 17 */
	  "Illegal wait_time ""^a"".",		/* 18 */
	  "Illegal banner type ""^a""."),
       2 brief_message char (20) var init (/* 1 */ "^a",	/* 2 */
	  "Premature EOF.",				/* 3 - 4 */
	  (2) (1)"^a",				/* 5 */
	  "",					/* 6 */
	  "",					/* 7 */
	  "",					/* 8 */
	  "",					/* 9 */
	  "^d ^d",				/* 10 - 18 */
	  (9) (1)"^a");
