/* ***********************************************************
   *                                                         *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1981 *
   *                                                         *
   *                                                         *
   *********************************************************** */


/* Modified on 10/27/82 by FCH, [5.1-1], term subchannel name can only start with a/b/c/d, BUG14117(phx14117) */
/* Modified on 04/23/81 by FCH, [4.4-1], accept minus in station names, BUG468 */
/* Modified on 03/03/81 by FCH, [4.4-2], once per process initialization, BUG468 */
/* Modified on 02/27/81 by FCH, [4.4-1], BUG 467(TR9227), terminal name check */
/* Modified since Version 4.3 */

/* This procedure converts an ASCII list of terminal subchannels and
   their correspnding default station names to a binary control segment */

/*++

   BEGIN	/ <valid_terminal> :	/ LEX (2)			/ station \
	/ end ;			/ 			/ RETURN \
	/ <any-token>		/ ERROR (1) NEXT_STMT	/ BEGIN \
	/ <no-token>		/ ERROR (2)		/ RETURN \

   station / <valid_station> ;	/ add LEX (2)		/ BEGIN \
	/ <any-token>		/ ERROR (1) NEXT_STMT	/ BEGIN \
	/ <no-token>		/ ERROR (2)		/ RETURN \

++*/

cv_cmcs_terminal_ctl: proc;

dcl  new_station_name char (12),
     new_terminal_name char (8),
     j fixed bin,
     aclinfo_ptr ptr,				/* for use by tssi_ */
     temp3 char (3);

%include cmcs_control_hdr;

%include cmcs_station_ctl;

%include cmcs_terminal_ctl;

%include cmcs_entry_dcls;

/* automatic */


declare (APstmt, APtoken) ptr,
         area_ptr ptr,				/* for use by lex_string_. */
         arg_length fixed bin (21),			/* length of command argument. */
         arg_ptr ptr,				/* ptr to command argument */
         bitcount fixed bin (24),
         code fixed bin (35),
         dname char (168),
         ename char (32),
         i fixed bin,
         n_chars fixed bin (21),
         object_name char (32),			/* entry name of output control seg */
        (pntep, object_ptr) ptr,			/* ptrs to base of pnte and pnt */
         source_ptr ptr;				/* ptr to base of persmf */

/* based */

declare  arg_string char (arg_length) based (arg_ptr) unaligned;

/* builtin */

declare (addr, collate, dimension, divide, index, length, null,
         reverse, string, substr, verify) builtin;

/* conditions */

declare  cleanup condition;

/* entries */

declare
         clock_ entry () returns (fixed bin (71)),
         com_err_ entry options (variable),
         cu_$arg_ptr entry (fixed bin, ptr, fixed bin (21), fixed bin (35)),
         cv_dec_check_ entry (char (*), fixed bin) returns (fixed bin (35)),
         expand_pathname_ entry (char (*), char (*), char (*), fixed bin (35)),
         get_group_id_ entry () returns (char (32) aligned),
         get_process_id_ entry () returns (bit (36)),
         get_wdir_ entry () returns (char (168) aligned),
         hcs_$delentry_seg entry (ptr, fixed bin (35)),
         hcs_$initiate entry (char (*), char (*), char (*), fixed bin (1), fixed bin (2), ptr, fixed bin (35)),
         hcs_$initiate_count entry (char (*), char (*), char (*), fixed bin (24), fixed bin (1), ptr, fixed bin (35)),
         hcs_$make_seg entry (char (*), char (*), char (*), fixed bin (5), ptr, fixed bin (35)),
         hcs_$set_bc_seg entry (ptr, fixed bin (24), fixed bin (35)),
         hcs_$terminate_noname entry (ptr, fixed bin (35)),
         hcs_$truncate_seg entry (ptr, fixed bin (18), fixed bin (35)),
         lex_error_ entry options (variable),
         lex_string_$init_lex_delims entry (char (*), char (*), char (*), char (*), char (*),
         bit (*), char (*) var, char (*) var, char (*) var, char (*) var),
         lex_string_$lex entry (ptr, fixed bin (21), fixed bin, ptr, bit (*), char (*), char (*), char (*),
         char (*), char (*), char (*) var, char (*) var, char (*) var, char (*) var, ptr, ptr, fixed bin (35)),
         translator_temp_$get_segment entry (char (*), ptr, fixed bin (35)),
         translator_temp_$release_all_segments entry (ptr, fixed bin (35)),

         tssi_$get_segment entry (char (*), char (*), ptr, ptr, fixed bin (35)),
         tssi_$finish_segment entry (ptr, fixed bin (24), bit (36) aligned, ptr, fixed bin (35)),
         tssi_$clean_up_segment entry (ptr),

         unique_chars_ entry (bit (*)) returns (char (15) aligned);

/* internal static */

declare ((BREAKS, IGBREAKS, LEXCTL, LEXDLM) char (128) varying,
/*[4.4-2]*/         first_time bit (1) aligned initial ("1"b)) int static;

dcl (LEGAL char (71) aligned initial ("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'_-^`~ ."),
     my_name char (20) initial ("cv_cmcs_terminal_ctl"),
     ALPHANUMERICS char (64) init ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-") /*[4.4-1]*/
     ) internal static options (constant);

/* external static */

declare ((error_table_$badopt, error_table_$entlong,
         error_table_$bad_name, error_table_$translation_failed) fixed bin (35),
         sys_info$max_seg_size fixed bin (18)
         ) external static;


/* program */

	call cu_$arg_ptr (1, arg_ptr, arg_length, code);

	if code ^= 0
	then do;

	     call com_err_ (code, my_name, "Usage: cv_cmcs_terminal_ctl pathname (-brief|-bf|-long|-lg)");
	     return;

	end;

	call expand_pathname_ (arg_string, dname, ename, code);

	if code ^= 0
	then do;

	     call com_err_ (code, my_name, "^a", arg_string);
	     return;

	end;

	call cu_$arg_ptr (2, arg_ptr, arg_length, code);

	if code = 0
	then if arg_string = "-brief" | arg_string = "-bf"
	     then SERROR_CONTROL = "01"b;
	     else if arg_string = "-long" | arg_string = "-lg"
	     then SERROR_CONTROL = "10"b;
	     else do;

		call com_err_ (error_table_$badopt, my_name, "^a", arg_string);
		return;

	     end;

	source_ptr = null;				/* Initialize for cleanup handler */
	object_ptr = null;				/* .. */
	area_ptr = null;				/* .. */
	aclinfo_ptr = null;				/* .. */

	on cleanup call clean_up;

	call hcs_$initiate_count (dname, ename, "", bitcount, 1b, source_ptr, code);

	if source_ptr = null
	then do;

report_error:

	     call com_err_ (code, my_name, "^a>^a", dname, ename);
	     return;

	end;

	i = index (ename, ".src") - 1;

	if i < 1 then do;

	     call com_err_ (error_table_$bad_name, my_name, "Source segment must have "".src"" suffix.");
	     return;

	end;

	if i + length (".control") > length (object_name)
	then do;

	     code = error_table_$entlong;
	     go to report_error;

	end;

	object_name = substr (ename, 1, i) || ".control";

	n_chars = divide (bitcount + 8, 9, 24, 0);

	dname = get_wdir_ ();

	call tssi_$get_segment (dname, object_name, object_ptr, aclinfo_ptr, code);

	if code ^= 0
	then do;

	     call com_err_ (code, my_name, "^a>^a", dname, object_name);
	     return;

	end;

	terminal_ctl_ptr = object_ptr;		/* actual working ptr - other is generic ptr */

	call cmcs_fillin_hdr_ (terminal_ctl_ptr, terminal_ctl_version, terminal_ctl_hdr_len, terminal_ctl_entry_len, code);

	if code ^= 0
	then do;

	     call com_err_ (code, my_name, "Setting common header data.");
	     return;

	end;

/*[4.4-2]*/	if first_time
/*[4.4-2]*/	then	do;

	     BREAKS = substr (collate, 1, 8) || substr (collate, 10, 24) || ":,()";
	     IGBREAKS = substr (BREAKS, 1, 8+24);

	     call lex_string_$init_lex_delims ("""", """", "/*", "*/", ";", "10"b,
		BREAKS, IGBREAKS, LEXDLM, LEXCTL);

	     call hcs_$initiate
		(get_wdir_ (), "cmcs_station_ctl.control", "cmcs_station_ctl.control", 0, 0, station_ctl_ptr, code);

	     if station_ctl_ptr = null ()
	     then do;

		call com_err_ (code, my_name,
		     "The cmcs_station_ctl.control segment must exist in the current working directory before this command can be run.");
		return;

	     end;

/*[4.4-2]*/			first_time = "1"b;

/*[4.4-2]*/		end;

	call translator_temp_$get_segment (my_name, area_ptr, code);

	if area_ptr = null ()
	then do;

	     call com_err_ (code, my_name, "Making temporary segment in process directory.");
	     return;

	end;

	call lex_string_$lex (source_ptr, n_chars, 0, area_ptr, "1000"b, """", """", "/*", "*/", ";",
	     BREAKS, IGBREAKS, LEXDLM, LEXCTL, APstmt, APtoken, code);

	if code ^= 0
	then do;

	     call com_err_ (code, my_name, ename);
	     return;

	end;

	Pthis_token = APtoken;

	call SEMANTIC_ANALYSIS ();

	if MERROR_SEVERITY > 1
	then do;

	     call com_err_ (error_table_$translation_failed, my_name, ename);
	     call hcs_$delentry_seg (object_ptr, code);

	end;
	else do;

	     bitcount = 36 * (terminal_ctl_hdr_len + terminal_ctl_entry_len * terminal_ctl.current_size);
	     call tssi_$finish_segment (object_ptr, bitcount, "100"b, aclinfo_ptr, code);

	     if code ^= 0
	     then call com_err_ (code, my_name, "Unable to set bitcount on ^a>^a to ^d", dname, object_name, bitcount);

	end;

	call clean_up;				/* terminate input segments */

	return;

/* Clean up procedure. Called if command is "quit" out of, and at end of normal processing. */

clean_up:
	procedure;

	     if source_ptr ^= null
	     then call hcs_$terminate_noname (source_ptr, code);

	     if object_ptr ^= null
	     then call hcs_$terminate_noname (object_ptr, code);

	     if area_ptr ^= null
	     then call translator_temp_$release_all_segments (area_ptr, code);

	     if aclinfo_ptr ^= null
	     then call tssi_$clean_up_segment (aclinfo_ptr);

	end /* clean_up */ ;




declare 1 error_control_table (2) aligned internal static,
        2 severity fixed bin (17) unaligned initial (
       (2)3),
        2 Soutput_stmt bit (1) unaligned initial (
         "1"b,
         "0"b),
        2 message char (64) varying initial (
         "Syntax error in ""^a"" statement.",
         "Premature end of input encountered."),
        2 brief_message char (20) varying initial (
         "^a",
         "Premature EOF.");

/*  */

valid_terminal: proc () returns (bit (1) aligned);

/*[4.4-1]*/
			/* name of communications channel, see CC92, Apendix A */
	i = length(token_value);

	if i < 6 | i > 32 then return("0"b);

/*[5.1-1]*/	i = index("abcdefghijklmnopqrst",substr(token_value,1,1));
	if i <= 0 then return("0"b);

	if substr(token_value,2,1) ^= "." then return("0"b);

	i = index("lh",substr(token_value,3,1));
	if i <= 0 then return("0"b);

	i = cv_dec_check_(substr(token_value,4,1),j);
	if j ^= 0 then return("0"b);

	if substr(token_value,3,1) = "h"
	then	do;	if i > 5 then return("0"b);	end;
	else	do;	if i > 2 then return("0"b);	end;

	i = cv_dec_check_(substr(token_value,5,2),j);

	if j ^= 0 then return("0"b);

/*[4.4-1]*/


	     new_terminal_name = token_value;

	     return ("1"b);

	end /* valid_terminal */ ;



valid_station: proc () returns (bit (1) aligned);

	     if token_value = "end" then return ("0"b);	/* special case */

	     if length (token_value) > 12 then return ("0"b);

	     if verify (token_value, ALPHANUMERICS) ^= 0 then return ("0"b);

	     new_station_name = token_value;

	     do station_ctl_eindex = 1 to station_ctl.current_size;

		station_ctl_eptr = addr (station_ctl.entries (station_ctl_eindex));

		if ^station_ctl_entry.inactive_sw
		then if station_ctl_entry.station_name = new_station_name
		     then return ("1"b);

	     end;

	     return ("0"b);

	end /* valid_station */ ;

/*  */

add:	proc ();

	     terminal_ctl.entry_count, terminal_ctl.current_size = terminal_ctl.current_size + 1;
	     string (terminal_ctl.flags (terminal_ctl.current_size)) = (36) "0"b;
	     terminal_ctl.device_channel (terminal_ctl.current_size) = new_terminal_name;
	     terminal_ctl.station_name (terminal_ctl.current_size) = new_station_name;

	     return;

	end /* add */ ;

