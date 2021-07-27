/* ***********************************************************
   *                                                         *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1981 *
   *                                                         *
   *                                                         *
   *********************************************************** */


/* Modified on 10/02/81 by FCH, incorrect diags sometimes generated, BUG512 */
/* Modified on 06/01/81 by FCH, [4.4-2], once per process initialization, BUG468 */
/* Modified on 04/22/81 by FCH, [4.4-1], accept 01 as level number, accept minus in queue names, BUG468 */
/* Modified since Version 4.3 */


/* This procedure converts the ASCII definition of a COBOL MCS queue hierarchy
   into its binary representation.
*/
/*++

 BEGIN		/ dcl 1			/			/ do_init \
		/ dcl 01			/			/ do_init \
		/ declare 1		/			/ do_init \
		/ declare 01		/			/ do_init \
		/ end ;			/ close_db		/ fini \
		/ <any-token>		/ ERROR (1) NEXT_STMT	/ BEGIN \
		/ <no-token>		/ ERROR (2)		/ abort \

 do_init		/			/ init_tree LEX (2) 	/ get_level_name \

 get_level_name	/ <valid_level_name> 	/ set_level_name LEX (1)	/ follow \
		/ <any-token>		/ ERROR (3) NEXT_STMT	/ BEGIN \
		/ <no-token>		/ ERROR (2)		/ abort \

 follow		/ ,			/ close_tree_level LEX (1)	/ get_level_no \
		/ ;			/ close_tree LEX (1)	/ BEGIN \
		/ queue_name		/ LEX (1) 		/ get_queue_name \
		/ command_line		/ LEX (1) 		/ get_command_line \
		/ mp_line			/ LEX (1) 		/ get_mp_line \
		/ cobol_program_id		/ LEX (1) 		/ get_program_id \
 follow_error	/ <any-token>		/ ERROR (4) NEXT_STMT	/ BEGIN \
		/ <no-token>		/ ERROR (2)		/ abort \

 get_queue_name	/ <valid_queue_name>	/ set_queue_name LEX (1) 	/ follow \
		/ <any-token>		/ ERROR (5) NEXT_STMT	/ BEGIN \
		/ <no-token>		/ ERROR (6)		/ abort \

 get_command_line	/ <quoted-string>		/ set_command_line LEX (1)	/ follow \
		/ <any-token>		/ ERROR (10) NEXT_STMT	/ BEGIN \
		/ <no-token>		/ ERROR (7)		/ abort \

 get_mp_line	/ <quoted-string>		/ set_mp_line LEX (1)	/ follow \
		/ <any-token>		/ ERROR (10) NEXT_STMT	/ BEGIN \
		/ <no-token>		/ ERROR (1)		/ abort \

 get_program_id	/ <valid_program_id>	/ set_program_id LEX (1)	/ follow \
		/ <any-token>		/ ERROR (11) NEXT_STMT	/ BEGIN \
		/ <no-token>		/ ERROR (1)		/ abort \

 get_level_no	/ <valid_level>		/ open_tree_level LEX (1)	/ get_level_name \
		/ <any-token>		/ ERROR (8) NEXT_STMT	/ BEGIN \
		/ <no-token>		/ ERROR (9)		/ abort \

 abort		/			/			/ RETURN \
 fini		/			/			/ RETURN \

   ++*/

cv_cmcs_tree_ctl: proc;

dcl  new_station_name char (12),
     new_terminal_name char (8),
     j fixed bin,
     aclinfo_ptr ptr,				/* for use by tssi_ */
     queue_name char (32),
     temp3 char (3);

/*  */
%include cmcs_control_hdr;
%include cmcs_entry_dcls;
%include cmcs_station_ctl;
%include cmcs_tree_ctl;
%include cmcs_vfile_rs;

/*  */

/* automatic */

/* levels structure, used to keep control information until the complete level entry is
   ready to be inserted into the tree_ctl structure */

dcl (current_level, previous_level, queue_level) fixed bin;


dcl 1 levels (4),
    2 flags,
     (3 cmd_sw bit (1),
      3 mp_sw bit (1),
      3 cobol_program_id_sw bit (1),
      3 queue_sw bit (1),
      3 filler bit (33)) unaligned,
    2 tree_entry_index fixed bin,
    2 subtree_count fixed bin,
    2 level_name char (12),
    2 queue_name char (32),
    2 cmd_line_len fixed bin,
    2 cmd_line char (128),
    2 mp_line_len fixed bin,
    2 mp_line char (128),
    2 cobol_program_id_len fixed bin,
    2 cobol_program_id char (128);


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

dcl  tree_ctl_entry_overlay (size (tree_ctl_entry)) fixed bin based (tree_ctl_eptr);
						/* used to zero out the entry before setting */

/* builtin */

declare (addr, collate, dimension, divide, index, length, null,
         reverse, size, string, substr, verify) builtin;

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
         hcs_$initiate_count entry (char (*), char (*), char (*), fixed bin (24), fixed bin (1), ptr, fixed bin (35)),
         hcs_$make_seg entry (char (*), char (*), char (*), fixed bin (5), ptr, fixed bin (35)),
         hcs_$set_bc_seg entry (ptr, fixed bin (24), fixed bin (35)),
         hcs_$terminate_noname entry (ptr, fixed bin (35)),
         hcs_$truncate_seg entry (ptr, fixed bin (18), fixed bin (35)),
        (ioa_, ioa_$ioa_switch) entry options (variable),
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
         test_sw bit (1) init ("0"b),
/*[4.4-2]*/         first_time bit (1) aligned initial ("1"b)) int static;

dcl (LEGAL char (71) aligned initial ("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'_-^`~ ."),
     my_name char (16) initial ("cv_cmcs_tree_ctl"),
     alphanumerics char (64) init ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-")  /*[4.4-1]*/
     ) internal static options (constant);
dcl  letters char (52) defined (alphanumerics);

/* external static */

declare ((error_table_$badopt, error_table_$entlong,
         error_table_$bad_name, error_table_$translation_failed) fixed bin (35),
         sys_info_$max_seg_size fixed bin (18)
         ) external static;


/*  */

	call cu_$arg_ptr (1, arg_ptr, arg_length, code);

	if code ^= 0
	then do;

	     call com_err_ (code, my_name, "Usage: cv_cmcs_tree_ctl pathname (-brief|-bf|-long|-lg)");
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

/*[5.0-1]*/	current_level,queue_level = 0;

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

/* Initialize Header Info */

	tree_ctl_ptr = object_ptr;			/* actual working ptr - other is generic ptr */

	call cmcs_fillin_hdr_ (object_ptr, tree_ctl_version, tree_ctl_hdr_len, tree_ctl_entry_len, code);

	if code ^= 0 then call com_err_ (code, my_name, "Continuing compilation.");

	tree_ctl.queue_count = 0;			/* not part of common hdr */

/* */

/*[4.4-2]*/	if first_time
	then do;

	     BREAKS = substr (collate, 1, 8) || substr (collate, 10, 24) || ":,()";
	     IGBREAKS = substr (BREAKS, 1, 8+24);

	     call lex_string_$init_lex_delims ("""", """", "/*", "*/", ";", "10"b,
		BREAKS, IGBREAKS, LEXDLM, LEXCTL);

/*[4.4-2]*/	     first_time = "1"b;

	end;

	call translator_temp_$get_segment (my_name, area_ptr, code);

	if area_ptr = null
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

	     bitcount = 36 * (tree_ctl_hdr_len + tree_ctl_entry_len * tree_ctl.current_size);

	     call tssi_$finish_segment (object_ptr, bitcount, "101"b, aclinfo_ptr, code); /* rw, still needs copysw */

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



declare 1 error_control_table (11) aligned internal static,
        2 severity fixed bin (17) unaligned initial (
		     (11) 3),
        2 Soutput_stmt bit (1) unaligned initial (
         "1"b,
     (10) (1) "0"b),
        2 message char (96) varying initial (
         "New declarations must begin with ""declare 01"" or ""dcl 01"": ^a", /* 01 */
         "Premature end of input encountered.",		/* 02 */
         "Invalid level name: ^a",			/* 03 */
         "Level name must be followed by "","", queue, command, mp, or "";""", /* 04 */
         "Invalid queue name: ^a",			/* 05 */
         "Unexpected EOF in source segment. Looking for queue name. ^a", /* 06 */
         "Unexpected EOF in source segment. Looking for command line. ^a", /* 07 */
         "Invalid tree level: ^a",			/* 08 */
         "Unexpected EOF in source segment. Looking for tree level number. ^a", /* 09 */
         "Need quoted string for command or mp line: ^a",	/* 10 */
         "Bad program-id for cobol_program_id: ^a"),	/* 11 */
        2 brief_message char (24) varying initial (
         "Bad Declare: ^a",				/* 01 */
         "Unexpected EOF",				/* 02 */
         "Bad level name: ^a",			/* 03 */
         "Bad level args: ^a",			/* 04 */
         "Bad Queue Name: ^a",			/* 05 */
         "Msg Queue Name: ^a",			/* 06 */
         "Msg Command Line: ^a",			/* 07 */
         "Bad Tree Level: ^a",			/* 08 */
         "Msg Tree Level: ^a",			/* 09 */
         "Need quoted string ^a",			/* 10 */
         "Bad program-id ^a");			/* 11 */

/*  */

/* */

valid_level: proc () returns (bit (1) aligned);

	     if test_sw
	     then call ioa_ ("Parse: valid_level: ""^a"".", token_value);

	     i = cv_dec_check_ (token_value, j);

	     if j ^= 0 then return ("0"b);

	     if (i < 1 | i > 4) then return ("0"b);

	     if i > current_level
	     then do;

		if i > current_level + 1 then return ("0"b);
		if queue_level = current_level then return ("0"b);

	     end;
	     else do;				/* new level <= current level */

		if queue_level = 0 then return ("0"b);	/* didn't specify a queue name for abs tree path */
		else if queue_level ^= current_level then return ("0"b); /* should never find this */

		queue_level = 0;			/* last level had good queue, set up for next time */

	     end;

	     previous_level = current_level;
	     current_level = i;
	     return ("1"b);

	end /* valid_level */ ;

/* */

valid_program_id: proc () returns (bit (1) aligned);

	     if test_sw
	     then call ioa_ ("Parse: valid_program_id: ""^a"".", token_value);

	     if length (token_value) > 30 then return ("0"b); /* COBOL variables limited to 30 chars */

	     if verify (token_value, alphanumerics) > 0 then return ("0"b);

	     if index (letters, substr (token_value, 1, 1)) = 0 then return ("0"b); /* 1st char must be letter */

	     return ("1"b);

	end /* valid_program_id */ ;

/* */

valid_queue_name: proc () returns (bit (1) aligned);

	     if test_sw
	     then call ioa_ ("Parse: valid_queue_name: ""^a"".", token_value);

	     if length (token_value) > 21 then return ("0"b); /* COBOL queue names limited to 21 chars, plus suffix */

	     if verify (token_value, alphanumerics) > 0 then return ("0"b);

	     if index (letters, substr (token_value, 1, 1)) = 0 then return ("0"b); /* 1st char must be letter */

	     return ("1"b);

	end /* valid_queue_name */ ;

/* */

valid_level_name: proc () returns (bit (1) aligned);

	     if test_sw
	     then call ioa_ ("Parse: valid_level_name: ""^a"".", token_value);

	     if length (token_value) > 12 then return ("0"b); /* COBOL variables limited to 12 chars */

	     if verify (token_value, alphanumerics) > 0 then return ("0"b);

	     if index (letters, substr (token_value, 1, 1)) = 0 then return ("0"b); /* 1st char must be letter */

	     return ("1"b);

	end /* valid_level_name */ ;

/* */

close_db:	proc ();

	     if test_sw
	     then call ioa_ ("Semantics: close_db: ""^a"".", token_value);

	     return;

	end /* close_db */ ;

/* */

close_tree: proc ();

	     if test_sw
	     then call ioa_ ("Semantics: close_tree: ""^a"".", token_value);

	     call close_tree_level;

	end /* close_tree */ ;

/* */

close_tree_level: proc ();

	     if test_sw
	     then call ioa_ ("Semantics: close_tree_level: ""^a"".", token_value);

	     i = levels (current_level).tree_entry_index; /* get location of tree_ctl_entry */
	     tree_ctl_eptr = addr (tree_ctl.entries (i)); /* for based operations */
	     tree_ctl_entry_overlay (*) = 0;		/* wipe the slate clean */

	     tree_ctl_entry.level_no = current_level;	/* for perusing the tree elsewhere */
	     tree_ctl_entry.cmd_sw = levels (current_level).cmd_sw;
	     tree_ctl_entry.queue_sw = levels (current_level).queue_sw;
	     tree_ctl_entry.mp_sw = levels (current_level).mp_sw;
	     tree_ctl_entry.cobol_program_id_sw = levels (current_level).cobol_program_id_sw;

	     queue_name,
		tree_ctl_entry.queue_name = levels (current_level).queue_name;
	     tree_ctl_entry.cmd_line_len = levels (current_level).cmd_line_len;
	     tree_ctl_entry.mp_line_len = levels (current_level).mp_line_len;
	     tree_ctl_entry.cmd_line = levels (current_level).cmd_line;

/* set ptr variables to null () for subsequent testing */

	     tree_ctl_entry.queue_ctl_eptr,
		tree_ctl_entry.iocb_ptr,
		tree_ctl_entry.msg_hdr_ptr,
		tree_ctl_entry.msg_seg_ptr,
		tree_ctl_entry.buffer_ptr,
		tree_ctl_entry.tseg_ptr = null ();

	     tree_ctl_entry.switch_name = "";		/* so we dont print junk for unused entries */

	     tree_ctl_entry.mp_line = levels (current_level).mp_line;
	     tree_ctl_entry.cobol_program_id_len = levels (current_level).cobol_program_id_len;
	     tree_ctl_entry.cobol_program_id = levels (current_level).cobol_program_id;

	     do i = 1 to 4;				/* copy all level names, including blank trailing names */

		tree_ctl_entry.level_names (i) = levels (i).level_name;

	     end;

	     do i = 1 to current_level;		/* copy all the subtree counts */

		j = levels (i).tree_entry_index;	/* index into tree_ctl for the given entry */
		tree_ctl.entries (j).subtree_count = levels (i).subtree_count;

	     end;

	     if tree_ctl_entry.queue_sw		/* if entry is for queue, bump count */
	     then do;				/* it's an entry for a queue */

		do j = 1 to tree_ctl.current_size - 1;

		     if queue_name = tree_ctl.entries (j).queue_name
		     then do;

			tree_ctl_entry.queue_ctl_eindex = tree_ctl.entries (j).queue_ctl_eindex;
						/* point to the first occurrance */
			go to close_tree_level_ret;
		     end;
		end;

		tree_ctl_entry.queue_ctl_eindex,
		     tree_ctl.queue_count = tree_ctl.queue_count + 1; /* drop-thru means first occurance */
	     end;

close_tree_level_ret:
	     return;

	end /* close_tree_level */ ;

/* */

init_tree: proc ();

	     if test_sw
	     then call ioa_ ("Semantics: init_tree: ""^a"".", token_value);

	     current_level, previous_level = 1;		/* initialize for new set */
	     call open_tree_level;

	     return;

	end /* init_tree */ ;

/* */

open_tree_level: proc ();

	     if test_sw
	     then call ioa_ ("Semantics: open_tree_level: ""^a"".", token_value);

	     tree_ctl.current_size, tree_ctl.entry_count = tree_ctl.current_size + 1; /* next place to store an entry */
	     levels (current_level).tree_entry_index = tree_ctl.current_size; /* remember it */

	     if current_level = 1
	     then do;

		string (levels (1).flags) = (36) "0"b;

		levels (1).level_name, levels (2).level_name, levels (3).level_name, levels (4).level_name,
		     levels (1).queue_name,
		     levels (1).cmd_line,
		     levels (1).mp_line,
		     levels (1).cobol_program_id = "";

		levels (1).subtree_count,
		     levels (1).cmd_line_len,
		     levels (1).mp_line_len,
		     levels (1).cobol_program_id_len = 0;

	     end;
	     else do;				/* current_level ^= 1 */

		do j = 1 to current_level - 1;

		     levels (j).subtree_count = levels (j).subtree_count + 1; /* bump all ancestor counts by 1 */

		end;

		if current_level ^= 4		/* clear out all following level names */
		then do i = current_level + 1 to 4;	/* just the trailing fields */

		     levels (i).level_name = "";

		end;

		j = current_level - 1;		/* copy from prev level, newer args overlay */

		string (levels (current_level).flags) = string (levels (j).flags);
		levels (current_level).level_name = levels (j).level_name;
		levels (current_level).queue_name = levels (j).queue_name;
		levels (current_level).subtree_count = 0;
		levels (current_level).cmd_line_len = levels (j).cmd_line_len;
		levels (current_level).cmd_line = levels (j).cmd_line;
		levels (current_level).mp_line_len = levels (j).mp_line_len;
		levels (current_level).mp_line = levels (j).mp_line;
		levels (current_level).cobol_program_id_len = levels (j).cobol_program_id_len;
		levels (current_level).cobol_program_id = levels (j).cobol_program_id;

		if current_level > previous_level
		then if previous_level = queue_level
		     then call ioa_ ("Warning: higher level follows queue_name level.");
		     else;
		else queue_level = 0;		/* ok - reset for next time */

	     end;

	     return;

	end /* open_tree_level */ ;

/* */

set_mp_line: proc ();

	     if test_sw
	     then call ioa_ ("Semantics: set_mp_line: ""^a"".", token_value);

	     if length (token_value) > 128
	     then do;

		levels (current_level).mp_line_len = 128; /* truncate and push on to catch other errors */
		levels (current_level).mp_line = substr (token_value, 1, 128);

		if test_sw
		then call ioa_ ("Warning: mp line truncated to 128 chars. Continuing.");

	     end;

	     else do;

		levels (current_level).mp_line_len = length (token_value);
		levels (current_level).mp_line = token_value;

	     end;

	     levels (current_level).mp_sw = "1"b;

	     return;

	end /* set_mp_line */ ;

/* */

set_command_line: proc ();

	     if test_sw
	     then call ioa_ ("Semantics: set_command_line: ""^a"".", token_value);

	     if length (token_value) > 128
	     then do;

		levels (current_level).cmd_line_len = 128; /* truncate and push on to catch other errors */
		levels (current_level).cmd_line = substr (token_value, 1, 128);

		if test_sw
		then call ioa_ ("Warning: command line truncated to 128 chars. Continuing.");

	     end;
	     else do;

		levels (current_level).cmd_line_len = length (token_value);
		levels (current_level).cmd_line = token_value;

	     end;
	     levels (current_level).cmd_sw = "1"b;

	     call ioa_ ("Warning: The command_line arguments are ignored in this version."); /* just so they know */

	     return;

	end /* set_command_line */ ;

/* */

set_program_id: proc ();

	     if test_sw
	     then call ioa_ ("Semantics: set_program_id: ""^a"".", token_value);

	     levels (current_level).cobol_program_id_len = length (token_value);
	     levels (current_level).cobol_program_id = token_value;

	     levels (current_level).cobol_program_id_sw = "1"b;
	     return;

	end /* set_program_id */ ;

/* */

set_queue_name: proc ();

	     if test_sw
	     then call ioa_ ("Semantics: set_queue_name: ""^a"".", token_value);

	     levels (current_level).queue_name = token_value;
	     queue_level = current_level;		/* to check that queue was given when needed */
	     levels (current_level).queue_sw = "1"b;

	     return;

	end /* set_queue_name */ ;

/* */

set_level_name: proc ();

	     if test_sw
	     then call ioa_ ("Semantics: set_level_name: ""^a"".", token_value);

	     levels (current_level).level_name = token_value;

	     return;

	end /* set_level_name */ ;

test:	entry;					/* used to print out parse and semantics calls */

	test_sw = "1"b;
	return;

