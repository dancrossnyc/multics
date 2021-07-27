/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1983 *
   *                                                         *
   *********************************************************** */

/*  DESCRIPTION:
	dm_translate_system_config_ - program to extract DMS configuration
     data from an ascii segment and return a pointer to a structure which
     summarizes the configuration parameters.
*/

/* HISTORY:
Written by M. Pandolf, December 1982.
Modified:
03/15/83 by M. Pandolf:  for default before journal keywords and for
	  logging process terminations.
05/05/83 by L. A. Newcomb:  added recovery_check_mode and fixed the old
	  subsystem_inhibit to be subsystem_disposition.  NOTE:  work
	  needs to be done on error reporting for duplicate settings,
	  such as specifying "^recover, recovery_check_mode".
11/04/83 by M. Pandolf: to check to see if default before journal ends in ".bj"
05/29/84 by Lindsey Spratt: Changed to use version 2 dm_system_config.
	  Removed several elements of the config, to whit; maximum
	  number_of_before_journals, inhibit subsystem, and daemon error
	  trace.  None of these were in use.
06/12/84 by Lindsey Spratt: Added code for shutdown_delay. Changed to check
	  for before_journal_size > 2 instead of 0.
*/
/* format: style4,indattr,ifthenstmt,ifthen,^indcomtxt,idind33 */
%page;
/*
   The DM configuration file contains information used at data management initialization
   time to configure the system to site dependent parameters.  These
   parameters override the values built in to the CDS named
   "dm_system_data_".

   The form of the DM configuration file is as follows:

   <DM config file> ::= [<spec>]...<end statement>
   <spec> ::=           <default before journal size> |
                        <maximum number of processes> |
                        <maximum number of transactions> |
                        <default before journal> |
                        <previous bootload status> |
                        <current bootload enable> |
                        <daemon idle timeout> |
		    <shutdown delay> |
                        <daemon log proc terms>
   <default before journal size> ::=
                        system_before_journal_size: <decimal integer>;
   <maximum number of processes> ::=
                        max_processes: <decimal integer>;
   <maximum number of transactions> ::=
                        max_transactions: <decimal integer>;
   <default before journal> ::=
                        default_before_journal: <path spec>;
   <previous bootload status> ::=
                        prev_bootload_status: <status option>[,<status option>]...;
   <current bootload enable> ::=
                        current_bootload_enable: force | ^force;
   <daemon idle timeout local> ::=
                        idle_timeout: <decimal integer>;
   <shutdown delay> ::=
		    shutdown_delay: <decimal integer>;
   <daemon log proc terms> ::=
                        log_proc_terms: on | off;
   <end statement> ::=  end;

   <path spec> ::=      dir=<path> | entry=<segment name> |
		    dir=<path>,entry=<segment name> |
                        entry=<segment name>,dir=<path> | <null string>

   <status option> ::=  hold | adopt | recover | recovery_check_mode | ^hold | ^adopt | ^recover | ^recovery_check_mode

   <path> ::=           <absolute pathname> | aim_dir | bootload_dir.
/*++

BEGIN
	/ system_before_journal_size : <decimal-integer> ; /
		LEX(2) [	if token.Nvalue > 2
			then dm_system_config.default_bj_size = token.Nvalue;
			else call ERROR(4)]
		LEX(2) / BEGIN \

	/ system_before_journal_size <any-token> /
		LEX(2) ERROR(4) NEXT_STMT / BEGIN \

	/ system_before_journal_size <no-token> /
		ERROR(8) / BEGIN \

	/ max_processes : <decimal-integer> ; /
		LEX(2) [	if token.Nvalue > 0
			then dm_system_config.max_n_proc = token.Nvalue;
			else call ERROR(4)]
		LEX(2) / BEGIN  \

	/ max_processes <any-token> /
		LEX(2) ERROR(4) NEXT_STMT / BEGIN \

	/ max_processes <no-token> /
		ERROR(8) / BEGIN \

	/ max_transactions : <decimal-integer> ; /
		LEX(2) [	if token.Nvalue > 0
			then dm_system_config.max_n_txn = token.Nvalue;
			else call ERROR(4)]
		LEX(2) / BEGIN  \

	/ max_transactions <any-token> /
		LEX(2) ERROR(4) NEXT_STMT / BEGIN \

	/ max_transactions <no-token> /
		ERROR(8) / BEGIN \

	/ idle_timeout : <decimal-integer> ; /
		LEX(2) [	if token.Nvalue > 0
			then dm_system_config.idle_timeout = token.Nvalue;
			else call ERROR(4)]
		LEX(2) / BEGIN  \

	/ idle_timeout <any-token> /
		LEX(2) ERROR(4) NEXT_STMT / BEGIN \

	/ idle_timeout <no-token> /
		ERROR(8) / BEGIN \

	/ shutdown_delay : <date_time_offset> ; /
		LEX(2) [	dm_system_config.shutdown_delay = convert_date_time_offset();
]
		LEX(2) / BEGIN  \

	/ shutdown_delay <any-token> /
		LEX(2) ERROR(4) NEXT_STMT / BEGIN \

	/ shutdown_delay <no-token> /
		ERROR(8) / BEGIN \

	/ default_before_journal : /
		LEX(2) / path_spec \

	/ prev_bootload_status : /
		[save_prev_dm_disp = dm_system_config.prev_dm_disp]
		LEX(2) / status_spec \

	/ current_bootload_enable : force ; /
		[dm_system_config.curr_dm_enable = DM_FORCE_ENABLE_NEW_BOOTLOAD] LEX(4) / BEGIN \

	/ current_bootload_enable : ^force ; /
		[dm_system_config.curr_dm_enable = DM_DO_NOT_FORCE_ENABLE_NEW_BOOTLOAD] LEX(5) / BEGIN \

	/ current_bootload_enable <any-token> /
		LEX(2) ERROR(4) NEXT_STMT / BEGIN \

	/ current_bootload_enable <no-token> /
		ERROR(8) / BEGIN \

	/ log_proc_terms : on ; /
		[dm_system_config.log_proc_terms = "1"b] LEX(4) / BEGIN \

	/ log_proc_terms : off ; /
		[dm_system_config.log_proc_terms = "0"b] LEX(4) / BEGIN \

	/ log_proc_terms <any-token> /
		LEX(2) ERROR(4) NEXT_STMT / BEGIN \

	/ log_proc_terms <no-token> /
		ERROR(8) / BEGIN \

	/ end ; <no-token> /
		return_table / RETURN \

	/ end ; <any-token> /
		ERROR(2) return_table / RETURN \

	/ <any-token> /
		ERROR(1) NEXT_STMT / BEGIN \

	/ <no-token> /
		ERROR(3) return_table / RETURN \

path_spec

	/ ; /
		LEX(1) / BEGIN \

	/ dir = <absolute_path> ; /
		LEX(2) [dm_system_config.default_bj.dir = token_value]
		LEX(2) / BEGIN \

	/ dir = <absolute_path> , /
		LEX(2) [dm_system_config.default_bj.dir = token_value]
		LEX(2) / path_spec \

	/ dir = <any-token> /
		LEX(2) ERROR(9) NEXT_STMT / BEGIN \

	/ entry = <before_journal_name> ; /
		LEX(2) [dm_system_config.default_bj.entry = token_value]
		LEX(2) / BEGIN \

	/ entry = <before_journal_name> , /
		LEX(2) [dm_system_config.default_bj.entry = token_value]
		LEX(2) / path_spec \

	/ entry = <any-token> /
		LEX(2) ERROR(10) NEXT_STMT / BEGIN \

	/ <any-token> /
		ERROR(4) NEXT_STMT / BEGIN \

	/ <no-token> /
		ERROR(8) / BEGIN \

status_spec

	/ hold ; /
		[dm_system_config.prev_dm_disp.hold = DM_HOLD_OLD_BOOTLOAD_DIRECTORY]
		LEX(2) / BEGIN \

	/ hold , /
		[dm_system_config.prev_dm_disp.hold = DM_HOLD_OLD_BOOTLOAD_DIRECTORY]
		LEX(2) / status_spec \

	/ hold <any-token> /
		[dm_system_config.prev_dm_disp = save_prev_dm_disp]
		LEX(1) ERROR(6) NEXT_STMT / BEGIN \

	/ ^hold ; /
		[dm_system_config.prev_dm_disp.hold = DM_DO_NOT_HOLD_OLD_BOOTLOAD_DIRECTORY]
		LEX(3) / BEGIN \

	/ ^hold , /
		[dm_system_config.prev_dm_disp.hold = DM_DO_NOT_HOLD_OLD_BOOTLOAD_DIRECTORY]
		LEX(3) / status_spec \

	/ ^hold <any-token> /
		[dm_system_config.prev_dm_disp = save_prev_dm_disp]
		LEX(1) ERROR(6) NEXT_STMT / BEGIN \

	/ adopt ; /
		[dm_system_config.prev_dm_disp.adopt = DM_ADOPT_OLD_BOOTLOAD]
		LEX(2) / BEGIN \

	/ adopt , /
		[dm_system_config.prev_dm_disp.adopt = DM_ADOPT_OLD_BOOTLOAD]
		LEX(2) / status_spec \

	/ adopt <any-token> /
		[dm_system_config.prev_dm_disp = save_prev_dm_disp]
		LEX(1) ERROR(6) NEXT_STMT / BEGIN \

	/ ^adopt ; /
		[dm_system_config.prev_dm_disp.adopt = DM_DO_NOT_ADOPT_OLD_BOOTLOAD]
		LEX(3) / BEGIN \

	/ ^adopt , /
		[dm_system_config.prev_dm_disp.adopt = DM_DO_NOT_ADOPT_OLD_BOOTLOAD]
		LEX(3) / status_spec \

	/ ^adopt <any-token> /
		[dm_system_config.prev_dm_disp = save_prev_dm_disp]
		LEX(1) ERROR(6) NEXT_STMT / BEGIN \

	/ recover ; /
		[dm_system_config.prev_dm_disp.recover = DM_RECOVER_OLD_BOOTLOAD]
		LEX(2) / BEGIN \

	/ recover , /
		[dm_system_config.prev_dm_disp.recover = DM_RECOVER_OLD_BOOTLOAD]
		LEX(2) / status_spec \

	/ recover <any-token> /
		[dm_system_config.prev_dm_disp = save_prev_dm_disp]
		LEX(1) ERROR(6) NEXT_STMT / BEGIN \

	/ ^recover ; /
		[dm_system_config.prev_dm_disp.recover = DM_DO_NOT_RECOVER_OLD_BOOTLOAD]
		LEX(3) / BEGIN \

	/ ^recover , /
		[dm_system_config.prev_dm_disp.recover = DM_DO_NOT_RECOVER_OLD_BOOTLOAD]
		LEX(3) / status_spec \

	/ ^recover <any-token> /
		[dm_system_config.prev_dm_disp = save_prev_dm_disp]
		LEX(1) ERROR(6) NEXT_STMT / BEGIN \

	/ recovery_check_mode ; /
		[dm_system_config.prev_dm_disp.recovery_check_mode = DM_RECOVERY_CHECK_MODE_ON]
		LEX(2) / BEGIN \

	/ recovery_check_mode , /
		[dm_system_config.prev_dm_disp.recovery_check_mode = DM_RECOVERY_CHECK_MODE_ON]
		LEX(2) / status_spec \

	/ recovery_check_mode <any-token> /
		[dm_system_config.prev_dm_disp = save_prev_dm_disp]
		LEX(1) ERROR(6) NEXT_STMT / BEGIN \

	/ ^recovery_check_mode ; /
		[dm_system_config.prev_dm_disp.recovery_check_mode = DM_RECOVERY_CHECK_MODE_OFF]
		LEX(3) / BEGIN \

	/ ^recovery_check_mode , /
		[dm_system_config.prev_dm_disp.recovery_check_mode = DM_RECOVERY_CHECK_MODE_OFF]
		LEX(3) / status_spec \

	/ ^recovery_check_mode <any-token> /
		[dm_system_config.prev_dm_disp = save_prev_dm_disp]
		LEX(1) ERROR(6) NEXT_STMT / BEGIN \

	/ <any-token> /
		ERROR(4) NEXT_STMT / BEGIN \

	/ <no-token> /
		ERROR(8) / BEGIN \


++*/

/* format: style2,ind3 */
dm_translate_system_config_:
   procedure (p_system_config_file_ptr, p_system_config_file_len, p_long_sw, p_area_ptr, p_system_config_ptr, p_code);

%page;

/* DECLARATIONS */

/* Parameter */

      dcl	    p_system_config_file_ptr
			       pointer parameter;	/* INPUT - pointer to ascii data */
      dcl	    p_system_config_file_len
			       fixed bin (21) parameter;
						/* INPUT - length of ascii data in bytes */
      dcl	    p_long_sw	       bit (1) aligned parameter;
						/* INPUT - true if errors to be printed out */
      dcl	    p_area_ptr	       pointer parameter;	/* INPUT - pointer to area where site data to be allocated */
      dcl	    p_system_config_ptr    pointer parameter;	/* OUTPUT - pointer to structure */
      dcl	    p_code	       fixed bin (35) parameter;
						/* OUTPUT - system status code */

/* Automatic */

      dcl	    code		       fixed bin (35);
      dcl	    breaks	       char (128) varying aligned;
      dcl	    ignored_breaks	       char (128) varying aligned;
      dcl	    lex_delims	       char (128) varying aligned;
      dcl	    lex_control_chars      char (128) varying aligned;
      dcl	    byte_count	       fixed bin (21);
      dcl	    source_ptr	       pointer;
      dcl	    temp_storage_ptr       pointer;
      dcl	    Pfirst_stmt_descriptor pointer;
      dcl	    Pfirst_token_descriptor
			       pointer;
      dcl	    abs_path	       char (168);
      dcl	    1 my_dm_system_config  aligned like dm_system_config;
      dcl	    1 save_prev_dm_disp    aligned like dm_system_config_info.prev_dm_disp;

/* Static, Internal */

      dcl	    1 error_control_table  dimension (10) internal static,
	      2 severity	       fixed bin (17) unaligned init (2, 1, 2, 2, 4, 2, 2, 3, 2, 2),
	      2 Soutput_stmt       bit (1) unaligned init ((10) (1)"1"b),
	      2 message	       char (96) varying
			       init ("An unknown statement has been encountered:  statement ignored.",
			       "Text follows the end statement and is being ignored.",
			       "The end statement is missing:  one has been supplied.",
			       """^a"" is not a valid keyword option:  statement ignored.",
			       "The end statement has been prematurely encountered.",
			       "An option delimiter is missing:  statement ignored.", "The option list is invalid.",
			       "The statement has prematurely ended.",
			       """^a"" is not a valid directory name:  statement ignored.",
			       """^a"" is not a valid before journal name:  statement ignored."),
	      2 brief_message      char (48) varying
			       init ("Unknown statement.", "Text follows the end statement.",
			       "Missing end statement.", "Invalid option.", "Premature end statement.",
			       "Missing option delimiter.", "Invalid option list.", "Premature statement end.",
			       "Invalid directory name.", "Invalid before journal name.");

/* Static, External */

      dcl	    error_table_$no_stmt_delim
			       fixed bin (35) ext static;
      dcl	    error_table_$translation_failed
			       fixed bin (35) ext static;
      dcl	    error_table_$improper_data_format
			       fixed bin (35) ext static;
      dcl	    error_table_$notalloc  fixed bin (35) ext static;

/* Based */

      dcl	    caller_area	       area based (p_area_ptr);

/* Constant */

      dcl	    ME		       char (32) aligned internal static options (constant)
			       init ("dm_translate_system_config_");

/* Entry */

      dcl	    absolute_pathname_     entry (char (*), char (*), fixed bin (35));
      dcl	    convert_date_to_binary_
			       entry (char (*), fixed bin (71), fixed bin (35));
      dcl	    dm_gen_checksum_       entry (ptr, fixed bin (18)) returns (fixed bin (35));
      dcl	    get_temp_segment_      entry (char (*) aligned, ptr, fixed bin (35));
      dcl	    lex_string_$init_lex_delims
			       entry (char (*), char (*), char (*), char (*), char (*), bit (*), char (*) var,
			       char (*) var, char (*) var, char (*) var);
      dcl	    lex_string_$lex	       entry (ptr, fixed bin (21), fixed bin (21), ptr, bit (*), char (*), char (*),
			       char (*), char (*), char (*), char (*) var, char (*) var, char (*) var, char (*) var,
			       ptr, ptr, fixed bin (35));
      dcl	    sub_err_	       entry () options (variable);
      dcl	    translator_temp_$get_segment
			       entry (char (*) aligned, ptr, fixed bin (35));
      dcl	    translator_temp_$release_all_segments
			       entry (ptr, fixed bin (35));
      dcl	    release_temp_segment_  entry (char (*) aligned, ptr, fixed bin (35));

/* Builtin */

      dcl	    null		       builtin;
      dcl	    substr	       builtin;
      dcl	    collate	       builtin;
      dcl	    addr		       builtin;
      dcl	    size		       builtin;
      dcl	    rtrim		       builtin;

/* Condition */

      dcl	    cleanup	       condition;
      dcl	    area		       condition;


/* END DECLARATIONS */

%page;

/* BEGIN CODE */

/* initialize some values */

      source_ptr = p_system_config_file_ptr;
      byte_count = p_system_config_file_len;
      temp_storage_ptr = null ();
      p_code = error_table_$translation_failed;

      error_control_table.Soutput_stmt (*) = p_long_sw;

/* set up cleanup handler */

      on cleanup
         begin;
	  call translator_temp_$release_all_segments (temp_storage_ptr, code);
	  call release_temp_segment_ (ME, dm_system_config_ptr, code);
	  goto dm_translate_system_config_exit;
         end;

/* get temp space for translation as required by rd */

      call translator_temp_$get_segment (ME, temp_storage_ptr, code);
      if code ^= 0
      then call dm_translate_system_config_abort ();

/* set up the lex function */

      breaks = substr (collate (), 1, 33) || ",:=^" || substr (collate (), 128, 1);
      ignored_breaks = substr (collate (), 1, 8) || substr (collate (), 10, 24) || substr (collate (), 128, 1);

      call
         lex_string_$init_lex_delims ("""", """", "/*", "*/", ";", "10"b, breaks, ignored_breaks, lex_delims,
         lex_control_chars);

/* initiate the defaults in the config structure */

      dm_system_config_ptr = addr (my_dm_system_config);

      dm_system_config.version = DM_CONFIG_VERSION_2;
      dm_system_config.idle_timeout = 0;
      dm_system_config.shutdown_delay = 0;
      dm_system_config.log_proc_terms = "1"b;
      dm_system_config.lock = ""b;
      dm_system_config.max_n_txn = 0;
      dm_system_config.max_n_proc = 0;
      dm_system_config.default_bj_size = 0;
      dm_system_config.default_bj.dir = "";
      dm_system_config.default_bj.entry = "";
      dm_system_config.prev_dm_disp.adopt = DM_ADOPT_OLD_BOOTLOAD;
      dm_system_config.prev_dm_disp.hold = DM_DO_NOT_HOLD_OLD_BOOTLOAD_DIRECTORY;
      dm_system_config.prev_dm_disp.recover = DM_RECOVER_OLD_BOOTLOAD;
      dm_system_config.prev_dm_disp.recovery_check_mode = DM_RECOVERY_CHECK_MODE_OFF;
      dm_system_config.curr_dm_enable = DM_DO_NOT_FORCE_ENABLE_NEW_BOOTLOAD;

/* call subroutine to lex the dsdt file */

      call
         lex_string_$lex (source_ptr, byte_count, 0, temp_storage_ptr, "1000"b, """", """", "/*", "*/", ";", breaks,
         ignored_breaks, lex_delims, lex_control_chars, Pfirst_stmt_descriptor, Pfirst_token_descriptor, code);
      if ^(code = 0 | code = error_table_$no_stmt_delim)
      then
         do;
	  call dm_translate_system_config_abort ();
         end;

      Pthis_token = Pfirst_token_descriptor;

      call SEMANTIC_ANALYSIS ();

      call translator_temp_$release_all_segments (temp_storage_ptr, code);
      call release_temp_segment_ (ME, dm_system_config_ptr, code);

dm_translate_system_config_exit:
      return;

/* BEGIN RELATIVE FUNCTIONS */

absolute_path:
   procedure () returns (bit (1) aligned);

/*
   this function returns true if the current token is either an absolute
   or one of the reserved pathname keywords "aim_dir" or "bootload_dir"
*/

      if token_value = "aim_dir" | token_value = "bootload_dir"
      then return ("1"b);

      if substr (token_value, 1, 1) ^= ">"
      then return ("0"b);

      call absolute_pathname_ (token_value, abs_path, code);
      if code ^= 0
      then return ("0"b);
      else return ("1"b);

   end absolute_path;

date_time_offset:
   procedure () returns (bit (1) aligned);

      dcl	    dto_convert	       bit (1) aligned init ("0"b);
      dcl	    dto_code	       fixed bin (35) init (0);
      dcl	    dto_constant_time_str  char (32) varying init ("January 1, 1980 9am");
      dcl	    dto_new_time_str       char (64) varying init ("");
      dcl	    dto_constant_time      fixed bin (71) init (0);
      dcl	    dto_new_time	       fixed bin (71) init (0);

      goto DTO_JOIN;
convert_date_time_offset:
   entry () returns (fixed bin (71));
      dto_convert = "1"b;
DTO_JOIN:
      dto_new_time_str = dto_constant_time_str || " + " || token_value;
      call convert_date_to_binary_ ((dto_new_time_str), dto_new_time, dto_code);
      if ^dto_convert
      then if dto_code ^= 0
	 then return ("0"b);
	 else return ("1"b);
      else if dto_code ^= 0
      then
         do;
	  call
	     sub_err_ (dto_code, ME, ACTION_DEFAULT_RESTART, null, 0, "^/Unable to convert ^a to its binary form.",
	     token_value);
	  return (0);
         end;
      call convert_date_to_binary_ ((dto_constant_time_str), dto_constant_time, dto_code);
      if dto_code ^= 0
      then
         do;
	  call
	     sub_err_ (dto_code, ME, ACTION_DEFAULT_RESTART, null, 0, "^/Unable to convert ^a to its binary form.",
	     dto_new_time_str);
	  return (0);
         end;

      return (dto_new_time - dto_constant_time);

   end date_time_offset;



before_journal_name:
   procedure () returns (bit (1) aligned);

/*
   this function returns true if the token is comprised of letters, digits,
   the underscore, and the period.  additionally, the token must end in
   ".bj".
*/

      if (token.Lvalue < 1) | (token.Lvalue > 32)
      then return ("0"b);

      if verify (token_value, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_.") ^= 0
      then return ("0"b);

      if substr (reverse (rtrim (token_value)), 1, 3) ^= "jb."
      then return ("0"b);

      return ("1"b);


   end before_journal_name;


/* BEGIN ACTION PROCEDURES */

return_table:
   procedure ();

      if p_area_ptr ^= null ()
      then
         do;

	  dm_system_config.checksum = dm_gen_checksum_ (dm_system_config_ptr, size (dm_system_config_info) - 1);

	  on area
	     begin;
	        code = error_table_$notalloc;
	        call dm_translate_system_config_abort ();
	     end;

	  allocate dm_system_config in (caller_area) set (p_system_config_ptr);

	  revert area;

	  p_system_config_ptr -> dm_system_config = dm_system_config_ptr -> dm_system_config;

         end;

      if MERROR_SEVERITY < 3
      then p_code = 0;

      return;

   end return_table;

dm_translate_system_config_abort:
   procedure ();

      p_code = code;
      call translator_temp_$release_all_segments (temp_storage_ptr, code);

      goto dm_translate_system_config_exit;

   end dm_translate_system_config_abort;
%page;
%include dm_system_config;
%page;
%include sub_err_flags;

