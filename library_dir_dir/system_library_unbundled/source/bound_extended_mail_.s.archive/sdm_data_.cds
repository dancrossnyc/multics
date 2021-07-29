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
  1) change(88-04-14,Blair), approve(88-04-14,MCR7842),
     audit(88-06-29,Lippard), install(88-07-26,MR12.2-1069):
     Increment the version number to reflect the changes for SCP6349, add
     search path capability to the mail system.
                                                   END HISTORY COMMENTS */


/* format: off */

/* Constant and static data used by the send_mail subsystem */

/* Created:  14 March 1978 by G. Palter */
/* Modified: 20 June 1978 by G. Palter to add info_directory */
/* Converted: 4 July 1978 by W. Olin Sibert to be rdm_data_ instead */
/* Converted: 28 December 1978 by G. Palter back to sdm_data_ */
/* Modified: 15 January 1979 by G. Palter to add no_abort option */
/* Modified: 19 March 1979 by G. Palter to make -no_message_id the default */
/* Modified: 29 December 1979 by W. Olin Sibert to add max_lock_wait_retries */
/* Modified: 25 April 1980 by G. Palter to add -abbrev and -profile options */
/* Modified: 16 February 1982 by G. Palter to add default_profile_ptr option */
/* Modified: 17 September 1982 by G. Palter to add ec_suffix and ec_search_list and to reflect new fill, prompt, and
      request loop control structure  */
/* Recoded:  August 1983 by G. Palter to use new argument processing technology for EXL/installed decision, to make the
      subsystem version a single character string, to support version 6 of the send_mail_options structure, and to add
      static data (first use/default From field) */

/* format: on,style4,delnl,insnl,ifthenstmt,ifthen */


sdm_data_:
     procedure () options (variable);


dcl  1 sdm_constants aligned,
       2 version character (32) varying,
       2 info_directory character (168) unaligned,
       2 special_message character (256) varying,
       2 ec_suffix character (32) unaligned,
       2 ec_search_list character (32) unaligned,
       2 default_options like send_mail_options aligned;

dcl  1 sdm_static aligned,
       2 first_invocation bit (1) aligned,
       2 default_from_field pointer;

dcl  1 cds_data aligned like cds_args;			/* arguments to create_data_segment_ subr */

dcl  argument character (argument_lth) unaligned based (argument_ptr);
dcl  argument_ptr pointer;
dcl  argument_lth fixed binary (21);
dcl  (n_arguments, argument_idx) fixed binary;

dcl  subsystem_type fixed binary;			/* unbundled/exl/development */
dcl  subsystem_version character (32) varying;
dcl  subsystem_info_directory character (168);

dcl  special_message character (256) varying;
dcl  special_message_given bit (1) aligned;

dcl  code fixed binary (35);

dcl  SDM_DATA_ character (32) static options (constant) initial ("sdm_data_");

dcl  DEFAULT_SUBSYSTEM_VERSION character (28) varying static options (constant) initial ("8.0g");
dcl  DEFAULT_SPECIAL_MESSAGE character (256) varying static options (constant) initial ("");

dcl  UNBUNDLED_SUBSYSTEM fixed binary static options (constant) initial (1);
dcl  UNBUNDLED_INFO_DIRECTORY character (168) static options (constant) initial (">doc>subsystem>mail_system>send_mail");

dcl  EXL_SUBSYSTEM fixed binary static options (constant) initial (2);
dcl  EXL_INFO_DIRECTORY character (168) static options (constant) initial (">exl>mail_system_dir>info>send_mail");

dcl  DEVELOPMENT_SUBSYSTEM fixed binary static options (constant) initial (3);
dcl  DEVELOPMENT_INFO_DIRECTORY character (168) static options (constant)
	initial (">udd>Multics>Palter>work>mail_system>info>send_mail");

/* format: off */
dcl (error_table_$bad_arg, error_table_$badopt, error_table_$bigarg)
	fixed binary (35) external;
/* format: on */

dcl  cu_$arg_count entry (fixed binary, fixed binary (35));
dcl  cu_$arg_ptr entry (fixed binary, pointer, fixed binary (21), fixed binary (35));
dcl  com_err_ entry () options (variable);
dcl  create_data_segment_ entry (pointer, fixed binary (35));

dcl  (addr, currentsize, index, maxlength, null, string) builtin;
%page;
/* Determine which type (unbundled/EXL/development) and version of the subsystem is being created */

	call cu_$arg_count (n_arguments, code);
	if code ^= 0 then do;			/* not a command */
	     call com_err_ (code, SDM_DATA_);
	     return;
	end;

	subsystem_type = UNBUNDLED_SUBSYSTEM;
	subsystem_version = DEFAULT_SUBSYSTEM_VERSION;
	special_message_given = "0"b;			/* default depends on the subsystem version */

	do argument_idx = 1 to n_arguments;

	     call cu_$arg_ptr (argument_idx, argument_ptr, argument_lth, code);
	     if code ^= 0 then do;
		call com_err_ (code, SDM_DATA_, "Fetching argument #^d.", argument_idx);
		return;
	     end;

	     if index (argument, "-") = 1 then		/* a control argument ... */
		if (argument = "-unbundled") | (argument = "-unb") then subsystem_type = UNBUNDLED_SUBSYSTEM;
		else if (argument = "-experimental") | (argument = "-exl") then subsystem_type = EXL_SUBSYSTEM;
		else if (argument = "-development") | (argument = "-dev") then subsystem_type = DEVELOPMENT_SUBSYSTEM;

		else if argument = "-version" then do;	/* specific value for the subsystem version */
		     if argument_idx = n_arguments then do;
			call com_err_ (code, SDM_DATA_, "Version string following ""^a"".", argument);
			return;
		     end;
		     argument_idx = argument_idx + 1;
		     call cu_$arg_ptr (argument_idx, argument_ptr, argument_lth, code);
		     if code ^= 0 then do;
			call com_err_ (code, SDM_DATA_, "Fetching argument #^d.", argument_idx);
			return;
		     end;
		     if argument_lth > maxlength (DEFAULT_SUBSYSTEM_VERSION) then do;
			call com_err_ (error_table_$bigarg, SDM_DATA_,
			     "Maximum length for the version string is ^d characters.  ""^a""",
			     maxlength (DEFAULT_SUBSYSTEM_VERSION), argument);
			return;
		     end;
		     subsystem_version = argument;
		end;

		else if (argument = "-message") | (argument = "-msg") then do;
		     if argument_idx = n_arguments then do;
			call com_err_ (code, SDM_DATA_, "Special message text following ""^a"".", argument);
			return;
		     end;
		     argument_idx = argument_idx + 1;
		     call cu_$arg_ptr (argument_idx, argument_ptr, argument_lth, code);
		     if code ^= 0 then do;
			call com_err_ (code, SDM_DATA_, "Fetching argument #^d.", argument_idx);
			return;
		     end;
		     if argument_lth > maxlength (special_message) then do;
			call com_err_ (error_table_$bigarg, SDM_DATA_,
			     "Maximum length for the special message is ^d characters.  ""^a""",
			     maxlength (special_message), argument);
			return;
		     end;
		     special_message = argument;
		     special_message_given = "1"b;
		end;
		else if (argument = "-no_message") | (argument = "-nmsg") then do;
		     special_message = "";		/* developer wants no message for this version */
		     special_message_given = "1"b;
		end;

		else do;
		     call com_err_ (error_table_$badopt, SDM_DATA_, """^a""", argument);
		     return;
		end;

	     else do;
		call com_err_ (error_table_$bad_arg, SDM_DATA_, """^a""", argument);
		return;
	     end;
	end;


/* Supply appropriate default values for the special message and subsystem info directory based on the type and version */

	if ^special_message_given then		/* defaults to builtin message only if builtin version */
	     if subsystem_version = DEFAULT_SUBSYSTEM_VERSION then
		special_message = DEFAULT_SPECIAL_MESSAGE;
	     else special_message = "";		/* ... any other version must have the message supplied */

	if subsystem_type = UNBUNDLED_SUBSYSTEM then subsystem_info_directory = UNBUNDLED_INFO_DIRECTORY;

	else if subsystem_type = EXL_SUBSYSTEM then do;
	     subsystem_version = subsystem_version || " EXL";
	     subsystem_info_directory = EXL_INFO_DIRECTORY;
	end;

	else /*** if subsystem_type = DEVELOPMENT_SUBSYSTEM then */
	     do;
	     subsystem_version = subsystem_version || " dev";
	     subsystem_info_directory = DEVELOPMENT_INFO_DIRECTORY;
	end;


/* Define values for the constant data used by the subsystem */

	sdm_constants.version = subsystem_version;
	sdm_constants.info_directory = subsystem_info_directory;
	sdm_constants.special_message = special_message;

	sdm_constants.ec_suffix = "sdmec";		/* use non-default exec_com suffix and search list */
	sdm_constants.ec_search_list = "mail_system";

	sdm_constants.default_options.version = SEND_MAIL_OPTIONS_VERSION_6;

	sdm_constants.fill_width = 72;

	sdm_constants.prompt_string = "";
	string (sdm_constants.prompt_control.flags) = ""b;
	sdm_constants.prompt_control.prompt_control = DEFAULT_PROMPT;

	sdm_constants.default_profile_ptr = null ();	/* default to use same profile as command level (if any) */
	sdm_constants.profile_ptr = null ();

	sdm_constants.original_text_indentation = 4;
	string (sdm_constants.original_text_control.flags) = ""b;
	sdm_constants.indent_original_text = "1"b;	/* indent the original text by default if it's included */

	string (sdm_constants.default_options.flags) = ""b;
	sdm_constants.notify = "1"b;
	sdm_constants.debug = (subsystem_type = DEVELOPMENT_SUBSYSTEM);
	sdm_constants.fill_control = DEFAULT_FILL;
	sdm_constants.request_loop_control = DEFAULT_REQUEST_LOOP;
						/* above code leaves acknowledge, brief, abbrev, and
						   auto_write off */


/* Define initial values for the static data used by the subsystem */

	sdm_static.first_invocation = "1"b;		/* force the initialization code to be run */

	sdm_static.default_from_field = null ();	/* value to be displayed as From field when it's empty */


/* Set up arguments for call to create_data_segment_ */

	cds_data.sections (1).p = addr (sdm_constants);
	cds_data.sections (1).len = currentsize (sdm_constants);
	cds_data.sections (1).struct_name = "sdm_constants";

	cds_data.sections (2).p = addr (sdm_static);
	cds_data.sections (2).len = currentsize (sdm_static);
	cds_data.sections (2).struct_name = "sdm_static";

	cds_data.seg_name = SDM_DATA_;

	cds_data.num_exclude_names = 0;
	cds_data.exclude_array_ptr = null ();

	string (cds_data.switches) = ""b;
	cds_data.switches.have_text, cds_data.switches.have_static = "1"b;
	cds_data.switches.separate_static = "1"b;


/* Call create_data_segment_ */

	call create_data_segment_ (addr (cds_data), code);

	if code ^= 0 then call com_err_ (code, SDM_DATA_);

	return;
%page;
%include send_mail_options;
%page;
%include cds_args;

     end sdm_data_;
