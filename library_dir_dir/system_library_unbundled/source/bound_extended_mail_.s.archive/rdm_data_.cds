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
  1) change(86-03-25,Herbst), approve(86-03-25,MCR7367),
     audit(86-04-28,Margolin), install(86-05-22,MR12.0-1059):
     Changed version to 9.3.
  2) change(86-08-26,Margolin), approve(86-08-26,MCR7508),
     audit(86-08-27,Blair), install(86-08-29,MR12.0-1142):
     Changed version to 9.3a.
  3) change(88-04-14,Blair), approve(88-04-14,MCR7842),
     audit(88-06-29,Lippard), install(88-07-26,MR12.2-1069):
     Increment the version number to reflect the changes for SCP6349, add
     search path capability to the mail system.
                                                   END HISTORY COMMENTS */


/* format: off */

/* Constant data used by the read_mail subsystem */

/* Created: 14 March 1978 by G. Palter */
/* Modified: 20 June 1978 by G. Palter to add info_directory */
/* Converted: 4 July 1978 by W. Olin Sibert from sdm_data_ */
/* Modified: 29 December 1979 by W. Olin Sibert */
/* Modified: 21 September 1982 by G. Palter to add ec_suffix and ec_search_list and remove several obsolete data items */
/* Recoded:  September 1983 by G.  Palter to use new argument processing for EXL/installed decision, to make the subsystem
      version a single character string, and to eliminate constants no longer used after converting to the mail system
      interface */

/* format: on,style4,delnl,insnl,ifthenstmt,ifthen */


rdm_data_:
     procedure () options (variable);


dcl  1 rdm_constants aligned,
       2 version character (32) varying,
       2 info_directory character (168) unaligned,
       2 special_message character (256) varying,
       2 ec_suffix character (32) unaligned,
       2 ec_search_list character (32) unaligned;

dcl  1 rdm_static aligned,
       2 first_invocation bit (1) aligned;

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

dcl  RDM_DATA_ character (32) static options (constant) initial ("rdm_data_");

dcl  DEFAULT_SUBSYSTEM_VERSION character (28) varying static options (constant) initial ("9.3b");
dcl  DEFAULT_SPECIAL_MESSAGE character (256) varying static options (constant) initial ("");

dcl  UNBUNDLED_SUBSYSTEM fixed binary static options (constant) initial (1);
dcl  UNBUNDLED_INFO_DIRECTORY character (168) static options (constant) initial (">doc>subsystem>mail_system>read_mail");

dcl  EXL_SUBSYSTEM fixed binary static options (constant) initial (2);
dcl  EXL_INFO_DIRECTORY character (168) static options (constant) initial (">exl>mail_system_dir>info>read_mail");

dcl  DEVELOPMENT_SUBSYSTEM fixed binary static options (constant) initial (3);
dcl  DEVELOPMENT_INFO_DIRECTORY character (168) static options (constant)
	initial (">udd>Multics>Palter>work>mail_system>info>read_mail");

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
	     call com_err_ (code, RDM_DATA_);
	     return;
	end;

	subsystem_type = UNBUNDLED_SUBSYSTEM;
	subsystem_version = DEFAULT_SUBSYSTEM_VERSION;
	special_message_given = "0"b;			/* default depends on the subsystem version */

	do argument_idx = 1 to n_arguments;

	     call cu_$arg_ptr (argument_idx, argument_ptr, argument_lth, code);
	     if code ^= 0 then do;
		call com_err_ (code, RDM_DATA_, "Fetching argument #^d.", argument_idx);
		return;
	     end;

	     if index (argument, "-") = 1 then		/* a control argument ... */
		if (argument = "-unbundled") | (argument = "-unb") then subsystem_type = UNBUNDLED_SUBSYSTEM;
		else if (argument = "-experimental") | (argument = "-exl") then subsystem_type = EXL_SUBSYSTEM;
		else if (argument = "-development") | (argument = "-dev") then subsystem_type = DEVELOPMENT_SUBSYSTEM;

		else if argument = "-version" then do;	/* specific value for the subsystem version */
		     if argument_idx = n_arguments then do;
			call com_err_ (code, RDM_DATA_, "Version string following ""^a"".", argument);
			return;
		     end;
		     argument_idx = argument_idx + 1;
		     call cu_$arg_ptr (argument_idx, argument_ptr, argument_lth, code);
		     if code ^= 0 then do;
			call com_err_ (code, RDM_DATA_, "Fetching argument #^d.", argument_idx);
			return;
		     end;
		     if argument_lth > maxlength (DEFAULT_SUBSYSTEM_VERSION) then do;
			call com_err_ (error_table_$bigarg, RDM_DATA_,
			     "Maximum length for the version string is ^d characters.  ""^a""",
			     maxlength (DEFAULT_SUBSYSTEM_VERSION), argument);
			return;
		     end;
		     subsystem_version = argument;
		end;

		else if (argument = "-message") | (argument = "-msg") then do;
		     if argument_idx = n_arguments then do;
			call com_err_ (code, RDM_DATA_, "Special message text following ""^a"".", argument);
			return;
		     end;
		     argument_idx = argument_idx + 1;
		     call cu_$arg_ptr (argument_idx, argument_ptr, argument_lth, code);
		     if code ^= 0 then do;
			call com_err_ (code, RDM_DATA_, "Fetching argument #^d.", argument_idx);
			return;
		     end;
		     if argument_lth > maxlength (special_message) then do;
			call com_err_ (error_table_$bigarg, RDM_DATA_,
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
		     call com_err_ (error_table_$badopt, RDM_DATA_, """^a""", argument);
		     return;
		end;

	     else do;
		call com_err_ (error_table_$bad_arg, RDM_DATA_, """^a""", argument);
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

	rdm_constants.version = subsystem_version;
	rdm_constants.info_directory = subsystem_info_directory;
	rdm_constants.special_message = special_message;

	rdm_constants.ec_suffix = "rdmec";		/* use non-default exec_com suffix and search list */
	rdm_constants.ec_search_list = "mail_system";


/* Define initial values for the static used by the subsystem */

	rdm_static.first_invocation = "1"b;		/* force the initialization code to be run */


/* Set up arguments for call to create_data_segment_ */

	cds_data.sections (1).p = addr (rdm_constants);
	cds_data.sections (1).len = currentsize (rdm_constants);
	cds_data.sections (1).struct_name = "rdm_constants";

	cds_data.sections (2).p = addr (rdm_static);
	cds_data.sections (2).len = currentsize (rdm_static);
	cds_data.sections (2).struct_name = "rdm_static";

	cds_data.seg_name = RDM_DATA_;

	cds_data.num_exclude_names = 0;
	cds_data.exclude_array_ptr = null ();

	string (cds_data.switches) = ""b;
	cds_data.switches.have_text, cds_data.switches.have_static = "1"b;
	cds_data.switches.separate_static = "1"b;


/* Call create_data_segment_ */

	call create_data_segment_ (addr (cds_data), code);

	if code ^= 0 then call com_err_ (code, RDM_DATA_);

	return;
%page;
%include cds_args;

     end rdm_data_;
