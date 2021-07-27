/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1985 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-08-01,Coren), approve(87-07-10,MCR7679), audit(87-02-27,GDixon),
     install(87-08-04,MR12.1-1055):
     Written.
  2) change(87-02-27,GDixon), approve(87-07-10,MCR7679),
     audit(87-05-19,Parisek), install(87-08-04,MR12.1-1055):
     Modified for move of login server from DSA software into the Multics
     Networking Architecture (MNA).  Info dir paths changed as a result.
                                                   END HISTORY COMMENTS */

/* format: style4,delnl,insnl,^ifthendo */
ls_data_:
     procedure ();

/* This is the CDS source for creating ls_data_ */
%page;
/* AUTOMATIC */

dcl  code fixed bin (35);
dcl  1 cda like cds_args;

dcl  1 ls_data aligned,
       2 login_server_info_dir char (168),		/* info directory for requests for login server itself */
       2 login_info_dir char (168),			/* info directory for regular login requests */
       2 connect_info_dir char (168),			/* info directory for connect-loop requests */
       2 suffix (0:9) char (4),			/* ordinal suffixes for numbers not in the range 10:19 */
       2 teens_suffix (0:9) char (4);			/* ordinal suffixes for numbers in teens */


/* INTERNAL STATIC */

dcl  OUR_NAME char (8) internal static options (constant) initial ("ls_data_");


/* ENTRIES */

dcl  com_err_ entry () options (variable);
dcl  create_data_segment_ entry (ptr, fixed bin (35));


/* BUILTINS AND CONDITIONS */

dcl  (addr, null, size, string) builtin;
%page;

/* info directory names...may need to be changed for installation */

	ls_data.login_server_info_dir = ">doc>subsystem>login_server_info";
	ls_data.login_info_dir = ">doc>subsystem>login_info";
	ls_data.connect_info_dir = ">doc>subsystem>login_connect_info";

/* end info directory names */

	ls_data.teens_suffix (*) = "th  ";
	ls_data.suffix (*) = "th  ";
	ls_data.suffix (1) = "st  ";
	ls_data.suffix (2) = "nd  ";
	ls_data.suffix (3) = "rd  ";

/* structure is all set up, now create the data segment */

	cda.sections (1).p = addr (ls_data);
	cda.sections (1).len = size (ls_data);
	cda.sections (1).struct_name = "ls_data";
	cda.sections (2).p = null ();
	cda.sections (2).len = 0;
	cda.sections (2).struct_name = "";

	cda.seg_name = OUR_NAME;
	cda.num_exclude_names = 0;
	cda.exclude_array_ptr = null ();
	string (cda.switches) = ""b;
	cda.have_text = "1"b;

	call create_data_segment_ (addr (cda), code);
	if code ^= 0
	then call com_err_ (code, OUR_NAME, "From create_data_segment_.");

	return;
%page;
%include cds_args;

     end ls_data_;
