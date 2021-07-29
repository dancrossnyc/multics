/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   *********************************************************** */


/* Constants and static data used internally by the Inter-Multics File Transfer (IMFT) Facility */

/* Created:  May 1982 by G. Palter */

/* format: style4,delnl,insnl,ifthenstmt,ifthen */


imft_data_:
     procedure () options (variable);


dcl  1 imft_constants aligned,			/* the constant data */
       2 default_queue_dirname character (168);

dcl  1 imft_static aligned,				/* static variables (may be changed during testing) */
       2 queue_dirname character (168);

dcl  1 cds_data aligned like cds_args;

dcl  code fixed binary (35);

dcl  IMFT_DATA_ character (32) static options (constant) initial ("imft_data_");

dcl  com_err_ entry () options (variable);
dcl  create_data_segment_ entry (pointer, fixed binary (35));

dcl  (addr, currentsize, null, string) builtin;


	imft_constants.default_queue_dirname = ">daemon_dir_dir>io_daemon_dir";

	imft_static.queue_dirname = imft_constants.default_queue_dirname;

	cds_data.sections (1).p = addr (imft_constants);
	cds_data.sections (1).len = currentsize (imft_constants);
	cds_data.sections (1).struct_name = "imft_constants";

	cds_data.sections (2).p = addr (imft_static);
	cds_data.sections (2).len = currentsize (imft_static);
	cds_data.sections (2).struct_name = "imft_static";

	cds_data.seg_name = IMFT_DATA_;
	cds_data.num_exclude_names = 0;
	cds_data.exclude_array_ptr = null ();

	string (cds_data.switches) = ""b;
	cds_data.switches.have_text = "1"b;		/* have constants ... */
	cds_data.switches.have_static = "1"b;		/* ... and static data */

	call create_data_segment_ (addr (cds_data), code);
	if code ^= 0 then call com_err_ (code, IMFT_DATA_);

	return;

/**/

%include cds_args;

     end imft_data_;
