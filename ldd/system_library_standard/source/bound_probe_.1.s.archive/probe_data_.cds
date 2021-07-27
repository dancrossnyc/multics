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
  1) change(88-10-18,WAAnderson), approve(88-10-18,MCR7952),
     audit(88-10-18,JRGray), install(88-10-24,MR12.2-1184):
     Probe source archive split. Part of C-Probe Support.
                                                   END HISTORY COMMENTS */


probe_data_: proc ();

/* *	External static and constant data for the probe debugger.
   *	Created: 05/28/79 W. Olin Sibert (from rdm_data_)
   *	Modified: 09/19/79 JRDavis to add "greeting msg."
   *	Modified: 10 Sep 79 JRD remove cleverness so as to not startle Martinson
   *      Modified:  5 May 80 JRD probe 5.2
   *	Modified: 17 April 1981 by M. N. Davidoff for probe 5.3.
   *	Modified: 05/05/82 S. Herbst probe 5.4
   *	Modified: 04/20/84 S. Herbst probe 5.5
*/

	dcl     1 cds_data		 aligned like cds_args; /* arguments to create_data_segment_ subr */

	dcl     code		 fixed binary (35);


	dcl     com_err_		 entry () options (variable);
	dcl     ioa_$rsnnl		 entry options (variable);
	dcl     ioa_		 entry options (variable);
	dcl     create_data_segment_	 entry (pointer, fixed binary (35));


	dcl     whoami		 char (12) internal static options (constant) init ("probe_data_");
	dcl     EXCLUDE_ARRAY	 (1) char (32) internal static options (constant) init ("pad*");

	dcl     (addr, hbound, null, size, string) builtin;

	dcl     EXL_INFO		 bit (1) aligned internal static options (constant) initial ("0"b);
	dcl     EXL_VERSION		 bit (1) aligned internal static options (constant) initial ("0"b);

	dcl     1 probe_constant_data	 aligned,
		2 major_version	 fixed bin,
		2 minor_version	 fixed bin,
		2 version_string	 char (32) aligned,
		2 info_directory	 char (168) aligned,
		2 debug_sw	 bit (1) aligned,
		2 catch_faults	 bit (1) aligned,
		2 scratch_segment_name char (32) aligned,
		2 break_segment_suffix char (32) aligned,
		2 prompt_string	 char (32) varying,
		2 greeting_msg	 char (256) varying,
		2 pad1		 fixed bin;

	dcl     1 probe_static_data	 aligned,
		2 probe_scratch_ptr	 pointer,
		2 probe_static_info_ptr pointer,
		2 invocation_list_ptr pointer,
		2 break_segment_ptr	 pointer;

/**/

	probe_constant_data.major_version = 5;
	probe_constant_data.minor_version = 5;

	if EXL_VERSION then
	     call ioa_ ("Generating probe_data_ exl version ^d.^d",
		probe_constant_data.major_version, probe_constant_data.minor_version);

	call ioa_$rsnnl ("probe ^d.^d", probe_constant_data.version_string, (0),
	     probe_constant_data.major_version, probe_constant_data.minor_version);

	if EXL_INFO then
	     probe_constant_data.info_directory = ">exl>info";
	else probe_constant_data.info_directory = ">doc>info";

	probe_constant_data.scratch_segment_name = "probe_scratch_";
	probe_constant_data.break_segment_suffix = "probe";
	probe_constant_data.prompt_string = "probe^[(^d)^;^s^]:^2x";
	probe_constant_data.greeting_msg = "";

	probe_constant_data.debug_sw = "0"b;
	probe_constant_data.catch_faults = "1"b;

	probe_static_data.probe_static_info_ptr = null ();
	probe_static_data.invocation_list_ptr = null ();
	probe_static_data.probe_scratch_ptr = null ();
	probe_static_data.break_segment_ptr = null ();

	unspec (cds_data) = "0"b;

	cds_data.sections (1).p = addr (probe_constant_data);
	cds_data.sections (1).len = size (probe_constant_data);
	cds_data.sections (1).struct_name = "probe_constant_data";

	cds_data.sections (2).p = addr (probe_static_data);
	cds_data.sections (2).len = size (probe_static_data);
	cds_data.sections (2).struct_name = "probe_static_data";

	cds_data.seg_name = whoami;

	cds_data.num_exclude_names = hbound (EXCLUDE_ARRAY, 1);
	cds_data.exclude_array_ptr = addr (EXCLUDE_ARRAY);

	string (cds_data.switches) = "0"b;
	cds_data.switches.have_text = "1"b;		/* only create text section */
	cds_data.switches.have_static = "1"b;

	call create_data_segment_ (addr (cds_data), code);

	if code ^= 0 then call com_err_ (code, whoami);

	return;

/**/

%include cds_args;
     end probe_data_;
