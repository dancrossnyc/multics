/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1984 *
   *                                                         *
   *********************************************************** */
ssu_info_directories_:
     procedure ();

/* This procedure creates the segment 'ssu_info_directories_' which contains
   the pathnames of directories containing info segs provided with the 
   subsystem utitlities.

   Written 84-06-22 by Paul W. Benjamin
*/

dcl 1 ssu_info_dir_data aligned,
    2 standard_requests char (168);

dcl 1 cds_data aligned like cds_args;

dcl  code fixed binary (35);

dcl  NAME character (32) static options (constant) 
     initial ("ssu_info_directories_");

dcl  com_err_ entry () options (variable);
dcl  create_data_segment_ entry (pointer, fixed binary (35));

dcl (addr, currentsize, null, string) builtin;

%include cds_args;


	ssu_info_dir_data.standard_requests 
	     = ">doc>subsystem>ssu_info_dirs>standard_requests";

	cds_data.sections (1).p = addr (ssu_info_dir_data);
	cds_data.sections (1).len = currentsize (ssu_info_dir_data);
	cds_data.sections (1).struct_name = "ssu_info_dir_data";

	cds_data.sections (2).p = null ();

	cds_data.seg_name = NAME;

	cds_data.num_exclude_names = 0;
	cds_data.exclude_array_ptr = null ();

	string (cds_data.switches) = "0"b;
	cds_data.switches.have_text = "1"b;

/* Call create_data_segment_ */

	call create_data_segment_ (addr (cds_data), code);

	if code ^= 0 then
	     call com_err_ (code, NAME);

	return;

     end ssu_info_directories_;
