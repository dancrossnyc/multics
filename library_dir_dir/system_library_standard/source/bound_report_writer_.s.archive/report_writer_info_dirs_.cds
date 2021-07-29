/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1984 *
   *                                                         *
   *********************************************************** */
/* format: off */
%skip(1);
/*

     This module creates the segment "report_writer_info_dirs_" which
     contains the pathnames of directories containing info segments
     provided with the report_writer_ (except for the subroutine info
     segment report_writer_.info).

     The following directories are provided:

     >doc>ss>report_writer_info_dirs
	contains all the report_writer_ request's info segments

     >doc>ss>report_writer_info_dirs>standard_requests
	contains links to all the report_writer_ request's info segments

     >doc>ss>report_writer_info_dirs>clv_request
	contains a link to the report_writer_ column_value request's info segment

     >doc>ss>report_writer_info_dirs>di_request
	contains a link to the report_writer_ display request's info segment

     >doc>ss>report_writer_info_dirs>dib_request
	contains a link to the report_writer_ display_builtins request's info segment

     >doc>ss>report_writer_info_dirs>lsfo_request
	contains a link to the report_writer_ list_format_options request's info segment

     >doc>ss>report_writer_info_dirs>rsfo_request
	contains a link to the report_writer_ restore_format_options request's info segment

     >doc>ss>report_writer_info_dirs>sfo_request
	contains a link to the report_writer_ set_format_options request's info segment

     >doc>ss>report_writer_info_dirs>svfo_request
	contains a link to the report_writer_ save_format_options request's info segment

     Known Bugs:

     Other Problems:

     History:

     Written - Al Dupuis - October 1984.

*/

report_writer_info_dirs_: proc;
%skip(1);
	report_writer_info_dir_data.standard_requests 
	     = ">doc>ss>report_writer_info_dirs>standard_requests";
	report_writer_info_dir_data.clv_request
	     = ">doc>ss>report_writer_info_dirs>clv_request";
	report_writer_info_dir_data.di_request
	     = ">doc>ss>report_writer_info_dirs>di_request";
	report_writer_info_dir_data.dib_request
	     = ">doc>ss>report_writer_info_dirs>dib_request";
	report_writer_info_dir_data.lsfo_request
	     = ">doc>ss>report_writer_info_dirs>lsfo_request";
	report_writer_info_dir_data.rsfo_request
	     = ">doc>ss>report_writer_info_dirs>rsfo_request";
	report_writer_info_dir_data.sfo_request
	     = ">doc>ss>report_writer_info_dirs>sfo_request";
	report_writer_info_dir_data.svfo_request
	     = ">doc>ss>report_writer_info_dirs>svfo_request";
%skip(1);
	cds_data.sections (1).p = addr (report_writer_info_dir_data);
	cds_data.sections (1).len = currentsize (report_writer_info_dir_data);
	cds_data.sections (1).struct_name = "report_writer_info_dir_data";
%skip(1);
	cds_data.sections (2).p = null;
	cds_data.sections (2).len = 0;
	cds_data.sections (2).struct_name = "";
%skip(1);
	cds_data.seg_name = REPORT_WRITER_INFO_DIRS_;
	cds_data.num_exclude_names = 0;
	cds_data.exclude_array_ptr = null;
%skip(1);
	unspec (cds_data.switches) = OFF;
	cds_data.switches.have_text = ON;
%skip(1);
	call create_data_segment_ (addr (cds_data), code);
	if code ^= 0 then
	call com_err_ (code, REPORT_WRITER_INFO_DIRS_);
%skip(1);
	return;
%page;
dcl OFF bit (1) aligned internal static options (constant) init ("0"b);
dcl ON bit (1) aligned internal static options (constant) init ("1"b);
%skip(1);
dcl REPORT_WRITER_INFO_DIRS_ char (32) internal static options (constant) init ("report_writer_info_dirs_");
%skip(1);
dcl addr builtin;
%skip(1);
dcl 1 cds_data aligned like cds_args;
dcl code fixed bin (35);
dcl com_err_ entry() options(variable);
dcl create_data_segment_ entry (ptr, fixed bin(35));
dcl currentsize builtin;
%skip(1);
dcl null builtin;
%skip(1);
dcl 1 report_writer_info_dir_data aligned,
      2 standard_requests char (168),
      2 clv_request char (168),
      2 di_request char (168),
      2 dib_request char (168),
      2 lsfo_request char (168),
      2 rsfo_request char (168),
      2 sfo_request char (168),
      2 svfo_request char (168);
%skip(1);
dcl unspec builtin;
%page;
%include cds_args;
%skip(3);
     end report_writer_info_dirs_;
