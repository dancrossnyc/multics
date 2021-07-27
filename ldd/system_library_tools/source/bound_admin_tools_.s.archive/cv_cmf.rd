/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   * Copyright (c) 1972 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   *********************************************************** */

/* format: style2,indcomtxt */

/* CV_CMF - Program to compile the Channel Master File into a Channel Definition Table.
   Written August 1975 by THVV
   Modified 751117 by PG to add access_name stmt
   Modified 760512 by Mike Grady to add baud stmt and FNP info
   Modified 760705 by D. M. Wells to understand about FTP channels
   Modified November 1976 by T. Casey to allow name: ttyNXX-ttyNYY; and Baud: NNN;
   Modified 761229 by D. M. Wells to allow compatibility for renamed items --
   line_type TELNET and service_types FTP and MC
   Modified 770624 by Robert Coren to make terminal types be character strings
   and to add dont_read_answerback attribute.
   Modified Fall 1978 by Larry Johnson for demultiplexing.
   Modified April 1979 by Larry Johnson to move most checking of multiplexer
   and sub-channel specifications into multiplexer specific modules,
   and to allow FNP's to be specified other than those running MCS.
   Modified July 1979 by C. Hornig to allow non-FNP MCS channels.
   Modified 1979 August 21 by Art Beattie to accomodate a change to cdt.incl.pl1.
   Modified January 1981 by E. N. Kittlitz to eliminate cdte.phone_no.
   Modified May 1981 by Robert Coren to call parse_tty_name_.
   Modified December 1981 by Benson I. Margulies for cdt tree.
   Modified April 1982 by Robert Coren for change to baud_rates.incl.pl1.
   Modified July 1982 by E. N. Kittlitz for [severity] and cmf suffix.
   Modified August 1982 by E. N. Kittlitz for check_acs.
   Modified January 1983 by Keith Loepere for generic_destination.
   Modified 84-01-09 by BIM for more access control flags.
*/


/* HISTORY COMMENTS:
  1) change(86-09-21,Beattie), approve(86-09-21,MCR7542),
     audit(86-10-31,Brunelle), install(86-11-12,MR12.0-1211):
     Remove references to the 963 and 029 preaccess commands and
     remove support for ARDS, 202_ETX, 2741 and 1050 in system
     interfaces.
  2) change(87-03-17,Beattie), approve(87-05-04,MCR7682),
     audit(87-05-29,Parisek), install(87-07-17,MR12.1-1042):
     Fix bug in mpx_error where errors in CMF that are detected by
     multiplexers were not getting displayed.
                                                   END HISTORY COMMENTS */


/*++

BEGIN	/ <no-token>			/ ERROR(2)				/ RETURN \

mainloop	/ FNP : <any-token> ;		/ LEX(2) open_fnp LEX(2) PUSH(mainloop)		/ do_fnp \
	/ name : <any-token> ;		/ LEX(2) open_chn LEX(2) PUSH(mainloop)		/ do_name \

	\" Master Keywords.  These supply defaults for all cdte's following.

	/ Access_class : <access_class> ;	/ LEX(4) [dft.access_class = access_class_value] / mainloop \
	/ Access_class :			/ LEX(2) ERROR(10) NEXT_STMT			/ mainloop \
	/ Check_acs :                           / LEX(2) PUSH(mainloop) PUSH (assign_dft_access)   / checksub \
	/ Attributes :			/ LEX(2) PUSH(mainloop) PUSH(assign_dft)	/ attsub \
	/ Baud : <legal_baud> ;		/ LEX(2) [dft.baud_rate = baud_rate_value; dft.flags.autobaud = ""b]
									LEX(2)	/ mainloop \
	/ Baud : auto ;			/ LEX(2) [dft.baud_rate = 0; dft.flags.autobaud = "1"b]
									LEX(2)	/ mainloop \
	/ Baud : none ;			/ LEX(2) [dft.baud_rate = 0; dft.flags.autobaud = ""b]
									LEX(2)	/ mainloop \
	/ Baud :				/ LEX(2) ERROR(29) NEXT_STMT			/ mainloop \
	/ Line_type : <legal_line_type> ;	/ LEX(2) [dft.line_type = line_type_value] LEX(2)	/ mainloop \
	/ Line_type :			/ LEX(2) ERROR(11) NEXT_STMT			/ mainloop \
	/ Terminal_type : <any-token> ;		/ LEX(2) check_terminal_type
					 [dft.initial_terminal_type = terminal_type_value] LEX(2)
										/ mainloop \
	/ Terminal_type :			/ LEX(2) ERROR(8) NEXT_STMT			/ mainloop \
	/ Comment : <any-token> ;		/ LEX(2) [dft.comment = token_value] LEX(2)	/ mainloop \
	/ Initial_command : <any-token> ;	/ LEX(2) [dft.initial_command = token_value; dft.execute_initial_command = token_value ^= ""] LEX(2) / mainloop \
	/ Generic_destination : <any-token> ;	/ LEX(2) [addr (dft.initial_command) -> generic_destination = token_value; dft.generic_destination_present = token_value ^= ""] LEX(2) / mainloop \
	/ Charge : <legal_charge> ;		/ LEX(2) [dft.charge_type = charge_value] LEX(2)	/ mainloop \
	/ Charge :			/ LEX(2) ERROR(12) NEXT_STMT			/ mainloop \
	/ Service : <legal_service> ;		/ LEX(2) [dft.service_type = service_value] LEX(2) / mainloop \
	/ Service :			/ LEX(2) ERROR(16) NEXT_STMT			/ mainloop \
	/ FNP_required_up_time : <decimal-integer> ; / LEX(2) [cdt.acceptable_fnp_tbf = token.Nvalue] LEX(2)
										/ mainloop \
	/ FNP_required_up_time :		/ LEX(2) ERROR(44) NEXT_STMT			/ mainloop \
	/ Spare_channel_count : <decimal-integer> ; / LEX(2) [cdt.spare_channel_count = token.Nvalue] LEX(2)
										/ mainloop \
	/ Spare_channel_count :		/ LEX(2) ERROR(47) NEXT_STMT			/ mainloop \

	/ end ;				/					/ RETURN \
	/ <any-token> :			/ ERROR(3) NEXT_STMT			/ mainloop \
	/ <any-token>			/ ERROR(4) NEXT_STMT			/ mainloop \
	/ <no-token>			/ ERROR(5)				/ RETURN \


	\" 
	\" do_name subroutine - handles name sub-keywords

do_name	/ access_class : <access_class> ;	/ LEX(4) [cdte.access_class = access_class_value] / do_name \
	/ access_class :			/ LEX(2) ERROR(10) NEXT_STMT			/ do_name \
	/ baud : auto ;			/ LEX(2) [cdte.flags.autobaud = "1"b; cdte.baud_rate = 0]
									LEX(2)	/ do_name \
	/ baud : none ;			/ LEX(2) [cdte.flags.autobaud = "0"b; cdte.baud_rate = 0]
									LEX(2)	/ do_name \
	/ baud : <legal_baud> ;		/ LEX(2) [cdte.baud_rate = baud_rate_value] LEX(2) / do_name \
	/ baud :				/ LEX(2) ERROR(29) NEXT_STMT			/ do_name \
          / check_acs :                           / LEX(2) PUSH(do_name) PUSH(assign_access)        / checksub \
	/ attributes :			/ LEX(2) PUSH(do_name) PUSH(assign_attr)	/ attsub \
	/ answerback : <any-token> ;		/ LEX(2) [cdte.answerback = token_value] LEX(2)	/ do_name \
	/ line_type : <legal_line_type>	/ LEX(2) [cdte.line_type = line_type_value] LEX(1) / line_type \
	/ line_type :			/ LEX(2) ERROR(11) NEXT_STMT			/ do_name \
	/ dataset : <legal_dataset>		/ LEX(2) [cdte.modem_type = dataset_value] LEX(1) / dataset \
	/ dataset :			/ LEX(2) ERROR (38) NEXT_STMT			/ do_name \
	/ terminal_type : <any-token> ;		/ LEX(2) check_terminal_type
					 [cdte.initial_terminal_type = terminal_type_value] LEX(2)
										/ do_name \
	/ terminal_type :			/ LEX(2) ERROR(8) NEXT_STMT			/ do_name \
	/ comment : <any-token> ;		/ LEX(2) [cdte.comment = token_value] LEX(2)	/ do_name \
	/ initial_command : <any-token> ;	/ LEX(2) [cdte.initial_command = token_value; cdte.execute_initial_command = token_value ^= ""] LEX(2) / do_name \
	/ generic_destination : <any-token> ;	/ LEX(2) [addr (cdte.initial_command) -> generic_destination = token_value; cdte.generic_destination_present = token_value ^= ""] LEX(2) / do_name \
	/ charge : <legal_charge> ;		/ LEX(2) [cdte.charge_type = charge_value] LEX(2)	/ do_name \
	/ charge :			/ LEX(2) ERROR(12) NEXT_STMT			/ do_name \
	/ service : <legal_service> ;		/ LEX(2) [cdte.service_type = service_value] LEX(2)
										/ do_name \
	/ service :			/ LEX(2) ERROR(16) NEXT_STMT			/ do_name \
	/ multiplexer_type : <legal_mpx_type> , <legal_fnp_service> ;
					/ LEX(2) [cdte.mpx_type = mpx_type_value]
					  LEX(2) [cdte.mpx_service = service_value] LEX(2)
										/ do_name \
	/ multiplexer_type : <legal_mpx_type> ,	/ LEX(4) ERROR(16) NEXT_STMT			/ do_name\
	/ multiplexer_type : <legal_mpx_type> ;	/ LEX(2) [cdte.mpx_type = mpx_type_value] LEX(2)	/ do_name \
	/ multiplexer_type :		/ LEX(2) ERROR(51) NEXT_STMT			/ do_name \
	/ <any-token>			/ close_chn				/ STACK_POP \
	/ <no-token>			/ close_chn				/ STACK_POP \

	\" 

	\" a couple of routines to handle line_type and dataset options

dataset	/ ,				/ LEX					/ ds_opt \
	/ ;				/ LEX					/ do_name \
	/ <any-token>			/ ERROR(4) NEXT_STMT			/ do_name \
	/ <no-token>			/ ERROR(5)				/ RETURN \

ds_opt	/ private_line			/ LEX [cdte.flags.private_line = "1"b]		/ dataset \
	/ <any-token>			/ ERROR(40) NEXT_STMT			/ do_name \
	/ <no-token>			/ ERROR(5)				/ RETURN \

line_type	/ ,				/ LEX					/ lt_opt \
	/ ;				/ LEX					/ do_name \
	/ <any-token>			/ ERROR(4) NEXT_STMT			/ do_name \
	/ <no-token>			/ ERROR(5)				/ RETURN \

lt_opt	/ ebcdic				/ LEX [cdte.flags.bsc_ebcdic = "1"b]		/ line_type \
	/ ascii				/ LEX [cdte.flags.bsc_ebcdic = "0"b]		/ line_type \
	/ transparent			/ LEX [cdte.flags.bsc_transparent = "1"b]	/ line_type \
	/ nontransparent			/ LEX [cdte.flags.bsc_transparent = "0"b]	/ line_type \
	/ <any-token>			/ ERROR(41) NEXT_STMT			/ do_name \
	/ <no-token>			/ ERROR(5)				/ RETURN \

	\" do_fnp subroutine - handles FNP sub-keywords

do_fnp	/ type : <legal_fnp_type> ;		/ LEX(2) [fnpe.type = fnp_type_value] LEX(2)	/ do_fnp \
	/ type :				/ LEX(2) ERROR(19) NEXT_STMT			/ do_fnp \
	/ memory : <decimal-integer> ;	/ LEX(2) [fnpe.memory = token.Nvalue] LEX(2)	/ do_fnp \
	/ memory :			/ LEX(2) ERROR(20) NEXT_STMT			/ do_fnp \
	/ lsla : <decimal-integer> ; 		/ LEX(2) [fnpe.nlslas = token.Nvalue] LEX(2)	/ do_fnp \
	/ lsla :				/ LEX(2) ERROR(21) NEXT_STMT			/ do_fnp \
	/ hsla : <decimal-integer> ;		/ LEX(2) [fnpe.nhslas = token.Nvalue] LEX(2)	/ do_fnp \
	/ hsla :				/ LEX(2) ERROR(21) NEXT_STMT			/ do_fnp \
	/ image : <any-token> ;		/ LEX(2) [fnpe.coreimage = token_value] LEX(2)	/ do_fnp \
	/ image :				/ LEX(2) ERROR(22) NEXT_STMT			/ do_fnp \
	/ additional_info : <any-token> ;	/ LEX(2) [fnpe.coreimage = token_value] LEX(2)	/ do_fnp\
	/ additional_info :			/ [fnpe.coreimage = ""] NEXT_STMT		/ do_fnp \
	/ service : <legal_fnp_service> ;	/ LEX(2) [fnpe.service_type = service_value] LEX(2) / do_fnp \
	/ service :			/ LEX(2) ERROR(16) NEXT_STMT			/ do_fnp \
	/ multiplexer_type : <legal_mpx_type> ;	/ LEX(2) [fnpe.mpx_type = mpx_type_value] LEX(2)	/do_fnp\
	/ multiplexer_type :		/ LEX(2) ERROR(51) NEXT_STMT			/do_fnp\
	/ <any-token>			/ close_fnp				/ STACK_POP \
	/ <no-token>			/ close_fnp				/ STACK_POP \

	\" 

	\" attsub subroutine - handles attributes lists

attsub	/				/ [sx=ON; ats=dft.attributes] / \

attloop	/ ;				/ LEX					/ STACK_POP \
	/ none				/ LEX [string(ats)=""b]					/ atts \
	/ ^				/ LEX [sx=OFF]				/ \
	/ set_modes			/ LEX [ats.set_modes=sx]		/ atts \
	/ audit				/ LEX [ats.audit_access_error=sx]		/ atts \
	/ hardwired			/ LEX [ats.hardwired=sx]		/ atts \
	/ check_answerback			/ LEX [ats.ck_answerback=sx]		/ atts \
	/ dont_read_answerback		/ LEX [ats.dont_read_answerback=sx]	/ atts \
	/ <any-token>			/ ERROR(14) NEXT_STMT			/ STACK_POP \
	/ <no-token>			/ ERROR(7)				/ RETURN \

atts	/ ,				/ LEX [sx = ON]				/ attloop \
	/ ;				/ LEX					/ STACK_POP \

	/ <any-token>			/ ERROR(15) NEXT_STMT			/ STACK_POP \
	/ <no-token>			/ ERROR(7)				/ RETURN \

assign_attr /				/ [cdte.attributes = ats]
										/ STACK_POP \
assign_dft /				/ [dft.attributes=ats]
										/ STACK_POP \


checksub  /                                       / [sx=ON;access=dft.access_control]                     / \

checkloop / ;                                     / LEX				         / STACK_POP \
	/ none				/ LEX [string(access)=""b]		         / checks \
          / all				/ LEX [string(access)=ALL_ACCESS_CHECKS]         / checks \
	/ ^				/ LEX [sx=OFF]			         / \
	/ login				/ LEX [access.login=sx]		         / checks \
	/ slave_dial			/ LEX [access.slave_dial=sx]		         / checks \
	/ dial_server			/ LEX [access.dial_server=sx]		         / checks \
	/ dial_out  			/ LEX [access.dial_out=sx]		         / checks \
	/ priv_attach			/ LEX [access.priv_attach=sx]		         / checks \
	/ <any-token>			/ ERROR(27) NEXT_STMT		         / STACK_POP \
	/ <no-token>			/ ERROR(26)			         / RETURN \

checks	/ ,				/ LEX [sx=ON]			         / checkloop \
	/ ;				/ LEX				         / STACK_POP \
	/ <any-token>			/ ERROR(27) NEXT_STMT		         / STACK_POP \
	/ <no-token>			/ ERROR(26)			         / RETURN \

assign_dft_access
          /                                       / [dft.access_control=access]                    / STACK_POP \
assign_access
         /				/ [cdte.access_control=access]	         / STACK_POP \

   ++*/


cv_cmf:
     procedure;

/* automatic */

	dcl     (APstmt, APtoken, areap, cmfp)
				 ptr;

	dcl     1 dft		 aligned like cdte;
	dcl     1 ats		 aligned like cdte.attributes;
	dcl     1 access		 aligned like cdte.access_control;
	dcl     ALL_ACCESS_CHECKS	 bit (5) init ("11111"b) int static options (constant);

	dcl     access_class_value	 (2) bit (72) aligned;
	dcl     charge_value	 fixed bin;
	dcl     service_value	 fixed bin;
	dcl     line_type_value	 fixed bin;
	dcl     terminal_type_value	 char (32);
	dcl     fnp_type_value	 fixed bin;
	dcl     dataset_value	 fixed bin;
	dcl     baud_rate_value	 fixed bin;
	dcl     mpx_type_value	 fixed bin;

	dcl     mpx_error_printedp	 ptr init (null ());
	dcl     system_areap	 ptr;

	dcl     dn		 char (168);
	dcl     (supplied_en, cmf_en, cdt_en)
				 char (32);
	dcl     (i, n)		 fixed bin;
	dcl     fnpno		 fixed bin;
	dcl     ndev		 fixed bin;
	dcl     bitc		 fixed bin (24);
	dcl     (argc, argx)	 fixed bin;
	dcl     ap		 ptr;
	dcl     al		 fixed bin (21);
	dcl     ec		 fixed bin (35);
	dcl     code		 fixed bin (35);
	dcl     created_table_segment	 bit (1) aligned;
	dcl     sx		 bit (1) aligned;
	dcl     fb35		 fixed bin (35);

	dcl     1 dvt		 (16) aligned,
		2 device_id	 char (8),
		2 device_prices	 (0:7) float;

/* based */

	dcl     bchr		 char (al) unal based (ap);
	dcl     system_area		 area based (system_areap);
	dcl     mpx_error_printed	 (hbound (mpx_types, 1), 99) bit (1) unal based (mpx_error_printedp);

/* builtin */

	declare (addr, bin, character, collate, dimension, divide, hbound, index, lbound, length, ltrim, max, null,
	        rank, rel, rtrim, size, string, substr, translate, unspec, verify, wordno)
				 builtin;

/* conditions */

	declare cleanup		 condition;
	declare sub_error_		 condition;

/* entries */

	dcl     convert_authorization_$from_string_range
				 entry ((2) bit (72) aligned, character (*), fixed binary (35));
	dcl     convert_status_code_	 entry (fixed bin (35), char (8) aligned, char (100) aligned);
	dcl     cu_$arg_count	 entry (fixed bin, fixed bin (35));
	dcl     cu_$arg_ptr		 entry (fixed bin, ptr, fixed bin (21), fixed bin (35));
	dcl     cdt_mgr_$thread	 entry (ptr, fixed bin (35));
	dcl     cv_dec_check_	 entry (char (*), fixed bin (35)) returns (fixed bin);
	dcl     find_condition_info_	 entry (ptr, ptr, fixed bin (35));
	dcl     get_wdir_		 entry () returns (char (168));
	dcl     get_group_id_	 entry () returns (char (32) aligned);
	dcl     get_shortest_path_	 entry (char (*)) returns (char (168));
	dcl     get_system_free_area_	 entry () returns (ptr);
	dcl     expand_pathname_	 entry (char (*), char (*), char (*), fixed bin (35));
	dcl     expand_pathname_$add_suffix
				 entry (char (*), char (*), char (*), char (*), fixed bin (35));
	dcl     com_err_		 entry options (variable);
	dcl     system_info_$device_prices
				 entry (fixed bin, ptr);
	dcl     lex_error_		 entry options (variable);
	dcl     lex_string_$init_lex_delims
				 entry (char (*), char (*), char (*), char (*), char (*), bit (*), char (*) var,
				 char (*) var, char (*) var, char (*) var);
	dcl     lex_string_$lex	 entry (ptr, fixed bin, fixed bin, ptr, bit (*), char (*), char (*), char (*),
				 char (*), char (*), char (*) var, char (*) var, char (*) var, char (*) var, ptr,
				 ptr, fixed bin (35));
	dcl     translator_temp_$get_segment
				 entry (char (*), ptr, fixed bin (35));
	dcl     translator_temp_$release_all_segments
				 entry (ptr, fixed bin (35));

	dcl     com_err_$suppress_name entry () options (variable);
	dcl     initiate_file_	 entry (char (*), char (*), bit (*), ptr, fixed bin (24), fixed bin (35));
	dcl     hcs_$make_seg	 entry (char (*), char (*), char (*), fixed bin (5), ptr, fixed bin (35));
	dcl     hcs_$truncate_seg	 entry (ptr, fixed bin, fixed bin (35));
	dcl     ttt_info_$terminal_data
				 entry (char (*), fixed bin, fixed bin, ptr, fixed bin (35));
	dcl     match_star_name_	 entry (char (*), char (*), fixed bin (35));
	dcl     sort_items_$general	 entry (ptr, entry);
	dcl     hcs_$make_entry	 entry (ptr, char (*), char (*), entry, fixed bin (35));
	dcl     parse_tty_name_	 entry (char (*), fixed bin, bit (1), fixed bin, fixed bin);
	dcl     pathname_		 entry (char (*), char (*)) returns (char (168));
	dcl     suffixed_name_$new_suffix
				 entry (char (*), char (*), char (*), char (32), fixed bin (35));
	dcl     terminate_file_	 entry (ptr, fixed bin (24), bit (*), fixed bin (35));

/* internal static */

	dcl     (
	        (
	        ON		 bit (1) initial ("1"b),
	        OFF		 bit (1) initial ("0"b),
	        my_name		 char (6) initial ("cv_cmf")
	        )			 options (constant),
	        first		 bit (1) initial ("1"b),
	        (LEXDLM, LEXCTL)	 char (128) var,
	        BREAKS		 char (128) var,
	        IGBREAKS		 char (128) var
	        )			 internal static;

	dcl     services		 (8) char (12) static options (constant)
				 init ("login", "FTP", "MC", "slave", "", "autocall", "inactive", "multiplexer");

/* external static */

	dcl     (
	        error_table_$translation_failed,
	        error_table_$noentry,
	        error_table_$too_many_args,
	        error_table_$badopt,
	        error_table_$zero_length_seg,
	        error_table_$noarg,
	        error_table_$bad_conversion
	        )			 fixed bin (35) external static;
	dcl     sys_info$max_seg_size	 ext fixed bin (18);

	dcl     cv_cmf_severity_	 fixed bin (35) external init (0);

/* include files */
/* format: off */
%page; %include cdt;
%page; %include author_dcl;
%page; %include line_types;
%page; %include baud_rates;
%page; %include dataset_names;
%page; %include multiplexer_types;
%page; %include dialup_values;
%page; %include sub_error_info;
%include condition_info_header;
%include condition_info;
	declare 1 auto_condition_info	 aligned like condition_info;
%page; %include terminate_file;
%page; %include access_mode_values;
/* format: on */

/* program */

	cmfp = null;				/* Initialize for cleanup handler */
	cdtp = null;				/* .. */
	areap = null;				/* .. */
	dn, supplied_en = "";
	created_table_segment = ""b;

	on cleanup
	     begin;
		call clean_up;			/* do any tidying up of address space */
		cv_cmf_severity_ = 5;		/* fie on you */
	     end;

	call cu_$arg_count (argc, ec);		/* Note AF calls */
	if ec ^= 0
	then do;
		call com_err_ (ec, my_name);
		go to severity_5_failure;
	     end;

	if argc = 0
	then do;
give_usage:
		call com_err_$suppress_name (0, my_name, "Usage: cv_cmf CMF (-brief|-bf|-long|-lg)");
		go to severity_5_failure;
	     end;

	do argx = 1 to argc;
	     call cu_$arg_ptr (argx, ap, al, (0));
	     if character (bchr, 1) ^= "-"
	     then do;
		     if supplied_en ^= ""
		     then do;
			     call com_err_ (error_table_$too_many_args, my_name,
				"Only one pathname may be given. ^a was the second.", bchr);
			     go to severity_5_failure;
			end;

		     call expand_pathname_ (bchr, dn, supplied_en, ec);
		     if ec ^= 0
		     then do;
path_error:
			     call com_err_ (ec, my_name, "^a", bchr);
			     go to severity_5_failure;
			end;
		     call expand_pathname_$add_suffix (bchr, "cmf", dn, cmf_en, ec);
		     if ec ^= 0
		     then go to path_error;

		     call suffixed_name_$new_suffix (supplied_en, "cmf", "cdt", cdt_en, ec);
						/* if we get this far, how can we fail? */
		     if ec ^= 0			/* still, let's have a look */
		     then go to path_error;

		end;				/* Pathname case */

	     else if bchr = "-bf"
	     then SERROR_CONTROL = "01"b;
	     else if bchr = "-brief"
	     then SERROR_CONTROL = "01"b;
	     else if bchr = "-long" | bchr = "-lg"
	     then SERROR_CONTROL = "10"b;
	     else if bchr = "-severity" | bchr = "-sv"
	     then do;
		     if argx >= argc
		     then do;
			     call com_err_ (error_table_$noarg, my_name, "After ""^a"".", bchr);
			     go to severity_5_failure;
			end;
		     argx = argx + 1;
		     call cu_$arg_ptr (argx, ap, al, ec);
		     fb35 = cv_dec_check_ (bchr, ec);
		     if ec ^= 0 | fb35 < 0 | fb35 > 5
		     then do;
			     call com_err_ (error_table_$bad_conversion, my_name,
				"Severity must be an integer in the range 0 - 5, not ""^a"".", bchr);
			     go to severity_5_failure;
			end;
		     MIN_PRINT_SEVERITY = fb35;
		end;
	     else do;
		     call com_err_ (error_table_$badopt, my_name, "^a", bchr);
		     go to severity_5_failure;
		end;
	end;					/* Arg Loop */

	if supplied_en = ""
	then go to give_usage;
	call system_info_$device_prices (ndev, addr (dvt));

	call initiate_file_ (dn, cmf_en, R_ACCESS, cmfp, bitc, ec);
	if ec = error_table_$noentry
	then if cmf_en ^= supplied_en
	     then do;
		     call initiate_file_ (dn, supplied_en, R_ACCESS, cmfp, bitc, ec);
		     if ec = 0
		     then do;
			     call com_err_ (0, my_name, "Warning: converting ^a. The segment should be named ^a.",
				pathname_ (dn, supplied_en), cmf_en);
			     cmf_en = supplied_en;
			end;
		end;
	if ec ^= 0
	then do;
cmf_error:
		call com_err_ (ec, my_name, "^a.", pathname_ (dn, cmf_en));
		go to severity_5_failure;
	     end;

	n = divide (bitc + 8, 9, 24, 0);
	if n = 0
	then do;
		ec = error_table_$zero_length_seg;
		go to cmf_error;
	     end;

	dn = get_wdir_ ();
	call hcs_$make_seg (dn, cdt_en, "", 1010b, cdtp, ec);
	created_table_segment = (ec = 0);
	if cdtp = null
	then do;
cdt_error:
		call com_err_ (ec, my_name, "^a.", pathname_ (dn, cdt_en));
		go to severity_5_failure;
	     end;

	call hcs_$truncate_seg (cdtp, 0, ec);		/* if we set it all to ""b, it would take all the page faults */
	if ec ^= 0
	then go to cdt_error;

	cdtep = addr (cdt.cdt_entry (1));

	cdt.author.proc_group_id = get_group_id_ ();	/* Initialize the header of the new cdt */
	cdt.author.table = "CDT";
	dn = get_shortest_path_ (dn);
	cdt.author.w_dir = substr (dn, 1, length (cdt.author.w_dir));
	cdt.author.last_install_time = 0;
	cdt.version = CDT_version;
	cdt.max_size = divide (sys_info$max_seg_size - bin (rel (cdtep), 18), size (cdte), 17, 0);
	cdt.acceptable_fnp_tbf = 5;
	cdt.spare_channel_count = 10;

	unspec (dft) = "0"b;			/* Zero the defaults .. the lazy way */
	cdtep = addr (dft);				/* Set up cdtep to point at the dfts. */
	dft.name = "";				/* Initialize defaults */
	dft.comment = "";
	dft.charge_type = 0;
	dft.line_type = LINE_UNKNOWN;
	dft.initial_terminal_type = "";		/* nothing special */
	dft.current_terminal_type = "";
	dft.tty_id_code = "";
	string (dft.user_name) = "";
	dft.baud_rate = 300;
	dft.modem_type = 0;
	string (dft.flags) = "0"b;
	dft.service_type = ANS_SERVICE;		/* dialup line */
	dft.answerback = "";
	dft.initial_command = "";
	call convert_authorization_$from_string_range (dft.access_class, "system_low", code);

	call translator_temp_$get_segment (my_name, areap, code);
	if areap = null
	then do;
		call com_err_ (code, my_name, "While making a temporary segment in the process directory.");
		go to severity_5_failure;
	     end;

	if first
	then do;
		BREAKS = substr (collate (), 1, 8) || substr (collate (), 10, 24) || "()*,:;^";
		IGBREAKS = substr (collate (), 1, 8) || substr (collate (), 10, 24);
		call lex_string_$init_lex_delims ("""", """", "/*", "*/", ";", "10"b, BREAKS, IGBREAKS, LEXDLM,
		     LEXCTL);
		first = "0"b;
	     end;

	call lex_string_$lex (cmfp, n, 0, areap, "100"b, """", """", "/*", "*/", ";", BREAKS, IGBREAKS, LEXDLM, LEXCTL,
	     APstmt, APtoken, ec);

	Pthis_token = APtoken;
	call SEMANTIC_ANALYSIS ();

/* now let individual sub-channels judge on their channels */

	system_areap = get_system_free_area_ ();
	allocate mpx_error_printed in (system_area);
	string (mpx_error_printed) = "0"b;

	do i = 1 to hbound (cdt.fnp_entry, 1);
	     fnpep = addr (cdt.fnp_entry (i));
	     if fnpe.state ^= FNP_FREE
	     then call process_subchans (fnpep, substr (collate (), rank ("a") + i, 1), fnpe.mpx_type);
	end;

	do i = 1 to cdt.current_size;
	     cdtep = addr (cdt.cdt_entry (i));
	     if cdte.service_type = MPX_SERVICE
	     then call process_subchans (cdtep, (cdte.name), (cdte.mpx_type));
	end;

/* thread proc will detect missing parents and the like */

	on sub_error_
	     begin;
		declare almsg		 char (100) aligned;
		call find_condition_info_ (null, addr (auto_condition_info), (0));
		sub_error_info_ptr = auto_condition_info.info_ptr;
		call convert_status_code_ (sub_error_info.status_code, "", almsg);
		if ^sub_error_info.default_restart	/* not warning */
		then do;
			call semant_error (6, (almsg), (sub_error_info.info_string));
			go to abort;
		     end;
		call semant_error (25, (almsg), (sub_error_info.info_string));
						/* Warning */
	     end;

	call cdt_mgr_$thread (cdtp, (0));		/* we look at condition, not code */
	revert sub_error_;

abort:
	if MERROR_SEVERITY > 2
	then do;
		call com_err_ (error_table_$translation_failed, my_name, cmf_en);
		if created_table_segment
		then bitc = -1;
		else bitc = 0;
	     end;
	else bitc = (wordno (addr (cdt.cdt_entry (1))) + size (cdte) * cdt.current_size) * 36;
	if bitc >= 0
	then do;
		call terminate_file_ (cdtp, bitc, TERM_FILE_TRUNC_BC_TERM, ec);
		if ec ^= 0
		then do;
			call com_err_ (ec, my_name, "Unable to set bitcount on ^a to ^d.", pathname_ (dn, cdt_en),
			     bitc);
			go to severity_5_failure;
		     end;
	     end;

	call clean_up;
	cv_cmf_severity_ = MERROR_SEVERITY;
	return;

severity_5_failure:
	call clean_up;
	cv_cmf_severity_ = 5;
	return;


clean_up:
     procedure;

	if cmfp ^= null
	then call terminate_file_ (cmfp, (0), TERM_FILE_TERM, (0));

	if cdtp ^= null
	then if created_table_segment
	     then do;
		     call terminate_file_ (cdtp, (0), TERM_FILE_DELETE, (0));
		     cdtp = null;
		end;
	     else call terminate_file_ (cdtp, (0), TERM_FILE_TRUNC_BC_TERM, (0));

	if areap ^= null
	then call translator_temp_$release_all_segments (areap, (0));

     end clean_up;

/* SYNTAX FUNCTIONS */

access_class:
     procedure () returns (bit (1) aligned);

	call convert_authorization_$from_string_range (access_class_value, token_value, code);
	return (code = 0);

     end access_class;




legal_line_type:
     proc returns (bit (1));

	dcl     temp_line_type	 character (16);	/* because we want to make changes to names, we need auto version */

	temp_line_type = token_value;

	if temp_line_type = "Network"
	then temp_line_type = "TELNET";

	do line_type_value = lbound (line_types, 1) to hbound (line_types, 1)
	     while (temp_line_type ^= line_types (line_type_value));
	end;
	if line_type_value > hbound (line_types, 1)
	then return ("0"b);				/* The 1050, 2741, ETX and ARDS line types are no longer valid. */
	if line_type_value = LINE_1050 | line_type_value = LINE_2741 | line_type_value = LINE_ARDS
	     | line_type_value = LINE_ETX
	then return ("0"b);
	return ("1"b);

     end legal_line_type;




legal_charge:
     proc returns (bit (1) aligned);

	if token_value = "none"
	then do;
		charge_value = 0;
		return ("1"b);
	     end;

	do charge_value = 1 to ndev while (token_value ^= dvt.device_id (charge_value));
	end;
	if charge_value > ndev
	then return ("0"b);
	return ("1"b);

     end legal_charge;

legal_service:
     proc returns (bit (1) aligned);

	dcl     temp_service_type	 character (12);	/* because we want to make changes, we need auto variable */

	temp_service_type = token_value;

	if temp_service_type = "mc"
	then temp_service_type = "MC";
	else if temp_service_type = "ftp"
	then temp_service_type = "FTP";

	do service_value = lbound (services, 1) to hbound (services, 1)
	     while (temp_service_type ^= services (service_value));
	end;
	if service_value > hbound (services, 1)
	then return ("0"b);
	return ("1"b);

     end legal_service;




legal_fnp_type:
     proc returns (bit (1) aligned);

	do fnp_type_value = 1 to hbound (fnp_types, 1) while (token_value ^= fnp_types (fnp_type_value));
	end;
	if fnp_type_value > hbound (fnp_types, 1)
	then do;
		if token_value ^= "HNP"
		then return ("0"b);
		fnp_type_value = 3;
	     end;
	return ("1"b);

     end legal_fnp_type;




legal_baud:
     proc returns (bit (1) aligned);

	baud_rate_value = cv_dec_check_ (token_value, code);
	if code ^= 0
	then return ("0"b);

	if baud_rate_value = 0
	then return ("1"b);
	if baud_rate_value = 133			/* no longer valid */
	then return ("0"b);
	do i = 1 to hbound (baud_table, 1) while (baud_table (i) ^= baud_rate_value);
	end;
	if i > hbound (baud_table, 1)
	then return ("0"b);
	return ("1"b);

     end legal_baud;

legal_fnp_service:
     proc returns (bit (1) aligned);

	if token_value = "active"
	then service_value = ACTIVE;
	else if token_value = "inactive"
	then service_value = INACTIVE;
	else return ("0"b);
	return ("1"b);

     end legal_fnp_service;




legal_dataset:
     proc returns (bit (1) aligned);

	do dataset_value = 1 to hbound (dataset_names, 1) while (token_value ^= dataset_names (dataset_value));
	end;
	if dataset_value > hbound (dataset_names, 1)
	then return ("0"b);
	return ("1"b);

     end legal_dataset;




legal_mpx_type:
     proc returns (bit (1) aligned);

	do mpx_type_value = 1 to hbound (mpx_types, 1) while (token_value ^= mpx_types (mpx_type_value));
	end;
	if mpx_type_value > hbound (mpx_types, 1)
	then return ("0"b);
	else return ("1"b);

     end legal_mpx_type;

/* SEMANTIC FUNCTIONS */

open_chn:
     proc;

	dcl     i			 fixed bin;
	dcl     fnp_no		 fixed bin;
	dcl     (name1, name2)	 char (64) var;
	dcl     (chan1, chan2)	 fixed bin;
	dcl     pic9		 picture "999999";

	if cdt.current_size = cdt.max_size
	then do;
		pic9 = cdt.max_size;
		call semant_error (9, ltrim (pic9, "0"), "");
		go to abort;
	     end;

	channels_open = 0;
	i = index (token_value, "-");			/* check for range of channels */
	if i = 0
	then do;					/* one channel */
		name1 = token_value;
		channels_open = 1;
	     end;
	else do;
		if i = 1 | i = token.Lvalue
		then do;				/* bad range syntax */
bad_pair:
			call ERROR (45);
			name1 = "dummy";
			channels_open = 1;
			go to make_cdt_entry;
		     end;
		name1 = substr (token_value, 1, i - 1);
		name2 = substr (token_value, i + 1);
		if length (name1) ^= length (name2)
		then go to bad_pair;
		if name1 = name2
		then go to bad_pair;
		do i = 1 to length (name1) while (substr (name1, i, 1) = substr (name2, i, 1));
		end;				/* count equal characters */
		channel_digits = length (name1) - i + 1;/* number of digits that will vary */
		if verify (substr (name1, i), "0123456789") ^= 0
		then go to bad_pair;
		if verify (substr (name2, i), "0123456789") ^= 0
		then go to bad_pair;
		chan1 = bin (substr (name1, i));	/* starting channel */
		chan2 = bin (substr (name2, i));	/* ending channel */
		if chan1 >= chan2
		then go to bad_pair;
		channels_open = chan2 - chan1 + 1;	/* number of channels to generate */
	     end;

	call check_chan_name ((name1), fnp_no, code);
	if code ^= 0
	then do;
		name1 = "dummy";
		channels_open = 1;
	     end;

make_cdt_entry:
	cdt.current_size, cdt.n_cdtes = cdt.n_cdtes + 1;
	cdtep = addr (cdt.cdt_entry (cdt.n_cdtes));
	cdte = dft;
	cdte.in_use = NOW_HUNG_UP;
	cdte.name = name1;

     end open_chn;

/* declare these variables in the external procedure, so open_chn and close_chn can share them */

	dcl     channels_open	 fixed bin;
	dcl     channel_digits	 fixed bin;




close_chn:
     proc;

	dcl     p			 ptr;
	dcl     new_name		 char (64) var;
	dcl     i			 fixed bin;
	dcl     pic9		 picture "99999999";
	dcl     fnp_no		 fixed bin;

	if cdt.current_size = 0
	then return;
	if cdte.answerback ^= ""
	then cdte.flags.ck_answerback = "1"b;

	if verify (cdte.comment, substr (collate (), 8, 9) || substr (collate (), 33)) ^= 0
	then call semant_error (18, (cdte.name), "");

	if cdte.flags.execute_initial_command & cdte.flags.generic_destination_present
	then do;
		call semant_error (55, (cdte.name), "");
		cdte.flags.execute_initial_command, cdte.flags.generic_destination_present = "0"b;
	     end;					/* can't allow since these fields overlay */

	if cdte.service_type = MPX_SERVICE
	then do;					/* check for consistency of multiplexer stuff */
		if cdte.mpx_type = 0
		then /* must be specified */
		     call semant_error (48, (cdte.name), "");
		if cdte.flags.execute_initial_command | cdte.flags.generic_destination_present
		then /* cant allow this, as field is redefined */
		     call semant_error (49, (cdte.name), "");
		unspec (cdte.initial_command) = "0"b;	/* reset for mpx use */
		cdte.flags.execute_initial_command, cdte.flags.generic_destination_present = "0"b;
		if cdte.mpx_service = 0
		then cdte.mpx_service = ACTIVE;
	     end;
	else if cdte.mpx_type ^= 0
	then /* cant have multiplexer type for non-mpx chan */
	     call semant_error (50, (cdte.name), "");


	do i = 2 to channels_open;
	     if cdt.current_size = cdt.max_size
	     then do;
		     pic9 = cdt.max_size;
		     call semant_error (9, ltrim (pic9, "0"), "");
		     go to abort;
		end;
	     new_name = rtrim (cdte.name);
	     pic9 = bin (substr (new_name, length (new_name) - channel_digits + 1)) + 1;
	     substr (new_name, length (new_name) - channel_digits + 1) =
		substr (pic9, length (pic9) - channel_digits + 1);
	     call check_chan_name ((new_name), fnp_no, code);

	     cdt.current_size, cdt.n_cdtes = cdt.current_size + 1;
						/* one more channel entry */
	     p = addr (cdt.cdt_entry (cdt.n_cdtes));	/* get ptr to new entry */
	     p -> cdte = cdte;			/* copy previous entry into this one */
	     cdtep = p;				/* move cdtep to latest entry */
	     cdte.name = new_name;			/* put in the new name */

	end;


	return;

     end close_chn;




/* Procedure to check syntax of a channel name */

check_chan_name:
     proc (name, fnp_no, code);

	dcl     name		 char (*);
	dcl     fnp_no		 fixed bin;
	dcl     code		 fixed bin (35);
	dcl     i			 fixed bin;
	dcl     p			 ptr;
	dcl     hsla		 bit (1);
	dcl     la_no		 fixed bin;
	dcl     subchan		 fixed bin;

	fnp_no = 0;
	code = 0;
	if name = "" | length (name) > length (cdte.name)
	then do;
bad_chan_name:
		call semant_error (28, name, "");
		code = 1;				/* error return */
		return;
	     end;
	if (substr (name, 1, 3) = "net" | substr (name, 1, 3) = "ftp") & length (name) = 6
	     & verify (substr (name, 4), "0123456789") = 0
	then ;
	else do;
		if substr (name, 2, 1) = "."
		then do;
			call parse_tty_name_ (name, fnp_no, hsla, la_no, subchan);
			if fnp_no < 0
			then fnp_no = 0;
		     end;
		if index (name, "..") ^= 0
		then go to bad_chan_name;
		if substr (name, length (name), 1) = "."
		then go to bad_chan_name;
		if substr (name, 1, 1) = "."
		then goto bad_chan_name;
	     end;

	do i = 1 to cdt.current_size;
	     p = addr (cdt.cdt_entry (i));
	     if p -> cdte.name = name
	     then call semant_error (13, name, "");
	end;

	return;

     end check_chan_name;

/* let the multiplexer rule on the validity of any of its sub-channels */

process_subchans:
     proc (cdt_entryp, mpx_name, mpx_type);

	dcl     cdt_entryp		 ptr;		/* cdtep or fnpep */
	dcl     mpx_name		 char (*);
	dcl     mpx_type		 fixed bin;

	dcl     (star_name, segname, entryname)
				 char (32);
	dcl     i			 fixed bin;
	dcl     p			 ptr;
	dcl     entvar		 entry (ptr, ptr, char (*), ptr, entry) variable;

	dcl     1 cdt_list		 aligned,
		2 count		 fixed bin,
		2 cdte_ptr	 (cdt.current_size) ptr unal;

	star_name = rtrim (mpx_name) || ".*";
	cdt_list.count = 0;
	do i = 1 to cdt.current_size;
	     p = addr (cdt.cdt_entry (i));
	     call match_star_name_ ((p -> cdte.name), star_name, code);
	     if code = 0
	     then do;
		     cdt_list.count = cdt_list.count + 1;
		     cdt_list.cdte_ptr (cdt_list.count) = p;
		end;
	end;

	if cdt_list.count > 1
	then call sort_items_$general (addr (cdt_list), chan_name_compare);

	segname = "as_" || rtrim (mpx_types (mpx_type)) || "_mpx_";
	entryname = rtrim (mpx_types (mpx_type)) || "_cv_cmf";
	call hcs_$make_entry (null (), segname, entryname, entvar, code);
	if code ^= 0
	then call semant_error (54, rtrim (segname) || "$" || rtrim (entryname), mpx_name);
	else do;
		mpx_type_value = mpx_type;
		call entvar (cdtp, cdt_entryp, mpx_name, addr (cdt_list), mpx_error);
	     end;

     end process_subchans;




chan_name_compare:
     proc (p, q) returns (fixed bin (1));

	dcl     (p, q)		 ptr unal;

	if p -> cdte.name < q -> cdte.name
	then return (-1);
	if p -> cdte.name > q -> cdte.name
	then return (+1);
	return (0);

     end chan_name_compare;

open_fnp:
     proc;

	if length (token_value) > 1
	then call ERROR (23);
	fnpno = index ("ABCDEFGH", token_value);
	if fnpno = 0
	then fnpno = index ("abcdefgh", token_value);
	if fnpno = 0
	then do;
		call ERROR (23);
		fnpno = 1;			/* to avoid blowing up later */
	     end;

	fnpep = addr (cdt.fnp_entry (fnpno));
	if fnpe.state > 0
	then call ERROR (24);

	fnpe.state = FNP_DOWN;			/* configured, not loaded */
	fnpe.service_type = ACTIVE;			/* defaults */
	fnpe.mpx_type = MCS_MPX;
	return;

     end open_fnp;




close_fnp:
     proc;

	return;

     end close_fnp;




check_terminal_type:
     proc;

	terminal_type_value = translate (token_value, "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz");
	if terminal_type_value = "NONE"
	then terminal_type_value = "";

	else do;
		call ttt_info_$terminal_data (terminal_type_value, -1, 0, null, code);
		if code ^= 0
		then call statement_error (46, token_value, "");
						/* warn them about this */
	     end;

     end check_terminal_type;

semant_error:
     proc (error_num, parm1, parm2);

	dcl     error_num		 fixed bin,
	        (parm1, parm2)	 char (*);
	dcl     (stmt_ptr, token_ptr)	 ptr init (null);

	goto call_error;

statement_error:
     entry (error_num, parm1, parm2);

	stmt_ptr = token.Pstmt;			/* print the source statement */
	token_ptr = Pthis_token;

call_error:
	if error_control_table (error_num).severity >= MIN_PRINT_SEVERITY
	then call lex_error_ (error_num, SERROR_PRINTED (error_num), (error_control_table.severity (error_num)),
		MERROR_SEVERITY, stmt_ptr, token_ptr, SERROR_CONTROL, (error_control_table.message (error_num)),
		(error_control_table.brief_message (error_num)), parm1, parm2);
	else do;
		MERROR_SEVERITY = max (MERROR_SEVERITY, error_control_table (error_num).severity);
		SERROR_PRINTED (error_num) = "1"b;
	     end;
	return;

     end;




/* this error routine is the interface to rdc errors for multiplexer parsers */

mpx_error:
     proc (error_num, severity, long_text, short_text, parm1, parm2);

	dcl     error_num		 fixed bin;
	dcl     severity		 fixed bin;
	dcl     (long_text, short_text)
				 char (*);
	dcl     (parm1, parm2)	 char (*);

	dcl     (i, j)		 fixed bin;

	i = error_num;
	if i < 1 | i > 99
	then i = 99;
	j = 100 * mpx_type_value + i;
	if severity >= MIN_PRINT_SEVERITY
	then call lex_error_ (j, mpx_error_printed (mpx_type_value, i), severity, MERROR_SEVERITY, null (), null (),
		SERROR_CONTROL, long_text, short_text, parm1, parm2);
	else do;
		MERROR_SEVERITY = max (MERROR_SEVERITY, severity);
		mpx_error_printed (mpx_type_value, i) = "1"b;
	     end;
	return;

     end mpx_error;

	dcl     1 error_control_table	 (55) aligned int static,
		2 severity	 fixed bin (17) unal init ((5) 3,
						/* 1-5 */
				 1,		/* 6 */
				 (2) 3,		/* 7 - 8 */
				 3,		/* 9 */
				 (7) 3,		/* 10-16 */
				 (2) 1,		/* 17-18 */
				 (6) 3,		/* 19-24 */
				 3,		/* 25 */
				 (20) 3,		/* 26-45 */
				 1,		/* 46 */
				 (7) 3,		/* 47-53 */
				 1,		/* 54 */
				 3),		/* 55 */
		2 Soutput_stmt	 bit (1) unaligned initial ("1"b,
						/* 1 */
				 "0"b,		/* 2 */
				 (2) (1)"1"b,	/* 3-4 */
				 "0"b,		/* 5 */
				 "0"b,		/* 6 */
				 "0"b,		/* 7 */
				 (9) (1)"1"b,	/* 8-16 */
				 (2) (1)"0"b,	/* 17-18 */
				 (6) (1)"1"b,	/* 19-24 */
				 "0"b,		/* 25 */
				 "0"b,		/* 26 */
				 "1"b,		/* 27 */
				 (2) (1)"1"b,	/* 28-29 */
				 (8) (1)"0"b,	/* 30-37 */
				 "1"b,		/* 38 */
				 "0"b,		/* 39 */
				 (2) (1)"1"b,	/* 40-41 */
				 (2) (1)"0"b,	/* 42-43 */
				 (2) (1)"1"b,	/* 44-45 */
				 "0"b,		/* 46 */
				 "1"b,		/* 47 */
				 (3) (1)"0"b,	/* 48-50 */
				 "1"b,		/* 51 */
				 (4) (1)"0"b),	/* 52-55 */
		2 message		 char (100) var init ("",
						/* 1 */
				 "CMF is empty.",	/* 2 */
				 "Unrecognizable statement.",
						/* 3 */
				 "Syntax error.",	/* 4 */
				 "Premature end of CMF encountered. Check for a missing end statement.",
						/* 5 */
				 "^a ^a",		/* MSG is from cdt_mgr_ */
						/* 6 */
				 "Premature end of CMF during attribute statement.",
						/* 7 */
				 "Invalid terminal type ""^a"".",
						/* 8 */
				 "Too many channels declared in CMF. Maximum is ^a.",
						/* 9 */
				 "Invalid access class ""^a"".",
						/* 10 */
				 "Invalid line type ""^a"".",
						/* 11 */
				 "Invalid charge ""^a""",
						/* 12 */
				 "Channel ^a is specified more than once in the CMF.",
						/* 13 */
				 "Invalid attribute ""^a"".",
						/* 14 */
				 "Syntax error. The ""none"" attribute must be followed by a semi-colon.",
						/* 15 */
				 "Invalid service type ""^a"".",
						/* 16 */
				 """^a"" appears to have the wrong line type. ARPANET channels must specify 'line_type: ^a;'",
						/* 17 */
				 "The comment for ""^a"" contains non-printing ASCII characters.",
						/* 18 */
				 "Invalid FNP type ""^a"".",
						/* 19 */
				 "Invalid FNP memory size ""^a"".",
						/* 20 */
				 """^a"" is not a valid lsla/hsla count.",
						/* 21 */
				 "Syntax error. No image specified.",
						/* 22 */
				 "Invalid FNP identifier ""^a"".",
						/* 23 */
				 "FNP ^a is specified more than once.",
						/* 24 */
				 "Fatal error while threading CDT tree. ^a ^a.",
						/* 25 */
				 "Premature of CMF while in check_acs statement.",
						/* 26 */
				 "Unrecognized check acs flag ""^a"".",
						/* 27 */
				 """^a"" is not a valid channel name.",
						/* 28 */
				 """^a"" is not a valid baud rate.",
						/* 29 */
				 "NOT USED",	/* 30 */
				 "NOT USED",	/* 31 */
				 "NOT USED",	/* 32 */
				 "NOT USED",	/* 33 */
				 "NOT USED",	/* 34 */
				 "NOT USED",	/* 35 */
				 "NOT USED",	/* 36 */
				 "NOT USED",	/* 37 */
				 "Invalid dataset type ""^a"".",
						/* 38 */
				 "NOT USED",	/* 39 */
				 "Unrecognized dataset option. ""^a""",
						/* 40 */
				 "Unrecognized line_type option. ""^a""",
						/* 41 */
				 "NOT USED",	/* 42 */
				 "NOT USED",	/* 43 */
				 "Invalid FNP required up time ""^a"".",
						/* 44 */
				 "Invalid channel name pair ""^a"".",
						/* 45 */
				 "Terminal type ""^a"" not found in TTT.",
						/* 46 */
				 "Invalid spare channel count ""^a"".",
						/* 47 */
				 "Multiplexer type not specified for multiplexer ""^a"".",
						/* 48 */
				 "Neither initial command nor generic destination can be specified for multiplexer: ""^a"".",
						/* 49 */
				 "Multiplexer type specified for non-multiplexer channel ""^a"".",
						/* 50 */
				 "Illegal multiplexer type: ""^a"".",
						/* 51 */
				 "Channel ""^a"" configured, but its parent, ""^a"", is not configured as a multiplexer.",
						/* 52 */
				 "Channel ""^a"" configured, but its parent, ""^a"", is not.",
						/* 53 */
				 "Cannot find ""^a"" to check configuration of ""^a"".",
						/* 54 */
				 "Only one of initial command and generic destination may be specified: ""^a""."),
						/* 55 */
		2 brief_message	 char (20) var init ((7) (1)"",
						/* 1-7 */
				 "^a",		/* 8 */
				 "",		/* 9 */
				 (5) (1)"^a",	/* 10-14  */
				 "",		/* 15 */
				 (6) (1)"^a",	/* 16-21 */
				 "",		/* 22 */
				 (3) (1)"^a",	/* 23-25 */
				 (2) (1)"FNP ^a ^a",/* 26-27 */
				 (24) (1)"^a",	/* 28-51 */
				 (2) (1)"^a on ^a", /* 52-53 */
				 "^a for ^a",	/* 54 */
				 "^a");		/* 55 */

/* ======================================================== */
