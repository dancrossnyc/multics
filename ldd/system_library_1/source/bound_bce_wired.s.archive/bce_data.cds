/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1984 *
   *                                                         *
   *********************************************************** */
/* BCE_DATA.CDS data for the Bootload Command Environment */
/* BIM 8/82 */
/* Modified by Keith Loepere various times in 83 for bce switches */
/* format: style4,indattr,ifthenstmt,ifthen,idind33,^indcomtxt */

bce_data:
     procedure;


declare  create_data_segment_		  entry (ptr, fixed bin (35));
declare  com_err_			  entry () options (variable);
declare  code			  fixed bin (35);
declare  PADSTAR			  (1) char (32) init ("pad*") int static options (constant);

declare  1 bce_data_static		  aligned,

/* bce switches--- these must be in entry-ptr order */

	 2 console_put_chars	  entry,
	 2 console_put_chars_data_ptr   ptr init (null),

	 2 console_get_line		  entry,
	 2 console_get_line_data_ptr    ptr init (null),

	 2 console_alert_put_chars	  entry,
	 2 console_alert_put_chars_data_ptr ptr init (null),

	 2 get_line		  entry,
	 2 get_line_data_ptr	  ptr init (null),

	 2 put_chars		  entry,
	 2 put_chars_data_ptr	  ptr init (null),

	 2 error_put_chars		  entry,
	 2 error_put_chars_data_ptr	  ptr init (null),

           2 exec_com_get_line	  entry,
	 2 command_abs_data_ptr	  pointer init (null),

/* miscelaneous */

	 2 free_area_ptr		  pointer init (null),
	 2 subsys_info_ptr		  pointer init (null),
	 2 number_of_temp_segs	  fixed bin;

%include cds_args;
declare  1 CDSA			  aligned like cds_args;
declare  (null, size)		  builtin;


	CDSA.sections (1).p = null ();
	CDSA.sections (1).len = 0;
	CDSA.sections (2).p = addr (bce_data_static);
	CDSA.sections (2).len = size (bce_data_static);
	CDSA.sections (2).struct_name = "bce_data_static";
	CDSA.seg_name = "bce_data";
	CDSA.num_exclude_names = 1;
	CDSA.exclude_array_ptr = addr (PADSTAR);
	CDSA.have_static = "1"b;
	CDSA.switches.have_text = "0"b;
	call create_data_segment_ (addr (CDSA), code);
	if code ^= 0
	then call com_err_ (code, "bce_data");
	return;
     end bce_data;
