/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   *********************************************************** */
/* Internal data for use by the COBOL and FORTRAN wrappers to the menu system. */
/* Created 29 March 1982 by Chris Jones */
/* format: style4,delnl,insnl,indattr,ifthen,declareind10,dclind10 */
fc_menu_data_:
     procedure options (variable);

dcl	1 fc_menu_data	   aligned,		/* per process data */
	  2 initialized	   bit (1),		/* on=>the pointer and length are valid */
	  2 menu_array_size	   fixed bin,		/* number of slots in menu_array */
	  2 menu_array_ptr	   ptr,			/* pointer the the array of menus */
	  2 window_array_size  fixed bin,		/* number of slots in window_array */
	  2 window_array_ptr   ptr,			/* pointer to the array of windows */
	  2 already_video	   bit (1) aligned,		/* set if the video system was already on */
	  2 auto_window	   bit (1) aligned,		/* set if we're managing the menu window automatically */
	  2 have_user_io_info  bit (1) aligned,		/* set if user_io_window_info is valid */
	  2 original_cleanup_handler
			   ptr,			/* points to cleanup handler we've subsumed */
	  2 auto_window_iocbp  ptr,			/* pointer to the automatic menu window */
	  2 user_io_window_info
			   like window_position_info, /* where the user_i/o window started */
	  2 auto_window_info   like window_position_info; /* where the automatic menu window is */

dcl	1 cds_data	   like cds_args;		/* args to create_data_segment_ */

dcl	code		   fixed bin (35);		/* status code */

dcl	FC_MENU_DATA_	   char (16) static options (constant) init ("fc_menu_data_");
						/* the name of the data segment we're creating */

dcl	com_err_		   entry () options (variable);
dcl	create_data_segment_   entry (ptr, fixed bin (35));

dcl	(addr, currentsize, null, string, unspec)
			   builtin;

/* Initialize the data to known values. */

	fc_menu_data.initialized = "0"b;
	fc_menu_data.menu_array_size = 0;
	fc_menu_data.menu_array_ptr = null ();
	fc_menu_data.window_array_size = 0;
	fc_menu_data.window_array_ptr = null ();
	fc_menu_data.already_video = "0"b;
	fc_menu_data.auto_window = "0"b;
	fc_menu_data.have_user_io_info = "0"b;
	fc_menu_data.auto_window_iocbp = null ();
	fc_menu_data.original_cleanup_handler = null ();
	unspec (fc_menu_data.user_io_window_info) = "0"b;
	unspec (fc_menu_data.auto_window_info) = "0"b;
	fc_menu_data.user_io_window_info.version = window_position_info_version_1;
	fc_menu_data.auto_window_info.version = window_position_info_version_1;

/* Initialize the create_data_segment_ args. */

	cds_data.sections (1).p = null ();
	cds_data.sections (1).len = 0;
	cds_data.sections (1).struct_name = "";
	cds_data.sections (2).p = addr (fc_menu_data);
	cds_data.sections (2).len = currentsize (fc_menu_data);
	cds_data.sections (2).struct_name = "fc_menu_data";

	cds_data.seg_name = FC_MENU_DATA_;

	cds_data.num_exclude_names = 0;
	cds_data.exclude_array_ptr = null ();

	string (cds_data.switches) = "0"b;
	cds_data.switches.have_static = "1"b;
	cds_data.switches.separate_static = "1"b;

	call create_data_segment_ (addr (cds_data), code);
	if code ^= 0 then
	     call com_err_ (code, FC_MENU_DATA_);

	return;
%page;
%include cds_args;
%page;
%include window_control_info;

     end fc_menu_data_;
