/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   *********************************************************** */
dial_out_sl_default_:
     procedure ();

/* automatic */

	declare code		 fixed binary (35);
	declare wdir		 char (168);

	declare 1 cdsa		 aligned like cds_args;

	declare 1 lists		 aligned,
		2 dial_out,
		  3 name_count	 fixed binary,
		  3 path_count	 fixed binary,
		  3 names		 (1) char (32),
		  3 paths		 (3) like search_path;

/* based */

	declare 1 search_path	 based,
		2 type		 fixed binary,
		2 pathname	 char (168);

/* builtin */

	declare addr		 builtin;
	declare hbound		 builtin;
	declare null		 builtin;
	declare rtrim		 builtin;
	declare size		 builtin;
	declare unspec		 builtin;

/* entry */

	declare com_err_		 entry options (variable);
	declare create_data_segment_	 entry (pointer, fixed binary (35));
	declare get_wdir_		 entry () returns (char (168));

%include sl_info;
%include cds_args;

/* program */

	lists.dial_out.name_count = hbound (lists.dial_out.names, 1);
	lists.dial_out.path_count = hbound (lists.dial_out.paths, 1);
	lists.dial_out.names (1) = "dial_out";
	lists.dial_out.paths (1).type = WORKING_DIR;
	lists.dial_out.paths (1).pathname = "-working_dir";
	lists.dial_out.paths (2).type = UNEXPANDED_PATH;
	lists.dial_out.paths (2).pathname = ">udd>[user project]>dial_out_dir";
	lists.dial_out.paths (3).type = ABSOLUTE_PATH;
	lists.dial_out.paths (3).pathname = ">site>dial_out_dir";

	unspec (cdsa) = ""b;
	cdsa.sections (1).p = addr (lists);
	cdsa.sections (1).len = size (lists);
	cdsa.sections (1).struct_name = "lists";
	cdsa.sections (2).p = null;
	cdsa.sections (2).struct_name = "";
	cdsa.seg_name = "dial_out_sl_default_";
	cdsa.exclude_array_ptr = null;
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), code);
	if code ^= 0
	then do;
		call com_err_ (code, "dial_out_sl_default_");
		return;
	     end;

	wdir = get_wdir_ ();

	call add_search_names (lists.dial_out.names (*));

	return;

add_search_names:
     proc (name_array);

	declare name_array		 dimension (*) char (32) aligned parameter;

	declare hbound		 builtin;
	declare lbound		 builtin;

	declare error_table_$segnamedup
				 fixed bin (35) ext static;

	declare hcs_$chname_file	 entry (char (*), char (*), char (*), char (*), fixed bin (35));

	declare i			 fixed bin;
	declare extra_name		 char (32);

	do i = lbound (name_array, 1) to hbound (name_array, 1);
	     extra_name = rtrim (name_array (i)) || ".search";
	     call hcs_$chname_file (wdir, "dial_out_sl_default_", "", extra_name, code);
	     if code ^= 0
	     then if code ^= error_table_$segnamedup
		then call com_err_ (code, "dial_out_sl_default_", "Adding name ^a", extra_name);
	end;

	return;
     end add_search_names;

     end dial_out_sl_default_;
