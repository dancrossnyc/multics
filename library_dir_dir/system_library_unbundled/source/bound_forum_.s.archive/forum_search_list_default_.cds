/* **************************************************************
   *                                                            *
   * Copyright, (C) Massachusetts Institute of Technology, 1984 *
   *                                                            *
   ************************************************************** */

/* This segment generates a database used to establish default search lists.

   Converted from alm to create_data_segment 11-Jul-78 by M. Davidoff.
   Added >unb to compose search list - EJW - Aug79
   Added declare (dcl) search list 07-Feb-80 by G. Dixon.
   Modified 03/27/80 by C. D. Tavares to add graphics paths.
   Modified 15 April 1980 by M. N. Davidoff to move >unb to last in compose
   search list.
   Modified 07/21/80 by CDT to add names "xxx.search" to final segment.
   Modified 29 May 1981 by J. Spencer Love to be the default for continuum only.
   Modified 01/21/82 Jay Pattin to change to forum.
   Modified 06/24/82 Jay Pattin to add [hd]>meetings
*/

forum_search_list_default_:
     procedure ();

/* automatic */

	declare code		 fixed binary (35);
	declare wdir		 char (168);

	declare 1 cdsa		 aligned like cds_args;

	declare 1 lists		 aligned,
		2 forum,
		  3 name_count	 fixed binary,
		  3 path_count	 fixed binary,
		  3 names		 (1) char (32),
		  3 paths		 (2) like search_path;

/* based */

	declare 1 search_path	 based,
		2 type		 fixed binary,
		2 pathname	 char (168);

/* builtin */

	declare addr		 builtin;
	declare hbound		 builtin;
	declare null		 builtin;
	declare size		 builtin;
	declare unspec		 builtin;

/* external */

	declare forum_data_$central_directory
				 char (168) external;

/* entry */

	declare com_err_		 entry options (variable);
	declare create_data_segment_	 entry (pointer, fixed binary (35));
	declare get_wdir_		 entry () returns (char (168));

%include sl_info;
%include cds_args;

/* program */

	lists.forum.name_count = hbound (lists.forum.names, 1);
	lists.forum.path_count = hbound (lists.forum.paths, 1);
	lists.forum.names (1) = "forum";
	lists.forum.paths (1).type = UNEXPANDED_PATH;
	lists.forum.paths (1).pathname = ">udd>[user project]>[user name]>meetings";
	lists.forum.paths (2).type = ABSOLUTE_PATH;
	lists.forum.paths (2).pathname = forum_data_$central_directory;

	unspec (cdsa) = ""b;
	cdsa.sections (1).p = addr (lists);
	cdsa.sections (1).len = size (lists);
	cdsa.sections (1).struct_name = "lists";
	cdsa.sections (2).p = null;
	cdsa.sections (2).struct_name = "";
	cdsa.seg_name = "forum_search_list_default_";
	cdsa.exclude_array_ptr = null;
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), code);
	if code ^= 0
	then do;
	     call com_err_ (code, "forum_search_list_default_");
	     return;
	end;

	wdir = get_wdir_ ();

	call add_search_names (lists.forum.names (*));

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
	     call hcs_$chname_file (wdir, "forum_search_list_default_", "", extra_name, code);
	     if code ^= 0
	     then if code ^= error_table_$segnamedup
		then call com_err_ (code, "forum_search_list_default_", "Adding name ^a", extra_name);
	end;

	return;
     end add_search_names;

     end forum_search_list_default_;
