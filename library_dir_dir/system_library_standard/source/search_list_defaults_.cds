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
  1) change(88-01-11,Blair), approve(88-02-25,MCR7840), audit(88-02-25,Dupuis),
     install(88-03-08,MR12.2-1034):
     Add entry for mrds to establish default search list.
                                                   END HISTORY COMMENTS */


/* This segment generates a database used to establish default search lists.

   Converted from alm to create_data_segment 11-Jul-78 by M. Davidoff.
   Added >unb to compose search list - EJW - Aug79
   Added declare (dcl) search list 07-Feb-80 by G. Dixon.
   Modified 03/27/80 by C. D. Tavares to add graphics paths.
   Modified 15 April 1980 by M. N. Davidoff to move >unb to last in compose
   search list.
   Modified 07/21/80 by CDT to add names "xxx.search" to final segment.
   Modified 07/20/81 by JM Stansbury to add pnotice paths.
   Modified 11/02/81 by E. N. Kittlitz to eliminate >ldd>include_2 in translator paths.
   Modified 07/05/82 by EJ Wallman to change compose list.
   Modified 09/21/83 by J. A. Bush to add mtape_arguments search_list
   Modified 11/02/83 by S. Herbst to add probe search list.
   Modified 11/04/83 by James A Falksen to add mrpg search list.
   Modified 08/10/84 by B. Braun to add structure, hardcore and dumps search lists.
   Modified 09/26/84 by B. Braun to correct structure paths to have trailing underscores.
   Modified 03/22/85 by G. Palter to add >tools>structure_library_6_ to the structure search list.
*/
/* format: style2 */
search_list_defaults_:
     procedure;

/* automatic */

	declare code		 fixed binary (35);
	declare wdir		 char (168);

	declare 1 cdsa		 aligned like cds_args;

	declare 1 lists		 aligned,
		2 comp		 bit (0) unaligned,
		2 compose,
		  3 name_count	 fixed binary,
		  3 path_count	 fixed binary,
		  3 names		 (2) char (32),
		  3 paths		 (4) like search_path,
		2 dcl		 bit (0) unaligned,
		2 declare,
		  3 name_count	 fixed binary,
		  3 path_count	 fixed binary,
		  3 names		 (2) char (32),
		  3 paths		 (1) like search_path,
		2 dict		 bit (0) unaligned,
		2 dictionary,
		  3 name_count	 fixed binary,
		  3 path_count	 fixed binary,
		  3 names		 (2) char (32),
		  3 paths		 (1) like search_path,
		2 dumps,
		  3 name_count	 fixed binary,
		  3 path_count	 fixed binary,
		  3 names		 (1) char (32),
		  3 paths		 (2) like search_path,
		2 ec		 bit (0) unaligned,
		2 exec_com,
		  3 name_count	 fixed binary,
		  3 path_count	 fixed binary,
		  3 names		 (2) char (32),
		  3 paths		 (1) like search_path,
		2 graphics,
		  3 name_count	 fixed binary,
		  3 path_count	 fixed binary,
		  3 names		 (1) char (32),
		  3 paths		 (3) like search_path,
		2 hardcore,
		  3 name_count	 fixed binary,
		  3 path_count	 fixed binary,
		  3 names		 (1) char (32),
		  3 paths		 (1) like search_path,
		2 info_segs	 bit (0) unaligned,
		2 info		 bit (0) unaligned,
		2 info_segments,
		  3 name_count	 fixed binary,
		  3 path_count	 fixed binary,
		  3 names		 (3) char (32),
		  3 paths		 (2) like search_path,
                    2 mrds,
                      3 name_count       fixed binary,
                      3 path_count       fixed binary,
                      3 names            (1) char (32),
                      3 paths            (1) like search_path,
		2 mrpg_lib,
		  3 name_count	 fixed binary,
		  3 path_count	 fixed binary,
		  3 names		 (1) char (32),
		  3 paths		 (1) like search_path,
		2 mtape_args	 bit (0) unaligned,
		2 mtape_arguments,
		  3 name_count	 fixed binary,
		  3 path_count	 fixed binary,
		  3 names		 (2) char (32),
		  3 paths		 (3) like search_path,
		2 trans		 bit (0) unaligned,
		2 translator,
		  3 name_count	 fixed binary,
		  3 path_count	 fixed binary,
		  3 names		 (2) char (32),
		  3 paths		 (3) like search_path,
		2 pnotice,
		  3 name_count	 fixed binary,
		  3 path_count	 fixed binary,
		  3 names		 (1) char (32),
		  3 paths		 (1) like search_path,
		2 pb		 bit (0) unaligned,
		2 probe,
		  3 name_count	 fixed binary,
		  3 path_count	 fixed binary,
		  3 names		 (2) char (32),
		  3 paths		 (2) like search_path,
		2 structure,
		  3 name_count	 fixed binary,
		  3 path_count	 fixed binary,
		  3 names		 (1) char (32),
		  3 paths		 (6) like search_path;

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

/* entry */

	declare com_err_		 entry options (variable);
	declare create_data_segment_	 entry (pointer, fixed binary (35));
	declare get_wdir_		 entry () returns (char (168));

%include sl_info;
%include cds_args;

/* program */

	lists.compose.name_count = hbound (lists.compose.names, 1);
	lists.compose.path_count = hbound (lists.compose.paths, 1);
	lists.compose.names (1) = "compose";
	lists.compose.names (2) = "comp";
	lists.compose.paths (1).type = WORKING_DIR;
	lists.compose.paths (1).pathname = "-working_dir";
	lists.compose.paths (2).type = UNEXPANDED_PATH;
	lists.compose.paths (2).pathname = ">udd>[user project]>compose_macros";
	lists.compose.paths (3).type = REFERENCING_DIR;
	lists.compose.paths (3).pathname = "-referencing_dir";
	lists.compose.paths (4).type = ABSOLUTE_PATH;
	lists.compose.paths (4).pathname = ">unb";

	lists.declare.name_count = hbound (lists.declare.names, 1);
	lists.declare.path_count = hbound (lists.declare.paths, 1);
	lists.declare.names (1) = "declare";
	lists.declare.names (2) = "dcl";
	lists.declare.paths (1).type = ABSOLUTE_PATH;
	lists.declare.paths (1).pathname = ">sss>pl1.dcl";

	lists.dictionary.name_count = hbound (lists.dictionary.names, 1);
	lists.dictionary.path_count = hbound (lists.dictionary.paths, 1);
	lists.dictionary.names (1) = "dictionary";
	lists.dictionary.names (2) = "dict";
	lists.dictionary.paths (1).type = ABSOLUTE_PATH;
	lists.dictionary.paths (1).pathname = ">unb>standard.dict";

	lists.dumps.name_count = hbound (lists.dumps.names, 1);
	lists.dumps.path_count = hbound (lists.dumps.paths, 1);
	lists.dumps.names (1) = "dumps";
	lists.dumps.paths (1).type = ABSOLUTE_PATH;
	lists.dumps.paths (1).pathname = ">dumps";
	lists.dumps.paths (2).type = ABSOLUTE_PATH;
	lists.dumps.paths (2).pathname = ">dumps>save_pdirs";

	lists.exec_com.name_count = hbound (lists.exec_com.names, 1);
	lists.exec_com.path_count = hbound (lists.exec_com.paths, 1);
	lists.exec_com.names (1) = "exec_com";
	lists.exec_com.names (2) = "ec";
	lists.exec_com.paths (1).type = WORKING_DIR;
	lists.exec_com.paths (1).pathname = "-working_dir";

	lists.graphics.name_count = hbound (lists.graphics.names, 1);
	lists.graphics.path_count = hbound (lists.graphics.paths, 1);
	lists.graphics.names (1) = "graphics";
	lists.graphics.paths (1).type = WORKING_DIR;
	lists.graphics.paths (1).pathname = "-working_dir";
	lists.graphics.paths (2).type = REFERENCING_DIR;
	lists.graphics.paths (2).pathname = "-referencing_dir";
	lists.graphics.paths (3).type = ABSOLUTE_PATH;
	lists.graphics.paths (3).pathname = ">unb";

	lists.hardcore.name_count = hbound (lists.hardcore.names, 1);
	lists.hardcore.path_count = hbound (lists.hardcore.paths, 1);
	lists.hardcore.names (1) = "hardcore";
	lists.hardcore.paths (1).type = ABSOLUTE_PATH;
	lists.hardcore.paths (1).pathname = ">ldd>h>e";

	lists.info_segments.name_count = hbound (lists.info_segments.names, 1);
	lists.info_segments.path_count = hbound (lists.info_segments.paths, 1);
	lists.info_segments.names (1) = "info_segments";
	lists.info_segments.names (2) = "info_segs";
	lists.info_segments.names (3) = "info";
	lists.info_segments.paths (1).type = ABSOLUTE_PATH;
	lists.info_segments.paths (1).pathname = ">doc>iml_info";
	lists.info_segments.paths (2).type = ABSOLUTE_PATH;
	lists.info_segments.paths (2).pathname = ">doc>info";

	lists.mrds.name_count = hbound (lists.mrds.names, 1);
	lists.mrds.path_count = hbound (lists.mrds.paths, 1);
	lists.mrds.names (1) = "mrds";
	lists.mrds.paths (1).type = WORKING_DIR;
	lists.mrds.paths (1).pathname = "-working_dir";

	lists.mrpg_lib.name_count = hbound (lists.mrpg_lib.names, 1);
	lists.mrpg_lib.path_count = hbound (lists.mrpg_lib.paths, 1);
	lists.mrpg_lib.names (1) = "mrpg_lib";
	lists.mrpg_lib.paths (1).type = REFERENCING_DIR;
	lists.mrpg_lib.paths (1).pathname = "-referencing_dir";

	lists.mtape_arguments.name_count = hbound (lists.mtape_arguments.names, 1);
	lists.mtape_arguments.path_count = hbound (lists.mtape_arguments.paths, 1);
	lists.mtape_arguments.names (1) = "mtape_arguments";
	lists.mtape_arguments.names (2) = "mtape_args";
	lists.mtape_arguments.paths (1).type = UNEXPANDED_PATH;
	lists.mtape_arguments.paths (1).pathname = ">udd>[user project]>[user name]>[user name].value";
	lists.mtape_arguments.paths (2).type = ABSOLUTE_PATH;
	lists.mtape_arguments.paths (2).pathname = ">site>mtape_arguments.value";
	lists.mtape_arguments.paths (3).type = ABSOLUTE_PATH;
	lists.mtape_arguments.paths (3).pathname = ">system_library_standard>mtape_arguments.value";

	lists.translator.name_count = hbound (lists.translator.names, 1);
	lists.translator.path_count = hbound (lists.translator.paths, 1);
	lists.translator.names (1) = "translator";
	lists.translator.names (2) = "trans";
	lists.translator.paths (1).type = WORKING_DIR;
	lists.translator.paths (1).pathname = "-working_dir";
	lists.translator.paths (2).type = UNEXPANDED_PATH;
	lists.translator.paths (2).pathname = ">udd>[user project]>include";
	lists.translator.paths (3).type = ABSOLUTE_PATH;
	lists.translator.paths (3).pathname = ">ldd>include";

	lists.pnotice.name_count = hbound (lists.pnotice.names, 1);
	lists.pnotice.path_count = hbound (lists.pnotice.paths, 1);
	lists.pnotice.names (1) = "pnotice";
	lists.pnotice.paths (1).type = ABSOLUTE_PATH;
	lists.pnotice.paths (1).pathname = ">tools";

	lists.probe.name_count = hbound (lists.probe.names, 1);
	lists.probe.path_count = hbound (lists.probe.paths, 1);
	lists.probe.names (1) = "probe";
	lists.probe.names (2) = "pb";
	lists.probe.paths (1).type = WORKING_DIR;
	lists.probe.paths (1).pathname = "-working_dir";
	lists.probe.paths (2).type = REFERENCING_DIR;
	lists.probe.paths (2).pathname = "-referencing_dir";

	lists.structure.name_count = hbound (lists.structure.names, 1);
	lists.structure.path_count = hbound (lists.structure.paths, 1);
	lists.structure.names (1) = "structure";
	lists.structure.paths (1).type = ABSOLUTE_PATH;
	lists.structure.paths (1).pathname = ">tools>structure_library_1_";
	lists.structure.paths (2).type = ABSOLUTE_PATH;
	lists.structure.paths (2).pathname = ">tools>structure_library_2_";
	lists.structure.paths (3).type = ABSOLUTE_PATH;
	lists.structure.paths (3).pathname = ">tools>structure_library_3_";
	lists.structure.paths (4).type = ABSOLUTE_PATH;
	lists.structure.paths (4).pathname = ">tools>structure_library_4_";
	lists.structure.paths (5).type = ABSOLUTE_PATH;
	lists.structure.paths (5).pathname = ">tools>structure_library_5_";
	lists.structure.paths (6).type = ABSOLUTE_PATH;
	lists.structure.paths (6).pathname = ">tools>structure_library_6_";

	unspec (cdsa) = ""b;
	cdsa.sections (1).p = addr (lists);
	cdsa.sections (1).len = size (lists);
	cdsa.sections (1).struct_name = "lists";
	cdsa.sections (2).p = null;
	cdsa.sections (2).struct_name = "";
	cdsa.seg_name = "search_list_defaults_";
	cdsa.exclude_array_ptr = null;
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), code);
	if code ^= 0
	then do;
		call com_err_ (code, "search_list_defaults_");
		return;
	     end;

	wdir = get_wdir_ ();

	call add_search_names (lists.compose.names (*));
	call add_search_names (lists.declare.names (*));
	call add_search_names (lists.dictionary.names (*));
	call add_search_names (lists.dumps.names (*));
	call add_search_names (lists.exec_com.names (*));
	call add_search_names (lists.graphics.names (*));
	call add_search_names (lists.hardcore.names (*));
	call add_search_names (lists.info_segments.names (*));
	call add_search_names (lists.mrds.names (*));
	call add_search_names (lists.mrpg_lib.names (*));
	call add_search_names (lists.mtape_arguments.names (*));
	call add_search_names (lists.pnotice.names (*));
	call add_search_names (lists.probe.names (*));
	call add_search_names (lists.structure.names (*));
	call add_search_names (lists.translator.names (*));

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
	     call hcs_$chname_file (wdir, "search_list_defaults_", "", extra_name, code);
	     if code ^= 0
	     then if code ^= error_table_$segnamedup
		then call com_err_ (code, "search_list_defaults_", "Adding name ^a", extra_name);
	end;

	return;
     end add_search_names;

     end search_list_defaults_;
