/* ***********************************************************
   *                                                         *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1981 *
   *                                                         *
   *                                                         *
   *********************************************************** */

/**** format: ind3,ll80,initcol6,indattr,^inddcls,dclind4,idind16	       */
/**** format: struclvlind2,^ifthenstmt,^ifthendo,^ifthen,^indnoniterdo       */
/**** format: ^inditerdo,^indnoniterend,^indthenelse,case,^indproc,^indend   */
/**** format: ^delnl,^insnl,comcol41,^indcom,^indblkcom,linecom,^indcomtxt   */

xdw_defaults_: procedure;

/* automatic */

dcl code		fixed binary (35);
dcl wdir		char (168);

dcl 1 cdsa	aligned like cds_args;

dcl 1 lists	aligned,
      2 xdw,
        3 name_count fixed binary,
        3 path_count fixed binary,
        3 names	(1) char (32),
        3 paths	(2) like search_path;

/* based */

dcl 1 search_path	based,
      2 type	fixed binary,
      2 pathname	char (168);

dcl (addr, hbound, null, size, unspec) builtin;

/* entry */

dcl com_err_	entry options (variable);
dcl create_data_segment_ entry (pointer, fixed binary (35));
dcl get_wdir_	entry () returns (char (168));

%include sl_info;
%include cds_args;

/* program */

      lists.xdw.name_count = hbound (lists.xdw.names, 1);
      lists.xdw.path_count = hbound (lists.xdw.paths, 1);
      lists.xdw.names (1) = "xdw";
      lists.xdw.paths (1).type = REFERENCING_DIR;
      lists.xdw.paths (1).pathname = "-referencing_dir";
      lists.xdw.paths (2).type = WORKING_DIR;
      lists.xdw.paths (2).pathname = "-working_dir";

      unspec (cdsa) = ""b;
      cdsa.sections (1).p = addr (lists);
      cdsa.sections (1).len = size (lists);
      cdsa.sections (1).struct_name = "lists";
      cdsa.sections (2).p = null;
      cdsa.sections (2).struct_name = "";
      cdsa.seg_name = "xdw_defaults_";
      cdsa.exclude_array_ptr = null;
      cdsa.switches.have_text = "1"b;

      call create_data_segment_ (addr (cdsa), code);
      if code ^= 0
      then do;
         call com_err_ (code, "xdw_defaults_");
         return;
      end;

      wdir = get_wdir_ ();

      call add_search_names (lists.xdw.names (*));

      return;

add_search_names: proc (name_array);

dcl name_array	dimension (*) char (32) aligned parameter;

dcl hbound	builtin;
dcl lbound	builtin;

dcl error_table_$segnamedup fixed bin (35) ext static;

dcl hcs_$chname_file entry (char (*), char (*), char (*), char (*),
		fixed bin (35));

dcl i		fixed bin;
dcl extra_name	char (32);

      do i = lbound (name_array, 1) to hbound (name_array, 1);
         extra_name = rtrim (name_array (i)) || ".search";
         call hcs_$chname_file (wdir, "xdw_defaults_", "", extra_name, code);
         if code ^= 0
         then if code ^= error_table_$segnamedup
	    then call com_err_ (code, "xdw_defaults_", "Adding name ^a",
		  extra_name);
      end;

      return;
   end add_search_names;

   end xdw_defaults_;
