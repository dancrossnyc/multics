/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1988                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   *********************************************************** */



/* HISTORY COMMENTS:
  1) change(88-04-13,Blair), approve(88-04-13,MCR7842),
     audit(88-06-29,Lippard), install(88-07-26,MR12.2-1069):
     Add -hd to the mlsys search list after -wd for SCP 6349.
                                                   END HISTORY COMMENTS */


/* format: off */

/* Creates the default search list for the Multics mail system:  This data segment is intended to be bound into
   bound_mail_system_ and, therefore, does not add the search list names to itself */

/* Created:  April 1982 by G. Palter from search_list_defaults_ */

/* format: on,style4,delnl,insnl,ifthenstmt,ifthen */


mlsys_search_list_default_:
     procedure () options (variable);


dcl  1 cdsa aligned like cds_args;

dcl  1 list aligned,
       2 mlsys bit (0) unaligned,			/* the mail_system search list */
       2 mail_system,
         3 name_count fixed binary,
         3 path_count fixed binary,
         3 names (2) character (32),
         3 paths (3) like search_path;

dcl  1 search_path based,				/* a single search path */
       2 type fixed binary,
       2 pathname character (168);

dcl  code fixed binary (35);

dcl  MLSYS_SEARCH_LIST_DEFAULT_ character (32) static options (constant) initial ("mlsys_search_list_default_");

dcl  com_err_ entry () options (variable);
dcl  create_data_segment_ entry (pointer, fixed binary (35));

dcl  (addr, currentsize, hbound, null, unspec) builtin;
%page;
/* Setup the definition of the search list */

	list.mail_system.name_count = hbound (list.mail_system.names, 1);
	list.mail_system.path_count = hbound (list.mail_system.paths, 1);
	list.mail_system.names (1) = "mail_system";
	list.mail_system.names (2) = "mlsys";
	list.mail_system.paths (1).type = WORKING_DIR;
	list.mail_system.paths (1).pathname = "-working_dir";
	list.mail_system.paths (2).type = HOME_DIR;
          list.mail_system.paths (2).pathname = "-home_dir";
	list.mail_system.paths (3).type = UNEXPANDED_PATH;
	list.mail_system.paths (3).pathname = ">udd>[user project]>[user name]>[user name].mlsys";


/* Create the data segment */

	unspec (cdsa) = ""b;
	cdsa.sections (1).p = addr (list);
	cdsa.sections (1).len = currentsize (list);
	cdsa.sections (1).struct_name = "list";
	cdsa.sections (2).p = null ();
	cdsa.sections (2).struct_name = "";
	cdsa.seg_name = MLSYS_SEARCH_LIST_DEFAULT_;
	cdsa.exclude_array_ptr = null ();
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), code);
	if code ^= 0 then call com_err_ (code, MLSYS_SEARCH_LIST_DEFAULT_);

	return;
%page;
%include sl_info;
%page;
%include cds_args;

     end mlsys_search_list_default_;
