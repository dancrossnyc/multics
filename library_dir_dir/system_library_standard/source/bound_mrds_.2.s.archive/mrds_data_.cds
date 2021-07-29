/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1988                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1981 *
   *                                                         *
   * Copyright (c) 1972 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   *********************************************************** */



/* HISTORY COMMENTS:
  1) change(86-05-14,Dupuis), approve(86-08-08,MCR7491), audit(86-08-08,Blair),
     install(86-08-15,MR12.0-1127):
     Added the "number_of_resultant_attributes" field to fix phx20301
     (mrds #154) and changed saved_res_version to rslt0003.
  2) change(86-06-01,Spitzer), approve(85-12-03,MCR7311),
     audit(86-08-29,Blair), install(86-10-16,MR12.0-1187):
     Add rmdb_info_directory.
  3) change(86-11-18,Blair), approve(86-11-18,PBF7311), audit(86-11-26,Dupuis),
     install(86-12-09,MR12.0-1237):
     Add the relation_blocking_factor so we have a consistent number of control
     intervals in all relations created.
  4) change(88-07-14,Hergert), approve(88-07-15,MCR7903),
     audit(88-07-15,Dupuis), install(88-08-01,MR12.2-1073):
     Added max_nested_expr for new parser.
                                                   END HISTORY COMMENTS */


mrds_data_: proc;

/* NOTES:

   This procedure creates the mrds_data_ database.
*/


/* HISTORY:

   78-09-01 J. A. Weeldreyer: Initially written.

   80-05-01 Jim Gray: Modified to delete unused data items,  and  to
   add comments.

   80-06-23 Jim Gray: Modified to change  max_string_size  to  4096,
   from  50000,  for  a more resonable maximum attribute size and to
   add max_line_size, to separate  the  function  of  the  two  data
   items.

   80-10-01 Lindsey Spratt: Changed the dsmd_version_number  from  4
   to 5.

   80-11-03 Jim Gray: Modified to add  temp_seg_name  item,  so  the
   routines  search, close, and delete_se have a common place to get
   this name used in managing the temp segs on a per opening basis.

   80-11-06 Jim Gray: Modified to add current_version_status  entry,
   so that display_mrds_dm can handle when not to display file info,
   and  mrds_rst_create_db  can  properly  init  the  version_status
   structure.

   80-11-07 Jim Gray: Modified to add  submodel_dir_name  entry,  so
   mrds and users will have a standard to refer to.

   80-12-09  Jim  Gray:  added  control_segment_name  which  is  now
   db.control  as  part  of the conversion from r-u to r-s-m-d scope
   modes.

   81-01-14 Rickie E. Brinegar: changed max_and_groups  from  10  to
   100.   This   should  only  affect  the  predicate  structure  in
   mrds_dsl_optimize. It will now allocate 100 pointers  instead  of
   10 pointers.

   81-03-20 Jim Gray: combined  mrds_data_  and  mdbm_data_  tables,
   also removed unused data items.

   81-03-26 Jim Gray : added max_vfile_wait_time to be used with the
   -share option at open time for the vfile attach descriptions.

   81-04-22 Jim Gray :  removed  max_db_open  as  part  of  removing
   module mrds_dm_open_table_mgr from mrds.


   81-04-24 Jim Gray : changed max_dbs value  from  64  to  128,  to
   allow  up to 128 simultaneous opening of databases. This will not
   up the 64 limit on version 3 databases.

   81-04-25 Jim Gray : changed  length  of  temp_seg_name  to  allow
   three  digits in the db_index part of the name, for getting up to
   128 openings.

   81-04-30 Jim Gray : changed max_relations  to  256,  as  part  of
   changing max rels that cmdb can create, and that are usable.

   81-05-19 Jim Gray : changed max_select_items, max_pred_nodes, and
   max_pred_ops  to  256,  to agree with change of max_attributes to
   256.

   81-07-02 Jim Gray : added  statistics  update  mrds  data  items,
   indicating  when  next  update is to be done in terms of relation
   references and real time. The update count value is set to number
   of possible tuple variables, so that max tup vars on one rel only
   causes one call to mu_get_rel_size for that relation. The  update
   time was set to five minutes initially.

   81-07-06 Jim Gray : added size of small relations to  be  updated
   once  per  S.E.  to reduce problems with large percentage changes
   for this case. Also reduced update count to 10, since things  are
   now  managed  on  an  S.E.  reference, rather than tuple variable
   reference manner.

   81-07-22 Jim Gray : added key_search_threshold for  the  strategy
   logic in mrds_dsl_gen_srch_prog.

   81-08-27  Davids:  changed  max_select_items,  max_pred_ops,  and
   max_pred_notes to 256 from 100.

   81-12-02 Davids: changed max_and_terms to 100 from 20.

   82-03-15 R. Lackey : Added valid_rel_and_attr_name_chars

   82-06-03 Mike Kubicar : Changed quiesce_wait (the default quiesce wait
   time to 0.  Default is now don't wait.

   83-02-14 R. Harvey : removed the following: file_id_len_pad, inv_thresh,
   key_search_threshold, max_children, max_kattr_len, max_vfile_wait_time.

   83-05-19 Davids: Added the saved_res_version element.

   83-10-24 Mike Kubicar : Added $max_tids_returned_per_call, the maximum
   number of tuple ids to return during calls to
   relation_manager_$get_tuple_ids.

   84-05-18 Bert Moberg : Changed lock_wait_time to 900

   84-09-07 John Hergert: Added caller_compile for dsl_$compile. Changed 
   saved_res_version to "rslt0001"

   85-04-08 Thanh Nguyen: Added max_safe_tids_returned_per_call with value 
   of one.

   85-04-14 Thanh Nguyen: Changed saved_res_version to "rslt0002".
*/

%include cds_args;


dcl 1 md aligned,					/* the values to go into mrds_data_ */
    2 caller_compile fixed bin (35) init (5),     	/* translate called by compile */
    2 caller_define_temp_rel fixed bin (35) init (4),	/* translate called by define_temp_rel */
    2 caller_delete fixed bin (35) init (1),		/* translate called by delete */
    2 caller_modify fixed bin (35) init (2),		/* translate called by modify */
    2 caller_retrieve fixed bin (35) init (3),		/* translate called by retrieve */
    2 cleanup_lock_wait fixed bin (35) init (120),	/* wait time to lock acs control segment in cleanup handlers */
    2 control_segment_name char (32) init ("db.control"),	/* name of database concurrency control segment */
    2 current_version fixed bin (35) init (4),		/* current database version */
    2 current_version_status fixed bin (35) init (8),	/* current version_status structure major number */
    2 dmd_version fixed bin (35) init (4),		/* version of model header structure */
    2 dsmd_version_number fixed bin (35) init (5),	/* version of submodel header structure */
    2 lit_string_size fixed bin (35) init (73728),	/* max length of a literal string */
    2 lock_wait fixed bin (35) init (900),		/* wait time to lock acs control segment */
    2 lock_wait_time fixed bin (35) init (900),		/* set_scope default wait time */
    2 max_and_groups fixed bin (35) init (100),		/* max "and_groups" allowed in s.e. pred tree */
    2 max_and_terms fixed bin (35) init (100),		/* max terms allowed in an and_group in pred tree */
    2 max_attributes fixed bin (35) init (256),		/* max attrs allowed per relation by CMDB */
    2 max_builtin_args fixed bin (35) init (4),		/* max number of arguments to a builtin function */
    2 max_data_length fixed bin (35) init (2000),		/* max temp rel record data length */
    2 max_dbs fixed bin (35) init (128),		/* number of database openings allowed */
    2 max_expr_items fixed bin (35) init (20),		/* stack depth for eval of s.e. expressions */
    2 max_expr_stack_size fixed bin (35) init (14),	/* stack depth for eval of s.e. expressions */
    2 max_id_len fixed bin (35) init (32),		/* max character length of a tuple variable name */
    2 max_key_len fixed bin (35) init (253),		/* max total chars from attrs making up key field in rels */
    2 max_line_size fixed bin (35) init (50000),		/* largest output line for cmdb listing */
    2 max_lit_string_size fixed bin (35) init (254),	/* max repeated string literal size */
    2 max_nested_expr fixed bin (35) init (5),		/* max number of expression that can be nested in a where clause */
    2 max_pred_depth fixed bin (35) init (30),		/* size of stack for conversion pred tree to disj. norm. form */
    2 max_pred_nodes fixed bin (35) init (256),		/* max number of pred tree tuple attr leaf nodes */
    2 max_pred_ops fixed bin (35) init (256),		/* max number of pred tree operator leaf nodes */
    2 max_relations fixed bin (35) init (256),		/* largest number of relations cmdb can create */
    2 max_select_items fixed bin (35) init (256),		/* s.e. select clause max item count */
    2 max_sets fixed bin (35) init (20),		/* s.e. max number of set operators */
    2 max_sf_args fixed bin (35) init (30),		/* max number of args for scalar function */
    2 max_string_size fixed bin (35) init (4096),		/* largest parsable token for cmdb */
    2 max_td_len fixed bin (35) init (10),		/* largest array space for token data */
    2 max_temp_rels fixed bin (35) init (20),		/* most simultaneous temp rels */
    2 max_tids_returned_per_call fixed bin (35) init (1000), /* Most tids returned in a call to relation_manager_$get_tuple_ids */
    2 max_safe_tids_returned_per_call fixed bin (35) init (1), /* Most tids returned in a call to relation_manager_$get_tuple_ids
      with a guaranty match for a select expression. */
    2 max_token_size fixed bin (35) init (65),		/* largest s.e. token length */
    2 max_tup_var fixed bin (35) init (20),		/* most s.e. tuple variables allowed */
    2 max_tuple_default fixed bin (35) init (1000),	/* blocked file relation -max_tuples option default */
    2 normal_mode fixed bin init (1),			/* normal data base access mode */
    2 number_of_resultant_attributes fixed bin init (500),  /* for refer extent of resultant_attributes_info structure */
    2 quiesce_mode fixed bin init (2),			/* quiesce data base access mode */
    2 quiesce_wait fixed bin (35) init (0),		/* wait time to quiesce files */
    2 relation_blocking_factor fixed bin init (255),        /* no. control intervals per segment */
    2 rmdb_info_directory char (168) init (">doc>ss>rmdb"),	/* directory containing rmdb info segs */
    2 saved_res_version char (8) init ("rslt0003"),         /* most up to date version of the resultant */
    2 statistics_update_count_interval fixed bin (35) init (10), /* number of rel ref times before statistics are next updated */
    2 statistics_update_time_interval fixed bin (71) init (300000000), /* real time til statistics next updated */
    2 statistics_update_small_rel_size fixed bin (35) init (100), /* max  size of rel to be updated every S.E. */
    2 submodel_dir_name char (16) init ("secure.submodels"), /* name of submodel_dir in new architecture */
    2 temp_seg_name char (23) init ("mrds_search_tidtemp.dbi"), /* common name for tid search temp segs */
    2 valid_id_chars char (128) varying
     init ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-") , /* legal s.e. token characters */
    2 valid_rel_and_attr_name_chars char (128) varying
     init ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-") ; /* Valid relation name character (Names can NOT begin with _ - or numeric) */

dcl 1 cdsa like cds_args;
dcl  code fixed bin (35);

dcl (addr,
     size,
     string,
     null) builtin;

dcl  create_data_segment_ entry (ptr, fixed bin (35));
dcl  com_err_ entry options (variable);

	cdsa.sections.p (1) = addr (md);		/* init. info for cds */
	cdsa.sections.len (1) = size (md);
	cdsa.sections.struct_name (1) = "md";
	cdsa.seg_name = "mrds_data_";
	cdsa.num_exclude_names = 0;
	cdsa.exclude_array_ptr = null;
	string (cdsa.switches) = "0"b;
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), code);
	if code ^= 0 then call com_err_ (code, "mrds_data_");
	return;

     end mrds_data_;
