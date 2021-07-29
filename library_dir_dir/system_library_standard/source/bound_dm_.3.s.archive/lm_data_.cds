/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1983 *
   *                                                         *
   *********************************************************** */
/* Description:
   Lock manager per_process definitions */

/* HISTORY:

Written by BIM, 0483.
Modified:
830426 added trace_ switch 
830506 BIM added ev_channel so we wouldn't create one per txn. 
841018 SSC added lock_list_meters_ptr
*/

/* format: style3,idind30 */
lm_data_:
     procedure;

/*  Automatic  */

dcl	code			fixed bin (35);
dcl	1 local_cds_args		aligned like cds_args;

dcl	1 lock_data		aligned,
	  2 current_txn_id		bit (36) aligned init (""b),
	  2 txn_table_ptr             pointer init (null ()),
	  2 lock_seg_ptr		pointer init (null ()),
	  2 lock_segments_ptr	pointer init (null ()),
	  2 n_lock_segments		fixed bin init (0),
	  2 lock_per_process_ptr	pointer init (null ()),
	  2 fast_lock_data_ptr        pointer init (null ()),
	  2 lock_list_meters_ptr	pointer init (null ()),
	  2 trace 		bit (1) aligned init ("0"b),
	  2 ev_channel		fixed bin (71) aligned init (0);

/*  Static  */

dcl	EXCLUDE_ARRAY		(1) char (32) int static options (constant) init ("pad*");
dcl	MYNAME			char (15) int static options (constant) init ("lm_data_");

/*  Entry  */

dcl	com_err_			entry options (variable);
dcl	create_data_segment_	entry (ptr, fixed bin (35));

/*  Builtin  */

dcl	addr			builtin;
dcl	hbound			builtin;
dcl	null			builtin;
dcl	size			builtin;
dcl	unspec			builtin;
%page;

	unspec (local_cds_args) = ""b;

	local_cds_args.sections (2).p = addr (lock_data);
	local_cds_args.sections (2).len = size (lock_data);
	local_cds_args.sections (2).struct_name = "lock_data";
	local_cds_args.seg_name = MYNAME;
	local_cds_args.exclude_array_ptr = addr (EXCLUDE_ARRAY);
	local_cds_args.num_exclude_names = hbound (EXCLUDE_ARRAY, 1);
	local_cds_args.switches.have_static = "1"b;

	call create_data_segment_ (addr (local_cds_args), code);
	if code ^= 0
	then call com_err_ (code, MYNAME);

	return;
%page;
%include cds_args;

     end lm_data_;
