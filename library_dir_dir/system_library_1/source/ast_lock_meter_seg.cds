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


/* format: style3 */
ast_lock_meter_seg:
     proc;					/* AST_LOCK_METER_SEG - Metering data for the Global AST Lock

   Written November 1981 by J. Bongiovanni

*/

/*  Automatic  */


dcl	1 cdsa		aligned like cds_args;
dcl	code		fixed bin (35);

/*  Static  */

dcl	EXCLUDE_PAD	(1) char (32) aligned int static options (constant) init ("pad*");
dcl	MYNAME		char (32) int static options (constant) init ("ast_lock_meter_seg");


/*  Entry  */

dcl	com_err_		entry options (variable);
dcl	create_data_segment_
			entry (ptr, fixed bin (35));
dcl	get_temp_segment_	entry (char (*), ptr, fixed bin (35));
dcl	release_temp_segment_
			entry (char (*), ptr, fixed bin (35));

/*  Condition  */

dcl	cleanup		condition;
%page;
	ast_lock_meter_segp = null ();

	on cleanup goto CLEAN_UP;

	call get_temp_segment_ (MYNAME, ast_lock_meter_segp, code);
	if code ^= 0
	then do;
		call com_err_ (code, MYNAME, "Getting temp segment");
		return;
	     end;

	ast_lock_meters.n_entries = 1;
	ast_lock_meters.max_n_entries = 1024;
	ast_lock_meters.meters (1).caller = null ();

	unspec (cdsa) = "0"b;
	cdsa.sections (1).p = ast_lock_meter_segp;
	cdsa.sections (1).len = currentsize (ast_lock_meters);
	cdsa.sections (1).struct_name = "ast_lock_meters";

	cdsa.seg_name = "ast_lock_meter_seg";
	cdsa.num_exclude_names = 1;
	cdsa.exclude_array_ptr = addr (EXCLUDE_PAD);

	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), code);
	if code ^= 0
	then call com_err_ (code, MYNAME);

CLEAN_UP:
	if ast_lock_meter_segp ^= null ()
	then call release_temp_segment_ (MYNAME, ast_lock_meter_segp, code);


%page;
%include cds_args;
%page;
%include ast_lock_meters;
     end ast_lock_meter_seg;
