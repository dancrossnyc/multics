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
ipc_data_:
     proc;

/* Static values needed by user ring ipc - replaces an alm version */
/* These are used to implement fast channels efficiently by making it */
/* inexpensive to detect if a process has pending wakeups. */

/* Coded by E Donner Jan 1981 */
/* Modified September 1983 by Chris Jones to only count event calls pending */

/* automatic */

dcl	1 cds_arguments	aligned like cds_args;
dcl	code		fixed bin (35);

/* constants  */

dcl	MOI		char (9) static init ("ipc_data_") options (constant);

/* Builtins */

dcl	addr		builtin;
dcl	null		builtin;
dcl	size		builtin;
dcl	string		builtin;
dcl	unspec		builtin;

/* entries */

dcl	com_err_		entry options (variable);
dcl	create_data_segment_
			entry (ptr, fixed bin (35));


dcl	1 ipc_data_	aligned,
	  2 event_calls_pending
			fixed bin,		/* count of call events pending */
	  2 fast_channel_events
			bit (36);			/* special events pending */


	unspec (ipc_data_) = ""b;

/* Fill in arguments to cds before creating data base */


	cds_arguments.sections (2).p = addr (ipc_data_);
	cds_arguments.sections (2).len = size (ipc_data_);
	cds_arguments.sections (2).struct_name = MOI;

	cds_arguments.seg_name = MOI;
	cds_arguments.exclude_array_ptr = null;

	string (cds_arguments.switches) = "0"b;
	cds_arguments.switches.have_static = "1"b;
	cds_arguments.switches.separate_static = "1"b;

	call create_data_segment_ (addr (cds_arguments), code);
	if code ^= 0
	then call com_err_ (MOI, code);
	return;


%include cds_args;

     end ipc_data_;
