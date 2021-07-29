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



inzr_stk0: proc;

/* This program is the initializer's stack during initialization */
/* This is so called because it must be found in bootstrap1's magic name 
   table, which limits names to 8 recognizable characters. */

/* Automatic */

dcl 1 cdsa aligned like cds_args;
dcl  code fixed bin (35);

/* Static */

dcl  name char (32) aligned static init ("inzr_stk0") options (constant);
dcl  exclude_pad (1) char (32) aligned static options (constant) init ("*");

/* Builtins */

dcl (addr, baseptr, bin, bit, hbound, mod, null, ptr, rel, size, string) builtin;

/* Entries */

dcl  com_err_ entry options (variable);
dcl  create_data_segment_ entry (ptr, fixed bin (35));


/* The initialization stack definition */


dcl 1 inzr_stk0 aligned,
    2 header like stack_header,
    2 frame like stack_frame;

/*  */

	 unspec (inzr_stk0) = ""b;

/* Initialize the stack header pointers. */

	inzr_stk0.header.stack_begin_ptr
	     = ptr (null (), bin (rel (addr (inzr_stk0.frame)))
			     - bin (rel (addr (inzr_stk0))));
	inzr_stk0.header.stack_end_ptr = inzr_stk0.header.stack_begin_ptr;

/* Now set up call to create data segment */

	cdsa.sections (1).p = addr (inzr_stk0);
	cdsa.sections (1).len = size (inzr_stk0);
	cdsa.sections (1).struct_name = "inzr_stk0";

	cdsa.seg_name = name;
	cdsa.num_exclude_names = 1;
	cdsa.exclude_array_ptr = addr (exclude_pad);

	string (cdsa.switches) = "0"b;
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), code);
	return;


% include cds_args;
% include stack_header;
% include stack_frame;
     end inzr_stk0;
