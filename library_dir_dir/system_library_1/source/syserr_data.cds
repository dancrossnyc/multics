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


/* SYSERR_DATA - The syserr database.	*/
/* Last modified (Date and reason):
   16 Nov 84 by K. Loepere to use up a full page.
   7 Mar 79 by D. Spector to fix "like to refer" bug
   2/6/76	by S. Webber Initial coding */

syserr_data: proc;

/* This program creates the syserr_data data base */

/* Automatic */

dcl 1 cdsa aligned like cds_args;
dcl  code fixed bin (35);
dcl  tsegp (1) ptr;

/* Static */

dcl  big_time fixed bin (71) init (100000000000000);
dcl  exclude_pad (1) char (32) aligned static options (constant) init ("pad*");
dcl  syserr_dataname char (11) aligned static init ("syserr_data") options (constant);

/* Builtins */

dcl (addr, bin, mod, rel, size, string) builtin;

/* Entries */

dcl  com_err_ entry options (variable);
dcl  create_data_segment_ entry (ptr, fixed bin (35));
dcl  get_temp_segments_ entry (char (*), (*) ptr, fixed bin (35));
dcl  release_temp_segments_ entry (char (*), (*) ptr, fixed bin (35));
%page;

/* The following structure describes the overall format of syserr_data */

dcl  syserr_datap ptr;

dcl 1 syserr_data aligned based (syserr_datap),
    2 log_meters (16) fixed bin,
    2 syserr_size fixed bin (18),			/* size of "sd" structure above */
    2 wired_log_size fixed bin (18),			/* size of "wlog" structure above */
    2 logger_proc_id bit (36),			/* process ID for logger HPROC */
    2 pad fixed bin,
    2 logger_ec fixed bin (71),			/* event channel for special wakeups for HPROC */
    2 syserr_area aligned like sd,
    2 wired_log_area aligned like wlog,
    2 pad_extra_wmess (size (wmess)) bit (36);		/* extra header at end of wired section */
%page;
	call get_temp_segments_ ("syserr_data", tsegp, code);
	syserr_datap = tsegp (1);
	wlog_ptr = addr (syserr_data.wired_log_area);

/* Now begins the initialization */

	syserr_data.syserr_size = size (sd);
	syserr_data.wired_log_area.head.bsize = 968;
	syserr_data.wired_log_size = size (wlog) + size (wmess);

	call check (addr (syserr_data.syserr_area), "syserr_area", 2);

/* Now set up call to create data base */

	cdsa.sections (1).p = addr (syserr_data);
	cdsa.sections (1).len = size (syserr_data);
	cdsa.sections (1).struct_name = "syserr_data";

	cdsa.seg_name = "syserr_data";
	cdsa.num_exclude_names = 1;
	cdsa.exclude_array_ptr = addr (exclude_pad);

	string (cdsa.switches) = "0"b;
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), code);

	call release_temp_segments_ ("syserr_data", tsegp, code);
%page;
check:	proc (where, message, modulo);

dcl  message char (*) parameter;
dcl  modulo fixed bin parameter;
dcl  where ptr parameter;

	     if mod (bin (rel (where), 18), modulo) ^= 0 then
		call com_err_ (0, syserr_dataname, "The variable ^a is not aligned on a ^d-word boundary.",
		message, modulo);

	end check;
%page; %include cds_args;
%page; %include syserr_data;
     end syserr_data;
