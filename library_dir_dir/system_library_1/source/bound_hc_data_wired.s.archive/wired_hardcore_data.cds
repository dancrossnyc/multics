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


wired_hardcore_data:
     procedure;

/*  wired_hardcore_data - this segment  contains wired data used by Multics hardcore
   Converted from alm to cds 6/5/80 by  J. A. Bush
   wire_pages constant added 6/5/80 by J. A. Bush
   Modified by J. A. Bush 08/26/80 for the DPS8/70M CPU
   Modified by J. Bongiovanni 1/81 to add fault counters
   Modified by J. A. Bush 5/08/81 to make per_system saving of history regs the default
   Modified by J. Bongiovanni 2/82 to add trap_invalid_masked
   Modified by C. Hornig March 1982 to add contiguous_io_buffers.
   Modified by R. Coppola October 1983 to add per processor cache counters
	        and make fault counters per-processor.
   Modified by Keith Loepere November 1983 for abort_request.
   Modified by Chris Jones August 1984 to remove contiguous_io_buffers.
*/

/* Automatic */

dcl  code fixed bin (35);
dcl  1 cdsa aligned like cds_args;

/* Entries */

dcl  com_err_ entry () options (variable);
dcl  create_data_segment_ entry (ptr, fixed bin (35));

/* Builtins */

dcl  (addr, null, size, string) builtin;

dcl  1 wired_hardcore_data aligned,			/* pl1 structure defining wired_hardcore_data  */
       2 system_page_size fixed bin,			/* size of a page in 36 bit words */
       2 wire_pages fixed bin,			/* number of pages wired by pmut$wire_and_mask */
       2 global_hregs bit (1) aligned,			/* per-system flag to save history regs */
       2 abort_request bit (1) aligned,			/* set by ocdcm_ when an unsolicited request is hit -
						This bit causes bce programs to possibly abort. */
       2 trap_invalid_masked bit (1) aligned,		/* per-system flag to crash on masked-in-user-ring */
       2 debug_check_options (16) fixed bin,		/* used to store options specified on the DEBG config card */
       2  cpu_a_flt_ctr_array (0:127) fixed bin (35),	/* per-cpu counters of faults  (refer to fault_table in fim) */
       2  cpu_b_flt_ctr_array (0:127) fixed bin (35),
       2  cpu_c_flt_ctr_array (0:127) fixed bin (35),
       2  cpu_d_flt_ctr_array (0:127) fixed bin (35),
       2  cpu_e_flt_ctr_array (0:127) fixed bin (35),
       2  cpu_f_flt_ctr_array (0:127) fixed bin (35),
       2  cpu_g_flt_ctr_array (0:127) fixed bin (35),
       2  cpu_h_flt_ctr_array (0:127) fixed bin (35),
       2  cpu_a_cache_err_ctr_array (18) fixed bin (35),	/* per-cpu counters of cache directory and duplicate directory errors */
       2  cpu_b_cache_err_ctr_array (18) fixed bin (35),
       2  cpu_c_cache_err_ctr_array (18) fixed bin (35),
       2  cpu_d_cache_err_ctr_array (18) fixed bin (35),
       2  cpu_e_cache_err_ctr_array (18) fixed bin (35),
       2  cpu_f_cache_err_ctr_array (18) fixed bin (35),
       2  cpu_g_cache_err_ctr_array (18) fixed bin (35),
       2  cpu_h_cache_err_ctr_array (18) fixed bin (35);


%page;
          unspec (wired_hardcore_data) = ""b;		/* Clear it out */

	wired_hardcore_data.system_page_size = 1024;	/* page size in 36  bit words */
	wired_hardcore_data.wire_pages = 4;		/* pmut$wire_and_mask now wires 4 pages */
	wired_hardcore_data.global_hregs = "1"b;	/* default is to save hregs per-system */

/* Now set up call to create data base */

	cdsa.sections (1).p = addr (wired_hardcore_data);
	cdsa.sections (1).len = size (wired_hardcore_data);
	cdsa.sections (1).struct_name = "wired_hardcore_data";

	cdsa.seg_name = "wired_hardcore_data";
	cdsa.num_exclude_names = 0;
	cdsa.exclude_array_ptr = null ();

	string (cdsa.switches) = "0"b;
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), code);
	if code ^= 0 then call com_err_ (code, "wired_hardcore_data");
	return;
%page;
%include cds_args;

     end wired_hardcore_data;
