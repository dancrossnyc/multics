/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   *********************************************************** */

/* PRDS - The Processor Data Segment and Processor Stack.
   /* Last modified (Date and reason):
   2/6/76	by S. Webber Initial coding
   6/15/77 by M. Weaver to  null signal and sct pointers
   8/25/80 by J. A. Bush for the dps8/70m cpu
   2/22/81 by J. Bongiovanni for fast_connect_code
   6/27/81 by J. Bongiovanni for idle_temp
   10/11/83 by R. Coppola to adjust for size change of fast connect code
		and validate that apt_ptr& ignore_pl are on correct mod
*/


/* HISTORY COMMENTS:
  1) change(88-05-24,Farley), approve(88-06-30,MCR7927),
     audit(88-07-12,Fawcett), install(88-08-02,MR12.2-1076):
     Added fault_reg_last_saved and hregs_last_saved time values, to make sure
     that the associated data is only saved once per fault.
                                                   END HISTORY COMMENTS */


/* ******************************************************
   *                                                    *
   *                                                    *
   * Copyright (c) 1972 by Massachusetts Institute of   *
   * Technology and Honeywell Information Systems, Inc. *
   *                                                    *
   *                                                    *
   ****************************************************** */


prds: proc;

/* This program creates the prds data base */

/* Automatic */

dcl 1 cdsa aligned like cds_args;
dcl  code fixed bin (35);

/* Static */

dcl  prdsname char (4) aligned static init ("prds") options (constant);
dcl  exclude_pad (1) char (32) aligned static options (constant) init ("pad*");

/* The following must correspond to the size of the fast connect code in
   fast_connect_init									*/

dcl  FAST_CONNECT_CODE_WORDS init (72) fixed bin int static options (constant);

/* Builtins */

dcl (addr, bin, mod, rel, size, string, unspec) builtin;

/* Entries */

dcl  com_err_ entry options (variable);
dcl  create_data_segment_ entry (ptr, fixed bin (35));
dcl  get_temp_segment_ entry (char (*), ptr, fixed bin (35));
dcl  release_temp_segment_ entry (char (*), ptr, fixed bin (35));




dcl  prdsp ptr;

dcl 1 prds aligned based (prdsp),
    2 header aligned like stack_header,			/* standard stack header */
    2 interrupt_data aligned like mc,			/* MC for interrupts */
    2 fim_data aligned like mc,			/* MC for connect faults, timer runouts */
    2 sys_trouble_data aligned like mc,			/* MC for saved sys trouble data */
    2 ignore_data aligned like scu,			/* for SCU data to be ignored at certain times */
    2 iitemp fixed bin (71),				/* temporary used by ii (surprise!) */
    2 last_recorded_time fixed bin (71),		/* used by traffic control */
    2 idle_ptr ptr,					/* pointer to idle process APTE for this processor */
    2 simulated_mask fixed bin (71),			/* simulated system controller mask register */
    2 am_data bit (0),				/* to get addr of associative memory data block */
    2 ptw_am_regs (4*16) fixed bin (35),		/* page table regs (4 sets of 16 for dps8/70m) */
    2 ptw_am_ptrs (4*16) fixed bin (35),		/* page table pointers (4 sets of 16 for dps8/70m) */
    2 sdw_am_regs (4*16) fixed bin (71),		/* segment desc. regs (4 sets of 16 for dps8/70m) */
    2 sdw_am_ptrs (4*16) fixed bin (35),		/* segment desc. pointers (4 sets of 16 for dps8/70m) */
    2 processor_pattern bit (8) aligned,		/* 1 bit ON for this processor */
    2 processor_tag fixed bin (3),			/* CPU tag from maintenance panel */
    2 last_timer_setting bit (27) aligned,		/* last timer value loaded for this CPU */
    2 depth fixed bin,				/* depth in eligible queue for running process */
    2 mode_reg bit (36) aligned,			/* mode register for this processor */
    2 cache_luf_reg bit (36) aligned,			/* cache mode register for this CPU */
    2 fault_reg bit (72) aligned,			/* place to store the fault register */
    2 fault_reg_last_saved fixed bin (71),		/* time register last saved */
    2 hregs_last_saved fixed bin (71),			/* time history regs last saved */
    2 apt_ptr ptr,					/* -> apte running on this cpu		*/
    2 idle_temp fixed bin (71),			/* used by idle process			*/


/*  The following contains code used for handling connect faults for this processor			*/


    2 fast_connect_code (FAST_CONNECT_CODE_WORDS) bit (36) aligned,
    2 fast_connect_code_end bit (36) aligned,		/* marker for fast_connect_init		*/
    2 mode_reg_enabled bit (36) aligned,		/* used to set mode register			*/
    2 pad_mod_8 (6) fixed bin,
    2 ignore_pl (8) bit (36) aligned,                       /* used by wired fim to spl/lpl */
    2 pad_mod_16 (8) bit (36) aligned,
    2 processor_stack aligned like stack_frame;		/* first stack frame location */



	call get_temp_segment_ ("prds", prdsp, code);

	unspec (prds) = ""b;


/* Now make some checks on alignment of certain variables */

	call check (addr (prds.idle_ptr), "idle_ptr", 2);
	call check (addr (prds.processor_stack), "processor_stack", 16);
	call check (addr (prds.ptw_am_regs), "ptw_am_regs", 16);
	call check (addr (prds.sdw_am_regs), "sdw_am_regs", 32);
	call check (addr (prds.fast_connect_code), "fast_connect_code", 2);
	call check (addr (prds.ignore_pl), "ignore_pl",8);
	call check (addr (prds.apt_ptr), "apt_ptr", 2);

/* Now set up call to create data base */

	cdsa.sections (1).p = addr (prds);
	cdsa.sections (1).len = size (prds);
	cdsa.sections (1).struct_name = "prds";

	cdsa.seg_name = "prds";
	cdsa.num_exclude_names = 1;
	cdsa.exclude_array_ptr = addr (exclude_pad);

	string (cdsa.switches) = "0"b;
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), code);

	call release_temp_segment_ ("prds", prdsp, code);



check:	proc (where, message, modulo);

dcl  where ptr;
dcl  message char (*);
dcl  modulo fixed bin;

	     if mod (bin (rel (where), 18), modulo) ^= 0
	     then call com_err_ (0, prdsname, "The variable ^a is not aligned on a ^d-word boundary.", message, modulo);

	end check;


% include cds_args;


% include stack_header;


% include stack_frame;

% include mc;


     end prds;
