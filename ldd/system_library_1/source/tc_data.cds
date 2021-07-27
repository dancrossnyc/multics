/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1984 *
   *                                                         *
   * Copyright (c) 1972 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   *********************************************************** */

/* TC_DATA - This is the Traffic Controller Database. */
/* Last modified (Date and reason):
   2/6/76	by S. Webber Initial coding  
   6/20/79 by Mike Grady to init max_maxe
   3/4/81 by J. Bongiovanni not to set prds_length (it's done from the header
         or the TBLS Config Card)
   3/21/81 by J. Bongiovanni for max_stopped_stack_0, initialization NTO,
         response time metering
   6/27/81 by J. Bongiovanni for tuning parameter changes (-tcpu, +pre_empt_sample_time,
         gp_at_notify and gp_at_ptlnotify off by default
   1/82 BIM for stk truncation tuning parms.
   4/27/82 by J. Bongiovanni to change post_purge to OFF
   August 1982, J. Bongiovanni, for realtime_io parameters
   830916 to replace tty_polling_time with opc_polling_time... -E. A. Ranzenbach
   840120 to initialize apt_size by Keith Loepere
   841029 by M. Pandolf to init tc_suspend_lock
   841206 for next_ring0_timer by Keith Loepere.
*/

tc_data: proc;

/* This program creates the tc_data base */

/* Automatic */

dcl 1 cdsa aligned like cds_args;
dcl  code fixed bin (35);
dcl  big_time fixed bin (71);

/* Based */

dcl 1 tc_data aligned like tcm based (tcmp);

/* Static */

dcl  exclude_pad (1) char (32) aligned static options (constant) init ("pad*");

/* Builtins */

dcl (addr, bin, null, rel, size, string, unspec) builtin;

/* Entries */

dcl  com_err_ entry options (variable);
dcl  create_data_segment_ entry (ptr, fixed bin (35));
dcl  get_temp_segment_ entry (char (*), ptr, fixed bin (35));
dcl  release_temp_segment_ entry (char (*), ptr, fixed bin (35));




	call get_temp_segment_ ("tc_data", tcmp, code);

/* Check offsets assumed by BOS */

	call check_offset_for_bos (addr (tc_data.apt_offset), 171, "apt_offset");
	call check_offset_for_bos (addr (tc_data.apt_size), 203, "apt_size");
	call check_offset_for_bos (addr (tc_data.apt_entry_size), 215, "apt_entry_size");


	tc_data.apt_offset = rel (addr (tc_data.apt));
	tc_data.apt_lock = -1;			/* -1 = unlocked */
	tc_data.metering_lock = -1;			/* 0 = locked */
	tc_data.working_set_factor = 1;
	tc_data.ncpu = 0;
	tc_data.itt_size = 155;
	tc_data.dst_size = 155;
	tc_data.apt_size = 40;
	tc_data.initializer_id = (36)"1"b;
	tc_data.max_eligible = 6*262144;
	tc_data.max_max_eligible = 16*262144;
	tc_data.max_stopped_stack_0 = 4;
	tc_data.apt_entry_size = size (apte);
	tc_data.pds_length = 1024;

	tc_data.interactive_q.fp = rel (addr (tc_data.interactive_q));
	tc_data.interactive_q.bp = rel (addr (tc_data.interactive_q));
	tc_data.interactive_q.sentinel = (36)"1"b;

	tc_data.max_hproc_segno = 191;		/* largest (default) hardcore segment number */
	tc_data.dst_ptr = null;
	tc_data.old_user = null;
	tc_data.tefirst = 2000000;
	tc_data.telast = 2000000;
	tc_data.timax = 8000000;
	tc_data.process_initial_quantum = 2000000;
	tc_data.gp_at_notify = 0;			/* off by default */
	tc_data.gp_at_ptlnotify = 0;			/* off by default */
	tc_data.pre_empt_sample_time = 40000;		/* 40 milliseconds */
	tc_data.max_timer_register = 40000;

	tc_data.tc_suspend_lock = ""b;

	tc_data.sort_to_elhead = 1;
	tc_data.auto_tune_ws = 1;
	tc_data.ocore = .01b;
	tc_data.stk_truncate = "1"b;
	tc_data.stk_truncate_always = "0"b;

/* See fast_hc_ipc, but the rolling average of steps/block is calculated */
/* as NEW_AVERAGE = factor*NEW_VALUE + (1-factor)*OLD_AVERAGE */

	tc_data.stk_trunc_avg_f1 = 0.9375; /* 15/16 */
	tc_data.stk_trunc_avg_f2 = 1b - tc_data.stk_trunc_avg_f1;

	tc_data.lock_error_severity = 1; /* CRASH */

	tc_data.realtime_q.fp = rel (addr (tc_data.realtime_q));
	tc_data.realtime_q.bp = rel (addr (tc_data.realtime_q));
	tc_data.realtime_q.sentinel = (36)"1"b;

	tc_data.eligible_q_head.fp = rel (addr (tc_data.eligible_q_tail));
	tc_data.eligible_q_head.bp = "0"b;
	tc_data.eligible_q_head.sentinel = (36)"1"b;

	tc_data.eligible_q_tail.fp = rel (addr (tc_data.idle_tail));
	tc_data.eligible_q_tail.bp = rel (addr (tc_data.eligible_q_head));
	tc_data.eligible_q_tail.sentinel = (36)"1"b;

	tc_data.idle_tail.fp = "0"b;
	tc_data.idle_tail.bp = rel (addr (tc_data.eligible_q_tail));
	tc_data.idle_tail.sentinel = (36)"1"b;

	tc_data.min_eligible = 2*262144;
	tc_data.guaranteed_elig_inc = 250000;
	tc_data.priority_sched_inc = 80000000;
	tc_data.int_q_enabled = 1;
	tc_data.fnp_buffer_threshold = 30;		/* fnp tries to keep > this many free buff */

	unspec (big_time) = "000000000000000000001111111111111111111111111111111111111111111111111111"b;
	tc_data.end_of_time = big_time;
	tc_data.next_alarm_time = big_time;		/* gets zeroed by tc_init$part_2 */
	tc_data.priority_sched_time = big_time;
	tc_data.opc_polling_time = big_time;
	tc_data.disk_polling_time = big_time;
	tc_data.tape_polling_time = big_time;
	tc_data.imp_polling_time = big_time;
	tc_data.mos_polling_time = big_time;
	tc_data.volmap_polling_time = big_time;
	tc_data.next_ring0_timer = big_time;
	tc_data.realtime_io_deadline = 0;
	tc_data.realtime_io_quantum = 5000;		/* 5 milliseconds */

	tc_data.max_channels = 6;

	tc_data.init_wait_timeout = 5000000;		/* 5 second NTO during initialization		*/
	tc_data.init_timeout_severity = 0;		/* beep					*/
	tc_data.vcpu_response_bounds_size = VCPU_RESPONSE_BOUNDS;
	tc_data.vcpu_response_bounds (1) = 500000;	/* 1/2 second				*/
	tc_data.vcpu_response_bounds (2) = 1000000;	/* 1 second				*/
	tc_data.vcpu_response_bounds (3) = 10000000;	/* 10 seconds				*/
	

	tc_data.default_procs_required = (8) "1"b;	/* all CPUs */
/* Now set up call to create data base */

	cdsa.sections (1).p = addr (tc_data);
	cdsa.sections (1).len = size (tc_data);
	cdsa.sections (1).struct_name = "tc_data";

	cdsa.seg_name = "tc_data";
	cdsa.num_exclude_names = 1;
	cdsa.exclude_array_ptr = addr (exclude_pad);

	string (cdsa.switches) = "0"b;
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), code);

	call release_temp_segment_ ("tc_data", tcmp, code);


check_offset_for_bos:
	proc (item_ptr, bos_offset, item_name);
	
	dcl item_ptr ptr;				/* pointer to item in tc_data			*/
	dcl bos_offset fixed bin (18);		/* location assumed by BOS			*/
	dcl item_name char (*);			/* name of item in structure			*/
	
	if bin (rel (item_ptr)) ^= bos_offset
	     then call com_err_ (0, "tc_data", "^a not at BOS-assumed offset (^d=^oo)",
	     item_name, bos_offset, bos_offset);
	
	
     end check_offset_for_bos;
     
	
	



% include cds_args;



% include apte;



% include tcm;



% include hc_lock;

     end tc_data;
