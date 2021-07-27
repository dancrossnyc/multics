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


/* PDS - The Process Data Segment

   Last modified (Date and reason):
   2/6/76	by S. Webber Initial coding
   9/17/76 by R. Bratt to add seg_fault, bounds_fault, vtoc_read, and vtoc_write meters.
   11/03/76 by M. Weaver to extend stack header
   04/20/77 by M. Weaver to delete rntp and 7/77 to add name template_pds
   06/07/78 by E. Donner to add ring_events (to prevent delayed ipc wakeups)
   05/10/79 by B. Margulies to eliminate exmode_level
   05/09/79 by Mike Grady to use shared ring 0 stacks
   08/17/79 by J. A. Bush for exp under/overflow restart switches & cache parity diagnostics
   02/28/80 by B. Margulies to use the include file for the default overflow
   08/26/80 by J. A. Bush  for the DPS8/70M CPU
   value.
   02/23/81 by J. Bongiovanni to remove temp_mode_reg (moved to prds$mode_reg_enabled)
   03/81 by E. Donner to remove next_itt and ect_pointers
    3/82 BIM for lock_array cleanup.
   11/82 by J. Bongiovanni to make force_write_limit per-ring
    2/83 by E. N. Kittlitz for hfp_exponent_enabled.
    830621 BIM for level improvements.
   10/83 by E. N. Kittlitz to resurrect obsolescent network_ptbl_idx for MR10.2.
    83-11-02 by E. N. Kittlitz for block_lock_count in low page, hex exponent control.
    83-11-21 BIM to inhibit quota and save history registers by default
	   in the initializer's process.
    83-12-01 E. N. Kittlitz for restart hex overflow fault control
    83-12-03 BIM to clear trace header properly. (and new trace format)
    84-12-10 Keith Loepere for throttle_segment_state_changes and other
	   covert channel related variables.
  1985-01-21, BIM: admin_privileges to record ring 1 priv settings.
  1985-04-08, BIM: no_audit_ring1_fs_object_ops to suppress auditing
	         while in the mseg primitives and RCP.
*/

/* format: style3,idind25 */
pds:
     procedure;

/* This program creates the pds data base */

/* Automatic */

dcl	1 cdsa		     aligned like cds_args;
dcl	code		     fixed bin (35);

/* Constants */

dcl	pdsname		     char (3) aligned static init ("pds") options (constant);
dcl	exclude_pad	     (1) char (32) aligned static options (constant) init ("pad*");

/* Builtins */

dcl	(addr, bin, bit, decimal, divide, float, hbound, mod, null, rel, size, string, unspec)
			     builtin;

/* Entries */

dcl	com_err_		     entry options (variable);
dcl	create_data_segment_     entry (ptr, fixed bin (35));
dcl	get_temp_segment_	     entry (char (*), ptr, fixed bin (35));
dcl	release_temp_segment_    entry (char (*), ptr, fixed bin (35));
dcl	hcs_$chname_file	     entry (char (*), char (*), char (*), char (*), fixed bin (35));
dcl	get_wdir_		     entry () returns (char (168));

/* External Static */

dcl	error_table_$segnamedup  fixed bin (35) ext;


dcl	pdsp		     ptr;

dcl	1 pds		     aligned based (pdsp),
	  2 page_fault_data	     like mc,		/* MC for page faults and timer runouts */
	  2 fim_data	     like mc,		/* MC for normal faults */
	  2 signal_data	     aligned like mc,	/* storage for MC being signalled */
	  2 history_reg_data     (64) fixed bin (71),	/* this must follow signal data */
	  2 process_group_id     char (32),		/* user id for current process */
	  2 cpu_time	     fixed bin (52),	/* number that when subtracted from clock reading gives
						   virtual cpu time */
	  2 virtual_delta	     fixed bin (52),	/* temporary used in calculating VCPU time */
	  2 virtual_time_at_eligibility
			     fixed bin (52),	/* temporary used in calculation of VCPU time */
	  2 temp_1	     fixed bin (71),	/* temporary */
	  2 temp_2	     fixed bin (71),	/* temporary */
	  2 time_1	     fixed bin (52),	/* page fault metering time */
	  2 time_v_temp	     fixed bin (52),	/* temporary used in calculating VCPU time */
	  2 fim_v_temp	     fixed bin (52),	/* VCPU temporary for the FIM */
	  2 fim_v_delta	     fixed bin (71),	/* VCPU temporary for the FIM */
	  2 save_history_regs    bit (1) aligned,	/* = "1"b if history registers are to be saved */
	  2 hregs_saved	     bit (1) aligned,	/* = "1"b if history regs were saved */
	  2 last_sp	     ptr,			/* stack pointer at getwork time */
	  2 apt_ptr	     ptr,			/* pointer to this process's APT entry */
	  2 arg_1		     fixed bin (71),	/* argument for pxss */
	  2 arg_2		     fixed bin (71),	/* argument for pxss */
	  2 arg_3		     fixed bin (71),	/* argument for pxss */
	  2 arg_4		     fixed bin (71),	/* argument for pxss */
	  2 access_authorization aligned like aim_template,
						/* access authorization for the process */
	  2 base_addr_reg	     bit (18) aligned,	/* for BAR mode use */
	  2 alarm_ring	     fixed bin (3),		/* setting for ring alarm register */
	  2 pxss_args_invalid    bit (36) aligned,	/* used by pxss masking/arg copying code */
	  2 processid	     bit (0) unaligned,	/* process ID (added segdef) */
	  2 process_id	     bit (36) aligned,	/* process ID */
	  2 vtime_count	     fixed bin,		/* depth counter used in VCPU calculation */
	  2 pstep		     bit (0) unaligned,	/* (added segdef for dstep) */
	  2 dstep		     bit (18) aligned,	/* rel pointer to ASTE for dseg */
	  2 wakeup_flag	     bit (36) aligned,	/* flag indicating type of wakeup */
	  2 pc_call	     bit (36) aligned,	/* flag saying type of wait */
	  2 audit_flags	     bit (36) aligned,	/* bits indicating types of auditing to do */
	  2 quota_inhib	     fixed bin aligned,	/* ON if quota checking to be inhibited */
	  2 covert_event_count   fixed bin,		/* count of covert channel related segment state change events */
	  2 page_waits	     fixed bin,		/* page faults */
	  2 number_of_pages_in_use
			     fixed bin,		/* used in calculating memory units */
	  2 post_purged	     fixed bin,		/* number of post purgings */
	  2 connect_pending	     bit (1) aligned,	/* turned on for delayed connects to be resent by fim */
	  2 segment_faults	     fixed bin (35),	/* count of segment faults taken by this process */
	  2 bounds_faults	     fixed bin (35),	/* count of bounds faults taken by this process */
	  2 vtoc_reads	     fixed bin (35),	/* vtoc read I/Os done for this process */
	  2 vtoc_writes	     fixed bin (35),	/* vtoc write I/Os done for this process */
	  2 mc_trace_seg	     fixed bin,		/* seg number of object segment being traced */
	  2 mc_trace_sw	     bit (2) aligned,	/* switch for M. C. Tracing "11"b => trace on */
	  2 stack_0_sdwp	     ptr aligned,		/* ptr to stack sdw in dseg */
	  2 stack_0_ptr	     ptr aligned,		/* ptr to base of ring 0 stack (wired for esd) */
	  2 tc_argp	     ptr,			/* arg ptr used by tc */
	  2 tc_mask	     bit (72) aligned,	/* save tc mask */
	  2 exp_undfl_rest	     bit (2) aligned,	/* fim restarts underflow: '1'b = binary, '01'b = hex */
	  2 exp_ovfl_rest	     bit (2) aligned,	/* fim restarts exp overflow: '1'b = binary, '01'b = hex */
	  2 eovfl_value	     bit (72) aligned,	/* value DFLD'ed by fim on restart binary overflow */
	  2 hex_eovfl_value bit (72) aligned,		/* value DFLD'ed by fim on restart hex overflow */
	  2 cpar_err_data	     bit (72) aligned,	/* cache parity error data (from cache) */
	  2 cpar_mem_data	     bit (72) aligned,	/* cache parity error data (from memory) */
	  2 cpar_info	     bit (36) aligned,	/* diagnose flag, cache level and absaddr # */
	  2 hfp_exponent_enabled bit (1) aligned,	/* user allowed to set IR hex exp bit */
	  2 pre_empt_poll_return pointer,
	  2 block_lock_count     fixed bin,		/* count of locks held */
	  2 throttle_segment_state_changes bit (1) aligned,/* limit bandwidth of segment state covert channels */
	  2 first_covert_event_time fixed bin (52),
	  2 pad_for_trace_mod16  (6) fixed bin,
	  2 trace		     (306) fixed bin (71),	/* system trace data */
						/* pds$trace + 16 defines the pds for idle procs */
	  2 timer_time_out	     fixed bin (52),	/* time out time for the process */
	  2 timer_channel	     fixed bin (71),	/* event channel for time out event */
	  2 term_channel	     fixed bin (71),	/* channel used to signal process termination */
	  2 term_proc	     bit (36) aligned,	/* process ID of process to signal term process */
	  2 pl1_machine	     fixed bin,		/* nonzero if we do pl1-like things */
	  2 validation_level     fixed bin (3),
	  2 condition_name	     aligned,		/* ACC string for condition name */
	    3 len		     fixed bin (8) unaligned,
	    3 chars	     char (31) unaligned,
	  2 pad_obsolete	     bit (36) aligned,
	  2 ips_mask	     (0:7) bit (35) aligned,	/* IPS masks */
	  2 auto_mask	     (0:7) bit (36) aligned,	/* array of automatic masks for IPS signals */
	  2 ring_alarm_val	     (0:7) fixed bin,	/* used in checking validation level changes */
	  2 lock_id	     bit (36) aligned,	/* UID used in some locking */
	  2 mc_trace_buf	     ptr unaligned,		/* packed ptr to mc_trace wired buffer */
	  2 pad_end_of_page_0    bit (0) unaligned,
	  2 pathname_am	     aligned like pam,	/* pathname associative memory */
	  2 initial_procedure    ptr,			/* first procedure executed in a new process */
	  2 account_id	     char (32) aligned,	/* not used yet */
	  2 access_name	     aligned,		/* alternate form of process group id */
	    3 user	     char (32) aligned,
	    3 project	     char (32) aligned,
	    3 tag		     char (32) aligned,
	  2 home_dir	     char (168) aligned,	/* home directory */
	  2 process_dir_name     char (32) aligned,	/* name of process directory */
	  2 wdir		     (0:7) ptr,		/* pointers to per-ring working directories */
	  2 wdir_uid	     (0:7) bit (36) aligned,	/* UID of per-ring working directories */
	  2 transparent	     bit (36) aligned,	/* transparent usage, mod, pd switch */
	  2 itt_head	     bit (18) aligned,	/* top of present ITT list */
	  2 max_access_authorization
			     aligned like aim_template,
						/* max authorization this user can attain */
	  2 stacks	     (0:7) ptr,		/* per-ring stack pointers */
	  2 kstp		     ptr,			/* pointer to start of KST */
	  2 events_pending	     bit (36) aligned,	/* special wakeups pending */
	  2 special_channels     bit (36) aligned,	/* special channels assigned */
	  2 event_masks	     (7) bit (36) aligned,	/* per-ring mask for special channels */
	  2 initial_ring	     fixed bin (3),		/* initial ring of execution for the process */
	  2 interrupt_ring	     fixed bin (3),		/* lowest ring in which IPS interrupts are allowed */
	  2 highest_ring	     fixed bin (3),		/* highest ring in which process can run */
	  2 prelinked_ring	     bit (8) aligned,	/* bit(i) is ON if ring (i) is prelinked */
	  2 unique_scu_index     bit (36) aligned,	/* used to tag MC */
	  2 max_lot_size	     (0:7) fixed bin,	/* sizes lots can grow to */
	  2 lot_stack_size	     (0:7) fixed bin,	/* size of lot in stack (0 -> lot not in stack) */
	  2 clr_stack_size	     (0:7) fixed bin,	/* size of CLR in stack */
	  2 link_meters_bins     (4) fixed bin,		/* histograms of linkage faults */
	  2 link_meters_times    (4) fixed bin (30),	/* histogram of linkage fault times */
	  2 link_meters_pgwaits  (4) fixed bin,		/* histogram of linkage faults PF's */
	  2 dmpr_copy_dirsegp    ptr,			/* ptr to temp segment into which dirs are copied */
	  2 dmpr_pvid	     bit (36),		/* pvid of volume being dumped */
	  2 dmpr_pvtx	     fixed bin,		/* pvtx of volume being dumped  */
	  2 first_call	     fixed bin,		/* ON until leave ring zero once */
	  2 mc_save_area	     bit (18) aligned,	/* rel pointer to start of saved MC area */
	  2 mc_save_ptr	     bit (18) aligned,	/* ptr to next mc save place */
	  2 mc_save_limit	     bit (18) aligned,	/* max address where MC can be saved */
	  2 useable_lot	     bit (8) aligned,	/* indicates whether lot can be referenced */
	  2 ring_events	     bit (36) aligned,	/* per-ring indicator that itt messages copied to ect */
	  2 force_write_limit    (0:7) fixed bin,	/* limit on force-writing */
						/* Following must be doubleword aligned! */
	  2 ipc_vars	     aligned,		/* holds state of fast_hc_ipc at block */
	    3 ap		     pointer unal,
	    3 retsw	     fixed bin (35),
	    3 save_entry_ret     fixed bin (35),
	    3 truncated_stacks   fixed bin (35),
	    3 chan	     fixed bin (71),
	    3 block_start_steps  fixed bin (35),
	    3 stk_temp	     fixed bin (35),
	  2 ipc_block_return     bit (36),		/* ipc block return address */
	  2 avg_block_steps	     fixed bin (35, 18),
	  2 admin_privileges     bit (36) aligned,        /* There is a 1 here for each privilege that must be reset on exit from ring 1 */
	  2 no_audit_ring1_fs_object_ops bit (1) aligned, /* Ring 1 has asked to turn off ring 0 auditing */
	  2 pad_for_data_mod16   (6) fixed bin (35),
	  2 data		     bit (0) aligned;	/* to mark end of PDS for MC save area */
%page;
	call get_temp_segment_ ("pds", pdsp, code);       /* Returns ZEROS */

/* Now begins the initialization */

	pds.process_group_id = "Initializer.SysDaemon.z";

	pds.access_authorization.categories = (18)"0"b;
	pds.access_authorization.level = 0;
	pds.access_authorization.dir = "1"b;		/* for initializer */
	pds.access_authorization.seg = "1"b;
	pds.access_authorization.rcp = "1"b;
	pds.access_authorization.ipc = "1"b;
	pds.access_authorization.soos = "1"b;		/* .. */

	pds.max_access_authorization.categories = (18)"1"b || (18)"0"b;
	pds.max_access_authorization.level = 7;

	pds.quota_inhib = 1;			 /* initializer ignore rqover until it is enabled */
	pds.vtime_count = -1;
	pds.process_id = (36)"1"b;
	pds.lock_id = (36)"1"b;
	pds.pl1_machine = 1;
	pds.ips_mask (*) = (35)"1"b;
	pds.force_write_limit (*) = 1;

	pds.save_history_regs = "1"b;
	pds.hregs_saved = "0"b;
	pds.history_reg_data (*) = 0;

	pds.mc_trace_buf = null;
	pds.mc_trace_sw = "0"b;
	pds.mc_trace_seg = 0;

	pds.eovfl_value = unspec (Default_exponent_control_overflow_value);
	pds.hex_eovfl_value = unspec (Default_hex_exponent_control_overflow_value);
						/* set default exp overflow restart value */
	pds.exp_ovfl_rest, pds.exp_undfl_rest = "0"b;

	pds.stack_0_sdwp = null;
	pds.stack_0_ptr = null;
	pds.pad_for_trace_mod16 (*) = 0;

	unspec (pds.trace) = ""b;
	trace_ptr = addr (pds.trace);
	trace.last_available = divide (hbound (pds.trace, 1) * size (page_trace_entry) - 8, 2, 17, 0);
	trace.threshold = .75 * float (decimal (trace.last_available));

	pds.initial_procedure = null;

	pds.access_name.user = "Initializer";
	pds.access_name.project = "SysDaemon";
	pds.access_name.tag = "z";

	pds.home_dir = ">system_control_1";
	pds.process_dir_name = ">process_dir_dir>!zzzzzzzbBBBBBB";

	pds.wdir (*) = null;
	pds.wdir_uid (*) = "0"b;



	pds.stacks (*) = null;

	pds.dmpr_pvid = "0"b;
	pds.dmpr_pvtx = 0;
	pds.dmpr_copy_dirsegp = null;

	pds.kstp = null;
	pds.first_call = 1;
	pds.initial_ring = 1;
	pds.interrupt_ring = 4;
	pds.highest_ring = 7;

	pds.max_lot_size (*) = 1024;

	pds.mc_save_area = rel (addr (pds.data));
	pds.mc_save_ptr = rel (addr (pds.data));
	pds.mc_save_limit = bit (bin (4096, 18), 18);	/* Allow for as many as fit in 4K. */

/* Now make some checks on alignment of certain variables */

	call check (addr (pds.ipc_vars), "ipc_vars", 2);
	call check (addr (pds.page_fault_data), "page_fault_data", 16);
	call check (addr (pds.trace), "trace", 16);
	call check (addr (pds.signal_data), "signal_data", 16);
	call check (addr (pds.eovfl_value), "eovfl_value", 2);
	call check (addr (pds.hex_eovfl_value), "hex_eovfl_value", 2);
	call check (addr (pds.data), "data", 16);
	if bin (rel (addr (pds.pad_end_of_page_0)), 18) ^= 1024
	then call com_err_ (0, pdsname, "Wired portion must end at 1024");

/* Now set up call to create data base */

	cdsa.sections (1).p = addr (pds);
	cdsa.sections (1).len = size (pds);
	cdsa.sections (1).struct_name = "pds";

	cdsa.seg_name = "pds";
	cdsa.num_exclude_names = 1;
	cdsa.exclude_array_ptr = addr (exclude_pad);

	string (cdsa.switches) = "0"b;
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), code);

	call release_temp_segment_ ("pds", pdsp, code);

	call hcs_$chname_file (get_wdir_ (), "pds", "", "template_pds", code);
	if code ^= 0
	then if code ^= error_table_$segnamedup
	     then call com_err_ (code, pdsname, "Unable to add name template_pds.");


check:
     proc (where, message, modulo);

dcl	where		     ptr;
dcl	message		     char (*);
dcl	modulo		     fixed bin;
dcl	remainder		     fixed bin;

	remainder = mod (bin (rel (where), 18), modulo);
	if remainder ^= 0
	then call com_err_ (0, pdsname, "The variable ^a is ^d words away from being aligned on a ^d-word boundary.",
		message, (modulo - remainder), modulo);

     end check;
%page; %include aim_template;
%page; %include cds_args;
%page; %include exponent_control_info;
%page; %include mc;
%page; %include pathname_am;
%page; %include sys_trace;
     end pds;
