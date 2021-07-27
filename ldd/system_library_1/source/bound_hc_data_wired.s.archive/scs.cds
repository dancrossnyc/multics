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


/* SCS - The System Communications Segment
   modified 3/27/77 by Noel I. Morris
   last modified 4/26/78 by J. A. Bush for processor testing
   Modified 2/79 by BSG for 8-cpu port expander
   Modified 9/16/80 by J. A. Bush for the DPS8/70M CPU
   Modified 1/09/81 W. Olin Sibert to remove all initializations to scs_and_clock_init
   Modified 01/16/81 W. Olin Sibert, to add scs$port_data
   Modified Jabuary 1981 by C. Hornig for new I/O.
   Modified 2/22/81 by J. Bongiovanni for fast connect code
   Modified 4/23/81 by J. Bongiovanni for cycle_priority_template
   Modified 4/09/82 by J. Bongiovanni for switch 0, processor_data_switch_value
   Modified 7/30/82 by J. Bongiovanni for trouble_processid
   Modified 4/11/83 by E. N. Kittlitz for drl_message_pointer.
   Modified 10/25/83 by Keith Loepere for start_of_scs
*/

scs:
     procedure;

/* Static */

dcl exclude_pad (1) char (32) static options (constant) init ("pad*");

/* Automatic */

dcl code fixed bin (35);
dcl 1 cdsa aligned like cds_args;
dcl i fixed bin;

/* Builtins */

dcl (addr, bin, bit, null, size, string, unspec) builtin;

/* Entries */

dcl create_data_segment_ entry (ptr, fixed bin (35));
%page;
dcl 1 scs aligned,					/* Information about system controllers */
      2 start_of_scs fixed bin (71),
      2 controller_data (0:7) aligned like scs$controller_data,
						/* per-controller info */
						/* Information about CPUs */
      2 processor_data (0:7) aligned like scs$processor_data,
						/* information about CPUs in the system */
      2 port_data (0:7) like scs$port_data aligned,	/* Info on what is connected to each SCU port */
      2 cow (0:7) like scs$cow,			/* Actual COW's. */
      2 cow_ptrs (0:7) aligned like scs$cow_ptrs,		/* Rel pointers to COW's. */
      2 reconfig_general_cow aligned like scs$reconfig_general_cow,
						/* Used for reconfiguration operations */
						/* MASKS and PATTERNS */
      2 sys_level aligned bit (72),			/* mask used while handling I/O interrupts */
      2 open_level aligned bit (72),			/* mask used during normal operation */
      2 processor_start_mask aligned bit (72),		/* mask used when starting up a CPU */
      2 cpu_test_mask aligned bit (72),			/* mask used for ISOLTS CPU testing */
      2 number_of_masks fixed bin,			/* number of masks (starting at sys_level) */
      2 processor_start_pattern bit (36) aligned,		/* SMIC pattern used to send processor start interrupt */
      2 cpu_test_pattern bit (36) aligned,		/* SMIC pattern used for ISOLTS processor testing */
      2 expanded_ports bit (1) unaligned dim (0:7),	/* Which ports have expanders */
						/* CAM and CACHE clear info */
      2 cam_pair fixed bin (71),			/* instructions XEDd when CAMing and clearing CACHE */
      2 cam_wait bit (8) aligned,			/* Used when evicting pages from main memory */
      2 pad1 fixed bin,				/* MASKING INSTRUCTIONS & POINTERS */
      2 set_mask (0:7) bit (36) aligned,		/* instructions to set mask (STAQ or SMCM) */
      2 read_mask (0:7) bit (36) aligned,		/* instructions to read mask (LDAQ or RMCM) */
      2 mask_ptr (0:7) ptr unaligned,			/* pointers for real or simulated masks */
						/* MISCELLANEOUS */
      2 idle_aptep (0:7) ptr unaligned,			/* pointer to idle process APTE for each processor */
      2 connect_lock bit (36) aligned,			/* lock for sending connects */
      2 reconfig_lock bit (36) aligned,			/* lock used during reconfiguration */
      2 trouble_flags bit (8) aligned,			/* checkoff flags for sys_trouble stopping */
      2 bos_restart_flags bit (8) aligned,		/* checkoff flags for restarting after sys_trouble */
      2 nprocessors fixed bin,			/* number of processors online */
      2 bos_processor_tag fixed bin (3),		/* CPU tag of processor running BOS */
      2 faults_initialized bit (1) aligned,		/* ON after faults have been enabled */
      2 sys_trouble_pending bit (1) aligned,		/* sys_trouble event is pending in the system */
      2 fast_cam_pending (0:7) bit (36) aligned,		/* checkoff flags for cam connect */
      2 interrupt_controller fixed bin (3),		/* port number of low order controller */
      2 cycle_priority_template bit (7) aligned,
      2 set_cycle_switches bit (1) aligned,
      2 processor_start_int_no fixed bin (5),		/* interrupt cell for starting a processor */
      2 processor bit (8) aligned,			/* bits ON for online CPUs */
      2 processor_start_wait bit (8) aligned,		/* checkoff flags for waiting for new processor */
      2 trouble_processid bit (36) aligned,		/* processid causing crash */
      2 drl_message_pointer ptr unal,			/* pointer to DRL message text */
      2 processor_test_data aligned like scs$processor_test_data,
						/* info for cpu testing */
      2 pad2 fixed bin,
      2 trouble_dbrs (0:7) fixed bin (71),		/* DBR values at system crash time */
      2 port_addressing_word (0:7) bit (3) aligned,	/* active module port number for each controller */
      2 cfg_data (0:7) fixed bin (71),			/* RSCR-CFG data from each controller */
      2 cfg_data_save fixed bin (71),			/* RSCR-CFG save area for ISOLTS CPU testing */
      2 processor_switch_data (0:4) bit (36) aligned,	/* actual processor RSW data */
      2 processor_switch_template (0:4) bit (36) aligned,	/* expected data from RSW 0 thru 4 */
      2 processor_switch_compare (0:4) bit (36) aligned,	/* discrepancies from expected data */
      2 processor_switch_mask (0:4) bit (36) aligned,	/* masks for comparing switch data */
      2 processor_data_switch_value bit (36) aligned,	/* Correct value of CPU data switches */
						/* Data used by init_sst and collect_free_core, from config cards. */
      2 controller_config_size (0:7) fixed bin (14) aligned,/* config card-stated size of controller */
      2 reconfig_locker_id char (32) aligned,		/* process group ID of process doing reconfiguration */
      2 scas_page_table (0:31) bit (36) aligned,		/* Page table for SCAS */
      2 end_of_scs fixed bin;				/* For initialization */
%page;
	unspec (scs) = "0"b;			/* clear entire structure */

/* Now set up for call to create_data_segment_ */

	cdsa.sections (1).p = addr (scs);
	cdsa.sections (1).len = size (scs);
	cdsa.sections (1).struct_name = "scs";

	cdsa.seg_name = "scs";
	cdsa.num_exclude_names = 1;
	cdsa.exclude_array_ptr = addr (exclude_pad);

	string (cdsa.switches) = "0"b;
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), code);
	return;
%page;
%include scs;
%include cds_args;

     end scs;
