/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1988                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1982 *
   *                                                         *
   * Copyright (c) 1972 by Massachusetts Institute of        *
   * Technology and Honeywell Information Systems, Inc.      *
   *                                                         *
   *********************************************************** */





/* HISTORY COMMENTS:
  1) change(88-07-27,Farley), approve(88-10-05,MCR7968),
     audit(88-10-10,Beattie), install(88-10-14,MR12.2-1166):
     Added message for new rcerr_addscu_memoverlap error.
                                                   END HISTORY COMMENTS */


rc_messages: proc;

/* This data base includes the messages that are reported during dynamic reconfiguration
   of system controllers and processors.

   The data base consists of a two dimensional array of character strings. The first
   dimension indicates the type of reconfiguration command as follows:

   *	0	general
   *	1	add cpu
   *	2	dl cpu
   *	3	add mem
   *	4	dl mem
   *	5	add page
   *	6	dl page

   The second dimension is controlled by the particular reconfiguration request.

   Modified March 1976 by Steve Webber --- Initial coding
   Modified Feb 1979 by BSG for 8-cpu port expander
   Modified Dec 1979 by Mike Grady for new add scu message
   Modified August 1984 by Chris Jones for new reconfiguration commands
*/

/* Automatic */

dcl 1 cdsa aligned like cds_args;
dcl  code fixed bin (35);
dcl 1 rc_messages_array aligned,
    2 rc_messages (0:7, 11) char (64) aligned;
dcl (addr, null, size, string) builtin;

/* Entries */

dcl  create_data_segment_ entry (ptr, fixed bin (35));

/* Now begins the initialization */

	string (rc_messages_array) = "";

	rc_messages (0, rcerr_locked - 11) = "Reconfiguration database is locked.";
	rc_messages (0, rcerr_online - 11) = "^a is already online.";
	rc_messages (0, rcerr_no_config - 11) = "^a is not configured.";
	rc_messages (0, rcerr_not_online - 11) = "^a is not online.";
	rc_messages (0, rcerr_range - 11) = "Request is not within range of a single controller.";
	rc_messages (0, rcerr_sprq_failed - 11) = "Could not set CPU required.";

	rc_messages (1, rcerr_addcpu_no_response) = "No response from ^a.";
	rc_messages (1, rcerr_addcpu_bad_switches) = "The following switches on ^a are set incorrectly: ^a";
	rc_messages (1, rcerr_addcpu_trouble) = "Trouble fault trying to start ^a.";
	rc_messages (1, rcerr_addcpu_startup) = "Startup fault trying to start ^a.";
	rc_messages (1, rcerr_addcpu_lockup) = "Lockup fault trying to start ^a.";
	rc_messages (1, rcerr_addcpu_gcos) = "^a is not in Multics mode.";
	rc_messages (1, rcerr_addcpu_amoff) = "Associative memories not enabled on ^a.";
	rc_messages (1, rcerr_addcpu_enable) = "^a is not enabled at MEM ^a.";

	rc_messages (2, rcerr_delcpu_no_stop) = "Cannot stop ^a.";
	rc_messages (2, rcerr_delcpu_last) = "^a is the only CPU.";
	rc_messages (2, rcerr_delcpu_no_good_blcpu) = "No acceptable bootload CPU would be left.";

	rc_messages (3, rcerr_addscu_size) = "Size of ^a disagrees with CPU switches.";
	rc_messages (3, rcerr_addscu_dup_mask) = "^a has duplicate mask assignments to CPU ^a.";
	rc_messages (3, rcerr_addscu_no_mask) = "^a does not have mask assigned to CPU ^a.";
	rc_messages (3, rcerr_addscu_bad_mask) = "^a has mask ^a assigned to non-CPU port.";
	rc_messages (3, rcerr_addscu_fault) = "^a cannot be accessed by CPU ^a.";
	rc_messages (3, rcerr_addscu_switches) = "Switches for ^a set improperly on CPU ^a.";
	rc_messages (3, rcerr_addscu_enable) = "^a is not enabled on CPU ^a.";
	rc_messages (3, rcerr_addscu_manual) = "^a is not in PROGRAM mode.";
	rc_messages (3, rcerr_addscu_oldexpand) = "^a is a 6000 SCU which may not have a port expander.";
	rc_messages (3, rcerr_addscu_bigconfig) = "^a has less memory than config card indicates.";
	rc_messages (3, rcerr_addscu_memoverlap) = "^a has a possible memory address overlap problem.";

	rc_messages (4, rcerr_delmain_nomem) = "Not enough main memory to remove ^a.";
	rc_messages (4, rcerr_delmain_abs_wired) = "Abs wired pages in ^a.";

	rc_messages (6, rcerr_delmain_nomem) = "Not enough main memory left.";
	rc_messages (6, rcerr_delmain_abs_wired) = "Abs wired pages in memory.";


/* Now set up call to create data base */

	cdsa.sections (1).p = addr (rc_messages_array);
	cdsa.sections (1).len = size (rc_messages_array);
	cdsa.sections (1).struct_name = "rc_messages_array";

	cdsa.seg_name = "rc_messages";
	cdsa.num_exclude_names = 0;
	cdsa.exclude_array_ptr = null ();

	string (cdsa.switches) = "0"b;
	cdsa.switches.have_text = "1"b;

	call create_data_segment_ (addr (cdsa), code);
%page;
% include rcerr;
%page;
% include cds_args;

     end rc_messages;
