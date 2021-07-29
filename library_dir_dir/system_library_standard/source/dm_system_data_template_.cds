/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1983 *
   *                                                         *
   *********************************************************** */



/* HISTORY COMMENTS:
  1) change(86-02-12,Pierret), approve(86-03-03,MCR7331),
     audit(86-04-28,Newcomb), install(86-05-06,MR12.0-1054):
     Changed to version 5.8.
  2) change(86-03-03,Pierret), approve(86-03-03,MCR7349),
     audit(86-04-28,Newcomb), install(86-05-06,MR12.0-1054):
     Changed to version 5.9.
                                                   END HISTORY COMMENTS */


/*  DESCRIPTION:

	Per-System Data Management Segment (actually, per-AIM authorization).
*/

/*  HISTORY:

Written by J. Bongiovanni, 08/15/82.
Modified:
08/17/82 by M. Pandolf:  to add before journal and transaction manager
	  global variables.
09/14/82 by M. Pandolf:  to add data management daemon event information.
08/04/82 by M. Pandolf:  to add timeout increment and default before
	  journal size.
10/18/82 by Lee A. Newcomb:  to add flag recovering_from_crash_sw.
11/10/82 by Lee A. Newcomb:  to change recovering_from_crash_sw to
	  current_dm_state (see dm_statuses.incl.pl1).
11/18/82 by J. Bongiovanni:  to add lock items.
12/22/82 by M. Pandolf:  to add log_lock.
03/15/83 by M. Pandolf:  to add initializer_name.
03/21/83 by M. Pandolf:  to add trace_stack_mode.
03/23/83 by M. Pandolf:  to add initializer_shutdown_delay,
	  default_bj_dirname, and default_bj_filename.
03/28/83 by M. Pandolf:  to change default_bj_(dir file)name to
	  bj_default_(dir file)name; and add log_proc_term.
06/08/83 by L. A. Newcomb: to add command_ms_name for talking to the Daemon.
11/04/83 by M. Pandolf: to clarify setting of default before journal names.
03/05/84 by Lee A. Newcomb:  changed to use the include file dm_system_states
	  (renamed from dm_statuses), and to use the new names therein.
06/12/84 by Lindsey Spratt: Changed initializer_shutdown_delay to be fixed bin
	  (71).  Removed the trace_stack_mode.
08/01/84 by Lee A. Newcomb:  added shutdown delay and time values and
	  default all the shutdown delays to five minutes.
09/05/84 by Lee A. Newcomb:  added the times for "user warning" and "begin
            shutdown" shutdown steps, and begin_shutdown_delay; renamed
	  initializer_shutdown_(delay time) to be user_shutdown_= for better
	  understanding, and added the saved shutdown time and reason cells
	  that are set by an operator or administrator.
10/18/84 by Stanford S. Cox: added lock_list_meters_rel & meter_fast_locks.
11/05/84 by Lindsey Spratt:  Renamed the module dm_system_data_template_ from
	  dm_system_data_.  This makes the per-system initialization
	  simpler, since only the system data segment in the per-bootload
	  dir has the name dm_system_data_, and the library system data
	  segment is always known as dm_system_data_template_.
01/08/85 by Steve Herbst: Deleted $log_lock.
01/25/85 by Lindsey Spratt:  Added $system_software_version, 1.0.
02/15/85 by Lindsey Spratt:  Changed $system_software_version to "1.1 dev" for
	  dev version of DM system.  Corresponding bound_dev version is
	  "1.1".
02/24/85 by Lindsey Spratt:  Re-added  $system_software_version, which was
	  backed out in the most recent installation of this module (back to
	  the modifications journalized by Herbst on 01/08/85).  The current
	  value of the version is "1.3 dev".  Also, removed the unused
	  DM_LOG_NAME constant.
02/27/85 by Lindsey Spratt: Changed $system_software_version to "2.1 dev".
03/19/85 by Lindsey Spratt: Changed version to "3.1 dev".
03/25/85 by Lindsey Spratt: Changed version to "4.2 dev".
03/26/85 by Lindsey Spratt: Changed version to "4.3 dev".
03/27/85 by Lindsey Spratt: Changed version to "4.4 dev".
03/28/85 by Lindsey Spratt: Changed version to "4.5 dev".
03/29/85 by Lindsey Spratt: Changed version to "4.6 dev".
04/01/85 by S. Cox: Changed to set on $meter_fast_locks.
04/04/85 by Lee A. Newcomb:  Fixed to set the default system BJ size to 4000
            CI's as approved, not 192.
04/12/85 by Lindsey Spratt: Changed version to "5.0".
04/12/85 by Lindsey Spratt: Changed version to "5.1".  This version has the
	  change to fm_put_.
04/18/85 by Lindsey Spratt: Changed version to "5.2".  This version includes:
	  fixes to fast_lock metering so that the meters are actually
	  incremented, and to the fast_lock metering and status commands;
	  pnotices for the dm_display_version and dm_set_free_area modules;
	  fixes to transaction_manager_ user shutdown to turn off the
	  shutdown timers; a fix to the file_manager_ to correctly
	  report via $suffix_info that max_length, dumper_switches and
	  safety_switch are not supported; and, a fix to dm file creation to
	  get the correct IACLs and to ignore the "e" access in the IACLs
	  (if any).
04/19/85 by Lindsey Spratt:  Changed version to "5.3".  This version has a fix
	  to im_basic_search to fix some problems with relative searches.
05/03/85 by Lindsey Spratt:  Changed version to "5.4".  This version has fixes
	  to the sys_pn_tbl management (in the file_manager_); fixes to
	  bjm_rollback, fixes to the handling of rollback handlers for
	  delete and create of DM files; adds dm_relation_status to the
	  tools; and, retains dm_vector_util_.
05/07/85 by Lindsey Spratt:  Changed version to "5.5".  This version has fixes
	  to file_manager_ to remove pages of a deleted DM file from the
	  "modified pages" list kept by the file_manager_.
05/15/85 by Lindsey Spratt:  Changed version to "5.6".  This version includes
	  the changes to use a dm_journal_seg_ which has ring brackets of
	  0,0,0 (it was readable (and read) in ring 2).
05/16/85 by Lindsey Spratt:  Changed version to "5.7".  This version includes
	  the change to tm_per_process_init_ to not init the dm_journal_seg_.
*/
/* format: style2,^inddcls,linecom,ifthendo,ifthen,^indnoniterdo,dclind5,idind35 */
%page;
dm_system_data_template_:
     proc ();

/* START OF DECLARATIONS */

/*  Automatic  */

dcl  code				fixed bin (35);
dcl  1 local_cds_args		aligned like cds_args;

dcl  1 system_data			aligned,		/* dm_system_data_template_$... */
       2 system_software_version	char (8) aligned,
       2 bootload_time		fixed bin (71),	/* Time of system bootload */
       2 initialization_time		fixed bin (71),	/* Time this data was initialized */
       2 initializer_event_chn	fixed bin (71),	/* channel for telling management process things */
       2 initialized		bit (1) aligned,	/* ON => data has been initialized */
       2 initializer_processid	bit (36) aligned,	/* Process ID who initialized */
       2 initializer_name		char (32) aligned,	/* Person.Project who initialized */
       2 initializer_wakeup_increment	fixed bin,	/* time in MINUTES that daemon waits for request */
       2 max_n_transactions		fixed bin,	/* Number of entries in various transaction tables */
       2 bj_max_n_journals		fixed bin,	/* Number of before journals per system */
       2 bj_max_n_processes		fixed bin,	/* Number of process to use before journals */
       2 bj_default_journal_size	fixed bin,	/* size in control intervals of before */
						/* journal made by daemon for system */
       2 area_rel			bit (18) aligned,	/* Offset of area  */
       2 fast_lock_data_rel		bit (18) aligned,	/* Offset of fast_lock_data */
       2 lock_ips_mask		bit (36) aligned,	/* IPS Mask to allow  QUIT, CPUT, ALRM */
       2 tm_tdt_relp		bit (18) aligned,
       2 bj_txt_relp		bit (18) aligned,
       2 lock_list_meters_rel		bit (18) aligned,	/* offset of lock_list_meters */
       2 meter_fast_locks		bit (1) aligned,
       2 log_proc_terms		bit (1) aligned,
       2 bj_default_dirname		char (168) aligned,
       2 bj_default_filename		char (32) aligned,
       2 current_dm_state		char (4) aligned,	/* SHUTDOWN INFORMATION: step delays, step times, and reason.
The delays are between each step of shutdown and they all default
to five minutes.  The times are absolute times for each shutdown
step and are set by the caretaker Daemon when it recieves the
system_shutdown_scheduled_ IPS or a request from the
operator/administrator interface.  The saved_* shutdown values
are set by the latter interface; if the Daemon has to choose
between the shutdown times in the saved values and the defaults
that are applied when a Multics shutdown is scheduled, the set of
times with the earliest user bump time is used.  There is
currently no requirement that the times must be in ascending
order; e.g., "user shutdown" can occur before "user warning",
etc.
*/
       2 (					/* delay between ... */
       begin_shutdown_delay,				/* user warning and begin shutdown */
       user_shutdown_delay,				/* begin shutdown and user shutdown */
       user_bump_delay,				/* user shutdown and bumping users */
       daemon_logout_delay,				/* bumping users and force Daemon logout */
						/* when to... */
       user_warning_time,				/* ...have user processes signal dm_shutdown_warning_ */
       begin_shutdown_time,				/* ...stop new transactions */
       user_shutdown_time,				/* ...have user processes signal dm_user_shutdown_ */
       user_bump_time,				/* ...bump users */
       daemon_logout_time,				/* ...force Daemon logout */
						/* operator/admin specified times */
       saved_user_warning_time,			/* ...have user processes signal dm_shutdown_warning_ */
       saved_begin_shutdown_time,			/* ...stop new transactions */
       saved_user_shutdown_time,			/* ...have user processes signal dm_user_shutdown_ */
       saved_user_bump_time,				/* ...bump users */
       saved_daemon_logout_time			/* ...force Daemon logout */
       )				fixed bin (71),
       2 (					/* why, oh why must we leave? */
       shutdown_reason,
       saved_shutdown_reason
       )				char (64) aligned,
       2 command_ms_name		char (32);	/* name in per-AIM dir of msg seg for talking to Daemon */

/* Constant */
dcl  (
     EXCLUDE_ARRAY			(1) char (32) init ("pad*"),
     MYNAME			char (24) init ("dm_system_data_template_")
     )				int static options (constant);

/*  Entry  */
dcl  com_err_			entry options (variable),
     create_data_segment_		entry (ptr, fixed bin (35));

/*  Builtin  */
dcl  (addr, hbound, null, size, unspec) builtin;
%page;
	unspec (system_data) = ""b;

	system_data.system_software_version = "5.9";
	system_data.initializer_wakeup_increment = 15;
	system_data.begin_shutdown_delay,		/* default shutdown delays are five minutes */
	     system_data.user_shutdown_delay, system_data.user_bump_delay, system_data.daemon_logout_delay =
	     5 * 60 * (10 ** 6);
	system_data.shutdown_reason, system_data.saved_shutdown_reason = "NO REASON FOR SHUTDOWN GIVEN";
	system_data.bj_max_n_journals = 20;
	system_data.bj_max_n_processes = 256;
	system_data.bj_default_journal_size = 4000;
	system_data.max_n_transactions = 128;
	system_data.current_dm_state = DM_SYSTEM_STATE_UNDEFINED;
	system_data.meter_fast_locks = "1"b;

/*
   NOTE:  the pathname of the journal used by processes in lieu of an
   explicitly chosen one (this pathname being known as the "default path")
   is set at DM bootload time by the procedure "dm_load_configuration_".
*/

	system_data.bj_default_dirname = "";
	system_data.bj_default_filename = "";

	unspec (local_cds_args) = ""b;

	local_cds_args.sections (1).p = addr (system_data);
	local_cds_args.sections (1).len = size (system_data);
	local_cds_args.sections (1).struct_name = "system_data";
	local_cds_args.seg_name = MYNAME;
	local_cds_args.exclude_array_ptr = addr (EXCLUDE_ARRAY);
	local_cds_args.num_exclude_names = hbound (EXCLUDE_ARRAY, 1);
	local_cds_args.switches.have_text = "1"b;

	call create_data_segment_ (addr (local_cds_args), code);
	if code ^= 0 then
	     call com_err_ (code, MYNAME);

	return;

/* end dm_system_data_template_; */
%page;
%include cds_args;
%page;
%include dm_system_states;


     end dm_system_data_template_;
