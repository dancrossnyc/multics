/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1983 *
   *                                                         *
   *********************************************************** */

/* DESCRIPTION:

   This is is the system wide table of requests that can be used to signal
   the  DMS  caretaker  Daemon.   The values of actions are sparse to enable
   grouping  of  similar  functions;  as  a new function is added, it can be
   conceptually placed with other actions of the same type.
*/

/* HISTORY:
   Written by Mike Pandolf, 10/28/82.
   Modified:
   11/04/82 by M. Pandolf:  to add bury_dead_process action.
   11/19/82 by M. Pandolf:  to add new_proc action.
   01/25/83 by M. Pandolf:  to add adjust_tdt_entry action and
   to rename bury to adjust_process_id.
   03/11/83 by M. Pandolf:  to add group VI actions.
   06/07/83 by M. Pandolf:  to add flush_journals.
   07/26/83 by M. Pandolf:  to add process_command.
   09/27/84 by Lee A. Newcomb:  added new_process_notificaitons and used DMS
   standard format.
   12/05/84 by R. Michael Tague: removed noop, flush_journals, list_ref_names,
   terminate_segno, and process_command.
   01/22/85 by R. Michael Tague: Added kill_txn.
*/

/* format: style5 */
dm_request_action_:
        procedure ();

/* START OF DECLARATIONS */

/* Parameter */
/* Automatic */
        dcl     (
	      code		fixed bin (35),
	      1 local_cds_args	aligned like cds_args
	      )			automatic,
	      1 request_actions	aligned automatic,
	        2 (shutdown, new_proc, adjust_tdt, adjust_txn,
		  adjust_tdt_entry, kill_txn, adjust_process_id,
		  new_process_notifications)
				fixed bin (17);

/*  Builtin  */
        dcl     (addr, hbound, size, unspec)
				builtin;

/*  Constant  */
        dcl     (
	      EXCLUDE_ARRAY		(1) char (32) init ("pad*"),
	      MYNAME		char (32) init ("dm_request_action_")
	      )			int static options (constant);

/*  Entry  */
        dcl     (
	      com_err_		entry options (variable),
	      create_data_segment_	entry (ptr, fixed bin (35))
	      );

/* END OF DECLARATIONS */
%page;
/* Group I - system action wakeup types */

        request_actions.shutdown = 102;
        request_actions.new_proc = 103;

/* Group II - transaction specific action wakeup types */

        request_actions.adjust_tdt = 201;
        request_actions.adjust_txn = 202;
        request_actions.adjust_tdt_entry = 203;
        request_actions.kill_txn = 204;

/* Group III - before journal specific action wakeup types */

/* Group IV - data management file specific action wakeup types */

/* Group V - general process cleanup & new process interfaces */

        request_actions.adjust_process_id = 501;
        request_actions.new_process_notifications = 502;

/* Group VI - special wakeup types */

/* Initialize cds structure */

        unspec (local_cds_args) = ""b;

        local_cds_args.sections (1).p = addr (request_actions);
        local_cds_args.sections (1).len = size (request_actions);
        local_cds_args.sections (1).struct_name = "request_actions";
        local_cds_args.seg_name = MYNAME;
        local_cds_args.exclude_array_ptr = addr (EXCLUDE_ARRAY);
        local_cds_args.num_exclude_names = hbound (EXCLUDE_ARRAY, 1);
        local_cds_args.switches.have_text = "1"b;

/* Call CDS to make the segment */

        call create_data_segment_ (addr (local_cds_args), code);
        if code ^= 0 then
	      call com_err_ (code, MYNAME);

        return;

/* end of dm_request_action_; */
%page;
%include cds_args;


        end dm_request_action_;
