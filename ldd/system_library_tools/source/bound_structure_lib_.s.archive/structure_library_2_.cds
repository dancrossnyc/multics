/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1983 *
   *                                                         *
   *********************************************************** */


/* format: off */

/* Modified December 1984 by Bonnie Braun to add io_page_tables. */
/* Modified December 1984 by Eric Swenson for event_channel_name. */
/* Modified December 1984 by Robert Coren to add hc_fast_lock and these notices. */
/* Modified Jan 21 1985 by B. Braun to delete reference to fgbx.incl.pl1 and add reference to flagbox.incl.pl1 */


/* HISTORY COMMENTS:
  1) change(86-08-12,Kissel), approve(86-08-12,MCR7473),
     audit(86-10-20,Fawcett), install(86-11-03,MR12.0-1206):
     Modified ect_structures to support control point management.  These
     changes were really made in February 1985 by G. Palter.
  2) change(86-08-12,Kissel), approve(86-08-12,MCR7479),
     audit(86-10-20,Fawcett), install(86-11-03,MR12.0-1206):
     Recompiled due to ect_structure changes to support async event channels.
                                                   END HISTORY COMMENTS */


structure_library_2_:
     procedure ();

/* First come all the structures */

dcl  sp pointer; /* something seems to need this */

begin; /* There is a conflict with "lock" */
   %include dir_lock_seg_;
   %include hc_lock;
      call add ("dir_lock_seg", addr (p -> dir_lock_seg));
      call add ("dir_lock_seg_header", addr (p -> dir_lock_seg_header));
      call add ("dir_lock", addr (p -> dir_lock));
      dcl 1 fast_lock aligned like lock based;
      call add ("fast_lock", addr (p -> fast_lock));
   end;
%include disk_table;
   dcl 1 disk_table aligned like dt based;
   call add ("disk_table", addr (p -> disk_table));
   dcl 1 disk_table_entry aligned like dte based;
   call add ("disk_table_entry", addr (p -> disk_table_entry));
   dcl 1 disk_table_lv_entry aligned like lve based;
   call add ("disk_table_lv_entry", addr (p -> disk_table_lv_entry));
%include dn355_data;
   call add ("datanet_info", addr (p -> datanet_info));
   call add ("fnp_info", addr (p -> fnp_info));
%include dn355_mailbox;
   call add ("datanet_mbx", addr (p -> datanet_mbx));
   dcl 1 short_fnp_sub_mbx aligned like sub_mbx based;
   call add ("short_fnp_sub_mbx", addr (p -> short_fnp_sub_mbx));
   call add ("fnp_sub_mbx", addr (p -> fnp_sub_mbx));
   call add ("input_sub_mbx", addr (p -> input_sub_mbx));
%include dskdcl;
   call add ("disk_data", addr (p -> disk_data));
   call add ("disktab", addr (p -> disktab));
   call add ("disk_channel_table", addr (p -> disk_channel_table)); 
   call add ("quentry", addr (p -> quentry));
   call add ("chantab", addr (p -> chantab));
   call add ("devtab", addr (p -> devtab));

%include ect_structures;
   call add ("ect_header", addr (p -> ect_header));
   ecit_lth = 0;		/* avoids WARNING 307 */
   call add ("ecit", addr (p -> ecit));
   call add ("wait_channel", addr (p -> wait_channel));
   call add ("call_channel", addr (p -> call_channel));
   call add ("event_message", addr (p -> event_message));
   call add ("itt_message", addr (p -> itt_message));
   call add ("event_message_data", addr (p -> event_message_data));
   call add ("waiting_control_point", addr (p -> waiting_control_point));
%include event_channel_name;
   call add ("event_channel_name", addr (p -> event_channel_name));
%include event_call_info;
   call add ("event_call_info", addr (p -> event_call_info));
%include event_wait_info;
   call add ("event_wait_info", addr (p -> event_wait_info));

%include fault_vector;
   dcl 1 fault_vector aligned like fv based;
   call add ("fault_vector", addr (p -> fault_vector));
%include flagbox;
   call add ("fgbx", addr (p -> fgbx));
%include fs_vol_label;
   dcl 1 disk_label aligned like label based;
   call add ("disk_label", addr (p -> disk_label));

%include hc_fast_lock;
   call add ("hc_fast_lock", addr (p -> hc_fast_lock));

begin;	/* to avoid conflict with lock_ptr in hc_fast_lock */
  %include io_page_tables;
     declare 1 io_page_table_256 (0:255) aligned like io_ptw based;
     declare 1 io_page_table_64 (0:63) aligned like io_ptw based;
     call add ("io_page_tables", addr (p -> io_page_tables));
     call add ("io_page_table_256", addr (p -> io_page_table_256));
     call add ("io_page_table_64", addr (p -> io_page_table_64));
     call add ("io_ptw", addr (p -> io_ptw));
  end;

%include io_special_status;
   call add ("io_special_status", addr (p -> io_special_status));
%include io_status;
   dcl 1 io_status aligned like status based;
   call add ("io_status", addr (p -> io_status));
%include io_status_entry;
   call add ("io_status_entry", addr (p -> io_status_entry));
   call add ("io_status_word", addr (p -> io_status_word));
%include io_syserr_msg; call add ("io_msg", addr (p -> io_msg));
%include iocbx; 	call add ("iocb", addr (p -> iocb));
begin; /* There is a conflict with "dte" */
   %include ioi_data;
      call addx ("ioi_data", addr (p -> ioi_data));
      call addx ("gte", addr (p -> gte)); /* addx because of conflict with */
      call addx ("cte", addr (p -> cte)); /* add in ioi_data! */
      call addx ("dte", addr (p -> dte));
      end;
%include iom_data;
   call add ("iom_data", addr (p -> iom_data));
   dcl 1 iom_per_device aligned like per_device based;
   call add ("iom_per_device", addr (p -> iom_per_device));
   dcl 1 iom_per_iom aligned like per_iom based;
   call add ("iom_per_iom", addr (p -> iom_per_iom));
   call add ("iom_mailbox_seg", addr (p -> iom_mailbox_seg));
   call add ("iom_mailbox", addr (p -> iom_mailbox));
   call add ("channel_mailbox", addr (p -> channel_mailbox));

/* Followed by the include file containing the code to do the work */

dcl  WHOAMI char (32) internal static options (constant) init ("structure_library_2_");

%include structure_library_code;

	end structure_library_2_;
