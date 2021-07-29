/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1988                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1983 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(88-06-03,Parisek), approve(88-06-10,MCR7920),
     audit(88-06-23,Hunter), install(87-07-05,MR12.2-1053):
     - Change reference of pitmsg.incl.pl1 to pit.incl.pl1 as the
       pitmsg.incl.pl1 name was removed from pit.incl.pl1 in MR12.1.
     - Declare builtins.
                                                   END HISTORY COMMENTS */

/* format: off */

structure_library_4_:
     procedure ();

/* First come all the structures */

%include mc;
   call add ("mc", addr (p -> mc));
   call add ("scu", addr (p -> scu));
   call add ("scux", addr (p -> scux));
%include mc_trace_buf; call add ("mc_trace_buf", addr (p -> mc_trace_buf));
%include mcs_modes_change_list;
   dcl 1 modes_change_list aligned like mcl based;
   call add ("modes_change_list", addr (p -> modes_change_list));
   dcl 1 modes_change_list_entry aligned like mcle based;
   call add ("modes_change_list_entry", addr (p -> modes_change_list_entry));
%include mcs_trace_data;
   dcl 1 mcs_trace_array aligned like trace_array based;
   call add ("mcs_trace_array", addr (p -> mcs_trace_array));
   dcl 1 mcs_trace_entry aligned like trace_entry based;
   call add ("mcs_trace_entry", addr (p -> mcs_trace_entry));
%include mdcs;
   call add ("mdcs", addr (p -> mdcs));
   dcl 1 mdcs_mdir aligned like mdirent based;
   call add ("mdcs_mdir", addr (p -> mdcs_mdir));
   dcl 1 mdcs_account aligned like acctent based;
   call add ("mdcs_account", addr (p -> mdcs_account));
   dcl 1 mdcs_path aligned like pathent based;
   call add ("mdcs_path", addr (p -> mdcs_path));
%include mstr;
   call add ("mst_label", addr (p -> mst_label));
   call add ("mstr_header", addr (p -> mstr_header));
   call add ("mstr_trailer", addr (p -> mstr_trailer));
   dcl 1 mst_volume_id aligned like volume_identifier based;
   call add ("mst_volume_id", addr (p -> mst_volume_id));

%include oc_data; 	call add ("oc_data", addr (p -> oc_data));
%include oc_log_meters;
   dcl 1 oc_log_meters aligned like olm based;
   call add ("oc_log_meters", addr (p -> oc_log_meters));

%include pathname_am;
   dcl 1 pathname_am aligned like pam based;
   call add ("pathname_am", addr (p -> pathname_am));
%include pcb; 	call add ("pcb", addr (p -> pcb));
begin;
   %include pit;
   %include user_attributes;
      call add ("pit", addr (p -> pit));
   end;
dcl  ptp pointer;
%include "ptw.adp";	call add ("adp_core_ptw", addr (p -> adp_core_ptw));
		call add ("adp_ptw", addr (p -> adp_ptw));
%include "ptw.l68";	call add ("l68_core_ptw", addr (p -> l68_core_ptw));
		call add ("l68_ptw", addr (p -> l68_ptw));
%include pv_holdt;  call add ("pv_holdt", addr (p -> pv_holdt));
%include pvt;
   call add ("pvt", addr (p -> pvt));
%include pvte;
   call add ("pvte", addr (p -> pvte));
   call add ("pvt_array", addr (p -> pvt_array));

%include rcp_com_seg;
   call add ("rcs", addr (p -> rcs));
   call add ("rcse", addr (p -> rcse));
%include rcp_data;
   call add ("rcpd", addr (p -> rcpd));
   dcl 1 rcp_dtype aligned like dtype based;
   call add ("rcp_dtype", addr (p -> rcp_dtype));
   dcl 1 rcp_device aligned like device based;
   call add ("rcp_device", addr (p -> rcp_device));
   dcl 1 rcp_volume aligned like volume based;
   call add ("rcp_volume", addr (p -> rcp_volume));
%include rnt;
   call add ("rnt", addr (p -> rnt));
   call add ("rnte", addr (p -> rnte));

/* Followed by the include file containing the code to do the work */

dcl  WHOAMI char (32) internal static options (constant) init ("structure_library_4_");
dcl  (divide, hbound, pointer) builtin;

%include structure_library_code;

	end structure_library_4_;
