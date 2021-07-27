/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1983 *
   *                                                         *
   *********************************************************** */
/* format: off */

/* Modified December 1984 by Bonnie Braun to add unpaged_page_tables. */
/* Modified December 1984 by Robert Coren to put tty_buf in a begin block, and to
   add these notices. */

structure_library_5_:
     procedure ();

/* First come all the structures */

dcl  sdwp pointer;
%include "sdw.adp";	call add ("adp_sdw", addr (p -> adp_sdw));
%include "sdw.l68";	call add ("l68_sdw", addr (p -> l68_sdw));
%include sdw_info;	call add ("sdw_info", addr (p -> sdw_info));
%include segdamage_msg; 
   call add ("segdamage", addr (segdamage)); /* NOT BASED */
%include signaller_stack; 
   call add ("signaller_stack", addr (p -> signaller_stack));
begin;
   %include slt;
      call add ("slt", addr (p -> slt));
      dcl 1 slt_name_seg aligned like name_seg based;
      call add ("slt_name_seg", addr (p -> slt_name_seg));
      dcl 1 slt_segname aligned like segnam based;
      call add ("slt_segname", addr (p -> slt_segname));
      dcl 1 slt_path aligned like path based;
      call add ("slt_path", addr (p -> slt_path));
      dcl 1 slt_acls aligned like acls based;
      call add ("slt_acls", addr (p -> slt_acls));
      end;
%include slte;	call add ("slte", addr (p -> slte));
%include sst;	call add ("sst", addr (p -> sst));
%include sstnt;     call add ("sstnt", addr (p -> sstnt));
%include stack_0_data; 
   dcl 1 stack_0_data aligned like sdt based;
   call add ("stack_0_data", addr (p -> stack_0_data));
   call add ("sdte", addr (p -> sdte));
%include stack_frame; call add ("stack_frame", addr (p -> stack_frame));
%include stack_header; call add ("stack_header", addr (p -> stack_header));
%include str;
   dcl 1 segment_trailer aligned like str based;
   call add ("segment_trailer", addr (p -> segment_trailer));
%include syserr_data;
   dcl 1 syserr_data aligned like sd based;
   call add ("syserr_data", addr (p -> syserr_data));
   dcl 1 wired_syserr_log aligned like wlog based;
   call add ("wired_syserr_log", addr (p -> wired_syserr_log));
   dcl 1 wired_syserr_message aligned like wmess based;
   call add ("wired_syserr_message", addr (p -> wired_syserr_message));
%include syserr_log_dcls;
   call add ("syserr_log_data", addr (p -> syserr_log_data));

%include tcb; 	call add ("tcb", addr (p -> tcb));
begin;
     %include tcm;
     %include hc_lock;
   dcl 1 tc_data aligned like tcm based;
   call add ("tc_data", addr (p -> tc_data));
   call add ("tcm", addr (p -> tcm));
   call add ("wct_entry", addr (p -> wct_entry));
end;
begin; /* because tty_buf includes hc_fast_lock */
   %include tty_buf;
      call add ("tty_buf", addr (p -> tty_buf));
      end;
begin; /* because blockp is declared already somewhere else */
   %include tty_buffer_block;
      dcl 1 tty_buffer aligned like buffer based;
      call add ("tty_buffer", addr (p -> tty_buffer));
      dcl 1 free_tty_buffer aligned like free_block based;
      call add ("free_tty_buffer", addr (p -> free_tty_buffer));
      end;

%include tty_tables;
   call add ("tty_tables_hdr", addr (p -> tty_tables_hdr));

%include unpaged_page_tables;
   dcl 1 iupt aligned like upt based;
   dcl 1 iupte aligned like upt_entry based;
   call add ("upt", addr (p -> upt));
   call add ("upt_entry", addr (p -> upt_entry));
   call add ("iupte", addr (p -> iupte));
   call add ("iupt", addr (p -> iupt));

%include vol_map;	call add ("vol_map", addr (p -> vol_map));
%include volume_registration;
   call add ("volume_registration", addr (p -> volume_registration));
%include vtoc_buffer; call add ("vtoc_buffer", addr (p -> vtoc_buffer));
%include vtoc_header; call add ("vtoc_header", addr (p -> vtoc_header));
%include vtoce;	call add ("vtoce", addr (p -> vtoce));

%include wire_proc_data;
   dcl 1 wire_proc_data aligned like wpd based;
   call add ("wire_proc_data", addr (p -> wire_proc_data));

%include wtcb;	call add ("wtcb", addr (p -> wtcb));

/* Followed by the include file containing the code to do the work */

dcl  WHOAMI char (32) internal static options (constant) init ("structure_library_5_");

%include structure_library_code;

	end structure_library_5_;
