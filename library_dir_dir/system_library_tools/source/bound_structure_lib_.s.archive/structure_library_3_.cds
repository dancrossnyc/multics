/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1983 *
   *                                                         *
   *********************************************************** */
/* format: off */

structure_library_3_:
     procedure ();

/* First come all the structures */

%include iom_dcw;
   dcl 1 iom_ddcw aligned like dcw based;
   call add ("iom_ddcw", addr (p -> iom_ddcw));
   dcl 1 iom_tdcw aligned like tdcw based;
   call add ("iom_tdcw", addr (p -> iom_tdcw));
%include iom_lpw;
   dcl 1 iom_lpw aligned like lpw based;
   call add ("iom_lpw", addr (p -> iom_lpw));
   dcl 1 iom_lpw_ext aligned like lpw_ext based;
   call add ("iom_lpw_ext", addr (p -> iom_lpw_ext));
%include iom_pcw;
   dcl 1 iom_pcw aligned like pcw based;
   call add ("iom_pcw", addr (p -> iom_pcw));
   dcl 1 iom_idcw aligned like idcw based;
   call add ("iom_idcw", addr (p -> iom_idcw));
%include iom_scw;
   dcl 1 iom_scw aligned like scw based;
   call add ("iom_scw", addr (p -> iom_scw));
%include its;
   call add ("its", addr (p -> its));
   call add ("its_unsigned", addr (p -> its_unsigned));
   call add ("itp", addr (p -> itp));
   call add ("itp_unsigned", addr (p -> itp_unsigned));
%include itt_entry;	call add ("itt_entry", addr (p -> itt_entry));

%include kst;
   call add ("kst", addr (p -> kst));
   call add ("kste", addr (p -> kste));

%include lct;
   call add ("lct", addr (p -> lct));
   call add ("lcte", addr (p -> lcte));
begin;
   %include linkdcl;
      dcl 1 link_pair aligned like link based;
      call add ("link_pair", addr (p -> link_pair));
      dcl 1 link_exp_word aligned like exp_word based;
      call add ("link_exp_word", addr (p -> link_exp_word));
      dcl 1 link_type_pair aligned like type_pair based;
      call add ("link_type_pair", addr (p -> link_type_pair));
      dcl 1 linkage_header aligned like header based;
      call add ("linkage_header", addr (p -> linkage_header));
      call add ("virgin_linkage_header", addr (p -> virgin_linkage_header));
      dcl 1 link_trap_word aligned like trap_word based;
      call add ("link_trap_word", addr (p -> link_trap_word));
      end;
%include lot;
   call add ("lot", addr (p -> lot));
   call add ("isot", addr (p -> isot));
   call add ("isot1", addr (p -> isot1));
%include lvt; 
   call add ("lvt", addr (p -> lvt));
   call add ("lvte", addr (p -> lvte));

/* Followed by the include file containing the code to do the work */

dcl  WHOAMI char (32) internal static options (constant) init ("structure_library_3_");

%include structure_library_code;

	end structure_library_3_;
