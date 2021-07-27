/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1983 *
   *                                                         *
   *********************************************************** */


/* HISTORY COMMENTS:
  1) change(87-04-26,GDixon), approve(87-05-01,MCR7741),
     audit(87-05-07,Parisek), install(87-08-04,MR12.1-1055):
     Upgraded for change to answer_table.incl.pl1.
                                                   END HISTORY COMMENTS */


/* format: off */

structure_library_1_:
     procedure ();

/* First come all the structures */

%include aim_template; call add ("aim_template", addr (p -> aim_template));
begin;
   %include answer_table;
   %include user_table_header;
      dcl 1 answer_table aligned like anstbl based;
      call add ("answer_table", addr (p -> answer_table));
   end;
%include apte; 	call add ("apte", addr (p -> apte));
%include area_structures; 
   call add ("area_header", addr (p -> area_header));
   call add ("extend_block", addr (p -> extend_block));
   dcl 1 area_block aligned like block based; /* better name */
   call add ("area_block", addr (p -> area_block));
%include ast_lock_meters;
   call add ("ast_lock_meters", addr (p -> ast_lock_meters));
%include aste;	call add ("aste", addr (p -> aste));

%include bos_dump;
   dcl 1 bos_dump aligned like dump based;	/* better name */
   call add ("bos_dump", addr (p -> bos_dump));

%include cdt; 	call add ("cdt", addr (p -> cdt));
%include author_dcl; /* Needed for cdt */
%include condition_info;
   call add ("condition_info", addr (p -> condition_info));
%include config_deck; 
   config_n_cards, config_max_cards = 0;
   call add ("config_deck", addr (p -> config_deck));
   call add ("config_card", addr (p -> config_card));
%include cmp; 
   call add ("cme", addr (p -> cme));
   call add ("mcme", addr (p -> mcme));
   call add ("cma", addr (p -> cma));

%include dbm;	call add ("dbm", addr (p -> dbm));
%include "dbr.adp";	call add ("adp_dbr", addr (p -> adp_dbr));
%include "dbr.l68";	call add ("l68_dbr", addr (p -> l68_dbr));
%include definition; call add ("definition", addr (p -> definition));

begin;  /* to avoid name conflicts with the poorly named directory stuff */
   %include dir_allocation_area;  
      dcl 1 dir_allocation_area aligned like area based;
      call add ("dir_allocation_area", addr (p -> dir_allocation_area));
   %include dir_acl;
      dcl 1 dir_acl_entry aligned like acl_entry based;
      dcl 1 dir_access_name aligned like access_name based;
      call add ("dir_acl_entry", addr (p -> dir_acl_entry));
      call add ("dir_access_name", addr (p -> dir_access_name));
   %include dir_entry;
      dcl 1 dir_entry aligned like entry based;
      call add ("dir_entry", addr (p -> dir_entry));
   %include dir_header;  
      dcl 1 dir_header aligned like dir based;
      call add ("dir_header", addr (p -> dir_header));
   %include dir_ht;
      dcl 1 dir_hash_table aligned like hash_table based;
      call add ("dir_hash_table", addr (p -> dir_hash_table));
   %include dir_link; /* Can't "like" to this one because of refers */
      begin;
         dcl 1 dir_link aligned like link based;
         call add ("dir_link", addr (p -> dir_link));
         end;
   %include dir_name;
      dcl 1 dir_name aligned like names based;
      call add ("dir_name", addr (p -> dir_name));
   end;

/* Followed by the include file containing the code to do the work */

dcl  WHOAMI char (32) internal static options (constant) init ("structure_library_1_");

%include structure_library_code;

	end structure_library_1_;
