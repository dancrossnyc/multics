/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1983 *
   *                                                         *
   *********************************************************** */
gtss_ext_:proc;

/* Generate object for "gtss_ext_" data.

   Author:    Dave Ward 1981
   Modified:  Ron Barstad  83-07-21  Remove dependency on ted_com, add include file
 */
/** Initialize cds_args **/

	cds_args_ptr=addr(space_for_cds_args);
	unspec(space_for_cds_args)="0"b;

/** Reference structure input to cds to assure it is in runtime table. **/

	if addr(gtss_ext_)=null() then ;

/** No text section **/
	cds_args_ptr -> cds_args.sections (1).p = null ();
	cds_args_ptr -> cds_args.sections (1).len = 0;
	cds_args_ptr -> cds_args.sections (1).struct_name = "NO_TEXT";

/** Static section **/
	cds_args_ptr -> cds_args.sections (2).p = addr (gtss_ext_);	/* Caller's data. */
	cds_args_ptr -> cds_args.sections (2).len = size (gtss_ext_);	/* No. words in data structure. */
	cds_args_ptr -> cds_args.sections (2).struct_name = "gtss_ext_";

	cds_args_ptr -> cds_args.seg_name = "gtss_ext_";	/* Entryname of object segment. */
	cds_args_ptr -> cds_args.num_exclude_names = 0;		/* All level 2 names are entry points. */
	cds_args_ptr -> cds_args.exclude_array_ptr = null ();
	cds_args_ptr -> cds_args.switches.defs_in_link = "0"b;	/* Definitions contiguous to text section. */
	cds_args_ptr -> cds_args.switches.separate_static = "0"b;	/* Static in linkage section (to bind). */
	cds_args_ptr -> cds_args.switches.have_text = "0"b;	/* No text section. */
	cds_args_ptr -> cds_args.switches.have_static = "1"b;	/* There is a static section. */
	cds_args_ptr -> cds_args.switches.pad = "0"b;		/* Must be zeroes (see create_data_segment_). */

	call create_data_segment_ (cds_args_ptr, code);
	if code ^= 0 
	   then 
	      call com_err_ (code, "cds_gtss_ext_");
	   else 
	      call com_err_( 0,"gtss_ext_","Object for gtss_ext_ created [^i words].",size(gtss_ext_));

	return;
%page;
/** Data for cds **/
dcl  addr                     builtin;
dcl  cds_args_ptr             ptr init(null());
dcl  code                     fixed bin(35);
dcl  com_err_                 entry options(variable);
dcl  create_data_segment_     entry(ptr,fixed bin(35));
dcl  null                     builtin;
dcl  size                     builtin;
dcl  unspec                   builtin;
dcl  1 space_for_cds_args     aligned like cds_args;
%page;
/** This data structure must exactly match that of gtss_ext_.incl.pl1 **/

dcl 1 gtss_ext_ aligned,
     2 aem                    fixed bin,
     2 bad_drl_rtrn           label,
     2 db                     (72) bit (1) unal,
     2 deferred_catalogs_ptr  ptr,
     2 dispose_of_drl         label,
     2 drl_rtrn               (4) label,
     2 drm_path               char (168),
     2 drun_jid               char (5),
     2 event_channel          fixed bin (71),
     2 finished               label,
     2 gdb_name               char (8),
     2 get_line               entry (ptr,ptr,fixed bin(21),fixed bin(21),fixed bin(35))variable,
     2 gtss_slave_area_seg    (4) ptr,
     2 hcs_work_area_ptr      ptr,
     2 homedir                char (64),
     2 last_k_was_out         bit (1) aligned,
     2 pdir                   char (168) varying,
     2 popup_from_pi          label,
     2 process_type           fixed bin (17),
     2 put_chars              entry (ptr,ptr,fixed bin(24),fixed bin(35)) variable,
     2 restart_from_pi        label,
     2 restart_seg_ptr        ptr,
     2 sig_ptr                ptr,
     2 stack_level_           fixed bin,
     2 suspended_process      bit (1),
     2 SYstarstar_file_no     fixed bin (24),
     2 user_id                char (26) var,
     2 work_area_ptr          ptr,

     2 CFP_bits aligned       like gtss_ext_$CFP_bits,

     2 com_reg aligned        like gtss_ext_$com_reg,

     2 flags aligned          like gtss_ext_$flags,

     2 statistics aligned     like gtss_ext_$statistics,

     2 aft aligned            like gtss_ext_$aft,

     2 ppt                    ptr,

     2 fast_lib aligned       like gtss_ext_$fast_lib,

     2 mcfc	  aligned like gtss_ext_$mcfc;

%page;
%include gtss_ext_;
%page;
%include cds_args;
end;
